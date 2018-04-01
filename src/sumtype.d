module sumtype;



struct SumType(Types...)
{
private:

	import std.meta: staticIndexOf;
	import std.conv: to;

	int tag;

	enum valueName(T) = "value" ~ staticIndexOf!(T, Types).to!string;

	union
	{
		static foreach (T; Types) {
			mixin(T.stringof ~ " " ~ valueName!T ~ ";");
		}
	}

public:

	static foreach (i, T; Types) {
		this(T val)
		{
			tag = i;
			mixin(valueName!T) = val;
		}
	}

	static foreach (i, T; Types) {
		void opAssign(T rhs)
		{
			tag = i;
			mixin(valueName!T) = rhs;
		}
	}
}

import std.traits: Parameters, Unqual;

private enum isHandlerFor(T, alias h) =
	is(typeof(h(T.init))) &&
	is(Unqual!T == Unqual!(Parameters!h[0]));

private enum isGenericHandlerFor(T, alias h) =
	is(typeof(h!T(T.init)));

template match(handlers...)
{
	auto match(Self : SumType!Types, Types...)(Self self)
	{
		pure static auto getHandlerIndices()
		{
			import std.traits: isCallable;

			struct Result
			{
				int[Types.length] indices;
				int[Types.length] generic;

				alias indices this;
			}

			Result result;

			static foreach (i, T; Types) {
				result[i] = -1;
				result.generic[i] = -1;

				static foreach (j, h; handlers) {
					static if (isCallable!h && isHandlerFor!(T, h)) {
						if (result[i] == -1) {
							result[i] = j;
						} else {
							assert(false,
									"multiple handlers given for type " ~ T.stringof);
						}
					} else static if (isGenericHandlerFor!(T, h)) {
						if (result.generic[i] == -1) {
							result.generic[i] = j;
						} else {
							assert(false,
								"multiple generic handlers match for type " ~ T.stringof);
						}
					}
				}
			}

			return result;
		}

		enum handlerIndices = getHandlerIndices;

		import std.meta: staticIndexOf;
		import std.conv: to;

		enum valueName(T) = "value" ~ staticIndexOf!(T, Types).to!string;

		final switch (self.tag) {
			static foreach (i, T; Types) {
				static if (handlerIndices[i] != -1) {
					case i:
						return handlers[handlerIndices[i]](mixin("self." ~ valueName!T));
				} else static if (handlerIndices.generic[i] != -1) {
					case i:
						return handlers[handlerIndices.generic[i]](mixin("self." ~ valueName!T));
				} else {
					static assert(false, "missing handler for type " ~ T.stringof);
				}
			}
		}
		assert(false); // unreached
	}
}

// Construction and basic matching
unittest {
	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	Foo y = Foo(3.14);

	assert(x.match!((int v) => true, (float v) => false));
	assert(y.match!((int v) => false, (float v) => true));
}

// Assignment
unittest {
	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	x = 3.14;
	assert(x.match!((float v) => true, (int v) => false));
}

// Duplicate and missing handlers
unittest {
	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	assert(!__traits(compiles, x.match!((int x) => true)));
	assert(!__traits(compiles, x.match!((int x) => true, (int x) => false)));
}

// Handlers for qualified types
unittest {
	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	Foo y = Foo(3.14);
	assert(x.match!((const int v) => true, (const float v) => false));
	assert(y.match!((const int v) => false, (const float v) => true));
}

// Delegate handlers
unittest {
	alias Foo = SumType!(int, float);
	int answer = 42;
	Foo x = Foo(42);
	Foo y = Foo(3.14);
	assert(x.match!((int v) => v == answer, (float v) => v == answer));
	assert(!y.match!((int v) => v == answer, (float v) => v == answer));
}

// Generic handler
unittest {
	import std.math: approxEqual;

	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	Foo y = Foo(3.14);
	assert(x.match!(v => v*2) == 84);
	assert(y.match!(v => v*2).approxEqual(6.28));
}

// Fallback to generic handler
unittest {
	import std.conv: to;

	alias Foo = SumType!(int, float, string);
	Foo x = Foo(42);
	Foo y = Foo("42");
	assert(x.match!((string v) => v.to!int, v => v*2) == 84);
	assert(y.match!((string v) => v.to!int, v => v*2) == 42);
}

// Duplicate generic handlers
unittest {
	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	assert(!__traits(compiles, x.match!(v => v*2, v => v + 1)));
}

// Multiple non-overlapping generic handlers
unittest {
	import std.math: approxEqual;
	import std.traits: isArray;

	alias Foo = SumType!(int, float, int[], char[]);
	Foo x = Foo(42);
	Foo y = Foo(3.14);
	Foo z = Foo([1, 2, 3]);
	Foo w = Foo(['a', 'b', 'c']);

	assert(x.match!(v => v*2, v => v.length) == 84);
	assert(y.match!(v => v*2, v => v.length).approxEqual(6.28));
	assert(w.match!(v => v*2, v => v.length) == 3);
	assert(z.match!(v => v*2, v => v.length) == 3);
}
