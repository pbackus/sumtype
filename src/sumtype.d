module sumtype;

struct SumType(Types...)
{
private:

	int tag;

	union
	{
		import std.conv: to;

		mixin template declareValue(T)
		{
			T value;
		}

		static foreach (i, T; Types) {
			mixin("mixin declareValue!T type" ~ i.to!string ~ ";");
		}
	}

	template valueField(T)
	{
		import std.conv: to;
		import std.meta: staticIndexOf;

		mixin("alias valueField = type" ~ staticIndexOf!(T, Types).to!string ~ ".value;");
	}

public:

	static foreach (i, T; Types) {
		this(T val)
		{
			tag = i;
			valueField!T = val;
		}
	}

	static foreach (i, T; Types) {
		void opAssign(T rhs)
		{
			tag = i;
			valueField!T = rhs;
		}
	}
}

import std.traits: Parameters, Unqual;

private enum isHandlerFor(T, alias h) =
	is(typeof(h(T.init))) &&
	is(Unqual!T == Unqual!(Parameters!h[0]));

private enum isGenericHandlerFor(T, alias h) =
	is(typeof(h!T(T.init)));

template visit(handlers...)
{
	auto visit(Self : SumType!Types, Types...)(Self self)
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

		final switch (self.tag) {
			static foreach (i, T; Types) {
				static if (handlerIndices[i] != -1) {
					case i:
						return handlers[handlerIndices[i]](self.valueField!T);
				} else static if (handlerIndices.generic[i] != -1) {
					case i:
						return handlers[handlerIndices.generic[i]](self.valueField!T);
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

	assert(x.visit!((int v) => true, (float v) => false));
	assert(y.visit!((int v) => false, (float v) => true));
}

// Assignment
unittest {
	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	x = 3.14;
	assert(x.visit!((float v) => true, (int v) => false));
}

// Duplicate and missing handlers
unittest {
	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	assert(!__traits(compiles, x.visit!((int x) => true)));
	assert(!__traits(compiles, x.visit!((int x) => true, (int x) => false)));
}

// Handlers for qualified types
unittest {
	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	Foo y = Foo(3.14);
	assert(x.visit!((const int v) => true, (const float v) => false));
	assert(y.visit!((const int v) => false, (const float v) => true));
}

// Delegate handlers
unittest {
	alias Foo = SumType!(int, float);
	int answer = 42;
	Foo x = Foo(42);
	Foo y = Foo(3.14);
	assert(x.visit!((int v) => v == answer, (float v) => v == answer));
	assert(!y.visit!((int v) => v == answer, (float v) => v == answer));
}

// Generic handler
unittest {
	import std.math: approxEqual;

	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	Foo y = Foo(3.14);
	assert(x.visit!(v => v*2) == 84);
	assert(y.visit!(v => v*2).approxEqual(6.28));
}

// Fallback to generic handler
unittest {
	import std.conv: to;

	alias Foo = SumType!(int, float, string);
	Foo x = Foo(42);
	Foo y = Foo("42");
	assert(x.visit!((string v) => v.to!int, v => v*2) == 84);
	assert(y.visit!((string v) => v.to!int, v => v*2) == 42);
}

// Duplicate generic handlers
unittest {
	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	assert(!__traits(compiles, x.visit!(v => v*2, v => v + 1)));
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

	assert(x.visit!(v => v*2, v => v.length) == 84);
	assert(y.visit!(v => v*2, v => v.length).approxEqual(6.28));
	assert(w.visit!(v => v*2, v => v.length) == 3);
	assert(z.visit!(v => v*2, v => v.length) == 3);
}

// Imported types
unittest {
	import std.typecons: Tuple, tuple;

	assert(__traits(compiles, (){ alias Foo = SumType!(Tuple!(int, int)); }));
}
