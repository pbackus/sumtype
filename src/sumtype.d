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

template match(handlers...)
{
	auto match(Self : SumType!Types, Types...)(Self self)
	{
		pure static int[Types.length] getHandlerIndices()
		{
			import std.traits: Parameters, Unqual;

			int[Types.length] result;

			foreach (i, T; Types) {
				result[i] = -1;
				foreach (j, h; handlers) {
					if (is(typeof(h(T.init))) &&
					    is(Unqual!T == Unqual!(Parameters!h[0]))) {
						if (result[i] == -1) {
							result[i] = j;
						} else {
							assert(false,
								"multiple handlers given for type " ~ T.stringof);
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
				} else {
					static assert(false, "missing handler for type " ~ T.stringof);
				}
			}
		}
		assert(false); // unreached
	}
}

unittest {
	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	Foo y = Foo(3.14);

	assert(x.match!((int v) => true, (float v) => false));
	assert(y.match!((int v) => false, (float v) => true));
}

unittest {
	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	x = 3.14;
	assert(x.match!((float v) => true, (int v) => false));
}

unittest {
	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	assert(!__traits(compiles, x.match!((int x) => true)));
	assert(!__traits(compiles, x.match!((int x) => true, (int x) => false)));
}

unittest {
	alias Foo = SumType!(int, float);
	int answer = 42;
	Foo x = Foo(42);
	Foo y = Foo(3.14);
	assert(x.match!((int v) => v == answer, (float v) => v == answer));
	assert(!y.match!((int v) => v == answer, (float v) => v == answer));
}
