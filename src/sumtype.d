module sumtype;



struct SumType(Types...)
{
private:

	import std.meta: staticIndexOf;
	import std.format: format;

	size_t tag;

	enum valueName(T) = "value%d".format(staticIndexOf!(T, Types));

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

	auto match(handlers...)()
	{
		import std.meta: anySatisfy, ApplyLeft, Filter;
		import std.traits: Parameters, Unqual;

		enum isHandlerFor(T, alias h) =
			is(typeof(h(T.init))) &&
			is(Unqual!T == Unqual!(Parameters!h[0]));

		alias handlersFor(T) = Filter!(ApplyLeft!(isHandlerFor, T), handlers);

		final switch (tag) {
			static foreach (i, T; Types) {
				static if (handlersFor!T.length == 1) {
					case i:
						return handlersFor!T[0](mixin(valueName!T));
				} else static if (handlersFor!T.length > 1) {
					static assert(false, "multiple handlers given for type " ~ T.stringof);
				} else static if (handlersFor!T.length == 0) {
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
	assert(!__traits(compiles, (){ x.match!((int x) => true); }()));
	assert(!__traits(compiles, (){ x.match!((int x) => true, (int x) => false); }()));
}
