module sumtype;

import std.variant: maxSize;
import std.meta: staticIndexOf;
import std.format: format;


struct SumType(Types...)
{
private:
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
		import std.traits: Parameters;

		dispatch:
		switch (type) {
			static foreach (i, T; AllowedTypes)
				static foreach (h; handlers)
					static if (is(T == Parameters!h[0])) {
						case i:
							return h(mixin(valueName!T));
							break dispatch;
					}

			default: throw new Exception("Missing handler");
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
