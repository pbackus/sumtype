/++
A sum type for modern D.

[SumType] serves the same purpose as Phobos's [std.variant.Algebraic], but is
free from some of `Algebraic`'s limitations; e.g., [SumType] can be used in
`pure`, `@safe`, and `@nogc` code without issue, provided the underlying types
allow it.

License: MIT
Author: Paul Backus
+/
module sumtype;

/**
 * A tagged union that can hold a single value of any of the specified types.
 */
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

// Construction
unittest {
	alias Foo = SumType!(int, float);
	assert(__traits(compiles, (){ Foo x = Foo(42); }));
	assert(__traits(compiles, () { Foo y = Foo(3.14); }));
}

// Assignment
unittest {
	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	assert(__traits(compiles, (){ x = 3.14; }));
}

// Imported types
unittest {
	import std.typecons: Tuple;

	assert(__traits(compiles, (){ alias Foo = SumType!(Tuple!(int, int)); }));
}


/**
 * Applies a type-appropriate handler to the value stored in a [SumType].
 *
 * For each type `T` that could be stored in the `SumType`, there must be a
 * single, unambiguous matching handler. Matches are chosen at compile time
 * according to the following rules:
 *
 * $(NUMBERED_LIST
 *   * If exactly one non-generic handler accepts a single argument of type `T`
 *     that handler matches. Implicit conversions are not taken into account,
 *     so, for example, a handler that accepts `long` will not be chosen as a
 *     match for `int`.
 *
 *   * Otherwise, if exactly one generic handler accepts a single argument of
 *     type `T`, that handler matches.
 *
 *   * Otherwise, there is no match for type `T`.
 * )
 *
 * Handlers may be functions, delegates, or objects with opCall overloads.
 *
 * Returns:
 *   The value returned from the handler that matches the currently-stored
 *   type.
 *
 * See_Also: [std.variant.visit]
 */
template visit(handlers...)
{
	/**
	 * The actual `visit` function.
	 *
	 * Params:
	 *   self = a `SumType` object
	 */
	auto visit(Self : SumType!Types, Types...)(Self self)
	{
		pure static auto getHandlerIndices()
		{
			import std.meta: staticIndexOf;
			import std.traits: hasMember, isCallable, isSomeFunction, Parameters, Unqual;
			import std.typecons: Flag, No, Yes;

			enum sameUnqual(T, U) = is(Unqual!T == Unqual!U);

			struct Indices
			{
				int[Types.length] regular;
				int[Types.length] generic;
			}

			Indices result;

			void addHandlerIndex(T)(int hid, Flag!"generic" generic = No.generic)
				if (staticIndexOf!(T, Types) >= 0)
			{
				int[] indices = generic ? result.generic[] : result.regular[];
				int tid = staticIndexOf!(T, Types);

				if (indices[tid] == -1) {
					indices[tid] = hid;
				} else {
					assert(false,
						"multiple " ~ (generic ? "generic" : "non-generic")
						~ " handlers given for type " ~ T.stringof);
				}
			}

			static foreach (i, T; Types) {
				result.regular[i] = -1;
				result.generic[i] = -1;
				static foreach (j, h; handlers) {
					// Regular handlers
					static if (isCallable!h && is(typeof(h(T.init)))) {
						// Functions and delegates
						static if (isSomeFunction!h) {
							static if (sameUnqual!(T, Parameters!h[0])) {
								addHandlerIndex!T(j);
							}
						// Objects with overloaded opCall
						} else static if (hasMember!(typeof(h), "opCall")) {
							static foreach (overload; __traits(getOverloads, typeof(h), "opCall")) {
								static if (sameUnqual!(T, Parameters!overload[0])) {
									addHandlerIndex!T(j);
								}
							}
						}
					// Generic handlers
					} else static if (is(typeof(h!T(T.init)))) {
						addHandlerIndex!T(j, Yes.generic);
					}
				}
			}
			return result;
		}

		enum handlerIndices = getHandlerIndices;

		final switch (self.tag) {
			static foreach (i, T; Types) {
				static if (handlerIndices.regular[i] != -1) {
					case i:
						return handlers[handlerIndices.regular[i]](self.valueField!T);
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

// Matching
unittest {
	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	Foo y = Foo(3.14);
	assert(x.visit!((int v) => true, (float v) => false));
	assert(y.visit!((int v) => false, (float v) => true));
}

// Duplicate and missing handlers
unittest {
	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	assert(!__traits(compiles, x.visit!((int x) => true)));
	assert(!__traits(compiles, x.visit!((int x) => true, (int x) => false)));
	assert(!__traits(compiles, x.visit!()));
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

// Separate opCall handlers
unittest {
	struct IntHandler
	{
		bool opCall(int arg)
		{
			return true;
		}
	}

	struct FloatHandler
	{
		bool opCall(float arg)
		{
			return false;
		}
	}

	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	Foo y = Foo(3.14);
	IntHandler handleInt;
	FloatHandler handleFloat;
	assert(x.visit!(handleInt, handleFloat));
	assert(!y.visit!(handleInt, handleFloat));
}

// Compound opCall handler
unittest {
	struct CompoundHandler
	{
		bool opCall(int arg)
		{
			return true;
		}

		bool opCall(float arg)
		{
			return false;
		}
	}

	alias Foo = SumType!(int, float);
	Foo x = Foo(42);
	Foo y = Foo(3.14);
	CompoundHandler handleBoth;
	assert(x.visit!handleBoth);
	assert(!y.visit!handleBoth);
}
