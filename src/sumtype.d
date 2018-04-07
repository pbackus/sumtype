/++
A sum type for modern D.

[SumType] serves the same purpose as Phobos's `std.variant.Algebraic`, but is
free from some of `Algebraic`'s limitations; e.g., [SumType] can be used in
`pure`, `@safe`, and `@nogc` code without issue, provided the underlying types
allow it.

License: MIT
Author: Paul Backus
+/
module sumtype;

struct This;

/**
 * A tagged union that can hold a single value from any of a specified set of
 * types
 *
 * The stored value can be operated on using [match|pattern matching].
 *
 * The special type `This` can be used as a placeholder to create
 * self-referential types, just like with `Algebraic`.
 *
 * See_Also: `std.variant.Algebraic`
 */
struct SumType(TypesParam...)
{
private:

	import std.meta: AliasSeq, staticIndexOf;
	import std.typecons: ReplaceType;

	alias Types = AliasSeq!(ReplaceType!(This, typeof(this), TypesParam));

	int tag;

	union
	{
		Types values;
	}

	alias value(T) = values[staticIndexOf!(T, Types)];

public:

	static foreach (i, T; Types) {
		/// Constructs a [SumType] holding a specific value
		this(T val)
		{
			tag = i;
			value!T = val;
		}
	}

	import std.traits: isAssignable;

	static foreach (i, T; Types) {
		static if (isAssignable!T) {
			/// Assigns a value to a [SumType] that can hold it
			void opAssign(T rhs)
			{
				tag = i;
				value!T = rhs;
			}
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

// Self assignment
unittest {
	alias Foo = SumType!(int, float);

	Foo x = Foo(42);
	Foo y = Foo(3.14);
	y = x;

	assert(y.tag == 0);
	assert(y.value!int == 42);
}

// Imported types
unittest {
	import std.typecons: Tuple;

	assert(__traits(compiles, (){ alias Foo = SumType!(Tuple!(int, int)); }));
}

// const and immutable types
unittest {
	assert(__traits(compiles, (){
		alias Foo = SumType!(const(int), immutable(float));
	}));
}

/// Recursive definition of a linked list:
unittest {
	import std.typecons: Tuple, tuple;

	struct Nil {}
	alias List = SumType!(
		Nil,
		Tuple!(int, "head", This*, "tail")
	);
	alias Cons = Tuple!(int, "head", List*, "tail");

	List* list(int[] items...)
	{
		if (items.length == 0)
			return new List(Nil());
		else
			return new List(Cons(items[0], list(items[1..$])));
	}

	int sum(List l)
	{
		return l.match!(
			(Nil _) => 0,
			(Cons cons) => cons.head + sum(*cons.tail)
		);
	}

	List* myList = list(1, 2, 3, 4, 5);

	assert(sum(*myList) == 15);
}

/**
 * Calls a type-appropriate handler with the value held in a [SumType].
 *
 * For each possible type the [SumType] can hold, the given handlers are
 * checked, in order, to see whether they accept a single argument of that type.
 * The first one that does is chosen as the match for that type. Implicit
 * conversions are not taken into account, so, for example, a handler that
 * accepts a `long` will not match the type `int`.
 *
 * Every type must have a matching handler. This is enforced at compile-time.
 *
 * Handlers may be functions, delegates, or objects with opCall overloads.
 * Templated handlers are also accepted, and will match any type for which they
 * can be instantiated.
 *
 * Returns:
 *   The value returned from the handler that matches the currently-held type.
 *
 * See_Also: `std.variant.visit`
 */
template match(handlers...)
{
	/**
	 * The actual `match` function.
	 *
	 * Params:
	 *   self = A [SumType] object
	 */
	auto match(Self : SumType!TypesParam, TypesParam...)(Self self)
	{
		alias Types = self.Types;

		pure static auto getHandlerIndices()
		{
			import std.meta: staticIndexOf;
			import std.traits: hasMember, isCallable, isSomeFunction, Parameters, Unqual;

			enum sameUnqual(T, U) = is(Unqual!T == Unqual!U);

			int[Types.length] indices;

			void setHandlerIndex(T)(int hid)
				if (staticIndexOf!(T, Types) >= 0)
			{
				int tid = staticIndexOf!(T, Types);

				if (indices[tid] == -1) {
					indices[tid] = hid;
				}
			}

			indices[] = -1;

			static foreach (T; Types) {
				static foreach (i, h; handlers) {
					// Regular handlers
					static if (isCallable!h && is(typeof(h(T.init)))) {
						// Functions and delegates
						static if (isSomeFunction!h) {
							static if (sameUnqual!(T, Parameters!h[0])) {
								setHandlerIndex!T(i);
							}
						// Objects with overloaded opCall
						} else static if (hasMember!(typeof(h), "opCall")) {
							static foreach (overload; __traits(getOverloads, typeof(h), "opCall")) {
								static if (sameUnqual!(T, Parameters!overload[0])) {
									setHandlerIndex!T(i);
								}
							}
						}
					// Generic handlers
					} else static if (is(typeof(h!T(T.init)))) {
						setHandlerIndex!T(i);
					}
				}
			}
			return indices;
		}

		enum handlerIndices = getHandlerIndices;

		final switch (self.tag) {
			static foreach (i, T; Types) {
				static if (handlerIndices[i] != -1) {
					case i:
						return handlers[handlerIndices[i]](self.value!T);
				} else {
					static assert(false, "No matching handler for type " ~ T.stringof);
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

	assert(x.match!((int v) => true, (float v) => false));
	assert(y.match!((int v) => false, (float v) => true));
}

// Missing handlers
unittest {
	alias Foo = SumType!(int, float);

	Foo x = Foo(42);

	assert(!__traits(compiles, x.match!((int x) => true)));
	assert(!__traits(compiles, x.match!()));
}

// No implicit converstion
unittest {
	alias Foo = SumType!(int, float);

	Foo x = Foo(42);

	assert(!__traits(compiles, x.match!((long v) => true, (float v) => false)));
}

// Handlers with qualified parameters
unittest {
	alias Foo = SumType!(int, float);

	Foo x = Foo(42);
	Foo y = Foo(3.14);

	assert(x.match!((const int v) => true, (const float v) => false));
	assert(y.match!((const int v) => false, (const float v) => true));
}

// Handlers for qualified types
unittest {
	alias Foo = SumType!(immutable(int), immutable(float));

	Foo x = Foo(42);

	assert(x.match!((immutable(int) v) => true, (immutable(float) v) => false));
	assert(x.match!((const(int) v) => true, (const(float) v) => false));
	assert(x.match!((immutable v) => true, v => false));
	assert(x.match!((const v) => true, v => false));
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

// Multiple non-overlapping generic handlers
unittest {
	import std.math: approxEqual;

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

/// Implicit matching:
unittest {
	import std.math: approxEqual, PI, sqrt;

	struct Cartesian { double x, y; }
	struct Polar { double r, theta; }
	alias Vector = SumType!(Cartesian, Polar);

	double length(Vector v)
	{
		// A branch matches if its right-hand side compiles
		return v.match!(
			cartesian => sqrt(cartesian.x^^2 + cartesian.y^^2),
			polar => polar.r
		);
	}

	Vector u = Cartesian(1, 1);
	Vector v = Polar(1, PI/4);

	assert(length(u).approxEqual(sqrt(2.0)));
	assert(length(v).approxEqual(1));
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

	assert(x.match!(handleInt, handleFloat));
	assert(!y.match!(handleInt, handleFloat));
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

	assert(x.match!handleBoth);
	assert(!y.match!handleBoth);
}

// Ordered matching
unittest {
	alias Foo = SumType!(int, float);

	Foo x = Foo(42);

	assert(x.match!((float v) => false, (int v) => true, (int v) => false));
	assert(x.match!(v => true, (int v) => false));
	assert(x.match!(v => 2*v, v => v + 1) == 84);
}
