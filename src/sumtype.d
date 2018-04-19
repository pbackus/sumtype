/++
A sum type for modern D.

[SumType] is an alternative to `std.variant.Algebraic` with improved
pattern-matching, full attribute correctness (`pure`, `@safe`, `@nogc`, and
`nothrow`), and no dependency on runtime type information (`TypeInfo`).

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
	import std.meta: AliasSeq, staticIndexOf;
	import std.typecons: ReplaceType;

	/// `AliasSeq` of the types this `SumType` can hold
	alias Types = AliasSeq!(ReplaceType!(This, typeof(this), TypesParam));

private:

	int tag;

	union
	{
		Types values;
	}

	alias value(T) = values[staticIndexOf!(T, Types)];

public:

	static foreach (i, T; Types) {
		/// Constructs a `SumType` holding a specific value
		this(T val)
		{
			tag = i;
			value!T = val;
		}
	}

	import std.traits: isAssignable;

	static foreach (i, T; Types) {
		static if (isAssignable!T) {
			/// Assigns a value to a `SumType` that can hold it
			void opAssign(T rhs)
			{
				tag = i;
				value!T = rhs;
			}
		}
	}

	/// Returns a string representation of the held value
	auto toString()
	{
		import std.conv: to;

		return this.match!(value => value.to!string);
	}
}

// Construction
unittest {
	alias Foo = SumType!(int, float);

	static assert(__traits(compiles, (){ Foo x = Foo(42); }));
	static assert(__traits(compiles, () { Foo y = Foo(3.14); }));
}

// Assignment
unittest {
	alias Foo = SumType!(int, float);

	Foo x = Foo(42);

	static assert(__traits(compiles, (){ x = 3.14; }));
}

// Self assignment
unittest {
	alias Foo = SumType!(int, float);

	Foo x = Foo(42);
	Foo y = Foo(3.14);

	static assert(__traits(compiles, y = x));
}

// Equality
unittest {
	alias Foo = SumType!(int, float);

	Foo x = Foo(42);
	Foo y = x;
	Foo z = Foo(3.14);

	assert(x == y);
	assert(x != z);
}

// Imported types
unittest {
	import std.typecons: Tuple;

	static assert(__traits(compiles, (){
		alias Foo = SumType!(Tuple!(int, int));
	}));
}

// const and immutable types
unittest {
	static assert(__traits(compiles, (){
		alias Foo = SumType!(const(int[]), immutable(float[]));
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

	List* nil()
	{
		return new List(Nil());
	}

	List* cons(int item, List* l)
	{
		return new List(Cons(item, l));
	}

	List* list(int[] items...)
	{
		if (items.length == 0)
			return nil;
		else
			return cons(items[0], list(items[1..$]));
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

// toString
unittest {
	import std.typecons: Tuple, tuple;
	import std.conv: to;

	class C {}

	alias Foo = SumType!(int[], double, Tuple!(string, C));

	int[] a = [1, 2, 3];
	double b = 3.14;
	Tuple!(string, C) c = tuple("test", new C);

	Foo x = Foo(a);
	Foo y = Foo(b);
	Foo z = Foo(c);

	assert(x.toString == a.to!string);
	assert(y.toString == b.to!string);
	assert(z.toString == c.to!string);
}

// Allowed types
unittest {
	import std.meta: AliasSeq;

	alias Foo = SumType!(int, float, This*);

	assert(is(Foo.Types == AliasSeq!(int, float, Foo*)));
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
 * can be implicitly instantiated.
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
			import std.traits: hasMember, isCallable, isSomeFunction, Parameters;

			// immutable overrides all other qualifiers, so this is true if and
			// only if the two types are the same up to qualifiers.
			enum sameBaseType(T, U) = is(immutable(T) == immutable(U));

			int[Types.length] indices;
			indices[] = -1;

			void setHandlerIndex(T)(int hid)
				if (staticIndexOf!(T, Types) >= 0)
			{
				int tid = staticIndexOf!(T, Types);

				if (indices[tid] == -1) {
					indices[tid] = hid;
				}
			}

			static foreach (T; Types) {
				static foreach (i, h; handlers) {
					static if (is(typeof(h(T.init)))) {
						// Regular handlers
						static if (isCallable!h) {
							// Functions and delegates
							static if (isSomeFunction!h) {
								static if (sameBaseType!(T, Parameters!h[0])) {
									setHandlerIndex!T(i);
								}
							// Objects with overloaded opCall
							} else static if (hasMember!(typeof(h), "opCall")) {
								static foreach (overload; __traits(getOverloads, typeof(h), "opCall")) {
									static if (sameBaseType!(T, Parameters!overload[0])) {
										setHandlerIndex!T(i);
									}
								}
							}
						// Generic handlers
						} else {
							setHandlerIndex!T(i);
						}
					}
				}
			}
			return indices;
		}

		enum handlerIndices = getHandlerIndices;

		final switch (self.tag) {
			static foreach (i, T; Types) {
				static assert(handlerIndices[i] != -1,
					"No matching handler for type `" ~ T.stringof ~ "`");

					case i:
						return handlers[handlerIndices[i]](self.value!T);
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

	static assert(!__traits(compiles, x.match!((int x) => true)));
	static assert(!__traits(compiles, x.match!()));
}

// No implicit converstion
unittest {
	alias Foo = SumType!(int, float);

	Foo x = Foo(42);

	static assert(!__traits(compiles,
		x.match!((long v) => true, (float v) => false)
	));
}

// Handlers with qualified parameters
unittest {
    alias Foo = SumType!(int[], float[]);

    Foo x = Foo([1, 2, 3]);
    Foo y = Foo([1.0, 2.0, 3.0]);

    assert(x.match!((const(int[]) v) => true, (const(float[]) v) => false));
    assert(y.match!((const(int[]) v) => false, (const(float[]) v) => true));
}

// Handlers for qualified types
unittest {
	alias Foo = SumType!(immutable(int[]), immutable(float[]));

	Foo x = Foo([1, 2, 3]);

	assert(x.match!((immutable(int[]) v) => true, (immutable(float[]) v) => false));
	assert(x.match!((const(int[]) v) => true, (const(float[]) v) => false));
	assert(x.match!((immutable v) => true, v => false));
	assert(x.match!((const v) => true, v => false));
	static assert(!__traits(compiles,
		x.match!((int[] v) => true, (immutable(float[]) v) => false)
	));
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

/// Generic handlers with implicit matching:
unittest {
	import std.math: approxEqual, PI, sqrt;

	struct Rectangular { double x, y; }
	struct Polar { double r, theta; }
	alias Vector = SumType!(Rectangular, Polar);

	double length(Vector v)
	{
		return v.match!(
			rect => sqrt(rect.x^^2 + rect.y^^2),
			polar => polar.r
		);
	}

	Vector u = Rectangular(1, 1);
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
