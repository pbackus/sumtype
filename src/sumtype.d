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
 * You can use `This` as a placeholder to create self-referential types, just
 * like with `Algebraic`.
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

	static foreach (i, T; Types) {
		/// Assigns a value to a [SumType] that can hold it
		void opAssign(T rhs)
		{
			tag = i;
			value!T = rhs;
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

/// Self-referential type using `This`:
unittest {
	import std.typecons: Tuple, tuple;

	alias Tree = SumType!(int, Tuple!(This*, "left", This*, "right"));
	alias Node = Tuple!(Tree*, "left", Tree*, "right");

	Node node(Tree* left, Tree* right)
	{
		return tuple!("left", "right")(left, right);
	}

	int[] inorder(Tree t)
	{
		return t.visit!(
			(int leaf) => [leaf],
			(Node node) => inorder(*node.left) ~ inorder(*node.right)
		);
	}

	Tree x =
		Tree(node(
			new Tree(1),
			new Tree(node(
				new Tree(2),
				new Tree(3)
			))
		));

	assert(inorder(x) == [1, 2, 3]);
}

/**
 * Applies a type-appropriate handler to the value held in a [SumType].
 *
 * For each type `T` that could be held in the [SumType], there must be a
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
 *   The value returned from the handler that matches the currently-held type.
 *
 * See_Also: `std.variant.visit`
 */
template visit(handlers...)
{
	/**
	 * The actual `visit` function.
	 *
	 * Params:
	 *   self = A [SumType] object
	 */
	auto visit(Self : SumType!TypesParam, TypesParam...)(Self self)
	{
		alias Types = self.Types;

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

			void setHandlerIndex(T)(int hid, Flag!"generic" generic = No.generic)
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
								setHandlerIndex!T(j);
							}
						// Objects with overloaded opCall
						} else static if (hasMember!(typeof(h), "opCall")) {
							static foreach (overload; __traits(getOverloads, typeof(h), "opCall")) {
								static if (sameUnqual!(T, Parameters!overload[0])) {
									setHandlerIndex!T(j);
								}
							}
						}
					// Generic handlers
					} else static if (is(typeof(h!T(T.init)))) {
						setHandlerIndex!T(j, Yes.generic);
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
						return handlers[handlerIndices.regular[i]](self.value!T);
				} else static if (handlerIndices.generic[i] != -1) {
					case i:
						return handlers[handlerIndices.generic[i]](self.value!T);
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
