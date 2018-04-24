/++
This module provides [SumType], an alternative to `std.variant.Algebraic` with
[match|improved pattern-matching], full attribute correctness (`pure`, `@safe`,
`@nogc`, and `nothrow` are inferred whenever possible), and no dependency on
runtime type information (`TypeInfo`).

License: MIT
Author: Paul Backus
+/
module sumtype;

/// $(B Arithmetic expression evaluator)
unittest {
	import std.functional: partial;
	import std.traits: EnumMembers;
	import std.typecons: Tuple;

	enum Op : string
	{
		Plus  = "+",
		Minus = "-",
		Times = "*",
		Div   = "/"
	}

	alias Expr = SumType!(
		double,
		string,
		Tuple!(Op, "op", This*, "lhs", This*, "rhs")
	);
	alias BinOp = Expr.Types[2];

	Expr* num(double value)
	{
		return new Expr(value);
	}

	Expr* var(string name)
	{
		return new Expr(name);
	}

	Expr* binOp(Op op, Expr* lhs, Expr* rhs)
	{
		return new Expr(BinOp(op, lhs, rhs));
	}

	alias sum  = partial!(binOp, Op.Plus);
	alias diff = partial!(binOp, Op.Minus);
	alias prod = partial!(binOp, Op.Times);
	alias quot = partial!(binOp, Op.Div);

	double eval(Expr expr, double[string] env)
	{
		return expr.match!(
			(double num) => num,
			(string var) => env[var],
			(BinOp bop) {
				double lhs = eval(*bop.lhs, env);
				double rhs = eval(*bop.rhs, env);
				final switch(bop.op) {
					static foreach(op; EnumMembers!Op) {
						case op:
							return mixin("lhs" ~ op ~ "rhs");
					}
				}
			}
		);
	}

	string pprint(Expr expr)
	{
		import std.format;

		return expr.match!(
			(double num) => "%g".format(num),
			(string var) => var,
			(BinOp bop) => "(%s %s %s)".format(
				pprint(*bop.lhs),
				bop.op,
				pprint(*bop.rhs)
			)
		);
	}

	Expr* myExpr = sum(var("a"), prod(num(2), var("b")));
	double[string] myEnv = ["a":3, "b":4, "c":7];

	assert(eval(*myExpr, myEnv) == 11);
	assert(pprint(*myExpr) == "(a + (2 * b))");
}

/** $(B Recursive definition of a linked list)
 *
 * Note: due to [dlang issue 1807](https://issues.dlang.org/show_bug.cgi?id=1807),
 * we can't use `List!T` as a parameter type directly, but instead must write
 * out the full expansion: `SumType!(Nil, Tuple!(T, "head", This*, * "tail"))`.
 */
unittest {
	import std.typecons: Tuple;

	struct Nil {}
	alias List(T) = SumType!(
		Nil,
		Tuple!(T, "head", This*, "tail")
	);
	alias Cons(T) = List!T.Types[1];

	List!T* nil(T)()
	{
		return new List!T(Nil());
	}

	List!T* cons(T)(T item, SumType!(Nil, Tuple!(T, "head", This*, "tail"))* l)
	{
		return new List!T(Cons!T(item, l));
	}

	List!T* list(T)(T[] items...)
	{
		if (items.length == 0)
			return nil!T;
		else
			return cons(items[0], list(items[1..$]));
	}

	R reduce(T, R)(
		SumType!(Nil, Tuple!(T, "head", This*, "tail")) l,
		R delegate (R, T) f,
		R init
	)
	{
		return l.match!(
			(Nil _) => init,
			(Cons!T c) => reduce(*c.tail, f, f(init, c.head))
		);
	}

	List!int* myList = list(1, 2, 3, 4, 5);

	assert(reduce(*myList, (int a, int e) => a + e, 0) == 15);
}

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
	static assert(__traits(compiles, (){ Foo y = Foo(3.14); }));
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
 * The first one that does is chosen as the match for that type.
 *
 * Implicit conversions are not taken into account, except between
 * differently-qualified versions of the same type. For example, a handler that
 * accepts a `long` will not match the type `int`, but a handler that accepts a
 * `const(int)[]` will match the type `immutable(int)[]`.
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
	// Tail-qualified parameters
	assert(x.match!((immutable(int)[] v) => true, (immutable(float)[] v) => false));
	assert(x.match!((const(int)[] v) => true, (const(float)[] v) => false));
	// Generic parameters
	assert(x.match!((immutable v) => true, v => false));
	assert(x.match!((const v) => true, v => false));
	// Unqualified parameters
	static assert(!__traits(compiles,
		x.match!((int[] v) => true, (float[] v) => false)
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
