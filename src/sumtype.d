/++
A sum type for modern D.

This module provides [SumType], an alternative to `std.variant.Algebraic` with
[match|improved pattern-matching], full attribute correctness (`pure`, `@safe`,
`@nogc`, and `nothrow` are inferred whenever possible), and no dependency on
runtime type information (`TypeInfo`).

License: MIT
Author: Paul Backus
+/
module sumtype;

/// $(H3 Basic usage)
unittest {
	import std.math: approxEqual;

	struct Fahrenheit { double degrees; }
	struct Celsius { double degrees; }
	struct Kelvin { double degrees; }

	alias Temperature = SumType!(Fahrenheit, Celsius, Kelvin);

	pure @safe @nogc nothrow
	Fahrenheit toFahrenheit(Temperature t)
	{
		return Fahrenheit(
			t.match!(
				(Fahrenheit f) => f.degrees,
				(Celsius c) => c.degrees * 9.0/5 + 32,
				(Kelvin k) => k.degrees * 9.0/5 - 459.4
			)
		);
	}

	Temperature t1 = Fahrenheit(98.6);
	Temperature t2 = Celsius(100);
	Temperature t3 = Kelvin(273);

	assert(toFahrenheit(t1).degrees.approxEqual(98.6));
	assert(toFahrenheit(t2).degrees.approxEqual(212));
	assert(toFahrenheit(t3).degrees.approxEqual(32));
}

/** $(H3 Structural matching)
 *
 * In the `length` and `horiz` functions below, the handlers for `match` do not
 * specify the types of their arguments. Instead, matching is done based on the
 * type's structure: any type with `x` and `y` properties will be matched by the
 * `rect` handlers, and any type with `r` and `theta` properties will be matched
 * by the `polar` handlers.
 */
unittest {
	import std.math: approxEqual, cos, PI, sqrt;

	struct Rectangular { double x, y; }
	struct Polar { double r, theta; }
	alias Vector = SumType!(Rectangular, Polar);

	pure @safe @nogc nothrow
	double length(Vector v)
	{
		return v.match!(
			rect => sqrt(rect.x^^2 + rect.y^^2),
			polar => polar.r
		);
	}

	pure @safe @nogc nothrow
	double horiz(Vector v)
	{
		return v.match!(
			rect => rect.x,
			polar => polar.r * cos(polar.theta)
		);
	}

	Vector u = Rectangular(1, 1);
	Vector v = Polar(1, PI/4);

	assert(length(u).approxEqual(sqrt(2.0)));
	assert(length(v).approxEqual(1));
	assert(horiz(u).approxEqual(1));
	assert(horiz(v).approxEqual(sqrt(0.5)));
}

/** $(H3 Arithmetic expression evaluator)
 *
 * This example makes use of the special placeholder type `This` to define a
 * [https://en.wikipedia.org/wiki/Recursive_data_type|recursive data type]: an
 * [https://en.wikipedia.org/wiki/Abstract_syntax_tree|abstract syntax tree] for
 * representing simple arithmetic expressions.
 */
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

	// An expression is either
	//  - a number,
	//  - a variable, or
	//  - a binary operation combining two sub-expressions.
	alias Expr = SumType!(
		double,
		string,
		Tuple!(Op, "op", This*, "lhs", This*, "rhs")
	);

	// Shorthand for the Tuple type above
	alias BinOp = Expr.Types[2];

	// Factory function for number expressions
	pure @safe
	Expr* num(double value)
	{
		return new Expr(value);
	}

	// Factory function for variable expressions
	pure @safe
	Expr* var(string name)
	{
		return new Expr(name);
	}

	// Factory function for binary operation expressions
	pure @safe
	Expr* binOp(Op op, Expr* lhs, Expr* rhs)
	{
		return new Expr(BinOp(op, lhs, rhs));
	}

	// Convenience wrappers for creating BinOp expressions
	alias sum  = partial!(binOp, Op.Plus);
	alias diff = partial!(binOp, Op.Minus);
	alias prod = partial!(binOp, Op.Times);
	alias quot = partial!(binOp, Op.Div);

	// Evaluate expr, looking up variables in env
	pure @safe nothrow
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

	// Return a "pretty-printed" representation of expr
	@safe
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

public import std.variant: This;

/**
 * A tagged union that can hold a single value from any of a specified set of
 * types
 *
 * The stored value can be operated on using [match|pattern matching].
 *
 * The special type `This` can be used as a placeholder to create
 * self-referential types, just like with `Algebraic`. See the
 * [sumtype#arithmetic-expression-evaluator|"Arithmetic expression evaluator" example] for
 * usage.
 *
 * A `SumType` is initialized by default with the `.init` property of its first
 * member type, just like a regular union.
 *
 * See_Also: `std.variant.Algebraic`
 */
struct SumType(TypeArgs...)
{
	import std.meta: AliasSeq;
	import std.typecons: ReplaceType;

	/// The types a `SumType` can hold
	alias Types = AliasSeq!(ReplaceType!(This, typeof(this), TypeArgs));

private:

	int tag;

	union Storage
	{
		Types values;
	}

	Storage storage;

public:

	static foreach (i, T; Types) {
		/// Constructs a `SumType` holding a specific value
		this(T val)
		{
			tag = i;
			storage.values[i] = val;
		}
	}

	import std.traits: isAssignable;

	static foreach (i, T; Types) {
		static if (isAssignable!T) {
			/// Assigns a value to a `SumType`
			void opAssign(T rhs)
			{
				import std.algorithm.mutation: moveEmplace;

				this.match!((ref value) { destroy(value); });
				tag = i;
				() @trusted { moveEmplace(rhs, storage.values[i]); }();
			}
		}
	}

	/// Returns a string representation of the currently-held value
	string toString()
	{
		import std.conv: to;

		return this.match!(value => value.to!string);
	}

	import std.meta: anySatisfy;
	import std.traits: hasElaborateCopyConstructor, hasElaborateDestructor;

	static if (anySatisfy!(hasElaborateDestructor, Types)) {
		~this()
		{
			this.match!((ref value) { destroy(value); });
		}
	}

	static if (anySatisfy!(hasElaborateCopyConstructor, Types)) {
		this(this)
		{
			this.match!((ref value) {
				static if (hasElaborateCopyConstructor!(typeof(value))) {
					value.__xpostblit;
				}
			});
		}
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

// Recursive types
unittest {
	alias Foo = SumType!(This*);
	assert(is(Foo.Types[0] == Foo*));
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

// Works alongside Algebraic
unittest {
	import std.variant;

	alias Bar = Algebraic!(This*);

	assert(is(Bar.AllowedTypes[0] == Bar*));
}

// Types with destructors
unittest {
	int destroyed;

	struct HasDtor
	{
		int n = 0;

		~this()
		{
			destroyed = n;
		}
	}

	alias Foo = SumType!(int, HasDtor);

	HasDtor h = HasDtor(123);

	{
		Foo x = h;
		destroyed = 0;
	}
	assert(destroyed == 123);

	{
		Foo x = 456;
		destroyed = 0;
	}
	assert(destroyed == 0);

	{
		Foo x = h;
		destroyed = 0;
		x = 456;
		assert(destroyed == 123);
	}

	{
		Foo x = 456;
		destroyed = 0;
		x = h;
		assert(destroyed == 0);
	}
}

// Types with postblits
unittest {
	bool postblitted;

	struct HasPostblit
	{
		this(this)
		{
			postblitted = true;
		}
	}

	alias Foo = SumType!(int, HasPostblit);

	Foo a = HasPostblit();

	postblitted = false;
	Foo b = a;
	assert(postblitted);

	Foo c;
	postblitted = false;
	c = a;
	assert(postblitted);
}

/**
 * Calls a type-appropriate function with the value held in a [SumType].
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
 * can be implicitly instantiated. See [sumtype#structural-matching|"Structural matching"] for
 * an example of templated handler usage.
 *
 * Returns:
 *   The value returned from the handler that matches the currently-held type.
 *
 * See_Also: `std.variant.visit`
 */
template match(handlers...)
{
	import std.typecons: Yes;

	/**
	 * The actual `match` function.
	 *
	 * Params:
	 *   self = A [SumType] object
	 */
	auto match(Self)(auto ref Self self)
		if (is(Self : SumType!TypeArgs, TypeArgs...))
	{
		return self.matchImpl!(Yes.exhaustive, handlers);
	}
}

/**
 * Attempts to call a type-appropriate function with the value held in a
 * [SumType], and throws on failure.
 *
 * Matches are chosen using the same rules as [match], but are not required to
 * be exhaustive—in other words, a type is allowed to have no matching handler.
 * If a type without a handler is encountered at runtime, a [MatchException]
 * is thrown.
 *
 * Returns:
 *   The value returned from the handler that matches the currently-held type,
 *   if a handler was given for that type.
 *
 * Throws:
 *   [MatchException], if the currently-held type has no matching handler.
 *
 * See_Also: `std.variant.tryVisit`
 */
template tryMatch(handlers...)
{
	import std.typecons: No;

	/**
	 * The actual `tryMatch` function.
	 *
	 * Params:
	 *   self = A [SumType] object
	 */
	auto tryMatch(Self)(auto ref Self self)
		if (is(Self : SumType!TypeArgs, TypeArgs...))
	{
		return self.matchImpl!(No.exhaustive, handlers);
	}
}

/// Thrown by [tryMatch] when an unhandled type is encountered.
class MatchException : Exception
{
	this(string msg, string file = __FILE__, size_t line = __LINE__)
	{
		super(msg, file, line);
	}
}

import std.typecons: Flag;

private template matchImpl(Flag!"exhaustive" exhaustive, handlers...)
{
	auto matchImpl(Self)(auto ref Self self)
		if (is(Self : SumType!TypeArgs, TypeArgs...))
	{
		alias Types = self.Types;

		pure static int[Types.length] getHandlerIndices()
		{
			import std.traits: hasMember, isCallable, isSomeFunction, Parameters;

			// immutable recursively overrides all other qualifiers, so the
			// right-hand side is true if and only if the two types are the
			// same up to qualifiers (i.e., they have the same structure).
			enum sameUpToQuals(T, U) = is(immutable(T) == immutable(U));

			int[Types.length] indices;
			indices[] = -1;

			void setHandlerIndex(int tid, int hid)
			{
				if (indices[tid] == -1) {
					indices[tid] = hid;
				}
			}

			static foreach (tid, T; Types) {
				static foreach (hid, handler; handlers) {
					static if (is(typeof(handler(self.storage.values[tid])))) {
						// Regular handlers
						static if (isCallable!handler) {
							// Functions and delegates
							static if (isSomeFunction!handler) {
								static if (sameUpToQuals!(T, Parameters!handler[0])) {
									setHandlerIndex(tid, hid);
								}
							// Objects with overloaded opCall
							} else static if (hasMember!(typeof(handler), "opCall")) {
								static foreach (overload; __traits(getOverloads, typeof(handler), "opCall")) {
									static if (sameUpToQuals!(T, Parameters!overload[0])) {
										setHandlerIndex(tid, hid);
									}
								}
							}
						// Generic handlers
						} else {
							setHandlerIndex(tid, hid);
						}
					}
				}
			}
			return indices;
		}

		enum handlerIndices = getHandlerIndices;

		import std.algorithm.searching: canFind;

		static foreach (hid, handler; handlers) {
			static assert(handlerIndices[].canFind(hid),
				"handler `" ~ handler.stringof ~ "` " ~
				"of type `" ~ typeof(handler).stringof ~ "` " ~
				"never matches"
			);
		}

		final switch (self.tag) {
			static foreach (tid, T; Types) {
				case tid:
					static if (handlerIndices[tid] != -1) {
						return handlers[handlerIndices[tid]](self.storage.values[tid]);
					} else {
						static if(exhaustive) {
							static assert(0,
								"No matching handler for type `" ~ T.stringof ~ "`");
						} else {
							throw new MatchException(
								"No matching handler for type `" ~ T.stringof ~ "`");
						}
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
	assert(x.match!((immutable v) => true));
	assert(x.match!((const v) => true));
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

// Structural matching
unittest {
	struct S1 { int x; }
	struct S2 { int y; }
	alias Foo = SumType!(S1, S2);

	Foo a = Foo(S1(0));
	Foo b = Foo(S2(0));

	assert(a.match!(s1 => s1.x + 1, s2 => s2.y - 1) == 1);
	assert(b.match!(s1 => s1.x + 1, s2 => s2.y - 1) == -1);
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

	assert(x.match!((int v) => true, v => false));
}

// Non-exhaustive matching
unittest {
	import std.exception: assertThrown, assertNotThrown;

	alias Foo = SumType!(int, float);

	Foo x = Foo(42);
	Foo y = Foo(3.14);

	assertNotThrown!MatchException(x.tryMatch!((int n) => true));
	assertThrown!MatchException(y.tryMatch!((int n) => true));
}

// Handlers with ref parameters
unittest {
	import std.math: approxEqual;
	import std.meta: staticIndexOf;

	alias Value = SumType!(long, double);

	auto value = Value(3.14);

	value.match!(
		(long) {},
		(ref double d) { d *= 2; }
	);

	enum tid = staticIndexOf!(double, Value.Types);

	assert(value.storage.values[tid].approxEqual(6.28));
}

// Unreachable handlers
unittest {
	alias MySum = SumType!(int, string);

	MySum s;

	static assert(!__traits(compiles,
		s.match!(
			(int _) => 0,
			(string _) => 1,
			(double _) => 2
		)
	));

	static assert(!__traits(compiles,
		s.match!(
			_ => 0,
			(int _) => 1
		)
	));
}
