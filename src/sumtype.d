/++
[SumType] is a generic discriminated union implementation that uses
design-by-introspection to generate safe and efficient code. Its features
include:

$(LIST
    * [match|Pattern matching.]
    * Support for self-referential types.
    * Full attribute correctness (`pure`, `@safe`, `@nogc`, and `nothrow` are
      inferred whenever possible).
    * A type-safe and memory-safe API compatible with DIP 1000 (`scope`).
    * No dependency on runtime type information (`TypeInfo`).
    * Compatibility with BetterC.
)

License: Boost License 1.0
Authors: Paul Backus
+/
module sumtype;

/// $(H3 Basic usage)
version (D_BetterC) {} else
@safe unittest {
    import std.math: approxEqual;

    struct Fahrenheit { double degrees; }
    struct Celsius { double degrees; }
    struct Kelvin { double degrees; }

    alias Temperature = SumType!(Fahrenheit, Celsius, Kelvin);

    // Construct from any of the member types.
    Temperature t1 = Fahrenheit(98.6);
    Temperature t2 = Celsius(100);
    Temperature t3 = Kelvin(273);

    // Use pattern matching to access the value.
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

    assert(toFahrenheit(t1).degrees.approxEqual(98.6));
    assert(toFahrenheit(t2).degrees.approxEqual(212));
    assert(toFahrenheit(t3).degrees.approxEqual(32));

    // Use ref to modify the value in place.
    pure @safe @nogc nothrow
    void freeze(ref Temperature t)
    {
        t.match!(
            (ref Fahrenheit f) => f.degrees = 32,
            (ref Celsius c) => c.degrees = 0,
            (ref Kelvin k) => k.degrees = 273
        );
    }

    freeze(t1);
    assert(toFahrenheit(t1).degrees.approxEqual(32));

    // Use a catch-all handler to give a default result.
    pure @safe @nogc nothrow
    bool isFahrenheit(Temperature t)
    {
        return t.match!(
            (Fahrenheit f) => true,
            _ => false
        );
    }

    assert(isFahrenheit(t1));
    assert(!isFahrenheit(t2));
    assert(!isFahrenheit(t3));
}

/** $(H3 Introspection-based matching)
 *
 * In the `length` and `horiz` functions below, the handlers for `match` do not
 * specify the types of their arguments. Instead, matching is done based on how
 * the argument is used in the body of the handler: any type with `x` and `y`
 * properties will be matched by the `rect` handlers, and any type with `r` and
 * `theta` properties will be matched by the `polar` handlers.
 */
version (D_BetterC) {} else
@safe unittest {
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
version (D_BetterC) {} else
@safe unittest {
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

    // Shorthand for Tuple!(Op, "op", Expr*, "lhs", Expr*, "rhs"),
    // the Tuple type above with Expr substituted for This.
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
                cast(string) bop.op,
                pprint(*bop.rhs)
            )
        );
    }

    Expr* myExpr = sum(var("a"), prod(num(2), var("b")));
    double[string] myEnv = ["a":3, "b":4, "c":7];

    assert(eval(*myExpr, myEnv) == 11);
    assert(pprint(*myExpr) == "(a + (2 * b))");
}

import std.format: FormatSpec, singleSpec;
import std.meta: AliasSeq, Filter, IndexOf = staticIndexOf, Map = staticMap;
import std.meta: NoDuplicates;
import std.meta: anySatisfy, allSatisfy;
import std.traits: hasElaborateCopyConstructor, hasElaborateDestructor;
import std.traits: isAssignable, isCopyable, isStaticArray;
import std.traits: ConstOf, ImmutableOf, TemplateArgsOf;
import std.typecons: ReplaceTypeUnless;
import std.typecons: Flag;

/// `This` placeholder, for use in self-referential types.
public import std.variant: This;

// Converts an unsigned integer to a compile-time string constant.
private enum toCtString(ulong n) = n.stringof[0 .. $ - "LU".length];

@safe unittest {
	assert(toCtString!0 == "0");
	assert(toCtString!123456 == "123456");
}

// True if a variable of type T can appear on the lhs of an assignment
private enum isAssignableTo(T) =
	isAssignable!T || (!isCopyable!T && isRvalueAssignable!T);

// toHash is required by the language spec to be nothrow and @safe
private enum isHashable(T) = __traits(compiles,
	() nothrow @safe { hashOf(T.init); }
);

/**
 * A tagged union that can hold a single value from any of a specified set of
 * types.
 *
 * The value in a `SumType` can be operated on using [match|pattern matching].
 *
 * To avoid ambiguity, duplicate types are not allowed (but see the
 * [sumtype#basic-usage|"basic usage" example] for a workaround).
 *
 * The special type `This` can be used as a placeholder to create
 * self-referential types, just like with `Algebraic`. See the
 * [sumtype#arithmetic-expression-evaluator|"Arithmetic expression evaluator" example] for
 * usage.
 *
 * A `SumType` is initialized by default to hold the `.init` value of its
 * first member type, just like a regular union. The version identifier
 * `SumTypeNoDefaultCtor` can be used to disable this behavior.
 *
 * See_Also: `std.variant.Algebraic`
 */
struct SumType(Types...)
	if (is(NoDuplicates!Types == Types) && Types.length > 0)
{
	/// The types a `SumType` can hold.
	alias Types = AliasSeq!(
		ReplaceTypeUnless!(isSumTypeInstance, This, typeof(this), TemplateArgsOf!SumType)
	);

private:

	enum bool canHoldTag(T) = Types.length <= T.max;
	alias unsignedInts = AliasSeq!(ubyte, ushort, uint, ulong);

	alias Tag = Filter!(canHoldTag, unsignedInts)[0];

	union Storage
	{
		template memberName(T)
			if (IndexOf!(T, Types) >= 0)
		{
			enum tid = IndexOf!(T, Types);
			mixin("enum memberName = `values_", toCtString!tid, "`;");
		}

		static foreach (T; Types) {
			mixin("T ", memberName!T, ";");
		}
	}

	Storage storage;
	Tag tag;

	/**
	 * Accesses the value stored in a SumType.
	 *
	 * This method is memory-safe, provided that:
	 *
	 *   1. A SumType's tag is always accurate.
	 *   2. A SumType cannot be assigned to in @safe code if that assignment
	 *      could cause unsafe aliasing.
	 *
	 * All code that accesses a SumType's tag or storage directly, including
	 * @safe code in this module, must be manually checked to ensure that it
	 * does not violate either of the above requirements.
	 */
	@trusted
	ref inout(T) get(T)() inout
		if (IndexOf!(T, Types) >= 0)
	{
		enum tid = IndexOf!(T, Types);
		assert(tag == tid);
		return __traits(getMember, storage, Storage.memberName!T);
	}

public:

	static foreach (tid, T; Types) {
		/// Constructs a `SumType` holding a specific value.
		this()(auto ref T value)
		{
			import core.lifetime: forward;

			static if (isCopyable!T) {
				mixin("Storage newStorage = { ",
					Storage.memberName!T, ": value",
				" };");
			} else {
				mixin("Storage newStorage = { ",
					Storage.memberName!T, " : forward!value",
				" };");
			}

			storage = newStorage;
			tag = tid;
		}

		static if (isCopyable!T) {
			/// ditto
			this()(auto ref const(T) value) const
			{
				mixin("const(Storage) newStorage = { ",
					Storage.memberName!T, ": value",
				" };");

				storage = newStorage;
				tag = tid;
			}

			/// ditto
			this()(auto ref immutable(T) value) immutable
			{
				mixin("immutable(Storage) newStorage = { ",
					Storage.memberName!T, ": value",
				" };");

				storage = newStorage;
				tag = tid;
			}
		} else {
			@disable this(const(T) value) const;
			@disable this(immutable(T) value) immutable;
		}
	}

	static if (allSatisfy!(isCopyable, Types)) {
		static if (anySatisfy!(hasElaborateCopyConstructor, Types)) {
			/// Constructs a `SumType` that's a copy of another `SumType`
			this(ref SumType other)
			{
				storage = other.match!((ref value) {
					alias T = typeof(value);

					mixin("Storage newStorage = { ",
						Storage.memberName!T, ": value",
					" };");

					return newStorage;
				});

				tag = other.tag;
			}

			/// ditto
			this(ref const(SumType) other) const
			{
				storage = other.match!((ref value) {
					alias OtherTypes = Map!(ConstOf, Types);
					enum tid = IndexOf!(typeof(value), OtherTypes);
					alias T = Types[tid];

					mixin("const(Storage) newStorage = { ",
						Storage.memberName!T, ": value",
					" };");

					return newStorage;
				});

				tag = other.tag;
			}

			/// ditto
			this(ref immutable(SumType) other) immutable
			{
				storage = other.match!((ref value) {
					alias OtherTypes = Map!(ImmutableOf, Types);
					enum tid = IndexOf!(typeof(value), OtherTypes);
					alias T = Types[tid];

					mixin("immutable(Storage) newStorage = { ",
						Storage.memberName!T, ": value",
					" };");

					return newStorage;
				});

				tag = other.tag;
			}
		}
	} else {
		/// `@disable`d if any member type is non-copyable.
		@disable this(this);
	}

	version(SumTypeNoDefaultCtor) {
		@disable this();
	}

	static foreach (tid, T; Types) {
		static if (isAssignableTo!T) {
			/**
			 * Assigns a value to a `SumType`.
			 *
			 * Assigning to a `SumType` is `@system` if any of the
			 * `SumType`'s members contain pointers or references, since
			 * those members may be reachable through external references,
			 * and overwriting them could therefore lead to memory
			 * corruption.
			 *
			 * An individual assignment can be `@trusted` if the caller can
			 * guarantee that there are no outstanding references to $(I any)
			 * of the `SumType`'s members when the assignment occurs.
			 */
			ref SumType opAssign()(auto ref T rhs)
			{
				import core.lifetime: forward;
				import std.traits: hasIndirections, hasNested;
				import std.meta: Or = templateOr;

				enum mayContainPointers =
					anySatisfy!(Or!(hasIndirections, hasNested), Types);

				static if (mayContainPointers) {
					cast(void) () @system {}();
				}

				this.match!((ref value) {
					static if (hasElaborateDestructor!(typeof(value))) {
						destroy(value);
					}
				});

				mixin("Storage newStorage = { ",
					Storage.memberName!T, ": forward!rhs",
				" };");

				storage = newStorage;
				tag = tid;

				return this;
			}
		}
	}

	static if (allSatisfy!(isAssignableTo, Types)) {
		static if (allSatisfy!(isCopyable, Types)) {
			/**
			 * Copies the value from another `SumType` into this one.
			 *
			 * See the value-assignment overload for details on `@safe`ty.
			 *
			 * Copy assignment is `@disable`d if any of `Types` is non-copyable.
			 */
			ref SumType opAssign(ref SumType rhs)
			{
				rhs.match!((ref value) { this = value; });
				return this;
			}
		} else {
			@disable ref SumType opAssign(ref SumType rhs);
		}

		/**
		 * Moves the value from another `SumType` into this one.
		 *
		 * See the value-assignment overload for details on `@safe`ty.
		 */
		ref SumType opAssign(SumType rhs)
		{
			import core.lifetime: move;

			rhs.match!((ref value) { this = move(value); });
			return this;
		}
	}

	/**
	 * Compares two `SumType`s for equality.
	 *
	 * Two `SumType`s are equal if they are the same kind of `SumType`, they
	 * contain values of the same type, and those values are equal.
	 */
	bool opEquals(this This, Rhs)(auto ref Rhs rhs)
		if (isSumType!Rhs && is(This.Types == Rhs.Types))
	{
		import std.meta: ApplyLeft;
		import std.traits: CopyTypeQualifiers;

		alias ThisTypes = Map!(ApplyLeft!(CopyTypeQualifiers, This), This.Types);
		alias RhsTypes = Map!(ApplyLeft!(CopyTypeQualifiers, Rhs), Rhs.Types);

		return AliasSeq!(this, rhs).match!((ref value, ref rhsValue) {
			/* Deduce tags from types of values.
			 *
			 * If a SumType has duplicate members--that is, two or more members
			 * with the same type but different tags--this will always choose
			 * the tag of the first one. This can happen is if a type qualifier
			 * applied to a SumType causes the types of two members to merge;
			 * for example, `const(SumType!(int, const(int)))` has two members
			 * of type `const(int)`.
			 *
			 * Duplicate members are considered equivalent, since they are
			 * impossible to distinguish by pattern matching.
			 */
			enum thisTid = IndexOf!(typeof(value), ThisTypes);
			enum rhsTid = IndexOf!(typeof(rhsValue), RhsTypes);

			enum sameTag = thisTid == rhsTid;
			enum sameType = is(typeof(value) == typeof(rhsValue));

			/* Checking for equal tags allows differently-qualified SumTypes
			 * to be compared, as long as the values support it.
			 *
			 * If either This or Rhs has duplicate members, then those members
			 * should be compared even if their tags do not match. Otherwise,
			 * if there are no duplicate members, sameType implies sameTag,
			 * so checking it is at worst redundant.
			 */
			static if (sameTag || sameType) {
				return value == rhsValue;
			} else {
				return false;
			}
		});
	}

	// Workaround for dlang issue 19407
	static if (__traits(compiles, anySatisfy!(hasElaborateDestructor, Types))) {
		// If possible, include the destructor only when it's needed
		private enum includeDtor = anySatisfy!(hasElaborateDestructor, Types);
	} else {
		// If we can't tell, always include it, even when it does nothing
		private enum includeDtor = true;
	}

	static if (includeDtor) {
		/// Calls the destructor of the `SumType`'s current value.
		~this()
		{
			this.match!((ref value) {
				static if (hasElaborateDestructor!(typeof(value))) {
					destroy(value);
				}
			});
		}
	}

	invariant {
		this.match!((ref value) {
			static if (is(typeof(value) == class)) {
				if (value !is null) {
					assert(value);
				}
			} else static if (is(typeof(value) == struct)) {
				assert(&value);
			}
		});
	}

	version (D_BetterC) {} else
	/**
	 * Returns a string representation of the `SumType`'s current value.
	 *
	 * Not available when compiled with `-betterC`.
	 */
	string toString(this T)()
	{
		import std.conv: to;

		return this.match!(to!string);
	}

	version (D_BetterC) {} else
	/**
	 * Handles formatted writing of the `SumType`'s value.
	 *
	 * Not available when compiled with `-betterC`.
	 *
	 * Params:
	 *   sink = Output range to write to.
	 *   fmt = Format specifier to use.
	 *
	 * See_Also: `std.format.formatValue`
	 */
	void toString(this This, Sink, Char)(ref Sink sink, const ref FormatSpec!Char fmt)
	{
		import std.format: formatValue;

		this.match!((ref value) {
			formatValue(sink, value, fmt);
		});
	}

	static if (allSatisfy!(isHashable, Map!(ConstOf, Types))) {
		// Workaround for dlang issue 20095
		version (D_BetterC) {} else
		/**
		 * Returns the hash of the `SumType`'s current value.
		 *
		 * Not available when compiled with `-betterC`.
		 */
		size_t toHash() const
		{
			return this.match!hashOf;
		}
	}
}

// Construction
@safe unittest {
	alias MySum = SumType!(int, float);

	assert(__traits(compiles, MySum(42)));
	assert(__traits(compiles, MySum(3.14)));
}

// Assignment
@safe unittest {
	alias MySum = SumType!(int, float);

	MySum x = MySum(42);

	assert(__traits(compiles, x = 3.14));
}

// Self assignment
@safe unittest {
	alias MySum = SumType!(int, float);

	MySum x = MySum(42);
	MySum y = MySum(3.14);

	assert(__traits(compiles, y = x));
}

// Equality
@safe unittest {
	alias MySum = SumType!(int, float);

	MySum x = MySum(123);
	MySum y = MySum(123);
	MySum z = MySum(456);
	MySum w = MySum(123.0);
	MySum v = MySum(456.0);

	assert(x == y);
	assert(x != z);
	assert(x != w);
	assert(x != v);

}

// Equality of differently-qualified SumTypes
version(D_BetterC) {} else
@safe unittest {
	alias SumA = SumType!(int, float);
	alias SumB = SumType!(const(int[]), int[]);
	alias SumC = SumType!(int[], const(int[]));

	int[] ma = [1, 2, 3];
	const(int[]) ca = [1, 2, 3];

	assert(const(SumA)(123) == SumA(123));
	assert(const(SumB)(ma[]) == SumB(ca[]));
	assert(const(SumC)(ma[]) == SumC(ca[]));
}

// Imported types
@safe unittest {
	import std.typecons: Tuple;

	assert(__traits(compiles, {
		alias MySum = SumType!(Tuple!(int, int));
	}));
}

// const and immutable types
@safe unittest {
	assert(__traits(compiles, {
		alias MySum = SumType!(const(int[]), immutable(float[]));
	}));
}

// Recursive types
@safe unittest {
	alias MySum = SumType!(This*);
	assert(is(MySum.Types[0] == MySum*));
}

// Allowed types
@safe unittest {
	import std.meta: AliasSeq;

	alias MySum = SumType!(int, float, This*);

	assert(is(MySum.Types == AliasSeq!(int, float, MySum*)));
}

// Works alongside Algebraic
version (D_BetterC) {} else
@safe unittest {
	import std.variant;

	alias Bar = Algebraic!(This*);

	assert(is(Bar.AllowedTypes[0] == Bar*));
}

// Types with destructors and postblits
@system unittest {
	int copies;

	static struct Test
	{
		bool initialized = false;
		int* copiesPtr;

		this(this) { (*copiesPtr)++; }
		~this() { if (initialized) (*copiesPtr)--; }
	}

	alias MySum = SumType!(int, Test);

	Test t = Test(true, &copies);

	{
		MySum x = t;
		assert(copies == 1);
	}
	assert(copies == 0);

	{
		MySum x = 456;
		assert(copies == 0);
	}
	assert(copies == 0);

	{
		MySum x = t;
		assert(copies == 1);
		x = 456;
		assert(copies == 0);
	}

	{
		MySum x = 456;
		assert(copies == 0);
		x = t;
		assert(copies == 1);
	}

	{
		MySum x = t;
		MySum y = x;
		assert(copies == 2);
	}

	{
		MySum x = t;
		MySum y;
		y = x;
		assert(copies == 2);
	}
}

// Doesn't destroy reference types
version (D_BetterC) {} else
@system unittest {
	bool destroyed;

	class C
	{
		~this()
		{
			destroyed = true;
		}
	}

	struct S
	{
		~this() {}
	}

	alias MySum = SumType!(S, C);

	C c = new C();
	{
		MySum x = c;
		destroyed = false;
	}
	assert(!destroyed);

	{
		MySum x = c;
		destroyed = false;
		x = S();
		assert(!destroyed);
	}
}

// Types with @disable this()
@safe unittest {
	static struct NoInit
	{
		@disable this();
	}

	alias MySum = SumType!(NoInit, int);

	assert(!__traits(compiles, MySum()));
	assert(__traits(compiles, MySum(42)));
}

// const SumTypes
@safe unittest {
	assert(__traits(compiles,
		const(SumType!(int[]))([1, 2, 3])
	));
}

// Equality of const SumTypes
@safe unittest {
	alias MySum = SumType!int;

	assert(__traits(compiles,
		const(MySum)(123) == const(MySum)(456)
	));
}

// Compares reference types using value equality
@safe unittest {
	import std.array: staticArray;

	static struct Field {}
	static struct Struct { Field[] fields; }
	alias MySum = SumType!Struct;

	static arr1 = staticArray([Field()]);
	static arr2 = staticArray([Field()]);

	auto a = MySum(Struct(arr1[]));
	auto b = MySum(Struct(arr2[]));

	assert(a == b);
}

// toString
version (D_BetterC) {} else
@safe unittest {
	import std.conv: text;

	static struct Int { int i; }
	static struct Double { double d; }
	alias Sum = SumType!(Int, Double);

	assert(Sum(Int(42)).text == Int(42).text, Sum(Int(42)).text);
	assert(Sum(Double(33.3)).text == Double(33.3).text, Sum(Double(33.3)).text);
	assert((const(Sum)(Int(42))).text == (const(Int)(42)).text, (const(Sum)(Int(42))).text);
}

// string formatting
version(D_BetterC) {} else
@safe unittest {
	import std.format: format;

	SumType!int x = 123;

	assert(format!"%s"(x) == format!"%s"(123));
	assert(format!"%x"(x) == format!"%x"(123));
}

// string formatting of qualified SumTypes
version(D_BetterC) {} else
@safe unittest {
	import std.format: format;

	int[] a = [1, 2, 3];
	const(SumType!(int[])) x = a;

	assert(format!"%(%d, %)"(x) == format!"%(%s, %)"(a));
}

// Github issue #16
version (D_BetterC) {} else
@safe unittest {
	alias Node = SumType!(This[], string);

	// override inference of @system attribute for cyclic functions
	assert((() @trusted =>
		Node([Node([Node("x")])])
		==
		Node([Node([Node("x")])])
	)());
}

// Github issue #16 with const
version (D_BetterC) {} else
@safe unittest {
	alias Node = SumType!(const(This)[], string);

	// override inference of @system attribute for cyclic functions
	assert((() @trusted =>
		Node([Node([Node("x")])])
		==
		Node([Node([Node("x")])])
	)());
}

// Stale pointers
version (D_BetterC) {} else
@system unittest {
	alias MySum = SumType!(ubyte, void*[2]);

	MySum x = [null, cast(void*) 0x12345678];
	void** p = &x.get!(void*[2])[1];
	x = ubyte(123);

	assert(*p != cast(void*) 0x12345678);
}

// Exception-safe assignment
version (D_BetterC) {} else
@safe unittest {
	static struct A
	{
		int value = 123;
	}

	static struct B
	{
		int value = 456;
		this(this) { throw new Exception("oops"); }
	}

	alias MySum = SumType!(A, B);

	MySum x;
	try {
		x = B();
	} catch (Exception e) {}

	assert(
		(x.tag == 0 && x.get!A.value == 123) ||
		(x.tag == 1 && x.get!B.value == 456)
	);
}

// Types with @disable this(this)
@safe unittest {
	import core.lifetime: move;

	static struct NoCopy
	{
		@disable this(this);
	}

	alias MySum = SumType!NoCopy;

	NoCopy lval = NoCopy();

	MySum x = NoCopy();
	MySum y = NoCopy();

	assert(__traits(compiles, SumType!NoCopy(NoCopy())));
	assert(!__traits(compiles, SumType!NoCopy(lval)));

	assert(__traits(compiles, y = NoCopy()));
	assert(__traits(compiles, y = move(x)));
	assert(!__traits(compiles, y = lval));
	assert(!__traits(compiles, y = x));

	assert(__traits(compiles, x == y));
}

// Github issue #22
version (D_BetterC) {} else
@safe unittest {
	import std.typecons;
	assert(__traits(compiles, {
		static struct A {
			SumType!(Nullable!int) a = Nullable!int.init;
		}
	}));
}

// Static arrays of structs with postblits
version (D_BetterC) {} else
@safe unittest {
	static struct S
	{
		int n;
		this(this) { n++; }
	}

	assert(__traits(compiles, SumType!(S[1])()));

	SumType!(S[1]) x = [S(0)];
	SumType!(S[1]) y = x;

	auto xval = x.get!(S[1])[0].n;
	auto yval = y.get!(S[1])[0].n;

	assert(xval != yval);
}

// Replacement does not happen inside SumType
version (D_BetterC) {} else
@safe unittest {
	import std.typecons : Tuple, ReplaceTypeUnless;
	alias A = Tuple!(This*,SumType!(This*))[SumType!(This*,string)[This]];
	alias TR = ReplaceTypeUnless!(isSumTypeInstance, This, int, A);
	static assert(is(TR == Tuple!(int*,SumType!(This*))[SumType!(This*, string)[int]]));
}

// Supports nested self-referential SumTypes
@safe unittest {
	import std.typecons : Tuple, Flag;
	alias Nat = SumType!(Flag!"0", Tuple!(This*));
	static assert(__traits(compiles, SumType!(Nat)));
	static assert(__traits(compiles, SumType!(Nat*, Tuple!(This*, This*))));
}

// Doesn't call @system postblits in @safe code
@safe unittest {
	static struct SystemCopy { @system this(this) {} }
	SystemCopy original;

	assert(!__traits(compiles, () @safe {
		SumType!SystemCopy copy = original;
	}));

	assert(!__traits(compiles, () @safe {
		SumType!SystemCopy copy; copy = original;
	}));
}

// Doesn't overwrite pointers in @safe code
@safe unittest {
	alias MySum = SumType!(int*, int);

	MySum x;

	assert(!__traits(compiles, () @safe {
		x = 123;
	}));

	assert(!__traits(compiles, () @safe {
		x = MySum(123);
	}));
}

// Types with invariants
version (D_BetterC) {} else
@system unittest {
	import std.exception: assertThrown;
	import core.exception: AssertError;

	struct S
	{
		int i;
		invariant { assert(i >= 0); }
	}

	class C
	{
		int i;
		invariant { assert(i >= 0); }
	}

	SumType!S x;
	x.match!((ref v) { v.i = -1; });
	assertThrown!AssertError(assert(&x));

	SumType!C y = new C();
	y.match!((ref v) { v.i = -1; });
	assertThrown!AssertError(assert(&y));
}

// Calls value postblit on self-assignment
@safe unittest {
	static struct S
	{
		int n;
		this(this) { n++; }
	}

	SumType!S x = S();
	SumType!S y;
	y = x;

	auto xval = x.get!S.n;
	auto yval = y.get!S.n;

	assert(xval != yval);
}

// Github issue #29
@safe unittest {
	assert(__traits(compiles, () @safe {
		alias A = SumType!string;

		@safe A createA(string arg) {
		return A(arg);
		}

		@safe void test() {
		A a = createA("");
		}
	}));
}

// SumTypes as associative array keys
version (D_BetterC) {} else
@safe unittest {
	assert(__traits(compiles, {
		int[SumType!(int, string)] aa;
	}));
}

// toString with non-copyable types
version(D_BetterC) {} else
@safe unittest {
	struct NoCopy
	{
		@disable this(this);
	}

	SumType!NoCopy x;

	assert(__traits(compiles, x.toString()));
}

// Can use the result of assignment
@safe unittest {
	alias MySum = SumType!(int, float);

	MySum a = MySum(123);
	MySum b = MySum(3.14);

	assert((a = b) == b);
	assert((a = MySum(123)) == MySum(123));
	assert((a = 3.14) == MySum(3.14));
	assert(((a = b) = MySum(123)) == MySum(123));
}

version(none) {
	// Known bug; needs fix for dlang issue 19902
	// Types with copy constructors
	@safe unittest {
		static struct S
		{
			int n;
			this(ref return scope inout S other) inout { n++; }
		}

		SumType!S x = S();
		SumType!S y = x;

		auto xval = x.get!S.n;
		auto yval = y.get!S.n;

		assert(xval != yval);
	}
}

// Types with disabled opEquals
@safe unittest {
	static struct S
	{
		@disable bool opEquals(const S rhs) const;
	}

	assert(__traits(compiles, SumType!S(S())));
}

// Types with non-const opEquals
@safe unittest {
	static struct S
	{
		int i;
		bool opEquals(S rhs) { return i == rhs.i; }
	}

	assert(__traits(compiles, SumType!S(S(123))));
}

// Incomparability of different SumTypes
@safe unittest {
	SumType!(int, string) x = 123;
	SumType!(string, int) y = 123;

	assert(!__traits(compiles, x != y));
}

/// True if `T` is an instance of the `SumType` template, otherwise false.
private enum bool isSumTypeInstance(T) = is(T == SumType!Args, Args...);

unittest {
	static struct Wrapper
	{
		SumType!int s;
		alias s this;
	}

	assert(isSumTypeInstance!(SumType!int));
	assert(!isSumTypeInstance!Wrapper);
}

/// True if `T` is a `SumType` or implicitly converts to one, otherwise false.
enum bool isSumType(T) = is(T : SumType!Args, Args...);

@safe unittest {
	static struct Wrapper
	{
		SumType!int s;
		alias s this;
	}

	static struct Container
	{
		SumType!int s;
	}

	assert(isSumType!(SumType!int));
	assert(isSumType!Wrapper);
	assert(!isSumType!Container);
}

/**
 * Calls a type-appropriate function with the value held in a [SumType].
 *
 * For each possible type the [SumType] can hold, the given handlers are
 * checked, in order, to see whether they accept a single argument of that type.
 * The first one that does is chosen as the match for that type. (Note that the
 * first match may not always be the most exact match.
 * See [#avoiding-unintentional-matches|"Avoiding unintentional matches"] for
 * one common pitfall.)
 *
 * Every type must have a matching handler, and every handler must match at
 * least one type. This is enforced at compile time.
 *
 * Handlers may be functions, delegates, or objects with opCall overloads. If a
 * function with more than one overload is given as a handler, all of the
 * overloads are considered as potential matches.
 *
 * Templated handlers are also accepted, and will match any type for which they
 * can be [implicitly instantiated](https://dlang.org/glossary.html#ifti). See
 * [sumtype#introspection-based-matching|"Introspection-based matching"] for an
 * example of templated handler usage.
 *
 * If multiple [SumType]s are passed to match, their values are passed to the
 * handlers as separate arguments, and matching is done for each possible
 * combination of argument types. See [#multiple-dispatch|"Multiple dispatch"] for
 * an example.
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
	 *   args = One or more [SumType] objects.
	 */
	auto ref match(SumTypes...)(auto ref SumTypes args)
		if (allSatisfy!(isSumType, SumTypes) && args.length > 0)
	{
		return matchImpl!(Yes.exhaustive, handlers)(args);
	}
}

/** $(H3 Avoiding unintentional matches)
 *
 * Sometimes, implicit conversions may cause a handler to match more types than
 * intended. The example below shows two solutions to this problem.
 */
@safe unittest {
    alias Number = SumType!(double, int);

    Number x;

    // Problem: because int implicitly converts to double, the double
    // handler is used for both types, and the int handler never matches.
    assert(!__traits(compiles,
        x.match!(
            (double d) => "got double",
            (int n) => "got int"
        )
    ));

    // Solution 1: put the handler for the "more specialized" type (in this
    // case, int) before the handler for the type it converts to.
    assert(__traits(compiles,
        x.match!(
            (int n) => "got int",
            (double d) => "got double"
        )
    ));

    // Solution 2: use a template that only accepts the exact type it's
    // supposed to match, instead of any type that implicitly converts to it.
    alias exactly(T, alias fun) = function (arg) {
        static assert(is(typeof(arg) == T));
        return fun(arg);
    };

    // Now, even if we put the double handler first, it will only be used for
    // doubles, not ints.
    assert(__traits(compiles,
        x.match!(
            exactly!(double, d => "got double"),
            exactly!(int, n => "got int")
        )
    ));
}

/** $(H3 Multiple dispatch)
 *
 * Pattern matching can be performed on multiple `SumType`s at once by passing
 * handlers with multiple arguments. This usually leads to more concise code
 * than using nested calls to `match`, as show below.
 */
@safe unittest {
    struct Point2D { double x, y; }
    struct Point3D { double x, y, z; }

    alias Point = SumType!(Point2D, Point3D);

    version(none) {
        // This function works, but the code is ugly and repetitive.
        // It uses three separate calls to match!
        @safe pure nothrow @nogc
        bool sameDimensions(Point p1, Point p2)
        {
            return p1.match!(
                (Point2D _) => p2.match!(
                    (Point2D _) => true,
                    _ => false
                ),
                (Point3D _) => p2.match!(
                    (Point3D _) => true,
                    _ => false
                )
            );
        }
    }

    // This version is much nicer.
    @safe pure nothrow @nogc
    bool sameDimensions(Point p1, Point p2)
    {
        alias doMatch = match!(
            (Point2D _1, Point2D _2) => true,
            (Point3D _1, Point3D _2) => true,
            (_1, _2) => false
        );

        return doMatch(p1, p2);
    }

    Point a = Point2D(1, 2);
    Point b = Point2D(3, 4);
    Point c = Point3D(5, 6, 7);
    Point d = Point3D(8, 9, 0);

    assert( sameDimensions(a, b));
    assert( sameDimensions(c, d));
    assert(!sameDimensions(a, c));
    assert(!sameDimensions(d, b));
}

/**
 * Attempts to call a type-appropriate function with the value held in a
 * [SumType], and throws on failure.
 *
 * Matches are chosen using the same rules as [match], but are not required to
 * be exhaustive—in other words, a type (or combination of types) is allowed to
 * have no matching handler. If a type without a handler is encountered at
 * runtime, a [MatchException] is thrown.
 *
 * Not available when compiled with `-betterC`.
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
version (D_Exceptions)
template tryMatch(handlers...)
{
	import std.typecons: No;

	/**
	 * The actual `tryMatch` function.
	 *
	 * Params:
	 *   args = One or more [SumType] objects.
	 */
	auto ref tryMatch(SumTypes...)(auto ref SumTypes args)
		if (allSatisfy!(isSumType, SumTypes) && args.length > 0)
	{
		return matchImpl!(No.exhaustive, handlers)(args);
	}
}

/**
 * Thrown by [tryMatch] when an unhandled type is encountered.
 *
 * Not available when compiled with `-betterC`.
 */
version (D_Exceptions)
class MatchException : Exception
{
	pure @safe @nogc nothrow
	this(string msg, string file = __FILE__, size_t line = __LINE__)
	{
		super(msg, file, line);
	}
}

/**
 * True if `handler` is a potential match for `Ts`, otherwise false.
 *
 * See the documentation for [match] for a full explanation of how matches are
 * chosen.
 */
template canMatch(alias handler, Ts...)
	if (Ts.length > 0)
{
	enum canMatch = is(typeof((Ts args) => handler(args)));
}

// Includes all overloads of the given handler
@safe unittest {
	static struct OverloadSet
	{
		static void fun(int n) {}
		static void fun(double d) {}
	}

	assert(canMatch!(OverloadSet.fun, int));
	assert(canMatch!(OverloadSet.fun, double));
}

// Like aliasSeqOf!(iota(n)), but works in BetterC
private template Iota(size_t n)
{
	static if (n == 0) {
		alias Iota = AliasSeq!();
	} else {
		alias Iota = AliasSeq!(Iota!(n - 1), n - 1);
	}
}

@safe unittest {
	assert(is(Iota!0 == AliasSeq!()));
	assert(Iota!1 == AliasSeq!(0));
	assert(Iota!3 == AliasSeq!(0, 1, 2));
}

private template matchImpl(Flag!"exhaustive" exhaustive, handlers...)
{
	auto ref matchImpl(SumTypes...)(auto ref SumTypes args)
		if (allSatisfy!(isSumType, SumTypes) && args.length > 0)
	{
		/* The stride that the dim-th argument's tag is multiplied by when
		 * converting TagTuples to and from case indices ("caseIds").
		 *
		 * Named by analogy to the stride that the dim-th index into a
		 * multidimensional static array is multiplied by to calculate the
		 * offset of a specific element.
		 */
		static size_t stride(size_t dim)()
		{
			import core.checkedint: mulu;

			size_t result = 1;
			bool overflow = false;

			static foreach (S; SumTypes[0 .. dim]) {
				result = mulu(result, S.Types.length, overflow);
			}

			/* The largest number matchImpl uses, numCases, is calculated with
			 * stride!(SumTypes.length), so as long as this overflow check
			 * passes, we don't need to check for overflow anywhere else.
			 */
			assert(!overflow);
			return result;
		}

		/* A TagTuple represents a single possible set of tags that `args`
		 * could have at runtime.
		 *
		 * Because D does not allow a struct to be the controlling expression
		 * of a switch statement, we cannot dispatch on the TagTuple directly.
		 * Instead, we must map each TagTuple to a unique integer and generate
		 * a case label for each of those integers. This mapping is implemented
		 * in `fromCaseId` and `toCaseId`.
		 *
		 * The mapping is done by pretending we are indexing into an
		 * `args.length`-dimensional static array of type
		 *
		 *   ubyte[SumTypes[0].Types.length]...[SumTypes[$-1].Types.length]
		 *
		 * ...where each element corresponds to the TagTuple whose tags can be
		 * used (in reverse order) as indices to retrieve it. The caseId for
		 * that TagTuple is the (hypothetical) offset, in bytes, of its
		 * corresponding element.
		 *
		 * For example, when `args` consists of two SumTypes with two member
		 * types each, the TagTuples corresponding to each case label are:
		 *
		 *   case 0:  TagTuple([0, 0])
		 *   case 1:  TagTuple([1, 0])
		 *   case 2:  TagTuple([0, 1])
		 *   case 3:  TagTuple([1, 1])
		 */
		static struct TagTuple
		{
			size_t[SumTypes.length] tags;
			alias tags this;

			invariant {
				static foreach (i; 0 .. tags.length) {
					assert(tags[i] < SumTypes[i].Types.length);
				}
			}

			this(ref const(SumTypes) args)
			{
				static foreach (i; 0 .. tags.length) {
					tags[i] = args[i].tag;
				}
			}

			static TagTuple fromCaseId(size_t caseId)
			{
				TagTuple result;

				// Most-significant to least-significant
				static foreach_reverse (i; 0 .. result.length) {
					result[i] = caseId / stride!i;
					caseId %= stride!i;
				}

				return result;
			}

			size_t toCaseId()
			{
				size_t result;

				static foreach (i; 0 .. tags.length) {
					result += tags[i] * stride!i;
				}

				return result;
			}
		}

		/* An AliasSeq of zero-argument functions that return, by ref, the
		 * member values of `args` needed for the case labeled with `caseId`.
		 *
		 * When used in an expression context (like, say, a function call), it
		 * will instead be interpreted as a sequence of zero-argument function
		 * *calls*, with optional parentheses omitted.
		 */
		template values(size_t caseId)
		{
			enum tags = TagTuple.fromCaseId(caseId);

			ref getValue(size_t i)()
			{
				enum tid = tags[i];
				alias T = SumTypes[i].Types[tid];
				return args[i].get!T;
			}

			alias values = Map!(getValue, Iota!(tags.length));
		}

		/* An AliasSeq of the types of the member values returned by the
		 * functions in `values!caseId`.
		 *
		 * Note that these are the actual (that is, qualified) types of the
		 * member values, which may not be the same as the types listed in
		 * the arguments' `.Types` properties.
		 *
		 * typeof(values!caseId) won't work because it gives the types
		 * of the functions, not the return values (even with @property).
		 */
		template valueTypes(size_t caseId)
		{
			enum tags = TagTuple.fromCaseId(caseId);

			template getType(size_t i)
			{
				enum tid = tags[i];
				alias T = SumTypes[i].Types[tid];
				alias getType = typeof(args[i].get!T());
			}

			alias valueTypes = Map!(getType, Iota!(tags.length));
		}

		/* The total number of cases is
		 *
		 *   Π SumTypes[i].Types.length for 0 ≤ i < SumTypes.length
		 *
		 * Or, equivalently,
		 *
		 *   ubyte[SumTypes[0].Types.length]...[SumTypes[$-1].Types.length].sizeof
		 *
		 * Conveniently, this is equal to stride!(SumTypes.length), so we can
		 * use that function to compute it.
		 */
		enum numCases = stride!(SumTypes.length);

		/* Guaranteed to never be a valid handler index, since
		 * handlers.length <= size_t.max.
		 */
		enum noMatch = size_t.max;

		// An array that maps caseIds to handler indices ("hids").
		enum matches = () {
			size_t[numCases] matches;

			// Workaround for dlang issue 19561
			foreach (ref match; matches) {
				match = noMatch;
			}

			static foreach (caseId; 0 .. numCases) {
				static foreach (hid, handler; handlers) {
					static if (canMatch!(handler, valueTypes!caseId)) {
						if (matches[caseId] == noMatch) {
							matches[caseId] = hid;
						}
					}
				}
			}

			return matches;
		}();

		import std.algorithm.searching: canFind;

		// Check for unreachable handlers
		static foreach (hid, handler; handlers) {
			static assert(matches[].canFind(hid),
				"`handlers[" ~ toCtString!hid ~ "]` " ~
				"of type `" ~ ( __traits(isTemplate, handler)
					? "template"
					: typeof(handler).stringof
				) ~ "` " ~
				"never matches"
			);
		}

		// Workaround for dlang issue 19993
		enum handlerName(size_t hid) = "handler" ~ toCtString!hid;

		static foreach (size_t hid, handler; handlers) {
			mixin("alias ", handlerName!hid, " = handler;");
		}

		immutable argsId = TagTuple(args).toCaseId;

		final switch (argsId) {
			static foreach (caseId; 0 .. numCases) {
				case caseId:
					static if (matches[caseId] != noMatch) {
						return mixin(handlerName!(matches[caseId]))(values!caseId);
					} else {
						static if(exhaustive) {
							static assert(false,
								"No matching handler for types `" ~ valueTypes!caseId.stringof ~ "`");
						} else {
							throw new MatchException(
								"No matching handler for types `" ~ valueTypes!caseId.stringof ~ "`");
						}
					}
			}
		}

		assert(false); // unreached
	}
}

// Matching
@safe unittest {
	alias MySum = SumType!(int, float);

	MySum x = MySum(42);
	MySum y = MySum(3.14);

	assert(x.match!((int v) => true, (float v) => false));
	assert(y.match!((int v) => false, (float v) => true));
}

// Missing handlers
@safe unittest {
	alias MySum = SumType!(int, float);

	MySum x = MySum(42);

	assert(!__traits(compiles, x.match!((int x) => true)));
	assert(!__traits(compiles, x.match!()));
}

// Handlers with qualified parameters
version (D_BetterC) {} else
@safe unittest {
	alias MySum = SumType!(int[], float[]);

	MySum x = MySum([1, 2, 3]);
	MySum y = MySum([1.0, 2.0, 3.0]);

	assert(x.match!((const(int[]) v) => true, (const(float[]) v) => false));
	assert(y.match!((const(int[]) v) => false, (const(float[]) v) => true));
}

// Handlers for qualified types
version (D_BetterC) {} else
@safe unittest {
	alias MySum = SumType!(immutable(int[]), immutable(float[]));

	MySum x = MySum([1, 2, 3]);

	assert(x.match!((immutable(int[]) v) => true, (immutable(float[]) v) => false));
	assert(x.match!((const(int[]) v) => true, (const(float[]) v) => false));
	// Tail-qualified parameters
	assert(x.match!((immutable(int)[] v) => true, (immutable(float)[] v) => false));
	assert(x.match!((const(int)[] v) => true, (const(float)[] v) => false));
	// Generic parameters
	assert(x.match!((immutable v) => true));
	assert(x.match!((const v) => true));
	// Unqualified parameters
	assert(!__traits(compiles,
		x.match!((int[] v) => true, (float[] v) => false)
	));
}

// Delegate handlers
version (D_BetterC) {} else
@safe unittest {
	alias MySum = SumType!(int, float);

	int answer = 42;
	MySum x = MySum(42);
	MySum y = MySum(3.14);

	assert(x.match!((int v) => v == answer, (float v) => v == answer));
	assert(!y.match!((int v) => v == answer, (float v) => v == answer));
}

version(unittest) {
	version(D_BetterC) {
		// std.math.approxEqual depends on core.runtime.math, so use a
		// libc-based version for testing with -betterC
		@safe pure @nogc nothrow
		private bool approxEqual(double lhs, double rhs)
		{
			import core.stdc.math: fabs;

			return (lhs - rhs) < 1e-5;
		}
	} else {
		import std.math: approxEqual;
	}
}

// Generic handler
@safe unittest {
	alias MySum = SumType!(int, float);

	MySum x = MySum(42);
	MySum y = MySum(3.14);

	assert(x.match!(v => v*2) == 84);
	assert(y.match!(v => v*2).approxEqual(6.28));
}

// Fallback to generic handler
version (D_BetterC) {} else
@safe unittest {
	import std.conv: to;

	alias MySum = SumType!(int, float, string);

	MySum x = MySum(42);
	MySum y = MySum("42");

	assert(x.match!((string v) => v.to!int, v => v*2) == 84);
	assert(y.match!((string v) => v.to!int, v => v*2) == 42);
}

// Multiple non-overlapping generic handlers
@safe unittest {
	import std.array: staticArray;

	alias MySum = SumType!(int, float, int[], char[]);

	static ints = staticArray([1, 2, 3]);
	static chars = staticArray(['a', 'b', 'c']);

	MySum x = MySum(42);
	MySum y = MySum(3.14);
	MySum z = MySum(ints[]);
	MySum w = MySum(chars[]);

	assert(x.match!(v => v*2, v => v.length) == 84);
	assert(y.match!(v => v*2, v => v.length).approxEqual(6.28));
	assert(w.match!(v => v*2, v => v.length) == 3);
	assert(z.match!(v => v*2, v => v.length) == 3);
}

// Structural matching
@safe unittest {
	static struct S1 { int x; }
	static struct S2 { int y; }
	alias MySum = SumType!(S1, S2);

	MySum a = MySum(S1(0));
	MySum b = MySum(S2(0));

	assert(a.match!(s1 => s1.x + 1, s2 => s2.y - 1) == 1);
	assert(b.match!(s1 => s1.x + 1, s2 => s2.y - 1) == -1);
}

// Separate opCall handlers
@safe unittest {
	static struct IntHandler
	{
		bool opCall(int arg)
		{
			return true;
		}
	}

	static struct FloatHandler
	{
		bool opCall(float arg)
		{
			return false;
		}
	}

	alias MySum = SumType!(int, float);

	MySum x = MySum(42);
	MySum y = MySum(3.14);

	assert(x.match!(IntHandler.init, FloatHandler.init));
	assert(!y.match!(IntHandler.init, FloatHandler.init));
}

// Compound opCall handler
@safe unittest {
	static struct CompoundHandler
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

	alias MySum = SumType!(int, float);

	MySum x = MySum(42);
	MySum y = MySum(3.14);

	assert(x.match!(CompoundHandler.init));
	assert(!y.match!(CompoundHandler.init));
}

// Ordered matching
@safe unittest {
	alias MySum = SumType!(int, float);

	MySum x = MySum(42);

	assert(x.match!((int v) => true, v => false));
}

// Non-exhaustive matching
version (D_Exceptions)
@system unittest {
	import std.exception: assertThrown, assertNotThrown;

	alias MySum = SumType!(int, float);

	MySum x = MySum(42);
	MySum y = MySum(3.14);

	assertNotThrown!MatchException(x.tryMatch!((int n) => true));
	assertThrown!MatchException(y.tryMatch!((int n) => true));
}

// Non-exhaustive matching in @safe code
version (D_Exceptions)
@safe unittest {
	SumType!(int, float) x;

	assert(__traits(compiles,
		x.tryMatch!(
			(int n) => n + 1,
		)
	));

}

// Handlers with ref parameters
@safe unittest {
	import std.meta: staticIndexOf;

	alias Value = SumType!(long, double);

	auto value = Value(3.14);

	value.match!(
		(long) {},
		(ref double d) { d *= 2; }
	);

	assert(value.get!double.approxEqual(6.28));
}

// Unreachable handlers
@safe unittest {
	alias MySum = SumType!(int, string);

	MySum s;

	assert(!__traits(compiles,
		s.match!(
			(int _) => 0,
			(string _) => 1,
			(double _) => 2
		)
	));

	assert(!__traits(compiles,
		s.match!(
			_ => 0,
			(int _) => 1
		)
	));
}

// Unsafe handlers
unittest {
	SumType!int x;
	alias unsafeHandler = (int x) @system { return; };

	assert(!__traits(compiles, () @safe {
		x.match!unsafeHandler;
	}));

	assert(__traits(compiles, () @system {
		return x.match!unsafeHandler;
	}));
}

// Overloaded handlers
@safe unittest {
	static struct OverloadSet
	{
		static string fun(int i) { return "int"; }
		static string fun(double d) { return "double"; }
	}

	alias MySum = SumType!(int, double);

	MySum a = 42;
	MySum b = 3.14;

	assert(a.match!(OverloadSet.fun) == "int");
	assert(b.match!(OverloadSet.fun) == "double");
}

// Overload sets that include SumType arguments
@safe unittest {
	alias Inner = SumType!(int, double);
	alias Outer = SumType!(Inner, string);

	static struct OverloadSet
	{
		@safe:
		static string fun(int i) { return "int"; }
		static string fun(double d) { return "double"; }
		static string fun(string s) { return "string"; }
		static string fun(Inner i) { return i.match!fun; }
		static string fun(Outer o) { return o.match!fun; }
	}

	Outer a = Inner(42);
	Outer b = Inner(3.14);
	Outer c = "foo";

	assert(OverloadSet.fun(a) == "int");
	assert(OverloadSet.fun(b) == "double");
	assert(OverloadSet.fun(c) == "string");
}

// Overload sets with ref arguments
@safe unittest {
	static struct OverloadSet
	{
		static void fun(ref int i) { i = 42; }
		static void fun(ref double d) { d = 3.14; }
	}

	alias MySum = SumType!(int, double);

	MySum x = 0;
	MySum y = 0.0;

	x.match!(OverloadSet.fun);
	y.match!(OverloadSet.fun);

	assert(x.match!((value) => is(typeof(value) == int) && value == 42));
	assert(y.match!((value) => is(typeof(value) == double) && value == 3.14));
}

// Overload sets with templates
@safe unittest {
	import std.traits: isNumeric;

	static struct OverloadSet
	{
		static string fun(string arg)
		{
			return "string";
		}

		static string fun(T)(T arg)
			if (isNumeric!T)
		{
			return "numeric";
		}
	}

	alias MySum = SumType!(int, string);

	MySum x = 123;
	MySum y = "hello";

	assert(x.match!(OverloadSet.fun) == "numeric");
	assert(y.match!(OverloadSet.fun) == "string");
}

// Github issue #24
@safe unittest {
	assert(__traits(compiles, () @nogc {
		int acc = 0;
		SumType!int(1).match!((int x) => acc += x);
	}));
}

// Github issue #31
@safe unittest {
	assert(__traits(compiles, () @nogc {
		int acc = 0;

		SumType!(int, string)(1).match!(
			(int x) => acc += x,
			(string _) => 0,
		);
	}));
}

// Types that `alias this` a SumType
@safe unittest {
	static struct A {}
	static struct B {}
	static struct D { SumType!(A, B) value; alias value this; }

	assert(__traits(compiles, D().match!(_ => true)));
}

// Multiple dispatch
@safe unittest {
	alias MySum = SumType!(int, string);

	static int fun(MySum x, MySum y)
	{
		import std.meta: Args = AliasSeq;

		return Args!(x, y).match!(
			(int    xv, int    yv) => 0,
			(string xv, int    yv) => 1,
			(int    xv, string yv) => 2,
			(string xv, string yv) => 3
		);
	}

	assert(fun(MySum(0),  MySum(0))  == 0);
	assert(fun(MySum(""), MySum(0))  == 1);
	assert(fun(MySum(0),  MySum("")) == 2);
	assert(fun(MySum(""), MySum("")) == 3);
}

/**
 * A [SumType] wrapper that provides direct access to methods and properties
 * common to all of its members.
 *
 * Attempting to access a property or call a method that is not available for
 * all members will result in a compile-time error.
 */
struct StructuralSumType(Types...)
{
	/// Provides access to the stored value as a [SumType] object.
	SumType!Types asSumType;

	static foreach (T; Types) {
		/// Constructs a `StructuralSumType` holding a specific value
		this()(auto ref T value)
		{
			import core.lifetime: forward;

			static if (isCopyable!T) {
				asSumType = SumType!Types(value);
			} else {
				asSumType = SumType!Types(forward!value);
			}
		}

		/// ditto
		this()(auto ref const(T) value) const
		{
			asSumType = const(SumType!Types)(value);
		}

		/// ditto
		this()(auto ref immutable(T) value) immutable
		{
			asSumType = immutable(SumType!Types)(value);
		}
	}

	static foreach(T; Types) {
		static if (isAssignableTo!T) {
			/// Assigns a value to a `StructuralSumType`
			ref StructuralSumType opAssign()(auto ref T rhs)
			{
				import core.lifetime: forward;

				asSumType = forward!rhs;
				return this;
			}
		}
	}

	/// Property access
	auto ref opDispatch(string name, this This, Args...)(auto ref Args args)
	{
		import core.lifetime: forward;

		enum handler = q{(ref value) {
			static if (args.length == 0) {
				return __traits(getMember, value, name);
			} else static if (args.length == 1) {
				/* If this is a "real" assignment, and not a property setter or
				 * single-argument method call, it won't work if the right-hand
				 * side is an AliasSeq.
				 */
				return __traits(getMember, value, name) = forward!(args[0]);
			} else {
				return __traits(getMember, value, name) = forward!args;
			}
		}};

		// Workaround for dlang issue 21243
		static if (__traits(compiles, asSumType.match!(mixin("ref ", handler)))) {
			return asSumType.match!(mixin("ref ", handler));
		} else {
			return asSumType.match!(mixin(handler));
		}
	}

	/// Unary operators
	auto ref opUnary(string op, this This)()
	{
		return asSumType.match!((ref value) => mixin(op, "value"));
	}

	/// Binary operators
	auto ref opBinary(string op, this This, Rhs)(auto ref Rhs rhs)
	{
		return asSumType.match!((ref value) => mixin("value", op, "rhs"));
	}

	/// ditto
	auto ref opBinaryRight(string op, this This, Lhs)(auto ref Lhs lhs)
	{
		return asSumType.match!((ref value) => mixin("lhs", op, "value"));
	}

	/// Assignment with an operator
	auto ref opOpAssign(string op, this This, Rhs)(Rhs rhs)
	{
		return asSumType.match!(ref (ref value) => mixin("value", op, "=", "rhs"));
	}

	/// Compares a `StructuralSumType`'s value with another value
	bool opEquals(Rhs)(auto ref Rhs rhs) const
	{
		return asSumType.match!((ref value) => value == rhs);
	}

	/// ditto
	auto opCmp(this This, Rhs)(auto ref Rhs rhs)
	{
		return asSumType.match!((ref value) {
			static if (__traits(compiles, value.opCmp(rhs))) {
				return value.opCmp(rhs);
			} else static if (__traits(compiles, rhs.opCmp(value))) {
				return -rhs.opCmp(value);
			} else {
				/* Built-in comparison operators always give a total
				 * ordering, so there's no need to handle NaN here.
				 */
				return value < rhs ? -1 : value > rhs ? 1 : 0;
			}
		});
	}

	/// Function call operator
	auto ref opCall(this This, Args...)(auto ref Args args)
	{
		import core.lifetime: forward;

		enum handler = q{(ref value) => value(forward!args)};

		// Workaround for dlang issue 21243
		static if (__traits(compiles, asSumType.match!(mixin("ref ", handler)))) {
			return asSumType.match!(mixin("ref ", handler));
		} else {
			return asSumType.match!(mixin(handler));
		}
	}

	/**
	 * Index operator
	 *
	 * Currently supports indexing with any number of arguments, and slicing
	 * with zero arguments.
	 */
	auto ref opIndex(this This, Args...)(auto ref Args args)
	{
		import core.lifetime: forward;

		return asSumType.match!(ref (ref value) => value[forward!args]);
	}

	/// ditto
	auto ref opIndex(this This)()
	{
		return asSumType.match!((ref value) => value[]);
	}
}

// Construction from value
@safe unittest {
	alias MySum = StructuralSumType!(int, float);

	assert(__traits(compiles, MySum(42)));
	assert(__traits(compiles, MySum(3.14)));
}

// const and immutable
@safe unittest {
	alias MySum = StructuralSumType!(int[]);

	const int[] ca;
	immutable int[] ia;

	assert(__traits(compiles, const(MySum)(ca)));
	assert(__traits(compiles, immutable(MySum)(ia)));
}

// Assignment
@safe unittest {
	alias MySum = StructuralSumType!(int, float);

	MySum x = 42;

	assert(__traits(compiles, x = 3.14));
}

// Self assignment
@safe unittest {
	alias MySum = StructuralSumType!int;

	MySum x;
	MySum y;

	x = y;
}

// Equality
@safe unittest {
	alias MySum = StructuralSumType!(int, float);

	MySum x = MySum(123);
	MySum y = MySum(123);
	MySum z = MySum(456);
	MySum w = MySum(123.0);
	MySum v = MySum(456.0);

	assert(x == 123);
	assert(x == y);
	assert(x != z);
	assert((x == w) == (123 == cast(float) 123.0));
	assert(x != v);
}

// Equality of const StructuralSumTypes
@safe unittest {
	alias MySum = StructuralSumType!int;

	assert(__traits(compiles,
		const(MySum)(123) == const(MySum)(456)
	));
}

// Types with @disable this(this)
@safe unittest {
	import core.lifetime: move;

	static struct NoCopy
	{
		@disable this(this);
	}

	alias MySum = StructuralSumType!NoCopy;

	NoCopy lval = NoCopy();

	MySum x = NoCopy();
	MySum y = NoCopy();

	assert(__traits(compiles, SumType!NoCopy(NoCopy())));
	assert(!__traits(compiles, SumType!NoCopy(lval)));

	assert(__traits(compiles, y = NoCopy()));
	assert(__traits(compiles, y = move(x)));
	assert(!__traits(compiles, y = lval));
	assert(!__traits(compiles, y = x));

	assert(__traits(compiles, x == y));
}

// Can use the result of assignment
@safe unittest {
	alias MySum = StructuralSumType!(int, float);

	MySum a = MySum(123);
	MySum b = MySum(3.14);

	assert((a = b) == b);
	assert((a = MySum(123)) == MySum(123));
	assert((a = 3.14) == MySum(3.14));
	assert(((a = b) = MySum(123)) == MySum(123));
}

// Matching
@safe unittest {
	alias MySum = StructuralSumType!(int, float);

	MySum x = MySum(42);
	MySum y = MySum(3.14);

	assert(x.asSumType.match!((int v) => true, (float v) => false));
	assert(y.asSumType.match!((int v) => false, (float v) => true));
}

// Non-exhaustive matching
version (D_Exceptions)
@system unittest {
	import std.exception: assertThrown, assertNotThrown;

	alias MySum = StructuralSumType!(int, float);

	MySum x = MySum(42);
	MySum y = MySum(3.14);

	assertNotThrown!MatchException(x.asSumType.tryMatch!((int n) => true));
	assertThrown!MatchException(y.asSumType.tryMatch!((int n) => true));
}

// Common property access
@safe unittest {
	static struct A
	{
		int member;
	}

	static struct B
	{
		int member;
	}

	alias MySum = StructuralSumType!(A, B);

	MySum x = A(123);
	MySum y = B(456);

	assert(x.member == 123);
	assert(y.member == 456);
}

// Qualified common property access
@safe unittest {
	static struct A
	{
		int[] member;
	}

	static struct B
	{
		int[] member;
	}

	alias MySum = StructuralSumType!(A, B);

	immutable int[1] a1 = [123];
	immutable int[1] a2 = [456];

	immutable MySum x = immutable(A)(a1);
	immutable MySum y = immutable(B)(a2);

	assert(x.member[0] == 123);
	assert(y.member[0] == 456);
}

// Properties with a common type
@safe unittest {
	static struct A
	{
		int member;
	}

	static struct B
	{
		double member;
	}

	alias MySum = StructuralSumType!(A, B);

	MySum x;

	assert(__traits(compiles, x.member));
}

// Property assignment
@safe unittest {
	static struct A
	{
		int member;
	}

	static struct B
	{
		int member;
	}

	alias MySum = StructuralSumType!(A, B);

	MySum x = A(123);
	x.member = 456;

	assert(x.member == 456);
}

// Properties are lvalues
@safe unittest {
	static struct A
	{
		int member;
	}

	static void inc(ref int n)
	{
		n++;
	}

	StructuralSumType!A x = A(42);
	inc(x.member);

	assert(x.member = 43);
}

// Member functions
@safe unittest {
	static struct A
	{
		int fun(int n, int m) { return n + m; }
	}

	static struct B
	{
		int fun(int n, int m) { return n * m; }
	}

	alias MySum = StructuralSumType!(A, B);

	MySum x = A();
	MySum y = B();

	assert(x.fun(2, 3) == 5);
	assert(y.fun(2, 3) == 6);
}

// Member functions + qualifiers
@safe unittest {
	static struct A
	{
		int fun(int n, int m) const { return n + m; }
	}

	static struct B
	{
		int fun(int n, int m) const { return n * m; }
	}

	alias MySum = StructuralSumType!(A, B);

	const MySum x = A();
	immutable MySum y = B();

	assert(x.fun(2, 3) == 5);
	assert(y.fun(2, 3) == 6);
}

// Qualified member function overloads
@safe unittest {
	static struct S
	{
		string fun() inout { return "inout"; }
		string fun() immutable { return "immutable"; }
	}

	immutable(StructuralSumType!S) x;
	assert(x.fun() == "immutable");
}

// Member functions with non-copyable arguments
@safe unittest {
	static struct NoCopy
	{
		@disable this(this);
	}

	static struct S
	{
		void fun(NoCopy nc) {}
	}

	StructuralSumType!S x;
	assert(__traits(compiles, x.fun(NoCopy())));
}

// Unary operators
@safe unittest {
	alias MySum = StructuralSumType!int;

	MySum x = 123;
	const MySum y = 456;
	immutable MySum z = 789;

	assert(-x == -123);
	assert(-y == -456);
	assert(-z == -789);
}

// Binary operators
@safe unittest {
	alias MySum = StructuralSumType!int;

	MySum x = 123;
	const MySum y = 456;
	immutable MySum z = 789;

	assert(x + y == 579);
	assert(z - y == 333);
}

// Qualified operator overloads
@safe unittest {
	static struct TestUnary
	{
		string opUnary(string op : "-")() inout { return "inout"; }
		string opUnary(string op : "-")() immutable { return "immutable"; }
	}

	static struct TestBinary
	{
		string opBinary(string op : "+")(inout(TestBinary) rhs) inout
		{
			return "inout";
		}
		string opBinary(string op : "+")(immutable(TestBinary) rhs) immutable
		{
			return "immutable";
		}
	}

	immutable(StructuralSumType!TestUnary) tu;
	immutable(StructuralSumType!TestBinary) tb;

	assert(-tu == "immutable");
	assert(tb + tb == "immutable");
}

// Comparison
@safe unittest {
	alias MySum = StructuralSumType!int;

	MySum x = 123;
	MySum y = 456;

	assert(x < 200);
	assert(y >= 200);
	assert(x < y);
	assert(y >= x);
}

// Partial ordering
@safe unittest {
	struct S
	{
		int n;

		double opCmp(S rhs)
		{
			// make numbers with different parity incomparable
			if ((n & 1) == (rhs.n & 1)) {
				return n < rhs.n ? -1 : n > rhs.n ? +1 : 0;
			} else {
				return double.nan;
			}
		}

		double opEquals(S rhs)
		{
			return opCmp(rhs);
		}
	}

	alias MySum = StructuralSumType!S;

	MySum x = S(1);
	MySum y = S(2);
	MySum z = S(3);

	assert(x < z);
	assert(!(y < z));
	assert(!(y >= z));
}

// Workaround for dlang issue 21269
version (D_BetterC) {} else
// Call operator
@safe unittest {
	StructuralSumType!(int function(int) @safe) x = (int n) @safe => n + 1;

	assert(x(123) == 124);
}

// Workaround for dlang issue 21269
version (D_BetterC) {} else
// Call operator that returns by ref
@safe unittest {
	int m;
	alias fun = ref (int _) @safe => m;
	StructuralSumType!(typeof(fun)) x = fun;
	x(0) = 123;

	assert(m == 123);
}

// Assignment + operator
@safe unittest {
	alias MySum = StructuralSumType!int;

	MySum x = 0;

	assert((x += 1) == 1);
	assert((x += 2) == 3);
	assert(((x += 1) += 1) == 5);
}

// Indexing
version(D_BetterC) {} else
@safe unittest {
	int[] a = [1, 2, 3];
	StructuralSumType!(int[]) x = a;

	assert(x[1] == 2);
}

// Index assignment
version(D_BetterC) {} else
@safe unittest {
	int[] a = [1, 2, 3];
	StructuralSumType!(int[]) x = a;
	x[1] = 4;

	assert(a[1] == 4);
}

// Slicing
version(D_BetterC) {} else
@safe unittest {
	int[] a = [1, 2, 3];
	StructuralSumType!(int[]) x = a;

	assert(x[] == a[]);
}

static if (__traits(compiles, { import std.traits: isRvalueAssignable; })) {
	import std.traits: isRvalueAssignable;
} else private {
	enum isRvalueAssignable(Lhs, Rhs = Lhs) = __traits(compiles, lvalueOf!Lhs = rvalueOf!Rhs);
	struct __InoutWorkaroundStruct{}
	@property T rvalueOf(T)(inout __InoutWorkaroundStruct = __InoutWorkaroundStruct.init);
	@property ref T lvalueOf(T)(inout __InoutWorkaroundStruct = __InoutWorkaroundStruct.init);
}
