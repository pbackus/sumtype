sumtype
=======

A sum type for modern D.

Features
--------

- Pattern matching.
- Self-referential types (`This`).
- Full attribute correctness—`pure`, `@safe`, `@nogc`, and `nothrow` when
  applicable.
- No runtime dependency on `TypeInfo`.
- No heap allocation/boxing.

[Documentation][docs]
---------------------

[docs]: https://pbackus.github.io/sumtype/sumtype.html

Example
-------

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


Installation
------------

If you're using dub, add the [sumtype](https://code.dlang.org/packages/sumtype)
package to your project as a dependency.

Alternatively, since it's a single, self-contained module, you can simply copy
`sumtype.d` to your source directory and compile as usual.
