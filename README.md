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


Installation
------------

If you're using dub, add the [sumtype](https://code.dlang.org/packages/sumtype)
package to your project as a dependency.

Alternatively, since it's a single, self-contained module, you can simply copy
`sumtype.d` to your source directory and compile as usual.
