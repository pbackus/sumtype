sumtype
=======

A sum type for modern D.

Random change to check that tests run on pull requests.

Features
--------

- Pattern matching, including support for introspection-based matching.
- Self-referential types, using `This`.
- Works with `pure`, `@safe`, `@nogc`, `nothrow`, and `immutable`.
- Compatible with `-betterC` and `-dip1000`.
- Zero runtime overhead compared to hand-written C.
    - No heap allocation.
    - Does not rely on runtime type information (`TypeInfo`).

Documentation
-------------

[View online on Github Pages.][docs]

`sumtype` uses [adrdox][] to generate its documentation. To build your own
copy, run the following command from the root of the `sumtype` repository:

    path/to/adrdox/doc2 --genSearchIndex --genSource -o generated-docs src

[docs]: https://pbackus.github.io/sumtype/sumtype.html
[adrdox]: https://github.com/adamdruppe/adrdox

Example
-------

    import std.math: isClose;

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

    assert(toFahrenheit(t1).degrees.isClose(98.6));
    assert(toFahrenheit(t2).degrees.isClose(212));
    assert(toFahrenheit(t3).degrees.isClose(32));

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
    assert(toFahrenheit(t1).degrees.isClose(32));

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

[![Open on run.dlang.io](https://img.shields.io/badge/run.dlang.io-open-blue.svg)](https://run.dlang.io/is/jYkbjP)

Installation
------------

If you're using dub, add the [sumtype](https://code.dlang.org/packages/sumtype)
package to your project as a dependency.

Alternatively, since it's a single, self-contained module, you can simply copy
`sumtype.d` to your source directory and compile as usual.
