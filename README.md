sumtype
=======

A sum type for modern D.

Features
--------

- Pattern matching.
- Self-referential types (`This`).
- Full attribute-correctness—`pure`, `@safe`, and `@nogc` where applicable.
- No runtime dependency on `TypeInfo`.

Example
-------

    import std.typecons: Tuple, tuple;

    struct Nil {}
    alias List(T) = SumType!(
        Nil,
        Tuple!(T, "head", This*, "tail")
    );
    alias Cons(T) = Tuple!(T, "head", List!T*, "tail");

    List!T* list(T)(T[] items...)
    {
        if (items.length == 0)
            return new List!T(Nil());
        else
            return new List!T(Cons!T(items[0], list(items[1..$])));
    }

    int sum(List!int l)
    {
        return l.match!(
            (Nil _) => 0,
            cons => cons.head + sum(*cons.tail)
        );
    }

    List!int* myList = list(1, 2, 3, 4, 5);

    assert(sum(*myList) == 15);


Installation
------------

If you're using dub, add the [sumtype](https://code.dlang.org/packages/sumtype)
package to your project as a dependency.

Alternatively, since it's a single, self-contained module, you can simply copy
`sumtype.d` to your source directory and compile as usual.
