sumtype
=======

A sum type for modern D.

Features
--------

- Pattern matching.
- Self-referential types (`This`).
- Full attribute-correctness—`pure`, `@safe`, and `@nogc` where applicable.
- No runtime dependency on `TypeInfo`.

[Documentation][docs]
---------------------

[docs]: https://pbackus.github.io/sumtype/

Example
-------

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


Installation
------------

If you're using dub, add the [sumtype](https://code.dlang.org/packages/sumtype)
package to your project as a dependency.

Alternatively, since it's a single, self-contained module, you can simply copy
`sumtype.d` to your source directory and compile as usual.
