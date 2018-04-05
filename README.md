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

    alias Tree = SumType!(int, Tuple!(This*, "left", This*, "right"));
    alias Node = Tuple!(Tree*, "left", Tree*, "right");

    Node node(Tree* left, Tree* right)
    {
        return tuple!("left", "right")(left, right);
    }

    int[] inorder(Tree t)
    {
        return t.match!(
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


Installation
------------

If you're using dub, add the [sumtype](https://code.dlang.org/packages/sumtype)
package to your project as a dependency.

Alternatively, since it's a single, self-contained module, you can simply copy
`sumtype.d` to your source directory and compile as usual.
