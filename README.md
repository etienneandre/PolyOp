# PolyOp

## Operations on polyhedra

###### v 1.0

Operations on non-necessarily closed, non-necessarily convex polyhedra: (relatively) simple layer on the [Parma Polyhedra Library](http://bugseng.com/products/ppl/) (PPL)
All operations use the `Pointset_Powerset NNC_Polyhedron` data structure from PPL


Some of the operations allowed by PolyOp:
- intersection
- union
- difference
- negation
- satisfiability
- time elapsing
- time backward-elapsing ("past")
- variable elimination (by existential quantification)
- Boolean tests (inclusion, equality…)
- exhibition of a concrete point in the polyhedron

From version 1.0, several operations can be performed sequentially using the same call to PolyOp

Basic call syntax:

```
polyop examples/example.polyop
```

See examples of the input syntax in `examples/example.polyop`.

(Some more examples in `tests/testcases/`)

For any information, please feel free to contact me:

https://lipn.univ-paris13.fr/~andre/
