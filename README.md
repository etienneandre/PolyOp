# PolyOp
Operations on non-necessarily closed, non-necessarily convex polyhedra: (relatively) simple layer on the Parma Polyhedra Library (PPL)
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

From version 0.4, several operations can be performed sequentially using the same call to PolyOp

See examples of the syntax in `examples/example.polyop`
