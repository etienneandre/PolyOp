# PolyOp

## Operations on polyhedra

###### v 1.3

Operations on non-necessarily closed, non-necessarily convex polyhedra: (relatively) simple layer on the [Parma Polyhedra Library](http://bugseng.com/products/ppl/) (PPL), with some hopefully useful additions.
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
- updates of variable (i.e. replacing their value with a linear combination of variables)
- `exhibitpoint`: exhibition of a concrete point in the polyhedron
- `zonepred`: computation of the predecessors of a subset of a zone within a source zone (given the set of variables subject to time-elapsing (typically clocks), and the set of variables reset between the two zones); this function is typically useful to reason about parametric zones in parametric timed automata or parametric time Petri nets [not sure this function corresponds to an actual problem in PTAs / PTPNs]
- `zonepredgu`: Given `Zn-1` and `Zn` such that `Zn` is the successor zone of `Zn-1` by guard `g-1` and updating variables in `Un-1` to some values, given `Zn+1` a set of concrete points (valuations) successor of zone `Zn` by elapsing of a set of variables `t`, by guard `gn`, updates `Rn`, then `zonepredgr(Zn-1, gn-1, Un-1, Zn, t, gn, Un, Zn+1)` computes the subset of points in `Zn` that are predecessors of `Zn` (by updates of `Un`, guard `gn`, elapsing of `t`), and that are direct successors (without time elapsing) of `Zn-1` via `gn-1` and `Un-1`. This function can typically used when running a backward analysis in a zone graph of a PTA / PTPN.

From version 1.0, several operations can be performed sequentially using the same call to PolyOp

Basic call syntax:

```
polyop examples/example.polyop
```

See examples of the input syntax in `examples/example.polyop`.

(Some more examples in `tests/testcases/`)

For any information, please feel free to contact me:

https://lipn.univ-paris13.fr/~andre/
