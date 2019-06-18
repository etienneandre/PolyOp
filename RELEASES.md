*******************************************************
# release 1.2    (2019-06-18, build 169)

### Features
* New operation: applies updates to a polyhedron (i.e. replacing their value with a linear combination of variables)
* New operation: `zonepredgu`: Given `Zn-1` and `Zn` such that `Zn` is the successor zone of `Zn-1` by guard `g-1` and updating variables in `Un-1` to some values, given `Zn+1` a set of concrete points (valuations) successor of zone `Zn` by elapsing of a set of variables `t`, by guard `gn`, updates `Rn`, then `zonepredgr(Zn-1, gn-1, Un-1, Zn, t, gn, Un, Zn+1)` computes the subset of points in `Zn` that are predecessors of `Zn` (by updates of `Un`, guard `gn`, elapsing of `t`), and that are direct successors (without time elapsing) of `Zn-1` via `gn-1` and `Un-1`. This function can typically used when running a backward analysis in a zone graph of a PTA / PTPN.


*******************************************************
# release 1.1    (2019-06-04, build 135)

### Features
* New operation to compute the predecessors of a subset of a zone within a source zone (given the set of variables subject to time-elapsing (typically clocks), and the set of variables reset between the two zones); this function is typically useful to reason about parametric zones in parametric timed automata or parametric time Petri nets


*******************************************************
# release 1.0    (2019-06-03, build 117)

### Features
* Allow exhibition of a point in a polyhedron
* Allow for more than one operation at a time

### Options
* Option `-debug` becomes `-verbose`

### Output
* Results are better organized
* Minor corrections in display (fractions, time)

### Internal
* Renamed most modules
* Added basic non-regression tests


*******************************************************
# release 0.3    (2019-01-22, build 55)

### Features
* Allow for "time past"
* Allow non-convex difference

### Bug fix
* Fix bugs in non-convex inclusion and equality checks

### Syntax
* Allows `OR`, `or` and `||` as disjunction symbols

### Internal
* Strip binary (much smaller size)


*******************************************************
# release 0.2    (2016-03-01, build 34)

### Features
* Allow for "not" operation
* Allow disjunctions (using `or`) in input constraints

### Internal
* Now using _oasis to compile
* Now PolyOp has a build number
* Relying entirely on PPL.Pointset_Powerset_NNC_Polyhedron (even when no disjunction is used)

### Dissemination
* Source code on GitHub


*******************************************************
# release 0.1    (2011-05-30)

First version of PolyOp
