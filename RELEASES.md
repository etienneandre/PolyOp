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
