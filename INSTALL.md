# Installing PolyOp

This installation works on a Linux-based OS.
Other OS were not tested.

Note: In case of problem, please read this INSTALL file carefully before reporting a bug.


#### Packages

Install the following packages

```
sudo apt-get install g++ m4 ocaml libextlib-ocaml libextlib-ocaml-dev libgmp-dev libgmp-ocaml libgmp-ocaml-dev libppl-dev oasis
```

(Installing these libraries might entail additional dependencies.)


#### Download and compile the Parma Polyehedra Library

Download the PPL source from here: http://bugseng.com/products/ppl/download

Compile the PPL binding for OCaml:

```
cd ppl-1.2
./configure --prefix=/usr
cd interfaces/OCaml/
make
sudo make install
```

### Download PolyOp

Download the PolyOp source


### Compile

```
cd polyop
```

Compile PolyOp:

```
sh build.sh
```

Enjoy!
