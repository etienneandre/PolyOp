#!/bin/sh

#################################################################
 #
 #                       BUILDATOR
 # 
 # Script to build PolyOp
 #
 # École Centrale Nantes, France
 # Université Paris 13, LIPN, CNRS, France
 # 
 # Author:        Étienne André
 # 
 # Created:       2016/02/29
 # Last modified: 2019/06/04
 #
################################################################

if [ -f "setup.ml" ]
then
  ocaml setup.ml -distclean
fi

oasis setup \
&& ocaml setup.ml -configure --enable-tests \
&& ocaml setup.ml -all
