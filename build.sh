#!/bin/sh

#################################################################
 #
 #                       BUILDATOR
 # 
 # Script to build PolyOp
 #
 # Universite Paris 13, Sorbonne Paris Cite, LIPN (France)
 # École Centrale Nantes, France
 # 
 # Author:        Etienne Andre
 # 
 # Created:       2016/02/29
 # Last modified: 2016/02/29
 #
################################################################

if [ -f "setup.ml" ]
then
  ocaml setup.ml -distclean
fi

oasis setup \
&& ocaml setup.ml -configure --enable-tests \
&& ocaml setup.ml -all
