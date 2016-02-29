############################################################
#
#                    PolyOp
#
#           (Operations on polyhedra)
#
#  National University of Singapore
#
#  Author:        Etienne ANDRE
#  Created:       2011/04/27
#  Last modified: 2011/05/30
#  Ocaml version: 3.12.0
#
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
############################################################


# flags for ocaml compiler
#OCAMLC_FLAGS = -g
OCAMLC_FLAGS = 

# ocaml compiler
OCAMLC = ocamlc $(OCAMLC_FLAGS)
OCAMLOPT = ocamlopt.opt $(OCAMLC_FLAGS)

# path variables
ifndef EXTLIB_PATH
  EXTLIB_PATH = /usr/lib/ocaml/extlib
endif
ifndef OCAML_PPL_PATH
#  OCAML_PPL_PATH = $(HOME)/local/lib/ppl
  OCAML_PPL_PATH = /usr/lib/ppl
endif 
ifndef OCAML_GMP_PATH
 # OCAML_GMP_PATH = $(HOME)/local/mlgmp
	OCAML_GMP_PATH = /usr/lib/ocaml/gmp
endif
ifndef CLIB_PATH
  CLIB_PATH = /usr/lib -I /usr/local/lib
endif 

# export paths for use in sub-makefiles
export EXTLIB_PATH
export OCAML_PPL_PATH
export OCAML_GMP_PATH

INCLUDE = -I $(SRC) -I $(EXTLIB_PATH) -I $(OCAML_PPL_PATH) -I $(OCAML_GMP_PATH) -I $(CLIB_PATH)

# native c libraries
CLIBS = -cclib -lm -cclib -lgmpxx -cclib -lgmp -cclib -lppl

# FOR STATIC COMPILING IN 32 BITS
STATIC32CLIBS = -cclib '-static -lppl -lcamlrun -ltinfo -lppl_ocaml -lstdc++ -lgmp -lgmpxx'

# ocaml lib files
OLIBS = str.cma unix.cma extLib.cma bigarray.cma gmp.cma ppl_ocaml.cma 

# native ocaml lib files
OOLIBS = str.cmxa unix.cmxa extLib.cmxa bigarray.cmxa gmp.cmxa ppl_ocaml.cmxa

# external libs for compiling with PPL support
export LIBS = $(CLIBS) $(OLIBS)
export OPTLIBS = $(CLIBS) $(OOLIBS) 

export STATIC32LIBS = $(STATIC32CLIBS) $(OLIBS)

# (old version)
# CLIBS = -cclib '-static -lppl -lpwl -lcamlrun -ltinfo -lppl_ocaml -lstdc++ -lmlgmp -lmpfr -lgmp -lgmpxx'

# ALLOWS STATIC COMPILING IN 64 BITS :-)
# CLIBS = -cclib '-static -lppl -lpwl -lppl_ocaml -lstdc++ -lmlgmp -lmpfr -lgmp -lgmpxx ' 

# external libs for compiling with PPL support
export LIBS = $(CLIBS) $(OLIBS)
# export OPTLIBS = $(CLIBS) $(OOLIBS) 


SRC = src

# FILES
.PREFIXES : +.
.SUFFIXES : .cmo .cmi .ml .mli .cmxo .cmx

# main object
MAIN = $(SRC)/PolyOp.cmo
MAIN_OPT = $(MAIN:.cmo=.cmx)

# sources to compile
FILES =  $(SRC)/Global.+ $(SRC)/NumConst.+ $(SRC)/LinearConstraint.+ $(SRC)/ProgramLexer.+ $(SRC)/ProgramParser.+ $(SRC)/ProgramPrinter.+ $(SRC)/ProgramConverter.+
OBJS = $(FILES:+=cmo)
OBJS_OPT = $(OBJS:.cmo=.cmx)

# header files
FILESMLI = $(SRC)/Global.+ $(SRC)/NumConst.+ $(SRC)/LinearConstraint.+ $(SRC)/ParsingStructure.+ $(SRC)/AbstractStructure.+ $(SRC)/ProgramPrinter.+ $(SRC)/ProgramConverter.+

# parsers and lexers 
LEXERS = $(SRC)/ProgramLexer.+
PARSERS = $(SRC)/ProgramParser.+

# target library
IMILIB = lib/polyop.cma
IMILIB_OPT = $(IMILIB:.cma=.cmxa)

# target executable
TARGET = bin/PolyOp
TARGET_OPT = bin/PolyOp.opt
TARGET_STATIC = bin/PolyOp32

default all: $(TARGET)
opt: $(TARGET_OPT)
static32: $(TARGET_STATIC)

$(IMILIB): header parser $(OBJS)
	@ echo [MKLIB] $@
	@ $(OCAMLC) -a -o $@ $(OBJS)  

$(IMILIB_OPT): header parser $(OBJS_OPT)  
	@ echo [MKLIB] $@
	@ $(OCAMLOPT) -a -o $@ $(OBJS_OPT)

$(TARGET): $(IMILIB) $(MAIN)
	@ echo [LINK] $(TARGET)
	@ $(OCAMLC) -o $(TARGET) $(INCLUDE) $(LIBS) $(IMILIB) $(MAIN)

$(TARGET_OPT): $(IMILIB_OPT) $(MAIN_OPT)
	@ echo [LINK] $(TARGET_OPT)
	$(OCAMLOPT) -o $(TARGET_OPT) $(INCLUDE) $(OPTLIBS) $(IMILIB_OPT) $(MAIN_OPT)

$(TARGET_STATIC): $(IMILIB) $(MAIN)
	@ echo [LINK] $(TARGET_STATIC)
	@ $(OCAMLC) -custom $(INCLUDE) -I $(CLIB_PATH) $(STATIC32LIBS) $(IMILIB) $(MAIN) -o $(TARGET_STATIC)

header: $(FILESMLI:+=cmi)

parser: $(PARSERS:+=ml) $(LEXERS:+=ml) header $(PARSERS:+=cmi)

$(SRC)/%.cmo: $(SRC)/%.ml $(SRC)/%.mli
	@ echo [OCAMLC] $<
	@ $(OCAMLC) -c $(INCLUDE) $<	

$(SRC)/%.cmo: $(SRC)/%.ml
	@ echo [OCAMLC] $<
	@ $(OCAMLC) -c $(INCLUDE) $<	

$(SRC)/%.cmx: $(SRC)/%.ml $(SRC)/%.mli
	@ echo [OCAMLOPT] $<
	@ $(OCAMLOPT) -c $(INCLUDE) $<	

$(SRC)/%.cmx: $(SRC)/%.ml
	@ echo [OCAMLOPT] $<
	@ $(OCAMLOPT) -c $(INCLUDE) $<	

$(SRC)/%.cmi: $(SRC)/%.mli
	@ echo [OCAMLC] $<
	@ $(OCAMLC) -c $(INCLUDE) $<

$(SRC)/%.cmi: $(SRC)/%.mly
	@ echo [YACC] $<
	@ ocamlyacc $<
	@ echo [OCAMLC] $(SRC)/$*.mli
	@ $(OCAMLC) -c $(INCLUDE) $(SRC)/$*.mli

$(SRC)/%.ml: $(SRC)/%.mly 
	@ echo [YACC] $<
	@ ocamlyacc $<

$(SRC)/%.ml: $(SRC)/%.mll 
	@ echo [LEX] $<
	@ ocamllex $< 

# dependencies
.depend:
	@ echo [OCAMLDEP]
	@ ocamldep -I $(SRC) $(SRC)/*.ml $(SRC)/*.mli > .depend 


test:
	make
	make exe

exe:
	bin/PolyOp test.polyop -debug low

count: clean
	@ for f in src/*.ml src/*.mli; do wc -l $$f; done | sort -n -r -


clean: rmtpf rmuseless
	@rm -rf $(LEXERS:+=ml) $(PARSERS:+=mli) $(PARSERS:+=ml)
	@rm -rf $(TARGET) $(IMILIB) $(TARGET_OPT) $(IMILIB_OPT)
	@rm -rf .depend
#	@cd test; make clean


rmtpf:
	@rm -rf *~


rmuseless:
	@rm -rf $(FILES:+=cmo) $(FILES:+=cmx) $(FILES:+=cmi) $(FILES:+=o) $(MAIN) $(MAIN:.cmo=.cmi) $(MAIN:.cmo=.cmx)
	@rm -rf $(FILESMLI:+=cmi)

include .depend
