# Frontend to dune.

.PHONY: default build install uninstall test clean
FLAGS=--profile release

default: build

CDUCE_LIB_DIR="`ocamlfind query cduce`"

build:
	@if [ ! -d "$(CDUCE_LIB_DIR)/+camlp4" ]; then\
		mkdir "$(CDUCE_LIB_DIR)/+camlp4";\
		ln -s "$(CDUCE_LIB_DIR)/../ocaml/camlp4/camlp4lib.cma" "$(CDUCE_LIB_DIR)/+camlp4";\
	fi
	@dune build repl.exe $(FLAGS)

run: build
	@rlwrap -a ./_build/default/repl.exe

machine: build
	@rlwrap -a ./_build/default/repl.exe --machine

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

top:
	dune utop $(FLAGS) 
# Optionally, remove all files/folders ignored by git as defined
# in .gitignore (-X)
