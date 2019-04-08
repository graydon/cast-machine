# Frontend to dune.

.PHONY: default build install uninstall test clean
FLAGS=--profile release

default: build

CDUCE_LIB_DIR="`ocamlfind query cduce`"

build:
	if [ ! -d "$(CDUCE_LIB_DIR)/+camlp4" ]; then\
		mkdir "$(CDUCE_LIB_DIR)/+camlp4";\
		ln -s "$(CDUCE_LIB_DIR)/../ocaml/camlp4/camlp4lib.cma" "$(CDUCE_LIB_DIR)/+camlp4";\
	fi
	dune build repl.exe $(FLAGS)
	rm -f cast.exe
	echo "#! /bin/sh" >> cast.exe
	echo "" >> cast.exe
	echo "rlwrap -a _build/default/repl.exe \"\$$@\" " >> cast.exe
	chmod +x cast.exe
	@echo ""
	@echo "Build finished. You can now start ./cast.exe"

debug:
	dune build repl.exe $(FLAGS) --debug-backtraces
	rlwrap -a ocamldebug ./_build/default/repl.exe

run: build
	@rlwrap -a ./_build/default/repl.exe

machine: build
	@rlwrap -a ./_build/default/repl.exe --machine

symbolic: build
	@rlwrap -a ./_build/default

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

watch:
	@dune watch ./repl.exe $(FLAGS)

top:
	dune utop $(FLAGS) 
# Optionally, remove all files/folders ignored by git as defined
# in .gitignore (-X)
