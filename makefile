# Frontend to dune.

.PHONY: default build install uninstall test clean
FLAGS=--profile release

default: build

CDUCE_LIB_DIR="`ocamlfind query cduce`"

build:
	@echo "If this fails due to camlp4lib, try 'make fix_linking' as root"
	dune build repl.exe $(FLAGS)
	rm -f cast.exe
	echo "#! /bin/sh" >> cast.exe
	echo "" >> cast.exe
	echo "rlwrap -a _build/default/repl.exe \"\$$@\" " >> cast.exe
	chmod +x cast.exe
	@echo ""
	@echo "Build finished. You can now start ./cast.exe"

fix_linking:
	ln -fs "$(CDUCE_LIB_DIR)/../ocaml/camlp4" "$(CDUCE_LIB_DIR)/+camlp4";\

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
