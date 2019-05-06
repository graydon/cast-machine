# Frontend to dune.

.PHONY: default build install uninstall test clean
FLAGS=

default: build
repl_exe="${CURDIR}/_build/default/repl.exe"
repl_bc="${CURDIR}/_build/default/repl.bc"

CDUCE_LIB_DIR="`ocamlfind query cduce`"

native:
	dune build repl.bc $(FLAGS)
	if [ -e ${repl_bc} ]; then\
		ln -f "${repl_bc}" "${CURDIR}/bin/repl.bc";\
	fi


build: native
	@echo "If this fails due to camlp4lib, try deleting the directory"
	@echo "'.opam/<switch>/lib/cduce/+camlp4' and running 'make fix_linking' as root"
	dune build repl.exe $(FLAGS)
	chmod +x cast.exe
	cp _build/default/repl.exe bin
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
