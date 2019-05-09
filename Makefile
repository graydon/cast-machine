.PHONY: main main_byte web clean

CDUCE := $(shell ocamlfind query cduce)

main:
	ocamlbuild -use-menhir -use-ocamlfind -I src main.native \
	-cflags '-w -58' \
	-cflags '-I '${CDUCE} \
	-lflags '-I '${CDUCE}' 'cduce_lib.cmxa  \
	-package expat -package pxp -package curl,camlp4.lib,num,dynlink

main_byte:
	ocamlbuild -use-menhir -use-ocamlfind -I src main.byte \
	-cflags '-w -58' \
	-cflags '-I '${CDUCE} \
	-lflags '-I '${CDUCE}' 'cduce_lib.cma  \
	-package expat -package pxp -package curl,camlp4.lib,num,dynlink

web:
	ocamlbuild -use-menhir -use-ocamlfind \
	-tag-line "not (<**/parser.*>): package(js_of_ocaml), package(js_of_ocaml-ppx)" \
	-cflags '-I '${CDUCE} \
	-lflags '-I '${CDUCE}' 'cduce_lib.cma web.byte \
	js_of_ocaml +nat.js +dynlink.js +toplevel.js +weak.js web.byte

dune:
	dune build ./repl.exe $(FLAGS)
	if [ -e ${repl_bc} ]; then\
		ln -f "${repl_bc}" "${CURDIR}/bin/repl.bc";\
	fi

top:
	dune utop

install: web
	scp base.js jquery.js style.css web.js index.html \
	cduce@dev.cduce.org:/var/www/cduce/ocaml/

clean:
	ocamlbuild -clean