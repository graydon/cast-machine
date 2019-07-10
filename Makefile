.PHONY: main bench main_byte web clean

CDUCE := $(shell ocamlfind query cduce)

main:
	cppo ${CURDIR}/src/exec.cppo.ml -o ${CURDIR}/src/exec.ml
	ocamlbuild -use-menhir -use-ocamlfind -I src main.native \
	-cflags '-w -58' \
	-cflags '-I '${CDUCE} \
	-lflags '-I '${CDUCE}' 'cduce_lib.cmxa  \
	-package expat -package pxp -package curl,camlp4.lib,num,dynlink

bench:
	@echo "Compiling in BENCH mode; no debug output"
	cppo -D BENCH ${CURDIR}/src/exec.cppo.ml -o ${CURDIR}/src/exec.ml
	ocamlbuild -use-menhir -use-ocamlfind -I src main.native \
	-cflags '-w -58' \
	-cflags '-I '${CDUCE} \
	-lflags '-I '${CDUCE}' 'cduce_lib.cmxa  \
	-package expat -package pxp -package curl,camlp4.lib,num,dynlink


monitor:
	@echo "Compiling in BENCH mode; no debug output"
	cppo -D BENCH -D MONITOR ${CURDIR}/src/exec.cppo.ml -o ${CURDIR}/src/exec.ml
	ocamlbuild -use-menhir -use-ocamlfind -I src main.native \
	-cflags '-w -58' \
	-cflags '-I '${CDUCE} \
	-lflags '-I '${CDUCE}' 'cduce_lib.cmxa  \
	-package expat -package pxp -package curl,camlp4.lib,num,dynlink


byte:
	cppo ${CURDIR}/src/exec.cppo.ml -o ${CURDIR}/src/exec.ml
	ocamlbuild -use-menhir -use-ocamlfind -tag debug -I src main.byte \
	-cflags '-w -58' \
	-cflags '-I '${CDUCE} \
	-lflags '-I '${CDUCE}' 'cduce_lib.cma  \
	-package expat -package pxp -package curl,camlp4.lib,num,dynlink

top: main
	ocamlmktop -use-ocamlfind -o toptest

web:
	ocamlbuild -use-menhir -use-ocamlfind \
	-tag-line "not (<**/parser.*>): package(js_of_ocaml), package(js_of_ocaml-ppx)" \
	-cflags '-I '${CDUCE} \
	-lflags '-I '${CDUCE}' 'cduce_lib.cma web.byte \
	js_of_ocaml +nat.js +dynlink.js +toplevel.js +weak.js web.byte


install: web
	scp base.js jquery.js style.css web.js index.html \
	cduce@dev.cduce.org:/var/www/cduce/ocaml/

clean:
	ocamlbuild -clean
