.PHONY: main main_byte

all:
	ocamlbuild -use-menhir -use-ocamlfind \
	-cflags '-I '`pwd`/../cduce/lib/ \
	-lflags '-I '`pwd`'/../cduce/lib/ 'cduce_lib.cmxa \
	main.native \
	-package expat -package pxp -package curl \
	-package camlp4 -package camlp4.lib \
	-package num


clean:
	rm -rf *.cmi *.cmo
	rm -rf main.exe
