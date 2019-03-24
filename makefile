.PHONY: main main_byte

all:
	ocamlbuild -classic-display -use-menhir -use-ocamlfind \
	-lflag -thread -cflag -thread \
	-package num,expat,curl,pxp,cduce,utop,ocaml-compiler-libs.toplevel,threads \
	main.byte

clean:
	rm -rf *.cmi *.cmo
	rm -rf main.exe
