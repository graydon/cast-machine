all:
	ocamlc -o main.exe types.ml main.ml

clean:
	rm -rf *.cmi *.cmo
	rm -rf main.exe