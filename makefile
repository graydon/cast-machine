# Frontend to dune.

.PHONY: default build install uninstall test clean
FLAGS=--profile release

default: build

build:
	@dune build repl.exe $(FLAGS)

run: build
	@rlwrap -a ./_build/default/repl.exe

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
