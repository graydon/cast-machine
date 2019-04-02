# cast-machine

## Build

Working on `ocaml 4.07.0`.

Main dependencies: 
- **opam**
- **utop**
- **dune** - working with version 1.8.2
- **cduce** version 1.0 - installed using the **setvariants** branch of the [repo](https://gitlab.math.univ-paris-diderot.fr/cduce/cduce) (`make install` puts cduce_lib inside `~/.opam/4.07.0/cduce/` which is then picked up by **ocamlfind** and then **dune**)
- **menhir**

To build, use the makefile (which is an interface to **dune**):
```
    make
```
And to run it
```
    make run
```


## Compilation issue

This bug is fixed automatically in the build rule of the makefile.
At compile time:
```
    Error: File unavailable: ~/.opam/4.07.0/lib/cduce/+camlp4/camlp4lib.cma
```
A workaround is to create the `+camlp4` directory and link the actual file from `camlp4`
(replacing '4.07.0' with the current **ocaml** switch, for example 'default' or '4.07.1')
```
    mkdir ~/.opam/4.07.0/lib/cduce/+camlp4/ &&
    ln -s ~/.opam/4.07.0/lib/ocaml/camlp4/camlp4lib.cma ~/.opam/4.07.0/lib/cduce/+camlp4/
```

