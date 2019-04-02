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

## Syntax

```
e ::=
    | c                                     % CDuce constant
    | x                                     % Variable
    | \ { tau : tau } x . e                 % Lambda abstractions
    | fun { tau : tau } x . e
    | e e                                   % Application
    | e % tau                               % Cast
    
c := any writable CDuce constant (pushing this definition may break the parser in fixable ways...)

x := ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '\'' '_']*

tau := t | ? (structural gradual types not yet available)

t := any writable CDuce type (cf. above)
``` 


To be clear: &lambda;<sup>&tau;<sub>1</sub> &rarr; &tau;<sub>2</sub></sup> x . e is written as `\ { tau_1 : tau_2 } x . e`.


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

