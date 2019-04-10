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
    ./cast.exe --machine --load <some file>
```
Or to try the repl
```
    ./cast.exe --machine
```
For example, to verify that this machine implements tail call elimination :
```
    ./cast.exe --load tests/factorial2.gcl --monitor
```

The options are:
```
    --machine               : use the abstract machine
    --debug                 : show debugging output
    --verbose (0|1|2)       : adjust verbosity
    --stepmode              : step-by-step execution (type b to go back, anything to go forward)
    --stepmode [integer]    : jump to step <integer> in step-mode
    --load filename         : load a file (see directory /tests)
    --monitor               : print monitor output
```
  

## Syntax

```
e ::=
    | c                                     % CDuce constant
    | x                                     % Variable
    | fun ( tau ) x -> e                 % Lambda abstractions
    | fun x -> e
    | fun xs -> e
    | e e                                   % Application
    | e % tau                               % Cast
    | let rec f x = e
    | let f x = e
    | let f : tau = e                 
    
c := any writable CDuce constant (pushing this definition may break the parser in fixable ways...)

x := ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '\'' '_']*
xs := x list

tau := t | t?

t := any writable CDuce type (cf. above)
t? := any CDuce type with question marks (gradual type) in it
``` 


To be clear: &lambda;<sup>&tau;</sup> x . e is written as `\ tau x . e` or `fun tau x -> e`.


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

