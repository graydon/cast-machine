# cast-machine

## Build

Working on `ocaml 4.07.0`.

Main dependencies: 
- **opam**
- **utop**
- **dune**
- **cduce** version 1.0, either the **cduce-next** or the **setvariants** branches of the [repo](https://gitlab.math.univ-paris-diderot.fr/cduce/cduce) work fine. It should be installed some place where ocamlfind can pick it up. From a clone of the repo, `make install` puts cduce_lib inside `~/.opam/<switch-name>/cduce/` which is in dune's path.
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
To check on tail call elimination :
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
prog := e ;;

e ::=
    | c                                     % CDuce constant
    | x                                     % Variable
    | fun ( tau ) x -> e                 % Lambda abstractions
    | fun x -> e
    | fun xs -> e
    | e e                                   % Application
    | e % tau                               % Cast
    | let rec f x = e in e
    | let f x = e in e                      % By default, f gets type ? -> ?
    | let f : tau = e in e                 
    
c := any writable CDuce constant (pushing this definition may break the parser in fixable ways...)

x := ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '\'' '_']*
xs := x list

tau := t | t?

t := any writable CDuce type (cf. above)
t? := any CDuce type with question marks (gradual type) in it
``` 


To be clear: &lambda;<sup>&tau;</sup> x . e is written as `\ tau x . e` or `fun tau x -> e`.

## Example program

Non-tail recursive factorial
```=ocaml
let rec fac n =
    if n then 1
    else n * f (n-1)
in fac 42;;
```

Tail recursive factorial
```
let rec fac = fun acc n ->
    if n then acc
    else fac (n*acc) (n-1)
in fac 1 42;;
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

