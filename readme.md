# cast-machine

## Build

Installed on `ocaml 4.07.0`.

Main dependencies: 
- `opam`
- `utop` - installed through `opam`
- `dune` - working with version 1.8.2
- `cduce` version 1.0 - installed using the `setvariants` branch of the [repo](https://gitlab.math.univ-paris-diderot.fr/cduce/cduce) (for me, `make install` puts cduce_lib inside `~/.opam/4.07.0/cduce/` which is then picked up by `dune`)
- `menhir`

To build, use the makefile (which is an interface to `dune`):
```
    make
```
And to run it
```
    make run
```


## Issues

Had a few strange bugs, among them:
```
    Error: File unavailable: ~/.opam/4.07.0/lib/cduce/+camlp4/camlp4lib.cma
```
which I suggest fixing by creating the `+camlp4` directory and linking 
the actual file from `camlp4`:
```
    mkdir ~/.opam/4.07.0/lib/cduce/+camlp4/ &&
    ln -s ~/.opam/4.07.0/lib/ocaml/camlp4/camlp4lib.cma ~/.opam/4.07.0/lib/cduce/+camlp4/
```
