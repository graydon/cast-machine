Installed on `ocaml 4.07.0`, with `dune`.
Had a few strange bugs, among them:
```
    Error: File unavailable: ~/.opam/4.07.0/lib/cduce/+camlp4/camlp4lib.cma
```
which I fixed (very poorly) by creating the weird `+camlp4` directory and linking 
the actual file from `camlp4`:
```
    mkdir ~/.opam/4.07.0/lib/cduce/+camlp4/ &&
    ln -s ~/.opam/4.07.0/lib/ocaml/camlp4/camlp4lib.cma ~/.opam/4.07.0/lib/cduce/+camlp4/
```