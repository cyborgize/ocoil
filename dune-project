(lang dune 3.0)

(package
 (name ocoil_lib)
 (depends ocaml dune sexplib0 ppx_sexp_conv uuidm))

(package
 (name ocoil)
 (depends ocaml dune ocoil_lib (cmdliner (>= 1.3.0))))