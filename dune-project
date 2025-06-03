(lang dune 3.0)

(package
 (name copper_trace_lib)
 (depends ocaml dune sexplib0 ppx_sexp_conv))

(package
 (name copper_trace)
 (depends ocaml dune copper_trace_lib (cmdliner (>= 1.3.0))))