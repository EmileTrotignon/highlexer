(lang dune 3.7)

(name highlexer)

(using menhir 2.1)

(generate_opam_files true)

(source
 (github EmileTrotignon/highlexer))

(authors "Emile Trotignon")

(maintainers "Emile Trotignon")

(license MIT)

(documentation https://ocaml.org/p/highlexer/latest/doc/index.html)

(package
 (name highlexer)
 (synopsis "An OCaml syntax highlighter based on the OCaml lexer.")
 (description "An OCaml syntax highlighter based on the OCaml lexer.")
 (tags
  (js syntax highlighting))
 (depends
  ocaml
  dune
  menhir
  ocamllex
  (alcotest :with-test)))

