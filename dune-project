(lang dune 3.11)

(name instrumentation2)

(generate_opam_files true)

(source
 (github formalsec/instrumentation2))

(authors "Filipe Marques")

(maintainers "Filipe Marques")

(license GPL-3.0-only)

(package
 (name instrumentation2)
 (synopsis "Instrumentation2")
 (description "Instrumentation2")
 (depends
  ocaml
  dune
  (cmdliner (>= 1.2.0))
  (yojson (>= 2.1.2))
  (bos (>= 0.2.1))
  (bisect_ppx (and :with-test (>= 2.5.0))))
 (tags
  (javascript nodejs exploit vulnerability)))
