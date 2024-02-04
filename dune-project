(lang dune 3.11)

(name instrumentation2)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license GPL-3.0-only)

(documentation https://url/to/documentation)

(package
 (name instrumentation2)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  (cmdliner (>= 1.2.0))
  (yojson (>= 2.1.2))
  (bos (>= 0.2.1)))
 (tags
  (topics "to describe" your project)))
