(lang dune 3.16)

(name http_server_ocaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name http_server_ocaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune oUnit)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
