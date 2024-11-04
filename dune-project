(lang dune 3.16)

(name erg)

(generate_opam_files true)

(source
 (github krios2146/erg))

(authors "Valdimir Kidyaev")

(maintainers "Valdimir Kidyaev")

(license MIT)

(documentation https://github.com/krios2146/erg)

(package
 (name erg)
 (synopsis "Low level HTTP/1.1 server implementation library")
 (depends ocaml dune (oUnit :with-test) (ocamlformat :dev)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
