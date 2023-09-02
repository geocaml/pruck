(lang dune 3.9)
(using mdx 0.3)

(name pruck)

(generate_opam_files true)

(source
 (github patricoferris/pruck))

(authors "Patrick Ferris")

(maintainers "Patrick Ferris")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name pruck)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
