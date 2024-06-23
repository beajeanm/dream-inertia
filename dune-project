(lang dune 3.15)

(name dream_inertia)

(generate_opam_files true)

(source
 (github beajeanm/dream-inertia))

(authors "Jean-Michel Bea")

(maintainers "Jean-Michel Bea")

(license MIT)

(package
 (name dream_inertia)
 (synopsis "A dream adapter for Inertia.js")
 (description "A dream adapter for Inertia.js")
 (depends
  (yojson
   (>= 2.2.1))
  (dream
   (>= 1.0.0~alpha6))
  ocaml
  dune))
