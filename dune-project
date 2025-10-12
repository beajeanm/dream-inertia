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
  (ocaml
   (>= 5.0))
  (dune
   (>= 3.20))
  (yojson
   (>= 3.0))
  (dream
   (>= 1.0.0~alpha8))
  (alcotest :with-test)
  (alcotest-lwt :with-test)
  ; Dev tools
  (bisect_ppx
   (and
    :with-dev-setup
    (>= 2.5.0)))
  (ocaml-lsp-server
   (and
    :with-dev-setup
    (>= 1.19.0)))
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.27.0)))
  (utop
   (and
    :with-dev-setup
    (>= 2.15.0)))))
