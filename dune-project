(lang dune 2.8)

(name dream-inertia)

(source (uri git+https://git.jm-bea.net/beajeanm/ocaml-inertia.git))
(homepage https://git.jm-bea.net/beajeanm/ocaml-inertia)
(bug_reports https://git.jm-bea.net/beajeanm/ocaml-inertia)

(license ISC)

(authors "Jean-Michel Bea")
(maintainers "Jean-Michel Bea")

(generate_opam_files true)

(package
 (name dream-inertia)
 (synopsis "Inertia handler from dream application")
 (description "Inertia handler from dream application")
 (depends

   (ocaml (>= 4.12))
   dream
   (ppx_yojson_conv (>= v0.14))
   (lambdasoup (>= 0.6.1))))

