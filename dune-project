(lang dune 2.0)
(name collage)
(version 0.1.0)

(source (github smimram/collage))
(license GPL-2.0)
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "Samuel Mimram <smimram@gmail.com>")
(generate_opam_files true)

(package
  (name collage)
  (synopsis "Generate photo collages")
  (description "Generate photo collages by putting multiple pictures, appropriately scaled, on one image. This is mostly useful to generate printed photo books.")
  (depends
    graphics
    camlimages
  )
)
