(lang dune 3.20)
(name advent_of_fpga)
(generate_opam_files true)
(package
 (name advent_of_fpga)
 (allow_empty)
 (depends
  (ocaml (>= 5.2.0+ox))
  core
  hardcaml
  hardcaml_waveterm
  ppx_hardcaml
  ppx_jane
  stdio))
