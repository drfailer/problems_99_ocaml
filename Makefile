all: ./bin/main.ml
	dune build
	dune exec problems_99_ocaml
