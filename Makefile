.PHONY: all test clean

all:
	dune build

test:
	dune runtest

clean:
	dune clean

format:
	ocamlformat --inplace $(git ls-files '*.ml' '*.mli')
