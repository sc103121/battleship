.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f battleship.zip
	zip -r battleship.zip . -x@exclude.lst

clean:
	dune clean
	rm -f battleship.zip

docs:
	dune build @doc