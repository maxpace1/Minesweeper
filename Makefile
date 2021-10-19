.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

demo.1:
	OCAMLRUNPARAM=b dune exec demo1/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

lines: 
	dune clean
	ocamlbuild -clean
	cloc --by-file --include-lang=OCaml .
	make build

zip:
	rm -f adventure.zip
	zip -r adventure.zip . -x@exclude.lst

clean:
	dune clean
	rm -f adventure.zip

doc:
	dune build @doc
