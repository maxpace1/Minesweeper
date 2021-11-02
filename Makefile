.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	dune exec --instrument-with bisect_ppx test/main.exe -- -runner sequential

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe


demo.1:
	clear
	OCAMLRUNPARAM=b dune exec demo1/main.exe

utopdemo.1:
	dune utop src

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
	rm -f minesweeper.zip
	zip -r minesweeper.zip . -x@exclude.lst

clean:
	dune clean
	rm -f minesweeper.zip
	rm -rf _coverage bisect*.coverage

bisect: clean test
	bisect-ppx-report html --theme=light

doc:
	dune build @doc

opendoc:
	open _build/default/_doc/_html/index.html

openbisect:
	open _coverage/index.html
	
