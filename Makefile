all: kokac

kokac: src/type.ml src/kokalane.ml src/parser.mly src/lexer.mll src/effect.ml src/infer.ml src/codegen.ml src/annot.ml src/error.ml src/syntax.ml
	dune build
	cp src/kokalane.exe ./kokac

clean:
	rm kokac
	rm -r _build

rapport: rapport.tex
	latexmk rapport.tex
	latexmk -c
	rm rapport.bbl
	rm rapport.run.xml
