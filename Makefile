all: kokac

kokac: src/type.ml src/kokalane.ml src/parser.mly src/lexer.mll src/effect.ml
	dune build
	cp src/kokalane.exe ./kokac
