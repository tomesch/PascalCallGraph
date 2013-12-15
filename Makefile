TARGET = parser

all: parser clean

parser: lexer.cmo parser.cmo main.cmo
	ocamlc -o $@ lexer.cmo parser.cmo main.cmo

clean:
	rm -rf parser.ml parser.mli lexer.ml lexer.cmi lexer.cmo main.cmi main.cmo parser.cmi parser.cmo syntax.cmi

depend:
	ocamldep *.ml *.mli > .depend

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi

.ml.cmo:
	ocamlc -c $<
.mli.cmi:
	ocamlc -c $<
.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc $<
.mly.mli:
	ocamlyacc $<

include .depend