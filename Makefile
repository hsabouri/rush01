RESULT = main
SOURCES = \
	pika.mli \
	pika.ml \
	main.ml

PACKS = lambda-term

OCAMLMAKEFILE = ~/.opam/default/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)
