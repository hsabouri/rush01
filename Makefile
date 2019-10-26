RESULT = main
SOURCES = \
	pika.mli	\
	save.mli	\
	pika.ml		\
	save.ml		\
	main.ml

PACKS = lambda-term

OCAMLMAKEFILE = ~/.opam/default/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)
