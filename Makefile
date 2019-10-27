RESULT = main
SOURCES = \
	render.ml	\
	pika.mli	\
	pika.ml		\
	save.mli	\
	save.ml		\
	main.ml

PACKS = lambda-term

OCAMLMAKEFILE = ~/.opam/default/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)
