#
# Pure OCaml, package from Opam, two directories
#

# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain
# - using *.mll and *.mly are handled automatically

# - we are using menhir, the modern replacement for OCamlYacc
OCB_FLAGS = -use-ocamlfind             -I src  # uses ocamlyacc
# OCB_FLAGS   = -use-ocamlfind -use-menhir -I src

OCB = 		ocamlbuild $(OCB_FLAGS)

default: main

clean:
			$(OCB) -clean

main:
			$(OCB) main.byte

build:
			$(OCB) test.byte

draw:
		  $(OCB) test_draw.byte

.PHONY: 	clean build draw main
