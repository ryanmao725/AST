ifeq ($(origin NETID), undefined)
NETID:=$(shell whoami)
endif

all: proj2_types.cmo proj2.cmo

proj2_types.cmi: proj2_types.mli
	ocamlc -c proj2_types.mli

proj2_types.cmo: proj2_types.ml proj2_types.cmi
	ocamlc -c proj2_types.ml

proj2.cmi: proj2.mli proj2_types.cmi
	ocamlc -c proj2.mli

proj2.cmo : proj2.ml proj2.mli proj2_types.cmo proj2.cmi
	ocamlc -c proj2.ml

test: proj2_types.cmo proj2.cmo
	ocaml proj2_types.cmo proj2.cmo proj2_test.ml

submit: proj2.ml
	tar -cvzf proj2_$(NETID).tar.gz --transform 's,^,proj2/,' proj2.ml
