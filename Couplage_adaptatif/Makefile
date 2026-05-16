.PHONY: all clean

all: protocol_adaptatif.i protocol_adaptatif run clean

protocol_adaptatif.i: protocol_adaptatif.mli
	ocamlc -c protocol_adaptatif.mli 

protocol_adaptatif: protocol_adaptatif.i
	ocamlc -c protocol_adaptatif.ml
	ocamlc -c adaptatif_test.ml
	ocamlc -g protocol_adaptatif.cmo adaptatif_test.cmo -o adap
	
run: protocol_adaptatif
	./adap

archivage:
	tar -zcvf tab.tar *c *h Makefile

clean: 
	rm -f *.cmi
	rm -f *.cmo
	rm adap
