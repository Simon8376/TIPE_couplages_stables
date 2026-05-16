.PHONY: all run_main clean

all: protocol_cyclique.i protocol_cyclique clean #main run_main

protocol_cyclique.i: protocol_cyclique.mli
	ocamlc -c protocol_cyclique.mli 

protocol_cyclique: protocol_cyclique.i
	ocamlc -c protocol_cyclique.ml
	ocamlc -c cyclique_test.ml
	ocamlc -g protocol_cyclique.cmo cyclique_test.cmo -o sim
	./sim


archivage:
	tar -zcvf tab.tar *c *h Makefile

clean: 
	rm -f *.cmi
	rm -f *.cmo
	rm sim