.PHONY: all run_main clean

all: protocol_simon.i protocol_simon clean #main run_main

protocol_simon.i: protocol_simon.mli
	ocamlc -c protocol_simon.mli 

protocol_simon: protocol_simon.i
	ocamlc -c protocol_simon.ml
	ocamlc -c simon_test.ml
	ocamlc -g protocol_simon.cmo simon_test.cmo -o sim
	./sim


archivage:
	tar -zcvf tab.tar *c *h Makefile

clean: 
	rm -f *.cmi
	rm -f *.cmo
