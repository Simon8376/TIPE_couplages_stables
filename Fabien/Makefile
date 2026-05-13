.PHONY: all run_main clean

all: protocol_fabien.i protocol_fabien clean

protocol_fabien.i: protocol_fabien.mli
	ocamlc -c protocol_fabien.mli 

protocol_fabien: protocol_fabien.i
	ocamlc -c protocol_fabien.ml
	ocamlc -c fabien_test.ml
	ocamlc -g protocol_fabien.cmo fabien_test.cmo -o fab
	./fab

archivage:
	tar -zcvf tab.tar *c *h Makefile

clean: 
	rm -f *.cmi
	rm -f *.cmo
	rm fab
