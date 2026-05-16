.PHONY: all run_main clean

all: protocol_dyna.i protocol_dyna run clean


protocol_dyna.i: protocol_dyna.mli
	ocamlc -c protocol_dyna.mli 


protocol_dyna: protocol_dyna.i
	ocamlc -c protocol_dyna.ml
	ocamlfind ocamlc -c -package unix dyna_test.ml
	ocamlfind ocamlc -g protocol_dyna.cmo dyna_test.cmo -linkpkg -package unix -o dyna
	

run: protocol_dyna
	./dyna

archivage:
	tar -zcvf tab.tar *c *h Makefile

clean: 
	rm -f *.cmi
	rm -f *.cmo
	rm dyna
