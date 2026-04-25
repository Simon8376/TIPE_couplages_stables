.PHONY: all run_main clean

all: blossom.i blossom test_blossom.i test_blossom protocol_fabien.i protocol_fabien clean #main run_main

blossom.i: blossom.mli
	ocamlc -c blossom.mli

blossom: blossom.i blossom.ml
	ocamlc -c blossom.ml

test_blossom.i: test_blossom.mli
	ocamlc -c test_blossom.mli

test_blossom: test_blossom.i test_blossom.ml
	ocamlc -c test_blossom.ml

protocol_fabien.i: protocol_fabien.mli
	ocamlc -c protocol_fabien.mli 

protocol_fabien: protocol_fabien.i
	ocamlc -c protocol_fabien.ml
	ocamlc -c fabien_test.ml
	ocamlc -g protocol_fabien.cmo fabien_test.cmo -o fab
	./fab

main: main.ml
	ocamlc -c main.ml
	ocamlc -o main blossom.cmo test_blossom.cmo main.cmo

run_main: main
	-./main

archivage:
	tar -zcvf tab.tar *c *h Makefile

clean: 
	rm -f *.cmi
	rm -f *.cmo
