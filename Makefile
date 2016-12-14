TOPNAME := mlml

.PHONY: clean

mktop:
	ocamlmktop -o $(TOPNAME) str.cma

knn:
	# ocamlc -o knn.out str.cma iris.ml knn.ml
	ocamlopt -o knn.out str.cmxa iris.ml knn.ml

clean:
	rm -f *.out *.cmo *.cmi *.cmx *.o

