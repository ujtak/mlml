TOPNAME := mlml

.PHONY: clean

mktop:
	ocamlmktop -o $(TOPNAME) str.cma

knn:
	ocamlc -o knn.out str.cma iris.ml knn.ml

clean:
	rm -f *.out *.cmo *.cmi

