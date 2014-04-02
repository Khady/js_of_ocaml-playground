ALL:
	ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax -syntax camlp4o -linkpkg -o temperature.byte temperature.ml
	js_of_ocaml temperature.byte
