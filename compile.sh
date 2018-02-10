#!/bin/bash

ocamlc -c simpltypes.ml
ocamlyacc simplparser.mly
ocamlc -c simplparser.mli
ocamlc -c simplparser.ml
ocamllex simpllexer.mll
ocamlc -c simpllexer.ml
ocamlc -c simpl.ml
ocamlc -o simpl.exe simpltypes.cmo simpllexer.cmo simplparser.cmo simpl.cmo