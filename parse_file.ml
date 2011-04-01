open Camlp4.PreCast

module Caml =
  Camlp4.Printers.OCaml.Make
    (Camlp4OCamlParser.Make
      (Camlp4OCamlRevisedParser.Make
        (Camlp4.OCamlInitSyntax.Make(Ast)(Gram)(Quotation))))

open Caml_ast

let parse_implem f : Caml_ast.str_item =
  let ic = open_in f in
  let strm = Stream.of_channel ic in
  let res = Caml.parse_implem (Loc.mk f) strm in
  close_in ic;
  Obj.magic res

let parse_interf f : Caml_ast.str_item =
  let ic = open_in f in
  let strm = Stream.of_channel ic in
  let res = Caml.parse_interf (Loc.mk f) strm in
  close_in ic;
  Obj.magic res

let print_implem ast output_file = Caml.print_implem ~output_file (Obj.magic ast)

