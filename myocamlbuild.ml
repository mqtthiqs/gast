open Ocamlbuild_plugin;;
open Command;;

let find_packages () =
  Ocamlbuild_pack.Lexers.blank_sep_strings &
    Lexing.from_string &
    run_and_read "ocamlfind list | cut -d' ' -f1"

let find_syntaxes () = ["camlp4o"; "camlp4r"]

let find_includes () = [("camlp4.parsers", "+camlp4/Camlp4Parsers")]

let _ =
  dispatch begin function
    | Before_options ->
      let ocamlfind x = S[A"ocamlfind"; x] in
      Options.ocamlc     := ocamlfind & A"ocamlc";
      Options.ocamlopt   := ocamlfind & A"ocamlopt";
      Options.ocamldep   := ocamlfind & A"ocamldep";
      Options.ocamldoc   := ocamlfind & A"ocamldoc";
      Options.ocamlmktop := ocamlfind & A"ocamlmktop"

    | After_rules ->

      flag ["ocaml"; "link"] & A"-linkpkg";

      List.iter begin fun pkg ->
	flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
	flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
	flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
	flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
      end (find_packages ());

      List.iter begin fun s ->
	flag ["ocaml"; "compile";  "syntax_"^s] & S[A"-syntax"; A s];
	flag ["ocaml"; "ocamldep"; "syntax_"^s] & S[A"-syntax"; A s];
	flag ["ocaml"; "doc";      "syntax_"^s] & S[A"-syntax"; A s];
      end (find_syntaxes ());

      List.iter begin fun (pkg,s) ->
	flag ["ocaml"; "compile"; "pkg_"^pkg] & S[A"-I"; A s];
	flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-I"; A s];
	flag ["ocaml"; "doc"; "pkg_"^pkg] & S[A"-I"; A s];
	flag ["ocaml"; "link"; "pkg_"^pkg] & S[A"-I"; A s]
      end (find_includes ());

      flag ["ocaml"; "link"; "pkg_camlp4.parsers"] & S[A"/usr/lib/ocaml/camlp4/Camlp4Parsers/Camlp4OCamlRevisedParser.cmo";A"/usr/lib/ocaml/camlp4/Camlp4Parsers/Camlp4OCamlParser.cmo"]

    | _ -> ()


  end
