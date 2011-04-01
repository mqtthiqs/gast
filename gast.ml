let dirname = "_gast"
let filename = "file.ml"

let astify f =
  Printf.printf "Parsing %s.\n" f;
  let ast = Parse_file.parse_implem f in
  Printf.printf "Sexping %s.\n" f;
  let sexp = Caml_ast.sexp_of_str_item ast in
  Printf.printf "Saving sexp in %s.\n" dirname;
  Sexp.to_dir dirname sexp

let ppify () =
  let sexp = Sexp.of_dir dirname in
  Printf.printf "Retyping sexp.\n";
  let ast = Caml_ast.str_item_of_sexp sexp in
  Parse_file.print_implem ast filename

(* Main: *)

let exec s args =
  match Unix.fork() with
    | 0 -> Unix.execvp s args
    | _ -> ignore(Unix.wait())

let _ =

  let command = Array.get Sys.argv 1 in

  match command with
    | "init" ->
      print_string "coucou";
      begin
	try Unix.mkdir dirname 0o755
	with Unix.Unix_error (e,_,_) ->
	  failwith ("This seems to be a gast repository already:"^Unix.error_message e)
      end;
      astify filename;
      Unix.chdir dirname;
      Printf.printf "Initializing repository.\n";
      begin match Unix.system "git init" with
	| Unix.WEXITED 0 -> ()
	| _ -> assert false
      end;
      Printf.printf "First commit.\n";
      begin match Unix.system "git add ." with
	| Unix.WEXITED 0 -> ()
	| _ -> assert false
      end;
      begin match Unix.system "git commit -m \"Initial commit.\"" with
	| Unix.WEXITED 0 -> ()
	| _ -> assert false
      end

    | _ ->
      Printf.printf "Removing old repository.\n";
      flush_all();
      Sexp.in_dir_abs dirname
	(fun () -> ignore(Unix.system "rm -r *"));
      astify filename;
      Sexp.in_dir_abs dirname
	(fun () -> ignore(Unix.system "git add ."); exec "git" Sys.argv;);
      ppify ();
