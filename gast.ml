let dirname = "_gast"
let filename = "file.ml"

let astify_file f =
  Printf.printf "Parsing %s.\n%!" f;
  let ast = Parse_file.parse_implem f in
  Printf.printf "Sexping %s.\n%!" f;
  let sexp = Caml_ast.sexp_of_str_item ast in
  Printf.printf "Saving sexp in %s.\n%!" dirname;
  Sexp.to_dir dirname sexp

let rec astify_dir src dst =
  Util.fold_dir
    (fun ent () ->
      if Sys.is_directory ent
      then begin
	Buffer.add_string dst ent;
	Buffer.add_string src ent;
	(try Unix.mkdir (Buffer.contents dst) 0o755
	 with Unix.Unix_error _ -> ());
	astify_dir src dst
      end
      else begin
      end


    ) () (Buffer.contents src)

let ppify_file () =
  Printf.printf "Reading sexp.\n%!";
  let sexp = Sexp.of_dir dirname in
  Printf.printf "Pretty-printing sexp.\n%!";
  let ast = Caml_ast.str_item_of_sexp sexp in
  Parse_file.print_implem ast filename

(* Main: *)

let exec s args =
  flush_all();
  match Unix.fork() with
    | 0 -> Unix.execvp s args
    | _ -> ignore(Unix.wait())

let system s = flush_all(); ignore(Unix.system s)

let _ =

  let command = Array.get Sys.argv 1 in

  if command = "init" then begin
    try Unix.mkdir dirname 0o755
    with Unix.Unix_error (e,_,_) -> ()
  end;

  astify_file filename;

  Util.in_dir_abs dirname
    (fun () ->
      Printf.printf "Calling git add . && git add -u.\n%!";
      system "git add . && git add -u";
      Printf.printf "Calling git [your arguments].\n%!";
      exec "git" Sys.argv);

  ppify_file ();
