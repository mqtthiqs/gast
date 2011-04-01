let dirname = "_gast"
let filename = "file.ml"

let astify f =
  Printf.printf "Parsing %s.\n" f;
  let ast = Parse_file.parse_implem f in
  (* Printf.printf "Sexping %s.\n" f; *)
  let sexp = Caml_ast.sexp_of_str_item ast in
  (* Printf.printf "Saving sexp in %s.\n" dirname; *)
  Sexp.to_dir dirname sexp

let ppify () =
  let sexp = Sexp.of_dir dirname in
  Printf.printf "Retyping sexp.\n";
  let ast = Caml_ast.str_item_of_sexp sexp in
  Parse_file.print_implem ast filename

(* Main: *)

let exec s args =
  flush_all();
  match Unix.fork() with
    | 0 -> Unix.execvp s args
    | _ -> ignore(Unix.wait())

let _ =

  let command = Array.get Sys.argv 1 in

  if command = "init" then begin
    try Unix.mkdir dirname 0o755
    with Unix.Unix_error (e,_,_) -> ()
  end;

  Printf.printf "Removing old repository.\n";
  Sexp.in_dir_abs dirname
    (fun () -> ignore(Unix.system "rm -r *"));
  astify filename;
  Sexp.in_dir_abs dirname
    (fun () -> ignore(Unix.system "git add . && git add -u"); exec "git" Sys.argv;);
  ppify ();
