open Sexplib.Sexp

let list_fold_left_i f =
  let rec it_list_f i a = function
    | [] -> a, i
    | b::l -> it_list_f (i+1) (f i a b) l
  in
  it_list_f

let input_file ch =
  let bufsize = 4096 in
  let buf = Buffer.create bufsize in
  let rec aux () =
    let s = String.create bufsize in
    let read = input ch s 0 bufsize in
    if read = 0 then buf else aux (Buffer.add_substring buf s 0 read)
  in Buffer.contents (aux ())

let in_dir d f =
  (try Unix.chdir d  with _ -> print_string d; assert false);
  let r = f() in
  (try Unix.chdir ".."  with _ -> assert false);
  r

let in_dir_abs d f =
  let c = Unix.getcwd() in
  (try Unix.chdir d  with _ -> print_string c; print_string d; assert false);
  let r = f() in
  Unix.chdir c;
  r

let rec rm_r f =
  try Unix.unlink f
  with Unix.Unix_error (Unix.EISDIR,_,_) ->
    (try(Unix.chdir f) with _ -> assert false);
    let h = Unix.opendir f in
    let rec aux () =
      try rm_r (Unix.readdir h)
      with Unix.Unix_error _ -> ()
    in aux (); Unix.closedir h

let rec rmdirs i =
  (* Printf.printf "rmdirs %d in %s\n%!" i (Unix.getcwd()); *)
  try
    in_dir_abs "." (fun () -> rm_r (string_of_int i)); rmdirs (i+1)
  with Unix.Unix_error _ -> ()

let to_dir d e =
  let rec aux = function
  | Atom s ->
    let ch = open_out_gen [Open_wronly;Open_creat;Open_trunc] 0o644 "a" in
    output_string ch s;
    close_out ch
  | List [] -> close_out (open_out ".e")
  | List l ->
    let _,i = list_fold_left_i (fun i () e ->
      let d = string_of_int i in
      (try Unix.mkdir d 0o755
       with Unix.Unix_error _ -> ());
      in_dir d (fun () -> aux e)
    ) 0 () l in
    try
      Unix.unlink "a"
    with Unix.Unix_error _ ->
      try Unix.unlink ".e" with Unix.Unix_error _ ->
	(* rmdirs i *) ignore i
  in
  in_dir d (fun () -> aux e)

let of_dir d : t =
  let rec dir () =
    try
      Atom(atom ())
    with Sys_error _ ->
      List(list 0)
  and list i =
  try
    let d = in_dir (string_of_int i) dir in
    let ds = list (i+1) in
    d :: ds
  with Unix.Unix_error _ -> []
  and atom () : string =
    let ch = open_in "a" in
    let s = input_file ch in
    close_in ch;
    s
  in in_dir_abs d dir

let of_caml_interf filename =
  let f = Parse_file.parse_interf filename in
  f
