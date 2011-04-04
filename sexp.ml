open Sexplib.Sexp

let to_dir d e =
  let rec rmdirs i =
  (* Printf.printf "rmdirs %d in %s\n%!" i (Unix.getcwd()); *)
    try
      Util.in_dir_abs "." (fun () -> Util.rm_r (string_of_int i)); rmdirs (i+1)
    with Unix.Unix_error _ -> ()
  in
  let rec aux = function
  | Atom s ->
    let ch = open_out_gen [Open_wronly;Open_creat;Open_trunc] 0o644 "a" in
    output_string ch s;
    close_out ch
  | List [] -> close_out (open_out ".e")
  | List l ->
    let _,i = Util.list_fold_left_i (fun i () e ->
      let d = string_of_int i in
      (try Unix.mkdir d 0o755
       with Unix.Unix_error _ -> ());
      Util.in_dir d (fun () -> aux e)
    ) 0 () l in
    try
      Unix.unlink "a"
    with Unix.Unix_error _ ->
      try Unix.unlink ".e" with Unix.Unix_error _ ->
	rmdirs i
  in
  Util.in_dir d (fun () -> aux e)

let of_dir d : t =
  let rec dir () =
    try
      Atom(atom ())
    with Sys_error _ ->
      List(list 0)
  and list i =
    try
      let d = Util.in_dir (string_of_int i) dir in
      let ds = list (i+1) in
      d :: ds
    with Unix.Unix_error _ -> []
  and atom () : string =
    let ch = open_in "a" in
    let s = Util.input_file ch in
    close_in ch;
    s
  in Util.in_dir_abs d dir

let of_caml_interf filename =
  let f = Parse_file.parse_interf filename in
  f
