
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

let fold_dir (f : string -> 'a -> 'a) (acc : 'a) (d : string) =
  let h = Unix.opendir d in
  let rec aux acc =
    try
      let ent = Unix.readdir h in
      if ent <> "." && ent <> ".."
      then aux (f ent acc)
      else aux acc
    with End_of_file -> acc
  in
  aux acc

let in_dir d f =
  Unix.chdir d;
  let r = f() in
  Unix.chdir "..";
  r

let in_dir_abs d f =
  let c = Unix.getcwd() in
  (try Unix.chdir d  with _ -> print_string c; print_string d; assert false);
  let r = f() in
  Unix.chdir c;
  r

let rec rm_r f =
  (* Printf.printf "rm_r %s in %s\n" f (Unix.getcwd()); *)
  try Unix.unlink f
  with Unix.Unix_error (Unix.EISDIR,_,_) ->
    in_dir f (fun () ->
      let h = Unix.opendir "." in
      fold_dir (fun f () -> rm_r f) () ".";
      Unix.closedir h
    ); Unix.rmdir f

let rec rmdirs i =
  (* Printf.printf "rmdirs %d in %s\n%!" i (Unix.getcwd()); *)
  try
    in_dir_abs "." (fun () -> rm_r (string_of_int i)); rmdirs (i+1)
  with Unix.Unix_error _ -> ()
