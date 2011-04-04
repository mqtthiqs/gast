open Pp;;
open Compat;;
let anomaly string = raise (Anomaly (string, str string));;
let anomalylabstrm string pps = raise (Anomaly (string, pps));;
exception UserError of string * std_ppcmds;;
let error string = raise (UserError (string, str string));;
let errorlabstrm l pps = raise (UserError (l, pps));;
exception AnomalyOnError of string * exn;;
exception AlreadyDeclared of std_ppcmds;;
let alreadydeclared pps = raise (AlreadyDeclared pps);;
let todo s = prerr_string ("TODO: " ^ (s ^ "\n"));;
exception Timeout;;
type loc = Loc.t;;
let dummy_loc = Loc.ghost;;
let join_loc = Loc.merge;;
let make_loc = make_loc;;
let unloc = unloc;;
type 'a located = (loc * 'a);;
let anomaly_loc (loc, s, strm) = Loc.raise loc (Anomaly (s, strm));;
let user_err_loc (loc, s, strm) = Loc.raise loc (UserError (s, strm));;
let invalid_arg_loc (loc, s) = Loc.raise loc (Invalid_argument s);;
let located_fold_left f x (_, a) = f x a;;
let located_iter2 f (_, a) (_, b) = f a b;;
let down_located f (_, a) = f a;;
exception Error_in_file of string * (bool * string * loc) * exn;;
let on_fst f (a, b) = ((f a), b);;
let on_snd f (a, b) = (a, (f b));;
let on_pi1 f (a, b, c) = ((f a), b, c);;
let on_pi2 f (a, b, c) = (a, (f b), c);;
let on_pi3 f (a, b, c) = (a, b, (f c));;
let pi1 (a, _, _) = a;;
let pi2 (_, a, _) = a;;
let pi3 (_, _, a) = a;;
let down_fst f x = f (fst x);;
let down_snd f x = f (snd x);;
let is_letter c = ((c >= 'a') && (c <= 'z')) or ((c >= 'A') && (c <= 'Z'));;
let is_digit c = (c >= '0') && (c <= '9');;
let is_ident_tail c =
  (is_letter c) or ((is_digit c) or ((c = '\'') or (c = '_')));;
let is_blank = function | ' ' | '\r' | '\t' | '\n' -> true | _ -> false;;
let explode s =
  let rec explode_rec n =
    if n >= (String.length s)
    then []
    else (String.make 1 (String.get s n)) :: (explode_rec (succ n))
  in explode_rec 0;;
let implode sl = String.concat "" sl;;
let strip s =
  let n = String.length s in
  let rec lstrip_rec i =
    if (i < n) && (is_blank s.[i]) then lstrip_rec (i + 1) else i in
  let rec rstrip_rec i =
    if (i >= 0) && (is_blank s.[i]) then rstrip_rec (i - 1) else i in
  let a = lstrip_rec 0
  and b = rstrip_rec (n - 1)
  in String.sub s a ((b - a) + 1);;
let drop_simple_quotes s =
  let n = String.length s
  in
    if (n > 2) & ((s.[0] = '\'') & (s.[n - 1] = '\''))
    then String.sub s 1 (n - 2)
    else s;;
let rec is_sub gdzie gl gi co cl ci =
  (ci >= cl) ||
    (((String.unsafe_get gdzie gi) = (String.unsafe_get co ci)) &&
       (is_sub gdzie gl (gi + 1) co cl (ci + 1)));;
let rec raw_str_index i gdzie l c co cl =
  (if (i + cl) > l then raise Not_found else ();
   let i' = String.index_from gdzie i c
   in
     if ((i' + cl) <= l) && (is_sub gdzie l i' co cl 0)
     then i'
     else raw_str_index (i' + 1) gdzie l c co cl);;
let string_index_from gdzie i co =
  if co = ""
  then i
  else
    raw_str_index i gdzie (String.length gdzie) (String.unsafe_get co 0) co
      (String.length co);;
let string_string_contains ~where ~what =
  try let _ = string_index_from where 0 what in true
  with | Not_found -> false;;
let plural n s = if n <> 1 then s ^ "s" else s;;
let ordinal n =
  let s = match n mod 10 with | 1 -> "st" | 2 -> "nd" | 3 -> "rd" | _ -> "th"
  in (string_of_int n) ^ s;;
let split_string_at c s =
  let len = String.length s in
  let rec split n =
    try
      let pos = String.index_from s n c in
      let dir = String.sub s n (pos - n) in dir :: (split (succ pos))
    with | Not_found -> [ String.sub s n (len - n) ]
  in if len = 0 then [] else split 0;;
let parse_loadpath s =
  let l = split_string_at '/' s
  in
    (if List.mem "" l
     then invalid_arg "parse_loadpath: find an empty dir in loadpath"
     else ();
     l);;
module Stringset =
  Set.Make(struct type t = string;; let compare = compare;; end);;
module Stringmap =
  Map.Make(struct type t = string;; let compare = compare;; end);;
type utf8_status = | UnicodeLetter | UnicodeIdentPart | UnicodeSymbol;;
exception UnsupportedUtf8;;
let table = Array.create (1 lsl 17) 0;;
let mask i =
  function
  | UnicodeLetter -> 1 lsl ((i land 7) lsl 1)
  | UnicodeIdentPart -> 2 lsl ((i land 7) lsl 1)
  | UnicodeSymbol -> 3 lsl ((i land 7) lsl 1);;
let reset_mask i = lnot (3 lsl ((i land 7) lsl 1));;
let mk_lookup_table_from_unicode_tables_for status tables =
  List.iter
    (List.iter
       (fun (c1, c2) ->
          for i = c1 to c2 do
            table.(i lsr 3) <-
              (table.(i lsr 3) land (reset_mask bla i blo)) lor
                (mask i status)
          done))
    tables;;
let lookup x =
  let v = (table.(x lsr 3) lsr ((x land 7) lsl 1)) land 3
  in
    if v = 1
    then UnicodeLetter
    else
      if v = 2
      then UnicodeIdentPart
      else if v = 3 then UnicodeSymbol else raise UnsupportedUtf8;;
let classify_unicode =
  let single c = [ (c, c) ]
  in
    (mk_lookup_table_from_unicode_tables_for UnicodeSymbol
       [ Unicodetable.sm; Unicodetable.sc; Unicodetable.so; Unicodetable.pd;
         Unicodetable.pc; Unicodetable.pe; Unicodetable.ps; Unicodetable.pi;
         Unicodetable.pf; Unicodetable.po ];
     mk_lookup_table_from_unicode_tables_for UnicodeLetter
       [ Unicodetable.lu; Unicodetable.ll; Unicodetable.lt; Unicodetable.lo ];
     mk_lookup_table_from_unicode_tables_for UnicodeIdentPart
       [ Unicodetable.nd; Unicodetable.nl; Unicodetable.no ];
     mk_lookup_table_from_unicode_tables_for UnicodeSymbol
       [ single 0x000B2; single 0x0002E ];
     mk_lookup_table_from_unicode_tables_for UnicodeLetter
       [ single 0x005F; single 0x00A0 ];
     mk_lookup_table_from_unicode_tables_for UnicodeIdentPart
       [ single 0x0027 ];
     lookup);;
exception End_of_input;;
let utf8_of_unicode n =
  if n < 128
  then String.make 1 (Char.chr n)
  else
    if n < 2048
    then
      (let s = String.make 2 (Char.chr (128 + (n mod 64)))
       in (s.[0] <- Char.chr (192 + (n / 64)); s))
    else
      if n < 65536
      then
        (let s = String.make 3 (Char.chr (128 + (n mod 64)))
         in
           (s.[1] <- Char.chr (128 + ((n / 64) mod 64));
            s.[0] <- Char.chr (224 + (n / 4096));
            s))
      else
        (let s = String.make 4 (Char.chr (128 + (n mod 64)))
         in
           (s.[2] <- Char.chr (128 + ((n / 64) mod 64));
            s.[1] <- Char.chr (128 + ((n / 4096) mod 64));
            s.[0] <- Char.chr (240 + (n / 262144));
            s));;
let next_utf8 s i =
  let err () = invalid_arg "utf8" in
  let l = (String.length s) - i
  in
    if l = 0
    then raise End_of_input
    else
      (let a = Char.code s.[i]
       in
         if a <= 0x7F
         then (1, a)
         else
           if ((a land 0x40) = 0) or (l = 1)
           then err ()
           else
             (let b = Char.code s.[i + 1]
              in
                if (b land 0xC0) <> 0x80
                then err ()
                else
                  if (a land 0x20) = 0
                  then (2, (((a land 0x1F) lsl 6) + (b land 0x3F)))
                  else
                    if l = 2
                    then err ()
                    else
                      (let c = Char.code s.[i + 2]
                       in
                         if (c land 0xC0) <> 0x80
                         then err ()
                         else
                           if (a land 0x10) = 0
                           then
                             (3,
                              ((((a land 0x0F) lsl 12) +
                                  ((b land 0x3F) lsl 6))
                                 + (c land 0x3F)))
                           else
                             if l = 3
                             then err ()
                             else
                               (let d = Char.code s.[i + 3]
                                in
                                  if (d land 0xC0) <> 0x80
                                  then err ()
                                  else
                                    if (a land 0x08) = 0
                                    then
                                      (4,
                                       (((((a land 0x07) lsl 18) +
                                            ((b land 0x3F) lsl 12))
                                           + ((c land 0x3F) lsl 6))
                                          + (d land 0x3F)))
                                    else err ()))));;
let check_initial handle j n s =
  match classify_unicode n with
  | UnicodeLetter -> ()
  | _ ->
      let c = String.sub s 0 j
      in
        handle
          ("Invalid character '" ^
             (c ^ ("' at beginning of identifier \"" ^ (s ^ "\"."))));;
let check_trailing handle i j n s =
  match classify_unicode n with
  | UnicodeLetter | UnicodeIdentPart -> ()
  | _ ->
      let c = String.sub s i j
      in
        handle
          ("Invalid character '" ^ (c ^ ("' in identifier \"" ^ (s ^ "\"."))));;
let check_ident_gen handle s =
  let i = ref 0
  in
    if s <> ".."
    then
      (try
         let (j, n) = next_utf8 s 0
         in
           (check_initial handle j n s;
            i := !i + j;
            try
              while true do
                let (j, n) = next_utf8 s !i
                in (check_trailing handle !i j n s; i := !i + j) done
            with | End_of_input -> ())
       with | End_of_input -> error "The empty string is not an identifier."
       | UnsupportedUtf8 ->
           error (s ^ ": unsupported character in utf8 sequence.")
       | Invalid_argument _ -> error (s ^ ": invalid utf8 sequence."))
    else ();;
let check_ident_soft = check_ident_gen warning;;
let check_ident = check_ident_gen error;;
let lowercase_unicode =
  let tree = Segmenttree.make Unicodetable.to_lower
  in
    fun unicode ->
      try
        match Segmenttree.lookup unicode tree with
        | `Abs c -> c
        | `Delta d -> unicode + d
      with | Not_found -> unicode;;
let lowercase_first_char_utf8 s =
  (assert (s <> "");
   let (j, n) = next_utf8 s 0 in utf8_of_unicode (lowercase_unicode n));;
let ascii_of_ident s =
  let check_ascii s =
    let ok = ref true
    in
      (String.iter
         (fun c -> if (Char.code c) >= 128 then ok := false else ()) s;
       !ok)
  in
    if check_ascii s
    then s
    else
      (let i = ref 0
       and out = ref ""
       in
         ((try
             while true do
               let (j, n) = next_utf8 s !i
               in
                 (out :=
                    if n >= 128
                    then Printf.sprintf "%s__U%04x_" !out n
                    else Printf.sprintf "%s%c" !out s.[!i];
                  i := !i + j)
               done
           with | End_of_input -> ());
          !out));;
let rec list_compare cmp l1 l2 =
  match (l1, l2) with
  | ([], []) -> 0
  | (_ :: _, []) -> 1
  | ([], _ :: _) -> (-1)
  | (x1 :: l1, x2 :: l2) ->
      (match cmp x1 x2 with | 0 -> list_compare cmp l1 l2 | c -> c);;
let list_intersect l1 l2 = List.filter (fun x -> List.mem x l2) l1;;
let list_union l1 l2 =
  let rec urec =
    function
    | [] -> l2
    | a :: l -> if List.mem a l2 then urec l else a :: (urec l)
  in urec l1;;
let list_unionq l1 l2 =
  let rec urec =
    function
    | [] -> l2
    | a :: l -> if List.memq a l2 then urec l else a :: (urec l)
  in urec l1;;
let list_subtract l1 l2 =
  if l2 = [] then l1 else List.filter (fun x -> not (List.mem x l2)) l1;;
let list_subtractq l1 l2 =
  if l2 = [] then l1 else List.filter (fun x -> not (List.memq x l2)) l1;;
let list_tabulate f len =
  let rec tabrec n = if n = len then [] else (f n) :: (tabrec (n + 1))
  in tabrec 0;;
let rec list_make n v =
  if n = 0
  then []
  else if n < 0 then invalid_arg "list_make" else v :: (list_make (n - 1) v);;
let size_w = Size.size_w coucouc heloo;;
let size_b = Size.size_b;;
let size_kb = Size.size_kb;;
let heap_size () =
  let stat = Gc.stat () and control = Gc.get () in
  let max_words_total = stat.Gc.heap_words + control.Gc.minor_heap_size
  in max_words_total * (Sys.word_size / 8);;
let heap_size_kb () = ((heap_size ()) + 1023) / 1024;;
let interrupt = ref false;;
let check_for_interrupt () =
  if !interrupt then (interrupt := false; raise Sys.Break) else ();;
