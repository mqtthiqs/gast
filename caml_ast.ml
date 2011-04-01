open Camlp4.PreCast

module Loc = struct
  include Loc
  type tuple = string * int * int * int * int * int * int * bool with sexp
  let sexp_of_t loc = sexp_of_tuple (Loc.to_tuple loc)
  let t_of_sexp sexp = Loc.of_tuple (tuple_of_sexp sexp)
end

type loc = Loc.t
and meta_bool =
  | BTrue | BFalse | BAnt of string
and rec_flag =
  | ReRecursive | ReNil | ReAnt of string
and direction_flag =
  | DiTo | DiDownto | DiAnt of string
and mutable_flag =
  | MuMutable | MuNil | MuAnt of string
and private_flag =
  | PrPrivate | PrNil | PrAnt of string
and virtual_flag =
  | ViVirtual | ViNil | ViAnt of string
and override_flag =
  | OvOverride | OvNil | OvAnt of string
and row_var_flag =
  | RvRowVar | RvNil | RvAnt of string
and 'a meta_option =
  | ONone | OSome of 'a | OAnt of string
and 'a meta_list =
  | LNil | LCons of 'a * 'a meta_list | LAnt of string
and ident =
  | IdAcc of loc * ident * ident
  | (* i . i *)
      IdApp of loc * ident * ident
  | (* i i *)
      IdLid of loc * string
  | (* foo *)
      IdUid of loc * string
  | (* Bar *)
      IdAnt of loc * string
and (* $s$ *)
  ctyp =
  | TyNil of loc
  | TyAli of loc * ctyp * ctyp
  | (* t as t *)
  (* list 'a as 'a *)
      TyAny of loc
  | (* _ *)
      TyApp of loc * ctyp * ctyp
  | (* t t *)
  (* list 'a *)
      TyArr of loc * ctyp * ctyp
  | (* t -> t *)
  (* int -> string *)
      TyCls of loc * ident
  | (* #i *)
  (* #point *)
      TyLab of loc * string * ctyp
  | (* ~s:t *)
      TyId of loc * ident
  | (* i *)
  (* Lazy.t *)
      TyMan of loc * ctyp * ctyp
  | (* t == t *)
  (* type t = [ A | B ] == Foo.t *)
  (* type t 'a 'b 'c = t constraint t = t constraint t = t *)
      TyDcl of loc * string * ctyp list * ctyp * (ctyp * ctyp) list
  | (* < (t)? (..)? > *)
  (* < move : int -> 'a .. > as 'a  *)
      TyObj of loc * ctyp * row_var_flag
  | TyOlb of loc * string * ctyp
  | (* ?s:t *)
      TyPol of loc * ctyp * ctyp
  | (* ! t . t *)
  (* ! 'a . list 'a -> 'a *)
      TyQuo of loc * string
  | (* 's *)
      TyQuP of loc * string
  | (* +'s *)
      TyQuM of loc * string
  | (* -'s *)
      TyVrn of loc * string
  | (* `s *)
      TyRec of loc * ctyp
  | (* { t } *)
  (* { foo : int ; bar : mutable string } *)
      TyCol of loc * ctyp * ctyp
  | (* t : t *)
      TySem of loc * ctyp * ctyp
  | (* t; t *)
      TyCom of loc * ctyp * ctyp
  | (* t, t *)
      TySum of loc * ctyp
  | (* [ t ] *)
  (* [ A of int and string | B ] *)
      TyOf of loc * ctyp * ctyp
  | (* t of t *)
  (* A of int *)
      TyAnd of loc * ctyp * ctyp
  | (* t and t *)
      TyOr of loc * ctyp * ctyp
  | (* t | t *)
      TyPrv of loc * ctyp
  | (* private t *)
      TyMut of loc * ctyp
  | (* mutable t *)
      TyTup of loc * ctyp
  | (* ( t ) *)
  (* (int * string) *)
      TySta of loc * ctyp * ctyp
  | (* t * t *)
      TyVrnEq of loc * ctyp
  | (* [ = t ] *)
      TyVrnSup of loc * ctyp
  | (* [ > t ] *)
      TyVrnInf of loc * ctyp
  | (* [ < t ] *)
      TyVrnInfSup of loc * ctyp * ctyp
  | (* [ < t > t ] *)
      TyAmp of loc * ctyp * ctyp
  | (* t & t *)
      TyOfAmp of loc * ctyp * ctyp
  | (* t of & t *)
      TyPkg of loc * module_type
  | (* (module S) *)
      TyAnt of loc * string
and (* $s$ *)
  patt =
  | PaNil of loc
  | PaId of loc * ident
  | (* i *)
      PaAli of loc * patt * patt
  | (* p as p *)
  (* (Node x y as n) *)
      PaAnt of loc * string
  | (* $s$ *)
      PaAny of loc
  | (* _ *)
      PaApp of loc * patt * patt
  | (* p p *)
  (* fun x y -> *)
      PaArr of loc * patt
  | (* [| p |] *)
      PaCom of loc * patt * patt
  | (* p, p *)
      PaSem of loc * patt * patt
  | (* p; p *)
      PaChr of loc * string
  | (* c *)
  (* 'x' *)
      PaInt of loc * string
  | PaInt32 of loc * string
  | PaInt64 of loc * string
  | PaNativeInt of loc * string
  | PaFlo of loc * string
  | PaLab of loc * string * patt
  | (* ~s or ~s:(p) *)
  (* ?s or ?s:(p) *)
      PaOlb of loc * string * patt
  | (* ?s:(p = e) or ?(p = e) *)
      PaOlbi of loc * string * patt * expr
  | PaOrp of loc * patt * patt
  | (* p | p *)
      PaRng of loc * patt * patt
  | (* p .. p *)
      PaRec of loc * patt
  | (* { p } *)
      PaEq of loc * ident * patt
  | (* i = p *)
      PaStr of loc * string
  | (* s *)
      PaTup of loc * patt
  | (* ( p ) *)
      PaTyc of loc * patt * ctyp
  | (* (p : t) *)
      PaTyp of loc * ident
  | (* #i *)
      PaVrn of loc * string
  | (* `s *)
      PaLaz of loc * patt
  | (* lazy p *)
      PaMod of loc * string
and (* (module M) *)
  expr =
  | ExNil of loc
  | ExId of loc * ident
  | (* i *)
      ExAcc of loc * expr * expr
  | (* e.e *)
      ExAnt of loc * string
  | (* $s$ *)
      ExApp of loc * expr * expr
  | (* e e *)
      ExAre of loc * expr * expr
  | (* e.(e) *)
      ExArr of loc * expr
  | (* [| e |] *)
      ExSem of loc * expr * expr
  | (* e; e *)
      ExAsf of loc
  | (* assert False *)
      ExAsr of loc * expr
  | (* assert e *)
      ExAss of loc * expr * expr
  | (* e := e *)
      ExChr of loc * string
  | (* 'c' *)
      ExCoe of loc * expr * ctyp * ctyp
  | (* (e : t) or (e : t :> t) *)
      ExFlo of loc * string
  | (* 3.14 *)
  (* for s = e to/downto e do { e } *)
      ExFor of loc * string * expr * expr * direction_flag * expr
  | ExFun of loc * match_case
  | (* fun [ mc ] *)
      ExIfe of loc * expr * expr * expr
  | (* if e then e else e *)
      ExInt of loc * string
  | (* 42 *)
      ExInt32 of loc * string
  | ExInt64 of loc * string
  | ExNativeInt of loc * string
  | ExLab of loc * string * expr
  | (* ~s or ~s:e *)
      ExLaz of loc * expr
  | (* lazy e *)
  (* let b in e or let rec b in e *)
      ExLet of loc * rec_flag * binding * expr
  | (* let module s = me in e *)
      ExLmd of loc * string * module_expr * expr
  | (* match e with [ mc ] *)
      ExMat of loc * expr * match_case
  | (* new i *)
      ExNew of loc * ident
  | (* object ((p))? (cst)? end *)
      ExObj of loc * patt * class_str_item
  | (* ?s or ?s:e *)
      ExOlb of loc * string * expr
  | (* {< rb >} *)
      ExOvr of loc * rec_binding
  | (* { rb } or { (e) with rb } *)
      ExRec of loc * rec_binding * expr
  | (* do { e } *)
      ExSeq of loc * expr
  | (* e#s *)
      ExSnd of loc * expr * string
  | (* e.[e] *)
      ExSte of loc * expr * expr
  | (* s *)
  (* "foo" *)
      ExStr of loc * string
  | (* try e with [ mc ] *)
      ExTry of loc * expr * match_case
  | (* (e) *)
      ExTup of loc * expr
  | (* e, e *)
      ExCom of loc * expr * expr
  | (* (e : t) *)
      ExTyc of loc * expr * ctyp
  | (* `s *)
      ExVrn of loc * string
  | (* while e do { e } *)
      ExWhi of loc * expr * expr
  | (* let open i in e *)
      ExOpI of loc * ident * expr
  | (* fun (type t) -> e *)
  (* let f x (type t) y z = e *)
      ExFUN of loc * string * expr
  | (* (module ME : S) which is represented as (module (ME : S)) *)
      ExPkg of loc * module_expr
and module_type =
  | MtNil of loc
  | (* i *)
  (* A.B.C *)
      MtId of loc * ident
  | (* functor (s : mt) -> mt *)
      MtFun of loc * string * module_type * module_type
  | (* 's *)
      MtQuo of loc * string
  | (* sig sg end *)
      MtSig of loc * sig_item
  | (* mt with wc *)
      MtWit of loc * module_type * with_constr
  | MtAnt of loc * string
and (* $s$ *)
  sig_item =
  | SgNil of loc
  | (* class cict *)
      SgCls of loc * class_type
  | (* class type cict *)
      SgClt of loc * class_type
  | (* sg ; sg *)
      SgSem of loc * sig_item * sig_item
  | (* # s or # s e *)
      SgDir of loc * string * expr
  | (* exception t *)
      SgExc of loc * ctyp
  | (* external s : t = s ... s *)
      SgExt of loc * string * ctyp * string meta_list
  | (* include mt *)
      SgInc of loc * module_type
  | (* module s : mt *)
      SgMod of loc * string * module_type
  | (* module rec mb *)
      SgRecMod of loc * module_binding
  | (* module type s = mt *)
      SgMty of loc * string * module_type
  | (* open i *)
      SgOpn of loc * ident
  | (* type t *)
      SgTyp of loc * ctyp
  | (* value s : t *)
      SgVal of loc * string * ctyp
  | SgAnt of loc * string
and (* $s$ *)
  with_constr =
  | WcNil of loc
  | (* type t = t *)
      WcTyp of loc * ctyp * ctyp
  | (* module i = i *)
      WcMod of loc * ident * ident
  | (* type t := t *)
      WcTyS of loc * ctyp * ctyp
  | (* module i := i *)
      WcMoS of loc * ident * ident
  | (* wc and wc *)
      WcAnd of loc * with_constr * with_constr
  | WcAnt of loc * string
and (* $s$ *)
  binding =
  | BiNil of loc
  | (* bi and bi *)
  (* let a = 42 and c = 43 *)
      BiAnd of loc * binding * binding
  | (* p = e *)
  (* let patt = expr *)
      BiEq of loc * patt * expr
  | BiAnt of loc * string
and (* $s$ *)
  rec_binding =
  | RbNil of loc
  | (* rb ; rb *)
      RbSem of loc * rec_binding * rec_binding
  | (* i = e *)
      RbEq of loc * ident * expr
  | RbAnt of loc * string
and (* $s$ *)
  module_binding =
  | MbNil of loc
  | (* mb and mb *)
  (* module rec (s : mt) = me and (s : mt) = me *)
      MbAnd of loc * module_binding * module_binding
  | (* s : mt = me *)
      MbColEq of loc * string * module_type * module_expr
  | (* s : mt *)
      MbCol of loc * string * module_type
  | MbAnt of loc * string
and (* $s$ *)
  match_case =
  | McNil of loc
  | (* a | a *)
      McOr of loc * match_case * match_case
  | (* p (when e)? -> e *)
      McArr of loc * patt * expr * expr
  | McAnt of loc * string
and (* $s$ *)
  module_expr =
  | MeNil of loc
  | (* i *)
      MeId of loc * ident
  | (* me me *)
      MeApp of loc * module_expr * module_expr
  | (* functor (s : mt) -> me *)
      MeFun of loc * string * module_type * module_expr
  | (* struct st end *)
      MeStr of loc * str_item
  | (* (me : mt) *)
      MeTyc of loc * module_expr * module_type
  | (* (value e) *)
  (* (value e : S) which is represented as (value (e : S)) *)
      MePkg of loc * expr
  | MeAnt of loc * string
and (* $s$ *)
  str_item =
  | StNil of loc
  | (* class cice *)
      StCls of loc * class_expr
  | (* class type cict *)
      StClt of loc * class_type
  | (* st ; st *)
      StSem of loc * str_item * str_item
  | (* # s or # s e *)
      StDir of loc * string * expr
  | (* exception t or exception t = i *)
      StExc of loc * ctyp * (*FIXME*) ident meta_option
  | (* e *)
      StExp of loc * expr
  | (* external s : t = s ... s *)
      StExt of loc * string * ctyp * string meta_list
  | (* include me *)
      StInc of loc * module_expr
  | (* module s = me *)
      StMod of loc * string * module_expr
  | (* module rec mb *)
      StRecMod of loc * module_binding
  | (* module type s = mt *)
      StMty of loc * string * module_type
  | (* open i *)
      StOpn of loc * ident
  | (* type t *)
      StTyp of loc * ctyp
  | (* value (rec)? bi *)
      StVal of loc * rec_flag * binding
  | StAnt of loc * string
and (* $s$ *)
  class_type =
  | CtNil of loc
  | (* (virtual)? i ([ t ])? *)
      CtCon of loc * virtual_flag * ident * ctyp
  | (* [t] -> ct *)
      CtFun of loc * ctyp * class_type
  | (* object ((t))? (csg)? end *)
      CtSig of loc * ctyp * class_sig_item
  | (* ct and ct *)
      CtAnd of loc * class_type * class_type
  | (* ct : ct *)
      CtCol of loc * class_type * class_type
  | (* ct = ct *)
      CtEq of loc * class_type * class_type
  | (* $s$ *)
      CtAnt of loc * string
and class_sig_item =
  | CgNil of loc
  | (* type t = t *)
      CgCtr of loc * ctyp * ctyp
  | (* csg ; csg *)
      CgSem of loc * class_sig_item * class_sig_item
  | (* inherit ct *)
      CgInh of loc * class_type
  | (* method s : t or method private s : t *)
      CgMth of loc * string * private_flag * ctyp
  | (* value (virtual)? (mutable)? s : t *)
      CgVal of loc * string * mutable_flag * virtual_flag * ctyp
  | (* method virtual (private)? s : t *)
      CgVir of loc * string * private_flag * ctyp
  | CgAnt of loc * string
and (* $s$ *)
  class_expr =
  | CeNil of loc
  | (* ce e *)
      CeApp of loc * class_expr * expr
  | (* (virtual)? i ([ t ])? *)
      CeCon of loc * virtual_flag * ident * ctyp
  | (* fun p -> ce *)
      CeFun of loc * patt * class_expr
  | (* let (rec)? bi in ce *)
      CeLet of loc * rec_flag * binding * class_expr
  | (* object ((p))? (cst)? end *)
      CeStr of loc * patt * class_str_item
  | (* ce : ct *)
      CeTyc of loc * class_expr * class_type
  | (* ce and ce *)
      CeAnd of loc * class_expr * class_expr
  | (* ce = ce *)
      CeEq of loc * class_expr * class_expr
  | (* $s$ *)
      CeAnt of loc * string
and class_str_item =
  | CrNil of loc
  | (* cst ; cst *)
      CrSem of loc * class_str_item * class_str_item
  | (* type t = t *)
      CrCtr of loc * ctyp * ctyp
  | (* inherit(!)? ce (as s)? *)
      CrInh of loc * override_flag * class_expr * string
  | (* initializer e *)
      CrIni of loc * expr
  | (* method(!)? (private)? s : t = e or method(!)? (private)? s = e *)
      CrMth of loc * string * override_flag * private_flag * expr * ctyp
  | (* value(!)? (mutable)? s = e *)
      CrVal of loc * string * override_flag * mutable_flag * expr
  | (* method virtual (private)? s : t *)
      CrVir of loc * string * private_flag * ctyp
  | (* value virtual (mutable)? s : t *)
      CrVvr of loc * string * mutable_flag * ctyp
  | CrAnt of loc * string
with sexp
