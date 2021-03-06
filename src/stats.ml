open Kernel
open Api

module Printer = Pp.Default
module B = Basic


type flag =
  | Md
  | Id
  | Matchee
  | Reduced
  | Pos

let meta_flag_md = B.mk_mident ""

let const_of s = B.mk_name meta_flag_md (B.mk_ident s)

let c_md = const_of "__MD__"

let c_id = const_of "__ID__"

let c_matchee = const_of "__MATCHEE__"

let c_reduced = const_of "__REDUCED__"

let c_pos = const_of "__POS__"

let c_name = const_of "__NAME__"

let c_all = const_of "__ALL__"

let meta_flag_of_term : Term.term -> flag list = fun term ->
  match term with
  | Const(_,n) when B.ident_eq (B.id n) (B.id c_md)      -> [Md]
  | Const(_,n) when B.ident_eq (B.id n) (B.id c_id)      -> [Id]
  | Const(_,n) when B.ident_eq (B.id n) (B.id c_name)    -> [Md;Id]
  | Const(_,n) when B.ident_eq (B.id n) (B.id c_matchee) -> [Matchee]
  | Const(_,n) when B.ident_eq (B.id n) (B.id c_reduced) -> [Reduced]
  | Const(_,n) when B.ident_eq (B.id n) (B.id c_all)     -> [Pos;Md;Id;Matchee;Reduced]
  | _ -> assert false

let flags : (Rule.rule_name, flag list) Hashtbl.t = Hashtbl.create 11

let register_flags : Rule.rule_name -> Term.term -> unit = fun rn t ->
  Hashtbl.add flags rn (meta_flag_of_term t)

let print_id = fun fmt id ->
  Format.fprintf fmt "id:%a" Printer.print_ident id

let print_md = fun fmt md ->
  Format.fprintf fmt "md:%a" Printer.print_mident md

let print_matchee = fun fmt t ->
  Format.fprintf fmt "b:%a" Printer.print_term (Lazy.force t)

let print_reduced = fun fmt t ->
  Format.fprintf fmt "a:%a" Printer.print_term (Lazy.force t)

let print_loc = fun fmt t ->
  Format.fprintf fmt "l:%a" Basic.pp_loc t

let print_flag = fun fmt name loc _ l r flag ->
  match flag with
  | Pos     -> Format.fprintf fmt "%a@." print_loc loc
  | Id      -> Format.fprintf fmt "%a@." print_id (B.id name)
  | Md      -> Format.fprintf fmt "%a@." print_md (B.md name)
  | Matchee -> Format.fprintf fmt "%a@." print_matchee l
  | Reduced -> Format.fprintf fmt "%a@." print_reduced r

let current_name = ref (B.mk_name (B.mk_mident "") (B.mk_ident ""))

let logger = fun _ rn b a ->
  let flags = try Hashtbl.find flags rn with Not_found -> [] in
  let name = !current_name in
  let loc = Term.get_loc (Lazy.force b) in
  List.iter (print_flag Format.std_formatter name loc rn b a) flags


module R : Reduction.S =
struct
  open Reduction
  include Default

  let whnf_cfg = {default_cfg with target = Whnf; logger;}

  let snf_cfg = {default_cfg with target = Snf; logger;}

  let whnf = reduction whnf_cfg

  let snf = reduction snf_cfg

  let rec are_convertible_lst sg : (Term.term * Term.term) list -> bool =
    function
    | [] -> true
    | (t1,t2)::lst -> are_convertible_lst sg
                        (if Term.term_eq t1 t2 then lst
                         else conversion_step (whnf sg t1, whnf sg t2) lst)

(* Convertibility Test *)
  and are_convertible sg t1 t2 =
    try are_convertible_lst sg [(t1,t2)]
    with NotConvertible -> false

  let matching_test _ _ = are_convertible
end

module T : Typing.S = Typing.Make(R)

let sg = ref (Signature.make (Basic.mk_mident "") (Files.find_object_file))

let init s =
  sg := Signature.make (Basic.mk_mident s) Files.find_object_file;
  Signature.get_name !sg

let _add_rules rs =
  let ris = List.map Rule.to_rule_infos rs in
  List.iter Rule.check_arity ris;
  Signature.add_rules !sg ris


module Stats : Api.Processor.S with type t = unit =
struct

  type t = unit

  let handle_entry env =
    let open Parsers.Entry in
    let open Term in
    let md = Env.get_name env in
    function
    | Decl(lc,id,st,ty) ->
      current_name := B.mk_name md id;
      begin
        match T.inference !sg ty with
        | Kind | Type _ -> Signature.add_declaration !sg lc id st ty
        | _ as s -> raise (Typing.Typing_error (Typing.SortExpected (ty,[],s)))
      end
    | Def(lc,id,opaque,ty_opt,te) ->
      current_name := B.mk_name md id;
      let ty = match ty_opt with
        | None -> T.inference !sg te
        | Some ty -> T.checking !sg te ty; ty
      in
      begin
        match ty with
        | Kind -> raise (Typing.Typing_error Typing.KindIsNotTypable)
        | _ ->
          if opaque then Signature.add_declaration !sg lc id Signature.Static ty
          else
            let _ = Signature.add_declaration !sg lc id Signature.Definable ty in
            let cst = B.mk_name (Signature.get_name !sg) id in
            let rule = Rule.(
                { name= Delta(cst) ;
                  ctx = [] ;
                  pat = Pattern(lc, cst, []);
                  rhs = te ;
                })
            in
            _add_rules [rule]
      end
    | Rules(_,rs) -> _add_rules rs
    | _ -> ()


  let get_data () = ()
end

module ConfigStats : Api.Processor.S with type t = unit =
struct
  type t = unit

  let mk_rule = fun r ->
    let open Rule in
    match r.pat with
    | Pattern(_,n,[Brackets(Term.Const(_,m))]) when  (B.id n) = B.mk_ident "trace_delta" ->
      register_flags (Delta(m)) r.rhs
    | Pattern(_,n,[Brackets(Term.Const(_,m))]) when  (B.id n) = B.mk_ident "trace_gamma" ->
      register_flags (Gamma(false,m)) r.rhs
    | _ -> assert false


  let handle_entry _ =  function
    | Parsers.Entry.Rules(_,rs) -> List.iter mk_rule rs
    | _ -> failwith "Stats should contains only rules"

  let get_data () = ()
end

let run_on_meta_file file =
  Processor.handle_files [file] (module ConfigStats)

let run_on_file file =
  Processor.handle_files [file] (module Stats)
