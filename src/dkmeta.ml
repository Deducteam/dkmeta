open Kernel
open Api
open Parsers

let version = "0.1"

module type Encoding =
sig
  val md : Basic.mident

  val entries : unit -> Entry.entry list

  val safe : bool

  val signature : Signature.t

  val encode_term : ?sg:Signature.t -> ?ctx:Term.typed_context -> Term.term -> Term.term

  val decode_term : Term.term -> Term.term

  val encode_rule : ?sg:Signature.t ->  'a Term.context Rule.rule -> 'a Term.context Rule.rule
end

module RNS = Set.Make(struct type t = Rule.rule_name let compare = compare end)

type cfg = {
  mutable meta_rules  : RNS.t option;
  (* Set of meta_rules used to normalize *)
  beta                : bool;
  (* Allows beta doing normalization *)
  encoding            : (module Encoding) option;
  (* Encoding specify a quoting mechanism *)
  env                 : Env.t
  (* The current environment (if the encoding needs type checking *)
}

let default_config =
  {
    meta_rules = None;
    beta       = true;
    encoding   = None;
    env        = Env.init (Parser.input_from_string (Basic.mk_mident "") "")
  }


(* A dkmeta configuration to a reduction configuration *)
let red_cfg : cfg -> Reduction.red_cfg =
  fun cfg ->
  let open Reduction in
  { default_cfg with
    beta = cfg.beta;
    target = Snf;
    strat = ByValue;
    select = Some
        (fun r ->
           match cfg.meta_rules with
           | None -> true
           | Some meta_rules -> RNS.mem r meta_rules)
  }

module PROD =
struct
  open Basic
  open Term

  let md = mk_mident "prod"

  let entries () =
    let mk_decl id =
      Entry.Decl(dloc,mk_ident id, Signature.Definable,mk_Type dloc)
    in
    List.map mk_decl ["prod"]

  let signature =
    let sg = Signature.make md Files.find_object_file in
    let mk_decl id =
      Signature.add_declaration sg dloc (mk_ident id) Signature.Definable (mk_Type dloc)
    in
    List.iter mk_decl ["prod"]; sg

  let safe = false

  let name_of str = mk_name md (mk_ident str)

  let const_of str = mk_Const dloc (name_of str)

  let rec encode_term t =
    match t with
    | Kind -> assert false
    | Type(lc) -> encode_type lc
    | DB(lc,x,n) -> encode_DB lc x n
    | Const(lc, name) -> encode_Const lc name
    | Lam(lc,x,mty,te) -> encode_Lam lc x mty te
    | App(f,a,args) -> encode_App f a args
    | Pi(lc,x,a,b) -> encode_Pi lc x a b

  and encode_type lc = mk_Type lc

  and encode_DB lc x n = mk_DB lc x n

  and encode_Const _ name = mk_Const dloc name

  and encode_Lam _ x mty te =
    let mty' = match mty with None -> None | Some ty -> Some (encode_term  ty) in
    mk_Lam dloc x mty' (encode_term  te)

  and encode_App f a args =
    mk_App2 (encode_term  f) (List.map (encode_term ) (a::args))

  and encode_Pi _ x a b =
    mk_App (const_of "prod") (encode_term a) [mk_Lam dloc x (Some (encode_term  a)) (encode_term  b)]

  (* Using typed context here does not make sense *)
  let encode_pattern  pattern : Rule.pattern = pattern

  let encode_rule (r:'a Rule.rule) =
    let open Rule in
    { r with
      pat = encode_pattern r.pat;
      rhs = encode_term r.rhs
    }

  let encode_term ?sg:_ ?ctx:_ t = encode_term t

  let encode_rule ?sg:_ r = encode_rule r

  let rec decode_term t =
    match t with
    | Kind             -> assert false
    | Pi _             -> assert false
    | App(f,a,args)    -> decode_App f a args
    | Lam(lc,x,mty,te) -> decode_Lam lc x mty te
    | Type _
    | DB _
    | Const _ -> t

  and decode_Lam lc x mty te =
    let mty' = match mty with None -> None | Some mty -> Some (decode_term mty) in
    mk_Lam lc x mty' (decode_term te)

  and decode_App f a args =
    match f with
    | Const(_,name) ->
      begin
        if name_eq name (name_of "prod") then
          match args with
          | [Lam(_,x,Some a, b)] -> mk_Pi dloc x (decode_term a) (decode_term b)
          | _ -> assert false
        else
            mk_App (decode_term f) (decode_term a) (List.map decode_term args)
      end
    | _ -> mk_App (decode_term f) (decode_term a) (List.map decode_term args)

end

module LF =
struct
  open Basic
  open Term

  let md = mk_mident "lf"

  let entries () =
    let mk_decl id =
      Entry.Decl(dloc,mk_ident id, Signature.Definable,mk_Type dloc)
    in
    List.map mk_decl ["ty"; "var";"sym";"lam";"app";"prod"]

  let signature =
    let sg = Signature.make md Files.find_object_file in
    let mk_decl id =
      Signature.add_declaration sg dloc (mk_ident id) Signature.Definable (mk_Type dloc)
    in
    List.iter mk_decl ["ty"; "var";"sym";"lam";"app";"prod"];
    sg

  let safe = false

  let name_of str = mk_name md (mk_ident str)

  let const_of str = mk_Const dloc (name_of str)

  let rec encode_term t =
    match t with
    | Kind -> assert false
    | Type(lc) -> encode_type lc
    | DB(lc,x,n) -> encode_DB lc x n
    | Const(lc, name) -> encode_Const lc name
    | Lam(lc,x,mty,te) -> encode_Lam lc x mty te
    | App(f,a,args) -> encode_App f a args
    | Pi(lc,x,a,b) -> encode_Pi lc x a b

  and encode_type _ = (const_of "ty")

  and encode_DB lc x n =
    mk_App (const_of "var") (mk_DB lc x n) []

  and encode_Const _ name =
    mk_App (const_of "sym") (mk_Const dloc name) []

  and encode_Lam _ x mty te =
    let mty' = match mty with None -> None | Some ty -> Some (encode_term  ty) in
    mk_App (const_of "lam") (mk_Lam dloc x mty' (encode_term  te)) []

  and encode_App f a args =
    mk_App (const_of "app") (encode_term  f) (List.map (encode_term ) (a::args))

  and encode_Pi _ x a b =
    mk_App (const_of "prod") (mk_Lam dloc x (Some (encode_term  a)) (encode_term  b)) []


  (* Using typed context here does not make sense *)
  let rec encode_pattern  pattern : Rule.pattern =
    let open Rule in
    match pattern with
    | Var(lc, id, n, ps) -> Var(lc,id,n, List.map (encode_pattern ) ps)
    | Brackets(term) -> Brackets(encode_term  term)
    | Lambda(lc, id, p) -> Pattern(lc,(name_of "lam"), [(Lambda(lc,id, encode_pattern  p))])
    | Pattern(lc,n,[]) ->
      Pattern(lc,name_of "sym", [Pattern(lc,n,[])])
    | Pattern(lc,n, ps) ->
      Pattern(lc, name_of "app", (Pattern(lc,name_of "sym",[Pattern(lc,n,[])]))::(List.map (encode_pattern) ps))

  let encode_rule (r:'a Rule.rule) =
    let open Rule in
    { r with
      pat = encode_pattern r.pat;
      rhs = encode_term r.rhs
    }

  let encode_term ?sg:_ ?ctx:_ t = encode_term t

  let encode_rule ?sg:_ r = encode_rule r

  let rec decode_term t =
    match t with
    | Kind             -> assert false
    | Type _           -> assert false
    | DB(lc,x,n)       -> decode_DB lc x n
    | Const(lc,name)   -> decode_Const lc name
    | Lam(lc,x,mty,te) -> decode_Lam lc x mty te
    | App(f,a,args)    -> decode_App f a args
    | Pi(lc,x,a,b)     -> decode_Pi lc x a b

  and decode_DB lc x n = mk_DB lc x n

  and decode_Const lc name =
    if name_eq name (name_of "ty") then mk_Type dloc else mk_Const lc name

  and decode_Lam lc x mty te =
    let mty' = match mty with None -> None | Some mty -> Some (decode_term mty) in
    mk_Lam lc x mty' (decode_term te)

  and decode_App f a args =
    match f with
    | Const(_,name) ->
      if name_eq name (name_of "prod") then
        match a with
        | Lam(_,x,Some a, b) -> mk_Pi dloc x (decode_term a) (decode_term b)
        | _ -> assert false
      else if name_eq name (name_of "sym") then
        decode_term a
      else if name_eq name (name_of "var") then
        decode_term  a
      else if name_eq name (name_of "app") then
        mk_App2 (decode_term a) (List.map decode_term args)
      else if name_eq name (name_of "lam") then
        decode_term a
      else
        mk_App (decode_term f) (decode_term a) (List.map decode_term args)

    | _ -> decode_App (decode_term f) (decode_term a) (List.map decode_term args)

  and decode_Pi _ _ _ _ = assert false
end

module APP =
struct
  open Basic
  open Term

  let md = mk_mident "app"

  let entries () =
    let mk_decl id =
      Entry.Decl(dloc,mk_ident id, Signature.Definable,mk_Type dloc)
    in
    List.map mk_decl ["ty"; "var";"sym";"lam";"app";"prod"]

  let signature =
    let sg = Signature.make md Files.find_object_file in
    let mk_decl id =
      Signature.add_declaration sg dloc (mk_ident id) Signature.Definable (mk_Type dloc)
    in
    List.iter mk_decl ["ty"; "var";"sym";"lam";"app";"prod"];
    sg

  let safe = true

  let name_of str = mk_name md (mk_ident str)

  let const_of str = mk_Const dloc (name_of str)

  let rec encode_term sg ctx t =
    match t with
    | Kind -> assert false
    | Type(lc) -> encode_type sg ctx lc
    | DB(lc,x,n) -> encode_DB sg ctx lc x n
    | Const(lc, name) -> encode_Const sg ctx lc name
    | Lam(lc,x,Some ty,te) -> encode_Lam sg ctx lc x ty te
    | Lam _ -> assert false
    | App(f,a,args) -> encode_App sg ctx f a args
    | Pi(lc,x,a,b) -> encode_Pi sg ctx lc x a b

  and encode_type _ _ _ = (const_of "ty")

  and encode_DB _ _ lc x n =
    mk_App (const_of "var") (mk_DB lc x n) []

  and encode_Const _ _ _ name =
    mk_App (const_of "sym") (mk_Const dloc name) []

  and encode_Lam sg ctx _ x ty te =
    let ctx' = (Basic.dloc, x, ty)::ctx in
    mk_App (const_of "lam") (mk_Lam dloc x (Some (encode_term sg ctx ty)) (encode_term sg ctx' te)) []

  and encode_App sg ctx f a args =
    encode_app2 sg ctx f (a::args)

  and encode_app2 sg ctx f args =
    let aux f f' a =
      let tyf  = Typing.Default.infer sg ctx f in
      let tyf' =
        begin
          match tyf with
          | Term.Pi _ -> PROD.encode_term tyf
          | _ -> tyf
        end
      in
      Term.mk_App2 f [a], mk_App (const_of "app") tyf' [f'; encode_term sg ctx a]
    in
    snd @@ List.fold_left (fun (f,f') a -> aux f f' a) (f,encode_term sg ctx f) args

  and encode_Pi sg ctx _ x a b =
    let ctx' = (Basic.dloc, x, a)::ctx in
    mk_App (const_of "prod") (mk_Lam dloc x (Some (encode_term sg ctx a)) (encode_term sg ctx' b)) []

  (* FIXME: complete this function *)
  let encode_pattern = fun _ _ pattern -> pattern
    (*
  let open Rule in

    match pattern with
    | Var(lc, id, n, ps) -> Var(lc,id,n, List.map (encode_pattern sg ctx) ps)
    | Brackets(term) -> Brackets(encode_term  sg ctx term)
    | Lambda(lc, id, p) -> Pattern(lc,(name_of "lam"), [(Lambda(lc,id, encode_pattern sg ctx p))])
    | Pattern(lc,n,[]) ->
      Pattern(lc,name_of "sym", [Pattern(lc,n,[])])
    | Pattern(lc,n, [a]) ->
      Pattern(lc, name_of "app", Pattern(lc, name_of "coucou", [])::((Pattern(lc,name_of "sym",[Pattern(lc,n,[])])))::(encode_pattern sg ctx a)::[])
    | Pattern(lc,n,a::l) ->
      Pattern(lc, name_of "app", Pattern(lc, name_of "coucou", [])::((Pattern(lc,name_of "sym",[Pattern(lc,n,[])])))::(encode_pattern sg ctx a)::[])
*)
  let encode_rule = fun sg r ->
    let r' = Rule.untyped_rule_of_rule_infos (Rule.to_rule_infos r) in
    let _,r'' = Typing.Default.check_rule sg r' in
    let open Rule in
    { r with
      pat = encode_pattern sg r''.ctx r.pat;
      rhs = encode_term sg r''.ctx r.rhs
    }

  let fake_sig () =
    Signature.make (Basic.mk_mident "") Files.find_object_file

  let encode_term ?(sg=fake_sig ()) ?(ctx=[]) t = encode_term sg ctx t

  let encode_rule ?(sg=fake_sig ()) r = encode_rule sg r

  let rec decode_term t =
    match t with
    | Kind   -> assert false
    | Type _ -> assert false
    | DB(lc,x,n) -> decode_DB lc x n
    | Const(lc,name) -> decode_Const lc name
    | Lam(lc,x,mty,te) -> decode_Lam lc x mty te
    | App(f,a,args) -> decode_App f a args
    | Pi(lc,x,a,b) -> decode_Pi lc x a b

  and decode_DB lc x n = mk_DB lc x n

  and decode_Const lc name =
    if name_eq name (name_of "ty") then mk_Type dloc else mk_Const lc name

  and decode_Lam lc x mty te =
    let mty' = match mty with None -> None | Some mty -> Some (decode_term mty) in
    mk_Lam lc x mty' (decode_term te)

  and decode_App f a args =
    match f with
    | Const(_,name) ->
      if name_eq name (name_of "prod") then
        match a with
        | Lam(_,x,Some a, b) -> mk_Pi dloc x (decode_term a) (decode_term b)
        | _ -> assert false
      else if name_eq name (name_of "sym") then
        decode_term a
      else if name_eq name (name_of "var") then
        decode_term  a
      else if name_eq name (name_of "app") then
        begin
          match args with
          | [f;a] ->
            Term.mk_App2 (decode_term f) [(decode_term a)]
          | _ -> assert false
        end
      else if name_eq name (name_of "lam") then
        decode_term a
      else
        mk_App (decode_term f) (decode_term a) (List.map decode_term args)

    | _ -> decode_App (decode_term f) (decode_term a) (List.map decode_term args)

  and decode_Pi _ _ _ _ = assert false
end

let encode sg cfg term =
  match cfg.encoding with
  | None -> term
  | Some (module E:Encoding) ->
    if E.safe then
      E.encode_term ~sg term
    else
      E.encode_term term

let decode cfg term =
  match cfg.encoding with
  | None -> term
  | Some (module E:Encoding) -> E.decode_term term

let normalize cfg term =
  let red = red_cfg cfg in
  let sg = Env.get_signature cfg.env in
  Reduction.Default.reduction red sg term




let sg = ref (Signature.make (Basic.mk_mident "") Files.find_object_file)

let init f =
  sg := Signature.make f Files.find_object_file

let mk_term env cfg term =
  (* Format.eprintf "b:%a@." Pp.print_term term; *)
  let sg    = Env.get_signature env in
  let term' = encode sg cfg term in
  (* Format.eprintf "a:%a@." Pp.print_term term'; *)
  let term'' = normalize cfg term' in
  (* Format.eprintf "n:%a@." Pp.print_term term''; *)
  decode cfg term''

exception Not_a_pattern

let rec pattern_of_term = fun t ->
  let open Term in
  match t with
  | Kind
  | Type _
  | Pi _ -> raise Not_a_pattern
  | Lam(lc,x,_,te) -> Rule.Lambda(lc,x,pattern_of_term te)
  | App(Const(lc,name),a,args) ->
    Rule.Pattern(lc,name, List.map pattern_of_term (a::args))
  | App(DB(lc,x,n),a,args) ->
    Rule.Var(lc,x, n, List.map pattern_of_term (a::args))
  | Const(lc,name) ->
    Rule.Pattern(lc,name, [])
  | DB(lc,x,n) ->
    Rule.Var(lc,x,n,[])
  | _ -> raise Not_a_pattern

let mk_rule env cfg (r: 'a Term.context Rule.rule) =
  let open Rule in
  match cfg.encoding with
  | None ->
    {r with rhs = normalize cfg r.rhs}
  | Some (module E:Encoding) ->
    let sg = Env.get_signature env in
    let r' = E.encode_rule ~sg r in
    let pat'  = pattern_of_term (E.decode_term (normalize cfg (Rule.pattern_to_term r'.pat))) in
    let rhs'  = normalize cfg r'.rhs in
    let rhs'' = decode cfg rhs' in
    {pat=pat';rhs=rhs'';ctx=r.ctx;name=r.name}

(*
  let t = Rule.pattern_to_term pat in
  let t' = mk_term cfg t in
  pattern_of_term t' *)

module D = Basic.Debug
type D.flag += D_meta
let _ = D.register_flag D_meta "Dkmeta"

let bmag fmt = "\027[90m" ^^ fmt ^^ "\027[0m%!"

let log fmt = D.debug D_meta (bmag fmt)

let mk_entry env = fun cfg entry ->
  let open Entry in
  let open Rule in
  let sg = Env.get_signature env in
  let md = Env.get_name env in
  match entry with
  | Decl(lc,id,st,ty) ->
    log "[NORMALIZE] %a" Basic.pp_ident id;
    let ty' = mk_term env cfg ty in
    Signature.add_declaration sg lc id st ty';
    Decl(lc,id, st , ty')
  | Def(lc,id,opaque, Some ty,te) ->
    log "[NORMALIZE] %a" Basic.pp_ident id;
    let cst = Basic.mk_name md id in
    let rule = { name= Delta(cst) ; ctx = [] ; pat = Pattern(lc, cst, []); rhs = te ; } in
    (*
    Signature.add_declaration !sg lc id Signature.Definable ty;
      Signature.add_rules !sg (List.map Rule.to_rule_infos [rule]); *)
    let ty' = mk_term env cfg ty in
    let te' = mk_term env cfg te in
    Signature.add_declaration sg lc id Signature.Definable ty';
    Signature.add_rules sg (List.map Rule.to_rule_infos [{rule with rhs=te'}]);
    Def(lc,id,opaque,Some ty', te')
  | Def _ ->
    failwith "type is missing and Dedukti is buggy so no location"
  | Rules(lc,rs) ->
    (* Signature.add_rules !sg (List.map Rule.to_rule_infos rs); *)
    let rs' = List.map (mk_rule env cfg) rs in
    Signature.add_rules sg (List.map Rule.to_rule_infos rs');
    Rules(lc,rs')
  | _ -> entry

let add_rule sg r =
  Signature.add_rules sg [(Rule.to_rule_infos r)]

(* Several rules might be bound to different constants *)
let add_rules sg rs = List.iter (add_rule sg) rs

let meta_of_rules : Rule.untyped_rule list -> cfg -> cfg = fun rules cfg ->
  let rule_names = List.map (fun (r:Rule.untyped_rule) -> r.Rule.name) rules in
  let sg = Env.get_signature cfg.env in
  add_rules sg rules;
  match cfg.meta_rules with
  | None ->
    { cfg with meta_rules = Some (RNS.of_list rule_names)}
  | Some mrules ->
    { cfg with meta_rules = Some (RNS.union (RNS.of_list (rule_names)) mrules) }


module MetaConfiguration : Processor.S with type t = Rule.untyped_rule list =
struct

  type t = Rule.untyped_rule list

  let rules = ref []
  let handle_entry _ = function
    | Entry.Rules(_,rs) -> rules := rs::!rules
    | _ -> ()

  let get_data () = List.flatten !rules

end

let meta_of_files ?cfg:(cfg=default_config) files =
  Processor.process_files files  meta_of_rules cfg (module MetaConfiguration)

let make_meta_processor cfg post_processing =
  let module Meta =
  struct
    type t = unit

    let handle_entry env entry = post_processing (mk_entry env cfg entry)

    let get_data () = ()
  end
  in
  (module Meta : Processor.S with type t = unit)
