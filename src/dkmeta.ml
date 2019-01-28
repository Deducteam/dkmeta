module type Encoding =
sig
  val md : Basic.mident

  val entries : unit -> Entry.entry list

  val signature : Signature.t

  val encode_term : Term.term -> Term.term

  val decode_term : Term.term -> Term.term

  val encode_rule : 'a Rule.rule -> 'a Rule.rule
end

let version = "0.1"

type cfg = {
  mutable meta_rules  : Rule.rule_name list option;
  beta                : bool;
  encoding            : (module Encoding) option;
  sg                  : Signature.t
}

let default_config =
  {
    meta_rules = None;
    beta = true;
    encoding = None;
    sg = Signature.make ""
  }

let red_cfg : cfg -> Reduction.red_cfg = fun cfg ->
  let open Reduction in
  { default_cfg with
    beta = cfg.beta;
    target = Snf;
    select = Some
        (fun r ->
           match cfg.meta_rules with
           | None -> true
           | Some meta_rules -> List.mem r meta_rules)
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
    let sg = Signature.make "prod" in
    let mk_decl id =
      Signature.add_declaration sg dloc (mk_ident id) Signature.Definable (mk_Type dloc)
    in
    List.iter mk_decl ["prod"]; sg

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

  and encode_Const lc name = mk_Const dloc name

  and encode_Lam lc x mty te =
    let mty' = match mty with None -> None | Some ty -> Some (encode_term  ty) in
    mk_Lam dloc x mty' (encode_term  te)

  and encode_App f a args =
    mk_App2 (encode_term  f) (List.map (encode_term ) (a::args))

  and encode_Pi lc x a b =
    mk_App (const_of "prod") (encode_term a) [mk_Lam dloc x (Some (encode_term  a)) (encode_term  b)]

  (* Using typed context here does not make sense *)
  let rec encode_pattern  pattern : Rule.pattern = pattern

  let encode_rule (r:'a Rule.rule) =
    let open Rule in
    { r with
      pat = encode_pattern r.pat;
      rhs = encode_term r.rhs
    }

  let rec decode_term t =
    match t with
    | Kind -> assert false
    | Pi(lc,x,a,b) -> assert false
    | App(f,a,args) -> decode_App f a args
    | Lam(lc,x,mty,te) -> decode_Lam lc x mty te
    | Type _
    | DB _
    | Const _ -> t

  and decode_Lam lc x mty te =
    let mty' = match mty with None -> None | Some mty -> Some (decode_term mty) in
    mk_Lam lc x mty' (decode_term te)

  and decode_App f a args =
    match f with
    | Const(lc,name) ->
      begin
        if name_eq name (name_of "prod") then
          match a with
          | App(_,Lam(_,x,Some a, b),[]) -> mk_Pi dloc x (decode_term a) (decode_term b)
          | _ -> assert false
        else
            mk_App (decode_term f) (decode_term a) (List.map decode_term args)
      end
    | _ -> decode_App (decode_term f) (decode_term a) (List.map decode_term args)

  and decode_Pi lc x a b = assert false

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
    let sg = Signature.make "lf" in
    let mk_decl id =
      Signature.add_declaration sg dloc (mk_ident id) Signature.Definable (mk_Type dloc)
    in
    List.iter mk_decl ["ty"; "var";"sym";"lam";"app";"prod"];
    sg

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

  and encode_type lc = (const_of "ty")

  and encode_DB lc x n =
    mk_App (const_of "var") (mk_DB lc x n) []

  and encode_Const lc name =
    mk_App (const_of "sym") (mk_Const dloc name) []

  and encode_Lam lc x mty te =
    let mty' = match mty with None -> None | Some ty -> Some (encode_term  ty) in
    mk_App (const_of "lam") (mk_Lam dloc x mty' (encode_term  te)) []

  and encode_App f a args =
    mk_App (const_of "app") (encode_term  f) (List.map (encode_term ) (a::args))

  and encode_Pi lc x a b =
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

  let rec decode_term t =
    match t with
    | Kind -> assert false
    | Type(lc) -> assert false
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
    | Const(lc,name) ->
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

  and decode_Pi lc x a b = assert false
end

type Basic.Debug.flag += D_meta

let encode cfg term =
  match cfg.encoding with
  | None -> term
  | Some (module E:Encoding) -> E.encode_term term

let decode cfg term =
  match cfg.encoding with
  | None -> term
  | Some (module E:Encoding) -> E.decode_term term

let normalize cfg term =
  let red = red_cfg cfg in
  begin
  match cfg.meta_rules with
  | Some l -> Format.eprintf "%d@." (List.length l)
  | None -> Format.eprintf "NON@."
end;
  Reduction.reduction red cfg.sg term

let mk_term cfg term =
  Format.eprintf "b:%a@." Pp.print_term term;
  let term' = encode cfg term in
  Format.eprintf "a:%a@." Pp.print_term term';
  let term'' = normalize cfg term' in
  Format.eprintf "n:%a@." Pp.print_term term'';
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

let mk_pattern cfg pat =
  let t = Rule.pattern_to_term pat in
  let t' = mk_term cfg t in
  pattern_of_term t'

let mk_entry = fun cfg md entry ->
  let open Entry in
  let open Rule in
  Format.eprintf "%a@." Entry.pp_entry entry;
  match entry with
  | Decl(lc,id,st,ty) ->
    begin
      match cfg.meta_rules with
      | None -> Signature.add_declaration cfg.sg lc id Signature.Definable ty;
      | Some _ -> ()
    end;
    Decl(lc,id, st , mk_term cfg ty)
  | Def(lc,id,opaque, Some ty,te) ->
    begin
      match cfg.meta_rules with
      | None ->
        let cst = Basic.mk_name md id in
        let rule = { name= Delta(cst) ; ctx = [] ; pat = Pattern(lc, cst, []); rhs = te ; } in
        Signature.add_declaration cfg.sg lc id Signature.Definable ty;
        Signature.add_rules cfg.sg (List.map Rule.to_rule_infos [rule])
      | _ -> ()
    end;
    Def(lc,id,opaque,Some (mk_term cfg ty), mk_term cfg te)
  | Def(lc,id,opaque, None,te) ->
    failwith "type is missing and Dedukti is buggy so no location"
  | Rules(lc,rs) ->
    let normalize_rule (r : 'a rule) : 'a rule =
      { r with
        pat = mk_pattern cfg r.pat;
        rhs = mk_term cfg r.rhs
      }
    in
    let rs' = List.map normalize_rule rs in
    begin
      match cfg.meta_rules with (* If None, everything is meta *)
      | None -> Signature.add_rules cfg.sg (List.map to_rule_infos rs')
      | _ -> ()
    end;
    Rules(lc,rs')
  | _ -> entry

let dummy_name = ""
let dummy_signature () = Signature.make dummy_name

let add_rule sg r =
  Signature.add_rules sg [(Rule.to_rule_infos r)]

(* Several rules might be bound to different constants *)
let add_rules sg rs = List.iter (add_rule sg) rs

let meta_of_file : cfg -> string -> cfg =
  fun cfg file ->
    let ic = open_in file in
    let mk_entry = function
      | Entry.Rules(_,rs) -> rs
      | _ -> []
    in
    let md = Basic.mk_mident file in
    let entries = Parser.Parse_channel.parse md ic in
    let rules = List.fold_left (fun r e -> r@mk_entry e) [] entries in
    List.iter (fun r -> Format.eprintf "d:%a@." Rule.pp_rule_infos (Rule.to_rule_infos r)) rules;
    let rule_names = List.map (fun (r:Rule.untyped_rule) -> r.Rule.name) rules in
    let encoded_rules =
      match cfg.encoding with
      | None -> rules
      | Some (module E) ->
        Signature.import_signature cfg.sg E.signature;
        List.map E.encode_rule rules
    in
    add_rules cfg.sg encoded_rules;
    { cfg with meta_rules = Some rule_names }
