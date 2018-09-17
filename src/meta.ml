open Basic
open Config
open Encoding
open Entry

type Debug.flag += D_meta

let normalize_meta ?(sg=None) term =
  match sg with
  | None ->
    Env.unsafe_reduction ~red:(Config.red_cfg ()) term
  | Some sg ->
    Reduction.reduction  (Config.red_cfg ()) sg term

let register_definition md id = add_meta_rule (Rule.Delta(mk_name md id))

let register_rules (rs:Rule.untyped_rule list) =
  List.iter (fun (r:Rule.untyped_rule) -> add_meta_rule r.Rule.name) rs

let encode ~ctx t =
  match Config.config.encoding with
  | None -> t
  | Some (module E) ->
    let t' = E.encode_term ~ctx t in
    t'

exception Not_a_pattern

let rec pattern_to_term t =
  let open Term in
  match t with
  | Kind
  | Type _
  | Pi _ -> raise Not_a_pattern
  | Lam(lc,x,_,te) -> Rule.Lambda(lc,x,pattern_to_term te)
  | App(Const(lc,name),a,args) ->
    Rule.Pattern(lc,name, List.map pattern_to_term (a::args))
  | App(DB(lc,x,n),a,args) ->
    Rule.Var(lc,x, n, List.map pattern_to_term (a::args))
  | Const(lc,name) ->
    Rule.Pattern(lc,name, [])
  | DB(lc,x,n) ->
    Rule.Var(lc,x,n,[])
  | _ -> raise Not_a_pattern

let pattern_to_term t =
  match t with
  | Term.App(Term.Const(lc,name),a,args) ->
    Rule.Pattern(lc,name, List.map pattern_to_term (a::args))
  | _ -> raise Not_a_pattern

let encode_pattern ~ctx p =
  match Config.config.encoding with
  | None -> p
  | Some (module E) -> E.encode_pattern ~ctx p

let encode_untyped_rule (r:Rule.untyped_rule) =
  let open Rule in
  { r with
    pat = encode_pattern [] r.pat;
    rhs = encode [] r.rhs
  }


let encode_typed_rule (r:Rule.typed_rule) =
  let open Rule in
  { r with
    pat = encode_pattern r.ctx r.pat;
    rhs = encode r.ctx r.rhs
  }


let decode t =
  match Config.config.encoding with
  | None -> t
  | Some (module E) -> E.decode_term t

let normalize ?(sg=None) ?(ctx=[]) term =
    let term' = encode ~ctx term in
    let term'' = normalize_meta ~sg:sg term' in
    decode term''

let mk_entry md e =
  match e with
  | Decl(lc,id,st,ty) -> Env.declare lc id st ty;
    begin
      let ty' = normalize ty in
      let kw = match st with
        | Signature.Static -> ""
        | Signature.Definable -> "def "
      in
      Format.printf "@[<2>%s%a :@ %a.@]@.@." kw pp_ident id Pp.print_term ty'
    end
  | Def(lc,id,opaque,ty_opt,te) -> Env.define lc id opaque te ty_opt;
    begin
      let kw = if opaque then "thm" else "def" in
      let ty_opt' =
        match ty_opt with
        | None -> None
        | Some t -> Some (normalize t)
      in
      let te' = normalize te in
      match ty_opt' with
      | None ->
        Format.printf "@[<hv2>%s %a :=@ %a.@]@.@." kw pp_ident id Pp.print_term te'
      | Some ty ->
        Format.printf "@[<hv2>%s %a :@ %a@ :=@ %a.@]@.@." kw
          pp_ident id Pp.print_term ty Pp.print_term te'
    end
  | Rules(lc,rs) ->
    let rs2 = Env.add_rules rs in
    let normalize_rule (r : Rule.typed_rule) : Rule.typed_rule =
      let open Rule in
      {r with rhs = normalize ~ctx:r.ctx r.rhs}
    in
    let rs' = List.map normalize_rule (snd (List.split rs2)) in
    let print_rule r =
      Format.printf "@[%a@].@.@." Pp.print_typed_rule r
    in
    List.iter print_rule rs'
  | _ -> ()
