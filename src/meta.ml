open Basic
open Config
open Encoding
open Entry

type Debug.flag += D_meta

let normalize_meta term =
  Env.unsafe_reduction ~red:(Config.red_cfg ()) term

let register_definition md id = add_meta_rule (Rule.Delta(mk_name md id))

let register_rules (rs:Rule.untyped_rule list) =
  List.iter (fun (r:Rule.untyped_rule) -> add_meta_rule r.Rule.name) rs

let encode t =
  match Config.config.encoding with
  | None -> t
  | Some (module E) ->
    Format.eprintf "before: %a@." Pp.print_term t;
    let t' = E.encode_term t in
    Format.eprintf "after: %a@." Pp.print_term t';
    t'

let decode t =
  match Config.config.encoding with
  | None -> t
  | Some (module E) -> E.decode_term t

let normalize term =
  let term' = encode term in
  let term'' = normalize_meta term' in
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
  | Rules(lc,rs) ->  ignore(Env.add_rules rs);
    let normalize_rule (r : Rule.untyped_rule) : Rule.untyped_rule =
      let open Rule in
      {r with rhs = normalize r.rhs}
    in
    let rs' = List.map normalize_rule rs in
    let print_rule r =
      Format.printf "@[%a@].@.@." Pp.print_untyped_rule r
    in
    List.iter print_rule rs'
  | _ -> ()
