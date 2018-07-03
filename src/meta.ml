open Basic
open Config
open Encoding
open Entry

let normalize_meta term =

  let filter name =
    List.mem name (Config.meta_rules ())
  in
  let red = let open Reduction in
    {
      beta = Config.beta ();
      select = Some filter;
      strategy = Snf;
      nb_steps = None;
    }
  in
  Env.unsafe_reduction ~red:red term


let normalize_encoding term =
  LF.encode_term term

let normalize term = normalize_meta term

let register_definition md id = add_meta_rule (Rule.Delta(mk_name md id))

let register_rules (rs:Rule.untyped_rule list) =
  List.iter (fun (r:Rule.untyped_rule) -> add_meta_rule r.Rule.name) rs

let mk_entry md is_meta e =
  match e with
  | Decl(lc,id,st,ty) ->
    begin
      match Env.declare lc id st ty with
      | OK () -> ()
      | Err e -> Errors.fail_env_error e
    end;
    if is_meta then Debug.debug Debug.d_notice
        "[DKMETA] Declarations %a appears in meta files" Pp.print_ident id
    else
      begin
        let ty' = normalize ty in
        let kw = match st with
          | Signature.Static -> ""
          | Signature.Definable -> "def "
        in
        Format.printf "@[<2>%s%a :@ %a.@]@.@." kw pp_ident id Pp.print_term ty'
      end
  | Def(lc,id,opaque,ty_opt,te) ->
    begin
      let define = if opaque then Env.define_op else Env.define in
      match define lc id te ty_opt with
      | OK () -> ()
      | Err e -> Errors.fail_env_error e
    end;
    if is_meta then register_definition md id
    else
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
  | Rules(rs) ->
    begin
      match Env.add_rules rs with
      | OK rs -> ()
      | Err e -> Errors.fail_env_error e
    end;
    if is_meta then register_rules rs
    else
      begin
        let normalize_rule (r : Rule.untyped_rule) : Rule.untyped_rule =
          let open Rule in
          {r with rhs = normalize r.rhs}
        in
        let rs' = List.map normalize_rule rs in
        let print_rule r =
          Format.printf "@[%a@].@.@." Pp.print_untyped_rule r
        in
        List.iter print_rule rs'
      end
  | _ -> ()
