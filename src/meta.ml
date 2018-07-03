open Basic
open Config
open Encoding
open Entry

(* TODO:
   - Cannot add meta rules on static symbols
*)

let normalize_meta term =

  let filter name =
    let meta_rules = Config.meta_rules () in
    match meta_rules with
    | [] -> true (* no meta file given, then everything is meta *)
    | _ -> List.mem name (Config.meta_rules ())
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

let register_definition md id = add_meta_rule (Rule.Delta(mk_name md id))

let register_rules (rs:Rule.untyped_rule list) =
  List.iter (fun (r:Rule.untyped_rule) -> add_meta_rule r.Rule.name) rs

let encode t =
  match !Config._encoding with
  | None -> t
  | Some enc ->
    if enc = "lf" then
      Encoding.LF.encode_term t
    else
      failwith "unknown encoding"

let decode t =
  match !Config._encoding with
  | None -> t
  | Some enc ->
    if enc = "lf" then
      Encoding.LF.decode_term t
    else
      assert false

let normalize term =
  Format.eprintf "before: %a@." Pp.print_term term;
  let term' = encode term in
  Format.eprintf "encoded: %a@." Pp.print_term term';
  let term'' = normalize_meta term' in
  Format.eprintf "normalized: %a@." Pp.print_term term'';
  Format.eprintf "decoded: %a@." Pp.print_term (decode term'');
  decode term''


let mk_entry md is_meta e =
  match e with
  | Decl(lc,id,st,ty) ->
    begin
      if not is_meta || not (Config.unsafe ()) then
        match Env.declare lc id st ty with
        | OK () -> ()
        | Err e -> Errors.fail_env_error e
      else
        Signature.add_declaration (Env.get_signature ()) lc id st ty
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
      if not is_meta || not (Config.unsafe ()) then
        let define = if opaque then Env.define_op else Env.define in
        match define lc id te ty_opt with
        | OK () -> ()
        | Err e -> Errors.fail_env_error e
      else
        begin
          let ty = match ty_opt with None -> Term.mk_Kind | Some ty -> ty in
          let st = if opaque then Signature.Static else Signature.Definable in
          Signature.add_declaration (Env.get_signature ()) lc id st ty;
          let open Rule in
          if not opaque then
            let cst = mk_name md id in
            let rule =
              { name= Delta(cst) ;
                ctx = [] ;
                pat = Pattern(lc, cst, []);
                rhs = te ;
              }
            in
            Signature.add_rules (Env.get_signature ()) [rule]
        end
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
      if not is_meta || not (Config.unsafe ()) then
          match Env.add_rules rs with
          | OK rs -> ()
          | Err e -> Errors.fail_env_error e
      else
        try
          Signature.add_rules (Env.get_signature ()) rs
        with Signature.SignatureError err -> Errors.fail_signature_error err
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
