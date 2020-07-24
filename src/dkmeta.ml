open Kernel
open Api
open Parsers

let version = "0.1"

module RNS = Set.Make (struct
  type t = Rule.rule_name

  let compare = compare
end)

type cfg = {
  mutable meta_rules : RNS.t list option;
  (* Set of meta_rules used to normalize *)
  beta : bool;
  (* Allows beta doing normalization *)
  register_before : bool;
  (* entries are registered before they have been normalized *)
  encode_meta_rules : bool;
  (* The encoding is used on the meta rules first except for products *)
  quoting : (module Quoting.S) option;
  (* Encoding specify a quoting mechanism *)
  unquoting : bool;
  (* If false, the term is not decoded after normalization *)
  env : Env.t; (* The current environment (if the encoding needs type checking *)
}

let default_config =
  {
    meta_rules = None;
    beta = true;
    quoting = None;
    register_before = true;
    encode_meta_rules = false;
    env = Env.init (Parser.input_from_string (Basic.mk_mident "") "");
    unquoting = true;
  }

(* A dkmeta configuration to a reduction configuration *)
let red_cfg : cfg -> Reduction.red_cfg list =
 fun cfg ->
  let open Reduction in
  let red_cfg =
    { default_cfg with beta = cfg.beta; target = Snf; strat = ByValue }
  in
  match cfg.meta_rules with
  | None -> [ { red_cfg with select = Some (fun _ -> true) } ]
  | Some l ->
      List.map
        (fun meta_rules ->
          { red_cfg with select = Some (fun r -> RNS.mem r meta_rules) })
        l

let quote sg cfg term =
  match cfg.quoting with
  | None -> term
  | Some (module E : Quoting.S) ->
      if E.safe then E.quote_term ~sg term else E.quote_term term

let unquote cfg term =
  match cfg.quoting with
  | None -> term
  | Some (module E : Quoting.S) -> E.unquote_term term

let normalize cfg term =
  let reds = red_cfg cfg in
  let sg = Env.get_signature cfg.env in
  List.fold_left
    (fun term red -> Reduction.Default.reduction red sg term)
    term reds

let mk_term cfg ?(env = cfg.env) term =
  (* Format.eprintf "b:%a@." Pp.print_term term; *)
  let sg = Env.get_signature env in
  let term' = quote sg cfg term in
  let term'' = normalize cfg term' in
  if cfg.unquoting then unquote cfg term'' else term''

exception Not_a_pattern

let rec pattern_of_term t =
  let open Term in
  match t with
  | Kind | Type _ | Pi _ -> raise Not_a_pattern
  | Lam (lc, x, _, te) -> Rule.Lambda (lc, x, pattern_of_term te)
  | App (Const (lc, name), a, args) ->
      Rule.Pattern (lc, name, List.map pattern_of_term (a :: args))
  | App (DB (lc, x, n), a, args) ->
      Rule.Var (lc, x, n, List.map pattern_of_term (a :: args))
  | Const (lc, name) -> Rule.Pattern (lc, name, [])
  | DB (lc, x, n) -> Rule.Var (lc, x, n, [])
  | _ -> raise Not_a_pattern

let mk_rule env cfg (r : Rule.partially_typed_rule) =
  let open Rule in
  match cfg.quoting with
  | None -> { r with rhs = normalize cfg r.rhs }
  | Some (module E : Quoting.S) ->
      let sg = Env.get_signature env in
      let r' = E.encode_rule ~sg r in
      let pat' = normalize cfg (Rule.pattern_to_term r'.pat) in
      let pat'' =
        if cfg.unquoting then pattern_of_term (E.unquote_term pat')
        else pattern_of_term pat'
      in
      let rhs' = normalize cfg r'.rhs in
      let rhs'' = if cfg.unquoting then unquote cfg rhs' else rhs' in
      { pat = pat''; rhs = rhs''; ctx = r.ctx; name = r.name }

(*
  let t = Rule.pattern_to_term pat in
  let t' = mk_term cfg t in
  pattern_of_term t' *)

module D = Basic.Debug

let debug_flag = D.register_flag "Dkmeta"

let bmag fmt = "\027[90m" ^^ fmt ^^ "\027[0m%!"

let log fmt = D.debug debug_flag (bmag fmt)

let mk_entry env cfg entry =
  let open Entry in
  let open Rule in
  let sg = Env.get_signature env in
  let md = Env.get_name env in
  match entry with
  | Decl (lc, id, sc, st, ty) ->
      log "[NORMALIZE] %a" Basic.pp_ident id;
      let ty' = mk_term cfg ~env ty in
      if cfg.register_before then Signature.add_declaration sg lc id sc st ty
      else Signature.add_declaration sg lc id sc st ty';
      Decl (lc, id, sc, st, ty')
  | Def (lc, id, sc, opaque, ty, te) -> (
      log "[NORMALIZE] %a" Basic.pp_ident id;
      let cst = Basic.mk_name md id in
      let rule =
        { name = Delta cst; ctx = []; pat = Pattern (lc, cst, []); rhs = te }
      in
      let safe_ty =
        match (cfg.quoting, ty) with
        | Some (module E : Quoting.S), None when E.safe -> Env.infer cfg.env te
        | _, Some ty -> ty
        | _, _ -> Term.mk_Type Basic.dloc
      in
      let safe_ty' = mk_term cfg ~env safe_ty in
      let te' = mk_term cfg ~env te in
      ( if cfg.register_before then
        let _ =
          Signature.add_declaration sg lc id sc (Signature.Definable Free)
            safe_ty
        in
        Signature.add_rules sg
          (List.map Rule.to_rule_infos [ { rule with rhs = te } ])
      else
        let _ =
          Signature.add_declaration sg lc id sc (Signature.Definable Free)
            safe_ty'
        in
        Signature.add_rules sg
          (List.map Rule.to_rule_infos [ { rule with rhs = te' } ]) );
      match ty with
      | None -> Def (lc, id, sc, opaque, None, te')
      | Some _ -> Def (lc, id, sc, opaque, Some safe_ty', te') )
  | Rules (lc, rs) ->
      (* Signature.add_rules !sg (List.map Rule.to_rule_infos rs); *)
      let rs' = List.map (mk_rule env cfg) rs in
      if cfg.register_before then
        Signature.add_rules sg (List.map Rule.to_rule_infos rs)
      else Signature.add_rules sg (List.map Rule.to_rule_infos rs');
      Rules (lc, rs')
  | _ -> entry

let add_rule sg r = Signature.add_rules sg [ Rule.to_rule_infos r ]

(* Several rules might be bound to different constants *)
let add_rules sg rs = List.iter (add_rule sg) rs

let meta_of_rules : ?staged:bool -> Rule.partially_typed_rule list -> cfg -> cfg
    =
 fun ?(staged = false) rules cfg ->
  let rule_names =
    List.map (fun (r : Rule.partially_typed_rule) -> r.Rule.name) rules
  in
  let sg = Env.get_signature cfg.env in
  add_rules sg rules;
  match cfg.meta_rules with
  | None -> { cfg with meta_rules = Some [ RNS.of_list rule_names ] }
  | Some [] -> assert false
  | Some (rs :: l) ->
      if staged then
        { cfg with meta_rules = Some (RNS.of_list rule_names :: rs :: l) }
      else
        {
          cfg with
          meta_rules = Some (RNS.union (RNS.of_list rule_names) rs :: l);
        }

module MetaConfiguration :
  Processor.S with type t = Rule.partially_typed_rule list = struct
  type t = Rule.partially_typed_rule list

  let rules = ref []

  let handle_entry _ = function
    | Entry.Rules (_, rs) -> rules := rs :: !rules
    (* TODO: Handle definitions *)
    | _ -> ()

  let get_data _ =
    let rs = List.flatten !rules in
    rules := [];
    rs
end

type _ Processor.t += MetaRules : Rule.partially_typed_rule list Processor.t

let _ =
  let equal (type a b) :
      a Processor.t * b Processor.t ->
      (a Processor.t, b Processor.t) Processor.Registration.equal option =
    function
    | MetaRules, MetaRules -> Some (Processor.Registration.Refl MetaRules)
    | _ -> None
  in
  Processor.Registration.register_processor MetaRules { equal }
    (module MetaConfiguration)

let meta_of_files ?(cfg = default_config) files =
  Processor.fold_files files
    ~f:(meta_of_rules ~staged:true)
    ~default:cfg MetaRules

let make_meta_processor cfg ~post_processing =
  let module Meta = struct
    type t = unit

    let handle_entry env entry = post_processing env (mk_entry env cfg entry)

    let get_data _ = ()
  end in
  (module Meta : Processor.S with type t = unit)
