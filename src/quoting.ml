open Kernel
open Api
open Parsers

module type S = sig
  val md : Basic.mident

  val entries : unit -> Entry.entry list

  val safe : bool

  val signature : Signature.t

  val quote_term :
    ?sg:Signature.t -> ?ctx:Term.typed_context -> Term.term -> Term.term

  val unquote_term : Term.term -> Term.term

  val encode_rule :
    ?sg:Signature.t -> Rule.partially_typed_rule -> Rule.partially_typed_rule
end

module PROD = struct
  open Basic
  open Term

  let md = mk_mident "prod"

  let entries () =
    let mk_decl id =
      Entry.Decl
        ( dloc,
          mk_ident id,
          Signature.Public,
          Signature.Definable Free,
          mk_Type dloc )
    in
    List.map mk_decl [ "ty"; "prod" ]

  let signature =
    let sg = Signature.make md Files.find_object_file in
    let mk_decl id =
      Signature.add_declaration sg dloc (mk_ident id) Signature.Public
        (Signature.Definable Free) (mk_Type dloc)
    in
    List.iter mk_decl [ "ty"; "prod" ];
    sg

  let safe = false

  let name_of str = mk_name md (mk_ident str)

  let const_of str = mk_Const dloc (name_of str)

  let rec quote_term t =
    match t with
    | Kind -> assert false
    | Type lc -> encode_type lc
    | DB (lc, x, n) -> encode_DB lc x n
    | Const (lc, name) -> encode_Const lc name
    | Lam (lc, x, mty, te) -> encode_Lam lc x mty te
    | App (f, a, args) -> encode_App f a args
    | Pi (lc, x, a, b) -> encode_Pi lc x a b

  and encode_type lc = mk_Const lc (name_of "ty")

  and encode_DB lc x n = mk_DB lc x n

  and encode_Const _ name = mk_Const dloc name

  and encode_Lam _ x mty te =
    let mty' =
      match mty with None -> None | Some ty -> Some (quote_term ty)
    in
    mk_Lam dloc x mty' (quote_term te)

  and encode_App f a args =
    mk_App2 (quote_term f) (List.map quote_term (a :: args))

  and encode_Pi _ x a b =
    mk_App (const_of "prod") (quote_term a)
      [ mk_Lam dloc x (Some (quote_term a)) (quote_term b) ]

  (* Using typed context here does not make sense *)
  let encode_pattern pattern : Rule.pattern = pattern

  let encode_rule (r : 'a Rule.rule) =
    let open Rule in
    { r with pat = encode_pattern r.pat; rhs = quote_term r.rhs }

  let quote_term ?sg:_ ?ctx:_ t = quote_term t

  let encode_rule ?sg:_ r = encode_rule r

  let rec unquote_term t =
    match t with
    | Kind -> assert false
    | Pi _ -> assert false
    | App (f, a, args) -> decode_App f a args
    | Lam (lc, x, mty, te) -> decode_Lam lc x mty te
    | Const (lc, name) -> decode_Const lc name
    | Type _ | DB _ -> t

  and decode_Const lc name =
    if name_eq name (name_of "ty") then mk_Type lc else mk_Const lc name

  and decode_Lam lc x mty te =
    let mty' =
      match mty with None -> None | Some mty -> Some (unquote_term mty)
    in
    mk_Lam lc x mty' (unquote_term te)

  and decode_App f a args =
    match f with
    | Const (_, name) ->
        if name_eq name (name_of "prod") then
          match args with
          | [ Lam (_, x, Some a, b) ] ->
              mk_Pi dloc x (unquote_term a) (unquote_term b)
          | _ -> assert false
        else
          mk_App (unquote_term f) (unquote_term a) (List.map unquote_term args)
    | _ -> mk_App (unquote_term f) (unquote_term a) (List.map unquote_term args)
end

module LF = struct
  open Basic
  open Term

  let md = mk_mident "lf"

  let entries () =
    let mk_decl id =
      Entry.Decl
        ( dloc,
          mk_ident id,
          Signature.Public,
          Signature.Definable Free,
          mk_Type dloc )
    in
    List.map mk_decl [ "ty"; "var"; "sym"; "lam"; "app"; "prod" ]

  let signature =
    let sg = Signature.make md Files.find_object_file in
    let mk_decl id =
      Signature.add_declaration sg dloc (mk_ident id) Signature.Public
        (Signature.Definable Free) (mk_Type dloc)
    in
    List.iter mk_decl [ "ty"; "var"; "sym"; "lam"; "app"; "prod" ];
    sg

  let safe = false

  let name_of str = mk_name md (mk_ident str)

  let const_of str = mk_Const dloc (name_of str)

  let rec quote_term t =
    match t with
    | Kind -> assert false
    | Type lc -> encode_type lc
    | DB (lc, x, n) -> encode_DB lc x n
    | Const (lc, name) -> encode_Const lc name
    | Lam (lc, x, mty, te) -> encode_Lam lc x mty te
    | App (f, a, args) -> encode_App f a args
    | Pi (lc, x, a, b) -> encode_Pi lc x a b

  and encode_type _ = const_of "ty"

  and encode_DB lc x n = mk_App (const_of "var") (mk_DB lc x n) []

  and encode_Const _ name = mk_App (const_of "sym") (mk_Const dloc name) []

  and encode_Lam _ x mty te =
    let mty' =
      match mty with None -> None | Some ty -> Some (quote_term ty)
    in
    mk_App (const_of "lam") (mk_Lam dloc x mty' (quote_term te)) []

  and encode_App f a args =
    let rec encode_app2 a args =
      match (a, args) with
      | _, [] -> assert false
      | a, [ x ] -> mk_App (const_of "app") a [ quote_term x ]
      | a, x :: l -> encode_app2 (mk_App (const_of "app") a [ quote_term x ]) l
    in
    encode_app2 (quote_term f) (a :: args)

  and encode_Pi _ x a b =
    mk_App (const_of "prod") (quote_term a)
      [ mk_Lam dloc x None (quote_term b) ]

  (* Using typed context here does not make sense *)
  let rec encode_pattern pattern : Rule.pattern =
    let open Rule in
    match pattern with
    | Var (lc, id, n, ps) -> Var (lc, id, n, List.map encode_pattern ps)
    | Brackets term -> Brackets (quote_term term)
    | Lambda (lc, id, p) ->
        Pattern (lc, name_of "lam", [ Lambda (lc, id, encode_pattern p) ])
    | Pattern (lc, n, []) -> Pattern (lc, name_of "sym", [ Pattern (lc, n, []) ])
    | Pattern (lc, n, ps) ->
        Pattern
          ( lc,
            name_of "app",
            Pattern (lc, name_of "sym", [ Pattern (lc, n, []) ])
            :: List.map encode_pattern ps )

  let encode_rule (r : 'a Rule.rule) =
    let open Rule in
    { r with pat = encode_pattern r.pat; rhs = quote_term r.rhs }

  let quote_term ?sg:_ ?ctx:_ t = quote_term t

  let encode_rule ?sg:_ r = encode_rule r

  let rec unquote_term t =
    match t with
    | Kind -> assert false
    | Type _ -> assert false
    | DB (lc, x, n) -> decode_DB lc x n
    | Const (lc, name) -> decode_Const lc name
    | Lam (lc, x, mty, te) -> decode_Lam lc x mty te
    | App (f, a, args) -> decode_App f a args
    | Pi (lc, x, a, b) -> decode_Pi lc x a b

  and decode_DB lc x n = mk_DB lc x n

  and decode_Const lc name =
    if name_eq name (name_of "ty") then mk_Type dloc else mk_Const lc name

  and decode_Lam lc x mty te =
    let mty' =
      match mty with None -> None | Some mty -> Some (unquote_term mty)
    in
    mk_Lam lc x mty' (unquote_term te)

  and decode_App f a args =
    match f with
    | Const (_, name) ->
        if name_eq name (name_of "prod") then
          match (a, args) with
          | a, [ Lam (_, x, None, b) ] ->
              mk_Pi dloc x (unquote_term a) (unquote_term b)
          | _ -> assert false
        else if name_eq name (name_of "sym") then unquote_term a
        else if name_eq name (name_of "var") then unquote_term a
        else if name_eq name (name_of "app") then
          mk_App2 (unquote_term a) (List.map unquote_term args)
        else if name_eq name (name_of "lam") then unquote_term a
        else
          mk_App (unquote_term f) (unquote_term a) (List.map unquote_term args)
    | _ ->
        decode_App (unquote_term f) (unquote_term a)
          (List.map unquote_term args)

  and decode_Pi _ _ _ _ = assert false
end

module APP = struct
  open Basic
  open Term

  let md = mk_mident "ltyped"

  let entries () =
    let mk_decl id =
      Entry.Decl
        ( dloc,
          mk_ident id,
          Signature.Public,
          Signature.Definable Free,
          mk_Type dloc )
    in
    List.map mk_decl [ "ty"; "var"; "sym"; "lam"; "app"; "prod" ]

  let signature =
    let sg = Signature.make md Files.find_object_file in
    let mk_decl id =
      Signature.add_declaration sg dloc (mk_ident id) Signature.Public
        (Signature.Definable Free) (mk_Type dloc)
    in
    List.iter mk_decl [ "ty"; "var"; "sym"; "lam"; "app"; "prod" ];
    sg

  let safe = true

  let name_of str = mk_name md (mk_ident str)

  let const_of str = mk_Const dloc (name_of str)

  let rec quote_term sg ctx t =
    match t with
    | Kind -> assert false
    | Type lc -> encode_type sg ctx lc
    | DB (lc, x, n) -> encode_DB sg ctx lc x n
    | Const (lc, name) -> encode_Const sg ctx lc name
    | Lam (lc, x, Some ty, te) -> encode_Lam sg ctx lc x ty te
    | Lam _ -> assert false
    | App (f, a, args) -> encode_App sg ctx f a args
    | Pi (lc, x, a, b) -> encode_Pi sg ctx lc x a b

  and encode_type _ _ _ = const_of "ty"

  and encode_DB _ _ lc x n = mk_App (const_of "var") (mk_DB lc x n) []

  and encode_Const _ _ _ name = mk_App (const_of "sym") (mk_Const dloc name) []

  and encode_Lam sg ctx lc x ty te =
    let ctx' = (lc, x, ty) :: ctx in
    let tyf = Typing.Default.infer sg ctx (mk_Lam lc x (Some ty) te) in
    let tyf' = PROD.quote_term tyf in
    mk_App (const_of "lam") tyf'
      [ mk_Lam dloc x (Some (quote_term sg ctx ty)) (quote_term sg ctx' te) ]

  and encode_App sg ctx f a args = encode_app2 sg ctx f (a :: args)

  and encode_app2 sg ctx f args =
    let aux f f' a =
      let tyf = Typing.Default.infer sg ctx f in
      let tyf' = PROD.quote_term tyf in
      ( Term.mk_App2 f [ a ],
        mk_App (const_of "app") tyf' [ f'; quote_term sg ctx a ] )
    in
    snd
    @@ List.fold_left
         (fun (f, f') a -> aux f f' a)
         (f, quote_term sg ctx f)
         args

  and encode_Pi sg ctx lc x a b =
    let ctx' = (lc, x, a) :: ctx in
    mk_App (const_of "prod")
      (mk_Lam dloc x (Some (quote_term sg ctx a)) (quote_term sg ctx' b))
      []

  let rec encode_pattern sg ctx pattern =
    let open Rule in
    let dummy = Var (Basic.dloc, mk_ident "_", 0, []) in
    let mk_pat_app l r =
      Pattern (Basic.dloc, name_of "app", [ dummy; l; encode_pattern sg ctx r ])
    in
    match pattern with
    | Var (lc, id, n, ps) -> Var (lc, id, n, List.map (encode_pattern sg ctx) ps)
    | Brackets term -> Brackets (quote_term sg ctx term)
    | Lambda (lc, id, p) ->
        Pattern
          ( lc,
            name_of "lam",
            [ dummy; Lambda (lc, id, encode_pattern sg ctx p) ] )
    | Pattern (lc, n, []) -> Pattern (lc, name_of "sym", [ Pattern (lc, n, []) ])
    | Pattern (lc, n, [ a ]) ->
        Pattern
          ( lc,
            name_of "app",
            [
              dummy;
              Pattern (lc, name_of "sym", [ Pattern (lc, n, []) ]);
              encode_pattern sg ctx a;
            ] )
    | Pattern (lc, n, a :: l) ->
        List.fold_left
          (fun p arg -> mk_pat_app p arg)
          (encode_pattern sg ctx (Pattern (lc, n, [])))
          (a :: l)

  let encode_rule sg r =
    let _, r' = Typing.Default.check_rule sg r in
    let open Rule in
    {
      r with
      pat = encode_pattern sg r'.ctx r.pat;
      rhs = quote_term sg r'.ctx r.rhs;
    }

  let fake_sig () = Signature.make (Basic.mk_mident "") Files.find_object_file

  let quote_term ?(sg = fake_sig ()) ?(ctx = []) t = quote_term sg ctx t

  let encode_rule ?(sg = fake_sig ()) r = encode_rule sg r

  let rec unquote_term t =
    match t with
    | Kind -> assert false
    | Type _ -> assert false
    | DB (lc, x, n) -> decode_DB lc x n
    | Const (lc, name) -> decode_Const lc name
    | Lam (lc, x, mty, te) -> decode_Lam lc x mty te
    | App (f, a, args) -> decode_App f a args
    | Pi (lc, x, a, b) -> decode_Pi lc x a b

  and decode_DB lc x n = mk_DB lc x n

  and decode_Const lc name =
    if name_eq name (name_of "ty") then mk_Type dloc else mk_Const lc name

  and decode_Lam lc x mty te =
    let mty' =
      match mty with None -> None | Some mty -> Some (unquote_term mty)
    in
    mk_Lam lc x mty' (unquote_term te)

  and decode_App f a args =
    match f with
    | Const (_, name) ->
        if name_eq name (name_of "prod") then
          match a with
          | Lam (_, x, Some a, b) ->
              mk_Pi dloc x (unquote_term a) (unquote_term b)
          | _ -> assert false
        else if name_eq name (name_of "sym") then unquote_term a
        else if name_eq name (name_of "var") then unquote_term a
        else if name_eq name (name_of "app") then
          match args with
          | [ f; a ] -> Term.mk_App2 (unquote_term f) [ unquote_term a ]
          | _ -> assert false
        else if name_eq name (name_of "lam") then
          match args with [ a ] -> unquote_term a | _ -> assert false
        else
          mk_App (unquote_term f) (unquote_term a) (List.map unquote_term args)
    | _ -> mk_App (unquote_term f) (unquote_term a) (List.map unquote_term args)

  and decode_Pi _ _ _ _ = assert false
end
