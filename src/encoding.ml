module type T =
sig
  open Basic
  open Term

  type t = term

  val md : mident

  val entries : unit -> Entry.entry list

  val encode_term : ?ctx:typed_context -> term -> t

  val decode_term : t -> term

end

module LF:T =
struct

  open Basic
  open Term

  type t = term

  let md = mk_mident "lf"

  let entries () =
    let mk_decl id = Entry.Decl(dloc,mk_ident id, Signature.Definable, mk_Type dloc) in
    List.map mk_decl ["ty"; "var";"sym";"lam";"app";"prod"]

  let name_of str = mk_name md (mk_ident str)

  let const_of str = mk_Const dloc (name_of str)

  let rec encode_term ?(ctx=[]) t =
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
    let mty' = match mty with None -> None | Some ty -> Some (encode_term ty) in
    mk_App (const_of "lam") (mk_Lam dloc x mty' (encode_term te)) []

  and encode_App f a args =
    mk_App (const_of "app") (encode_term f) (List.map encode_term (a::args))

  and encode_Pi lc x a b =
    mk_App (const_of "prod") (mk_Lam dloc x (Some (encode_term a)) (encode_term b)) []

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

module LFP:T =
struct

  open Basic
  open Term

  type t = term

  let md = mk_mident "lf"

  let name_of str = mk_name md (mk_ident str)

  let const_of str = mk_Const dloc (name_of str)

  let entries () =
    let file = "encodings/lf.dk" in
    let ic = open_in file in
    let md = Env.init file in
    Parser.parse_channel md ic

  let whnf x = Reduction.(Env.unsafe_reduction ~red:{default_cfg with target = Whnf} x)

  let rec encode_term ?(ctx=[]) t =
    match t with
    | Kind -> assert false
    | Type(lc) -> encode_type lc
    | DB(lc,x,n) -> encode_DB lc ctx x n
    | Const(lc, name) -> encode_Const lc ctx name
    | Lam(lc,x,mty,te) -> encode_Lam lc ctx x mty te
    | App(f,a,args) ->
      let f' = encode_term ~ctx f in
      let tyf = Env.infer ~ctx f in
      let b =
        match whnf tyf with
        | Pi(_,_,_,b) -> b
        | _ -> Format.eprintf "%a@." Pp.print_term tyf; assert false
      in
      let fa' = encode_App ctx tyf f' a in
      begin
        match args with
        | [] -> fa'
        | x::t ->
          List.fold_left (fun f' arg ->
              encode_App ctx (Subst.subst b a) f' arg ) fa' args
      end
    | Pi(lc,x,a,b) -> encode_Pi lc ctx x a b

  and encode_type lc =
    const_of "ty"

  and encode_DB lc ctx x n =
    let ty = Env.infer ~ctx (mk_DB lc x n) in
    let ty' = encode_term ~ctx ty in
    mk_App (const_of "var") ty' [mk_DB lc x n]

  and encode_Const lc ctx name =
    let ty = Env.infer (mk_Const lc name) in
    let ty' = encode_term ~ctx ty in
    mk_App (const_of "sym") ty' [(mk_Const lc name)]

  and encode_Lam lc ctx x mty te =
    let tya = match mty with None -> assert false | Some ty -> ty in
    let tya' = encode_term ~ctx  tya in
    let ctx' = (lc,x,tya)::ctx in
    let te' = encode_term ~ctx:ctx' te in
    let tyb = Env.infer ~ctx:ctx' te in
    let tyb' = mk_Lam lc x (Some (mk_App (const_of "eta") tya' [])) (encode_term ~ctx:ctx' tyb) in
    mk_App (const_of "lam") tya' [tyb';mk_Lam lc x (Some (mk_App (const_of "eta") tya' [])) te']

  and encode_App ctx tyf f' a =
    let a' = encode_term ~ctx a in
    let x,tya,tyb =
      match whnf tyf with
      | Pi(_,x,a,b) -> x,a,b
      | _ -> assert false
    in
    let tya' = encode_term ~ctx tya in
    let ctx' = (dloc,x,tya)::ctx in
    let tyb' = mk_Lam dloc x (Some (mk_App (const_of "eta") tya' [])) (encode_term ~ctx:ctx' tyb) in
    mk_App (const_of "app") tya' [tyb'; f'; a']

  and encode_Pi lc ctx x a b =
    let a' = encode_term ~ctx a in
    let ctx' = (lc,x,a)::ctx in
    mk_App (const_of "prod") a'
      [mk_Lam dloc x (Some (mk_App (const_of "eta") a' [])) (encode_term ~ctx:ctx' b)]

  let encode_entry e =
    let open Entry in
    match e with
    | Decl(lc,id,st,te) ->
      Decl(lc,id, st, encode_term ~ctx:[] te)
    | Def(lc,id,opq,mty,te) ->
      let ty = match mty with None -> assert false | Some ty -> ty in
      let mty' = mk_App (const_of "eta") (encode_term ~ctx:[] ty) [] in
      Def(lc,id, opq, Some mty',encode_term ~ctx:[] te)
    | _ -> failwith "commands are not handled right now"

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
      if name_eq name (name_of "eta") then
        match args with
        | [] -> decode_term a
        | x::args -> mk_App a x args
      else if name_eq name (name_of "prod") then
        match args with
        | [Lam(_,x,Some a, b)] -> mk_Pi dloc x (decode_term a) (decode_term b)
        | _ -> assert false
      else if name_eq name (name_of "sym") then
        match args with
        | [cst] -> cst
        | _ -> assert false
      else if name_eq name (name_of "var") then
        match args with
        | [cst] -> cst
        | _ -> assert false
      else if name_eq name (name_of "app") then
        match args with
        | [_;f;a] -> mk_App2 (decode_term f) [decode_term a]
        | _ -> assert false
      else if name_eq name (name_of "lam") then
        match args with
        | [_;f;] -> decode_term f
        | _ -> assert false
      else
        mk_App (decode_term f) (decode_term a) (List.map decode_term args)

    | _ -> decode_App (decode_term f) (decode_term a) (List.map decode_term args)

  and decode_Pi lc x a b = assert false
(*
  let rec decode_entry e =
    let open Entry in
    match e with
    | Decl(lc,id,st,te) ->
      Decl(lc,id, st, decode_term te)
    | Def(lc,id,opq, mty,te) ->
      let mty' = match mty with None -> None | Some ty -> Some (decode_term ty) in
      Def(lc,id, opq, mty', decode_term te)
    | _ -> failwith "commands are not handled right now"

  let entries path =
    let file = path in
    let oc = open_in file in
    Parser.parse_channel (Basic.mk_mident "lf") oc *)
end
