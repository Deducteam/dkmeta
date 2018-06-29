module type T =
sig
  open Basic
  open Term

  type t = term
  type ctx = loc * ident * term
  type db = int

  val md : mident

  val signature : unit -> Signature.t

  val encode_term : term -> t

  val decode_term : t -> term

end

module LF:T =
struct

  open Basic
  open Term

  type t = term
  type ctx = loc * ident * term
  type db = int

  let md = mk_mident "lf"

  let name_of str = mk_name md (mk_ident str)

  let const_of str = mk_Const dloc (name_of str)

  let signature () =
    begin
      match Env.import dloc md with
      | OK () -> ()
      | Err e -> Errors.fail_signature_error e
    end;
    let sg = Signature.make "fake" in Signature.import sg dloc md; sg

  let rec encode_term t =
    match t with
    | Kind -> assert false
    | Type(lc) -> encode_type lc
    | DB(lc,x,n) -> encode_DB lc x n
    | Const(lc, name) -> encode_Const lc name
    | Lam(lc,x,mty,te) -> encode_Lam lc x mty te
    | App(f,a,args) -> encode_App f a args
    | Pi(lc,x,a,b) -> encode_Pi lc x a b

  and encode_type lc =
    const_of "ty"

  and encode_DB lc x n = mk_DB lc x n

  and encode_Const lc name = mk_Const lc name

  and encode_Lam lc x mty te = mk_Lam lc x mty te

  and encode_App f a args = mk_App f a args

  and encode_Pi lc x a b =
    let a' = encode_term a in
    mk_App (const_of "prod") a' [mk_Lam dloc x (Some a') (encode_term b)]

  let encode_entry e =
    let open Entry in
    let sg = signature () in
    match e with
    | Decl(lc,id,st,te) ->
      Decl(lc,id, st, encode_term te)
    | Def(lc,id,opq,mty,te) ->
      let ty = match mty with None -> Typing.inference sg te | Some ty -> ty in
      let mty' = mk_App (const_of "eta") (encode_term ty) [] in
      Def(lc,id, opq, Some mty',encode_term te)
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
      else
        mk_App (decode_term f) (decode_term a) (List.map decode_term args)

    | _ -> decode_App (decode_term f) (decode_term a) (List.map decode_term args)

  and decode_Pi lc x a b = assert false

  let rec decode_entry e =
    let open Entry in
    match e with
    | Decl(lc,id,st,te) ->
      Decl(lc,id, st, decode_term te)
    | Def(lc,id,opq, mty,te) ->
      let mty' = match mty with None -> None | Some ty -> Some (decode_term ty) in
      Def(lc,id, opq, mty', decode_term te)
    | _ -> failwith "commands are not handled right now"
end
