(* TODO:
   - split the project in several modules
   - Make an API so that it can be use as a library
   - Use cmdliner
*)

let version = "0.1"

type entry =
  | Declaration of Basic.loc * Basic.ident * Signature.staticity * Term.term
  | Definition of Basic.loc * Basic.ident * Term.term option * Term.term
  | Opaque of Basic.loc * Basic.ident * Term.term option * Term.term
  | Cmd of Cmd.command

module type CONFIG =
sig

  val set_meta_file : string -> unit
  val meta_file : unit -> string option

  val add_module_file : string -> unit
  val module_files : unit -> string list

  val switch_beta_off : unit -> unit
  val beta : unit -> bool

  val use_non_linear : unit -> unit

  val set_output_file : string -> unit
  val output_file : unit -> string option

  val print_version : unit -> unit

  val set_encoding : string -> unit
end

module Config:CONFIG =
struct

  let _meta_file = ref None
  let _module_files = ref []
  let _beta = ref true
  let _output_file = ref None
  let _encoding = ref None


  let set_meta_file s =
    _meta_file := Some s

  let meta_file () = !_meta_file

  let add_module_file s =
    _module_files := s::!_module_files

  let module_files () = !_module_files

  let switch_beta_off () =
    _beta := false

  let beta () = !_beta

  let use_non_linear () =
    Rule.allow_non_linear := true

  let set_output_file s =
    _output_file := Some s

  let output_file () =
    !_output_file

  let print_version () = Format.printf "%s@." version

  let set_encoding s =
    _encoding := Some s
end

module type ENCODING =
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

module LF:ENCODING =
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
    let sg = Signature.make (mk_mident "fake") in Signature.import sg dloc md; sg

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
    let sg = signature () in
    match e with
    | Declaration(lc,id,st,te) ->
      Declaration(lc,id, st, encode_term te)
    | Definition(lc,id,mty,te) ->
      let ty = match mty with None -> Typing.inference sg te | Some ty -> ty in
      let mty' = mk_App (const_of "eta") (encode_term ty) [] in
      Definition(lc,id, Some mty',encode_term te)
    | Opaque(lc,id,mty,te) ->
      let ty = match mty with None -> Typing.inference sg te | Some ty -> ty in
      let mty' = mk_App (const_of "eta") (encode_term ty) [] in
      Opaque(lc,id, Some mty',encode_term te)
    | Cmd(cmd) -> failwith "commands are not handled right now"

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
    match e with
    | Declaration(lc,id,st,te) ->
      Declaration(lc,id, st, decode_term te)
    | Definition(lc,id,mty,te) ->
      let mty' = match mty with None -> None | Some ty -> Some (decode_term ty) in
      Definition(lc,id,mty', decode_term te)
    | Opaque(lc,id,mty,te) ->
      let mty' = match mty with None -> None | Some ty -> Some (decode_term ty) in
      Opaque(lc,id,mty', decode_term te)
    | Cmd(cmd) -> failwith "commands are not handled right now"
end

module Meta =
struct
  open Config
  open Basic

  let normalize_meta meta_file term =

    let filter name =
      match name with
      | Rule.Delta(cst) -> List.mem (md cst) (List.map mk_mident @@ module_files ())
      | Rule.Gamma(_,cst) -> mident_eq (md cst) meta_file
    in
    let red =
      {
        Reduction.beta = beta ();
        Reduction.select = Some filter
      }
    in
    match Env.reduction ~red:red Reduction.Snf term with
    | OK t -> t
    | Err err -> failwith "normalization error"

  let normalize_encoding term =
    LF.encode_term term

  let normalize term =
    match meta_file () with
    | None -> normalize_encoding term
    | Some mf -> normalize_meta (mk_mident mf) term


  let prefix md = mk_mident ("meta_"^(string_of_mident md))
  let mk_prelude lc md =
    (* To avoid circular dependencies if the meta files depends on this module *)
    let md' = prefix md in
    Env.init md';
    Format.printf "#NAME %a.@.@." pp_mident md;
    match meta_file () with
    | None -> debug 1 "No meta file given"
    | Some s ->
      let s = mk_mident s in
      match Env.import lc s with
      | OK () -> ()
      | Err err -> Format.printf "cannot import the meta file@."; Errors.fail_signature_error err

  let mk_declaration lc id st ty =
    let ty' = normalize ty in
    let kw = match st with
    | Signature.Static -> ""
    | Signature.Definable -> "def "
    in
    Format.printf "@[<2>%s%a :@ %a.@]@.@." kw pp_ident id Pp.print_term ty'

  let mk_definition lc id ty_opt te =
    let ty_opt' =
      match ty_opt with
      | None -> None
      | Some t -> Some (normalize t)
    in
    let te' = normalize te in
    match ty_opt' with
    | None ->
      Format.printf "@[<hv2>def %a :=@ %a.@]@.@." pp_ident id Pp.print_term te'
    | Some ty ->
      Format.printf "@[<hv2>def %a :@ %a@ :=@ %a.@]@.@."
        pp_ident id Pp.print_term ty Pp.print_term te'


  let mk_opaque lc id ty_opt te =
    let ty_opt' =
      match ty_opt with
      | None -> None
      | Some t -> Some (normalize t)
    in
    let te' = normalize te in
    match ty_opt' with
    | None ->
      Format.printf "@[<hv2>thm %a :=@ %a.@]@.@." pp_ident id Pp.print_term te'
    | Some ty ->
      Format.printf "@[<hv2>thm %a :@ %a@ :=@ %a.@]@.@."
        pp_ident id Pp.print_term ty Pp.print_term te'

  let mk_rules lst =
    let normalize_rule (r : Rule.untyped_rule) : Rule.untyped_rule =
      let open Rule in
      {r with rhs = normalize r.rhs}
    in
    let lst' = List.map normalize_rule lst in
    let print_rule r =
      Format.printf "@[%a@].@.@." Pp.print_untyped_rule r
    in
    List.iter print_rule lst'


  let mk_command lc = failwith "command are not supported right now"
  let mk_ending () = ()
end

module P = Parser.Make(Meta)

let args =
  let open Config in
  ["--meta-file", Arg.String set_meta_file, "The file containing the meta rules. It has to be typed checked";
   "--encode", Arg.String set_encoding, "Encoding the Dedukti file. Only LF encoding is supported right now";
   "--switch-beta-off", Arg.Unit switch_beta_off, "switch off beta while normalizing terms";
   "--module", Arg.String add_module_file, "Normalize against constants defined in an other module";
   "--non-linear", Arg.Unit use_non_linear, "Allows to have non-linear meta-rules";
   "--output", Arg.String set_output_file, "Output file";
   ("-I"      , Arg.String Basic.add_path         , "Add a directory to load path");
   "-version", Arg.Unit print_version, "print the version number"]

let parse lexbuf =
  try
    P.prelude Lexer.token lexbuf ;
    while true do
      P.line Lexer.token lexbuf
    done;
  with
  | Lexer.EndOfFile -> ()
  | P.Error       -> Errors.fail (Lexer.get_loc lexbuf)
                         "Unexpected token '%s'." (Lexing.lexeme lexbuf)

let run_on_file file =
  let input = open_in file in
  parse (Lexing.from_channel input);
  close_in input

let usage_msg () = "Usage: "^ Sys.argv.(0) ^" [options] files"


let _ = Arg.parse args run_on_file (usage_msg ())
