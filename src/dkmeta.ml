(* TODO:
   - split the project in several modules
   - Make an API so that it can be use as a library
   - Use cmdliner
*)




module Meta =
struct
  open Config
  open Encoding
  open Basic

  let normalize_meta meta_file term =

    let filter name =
      match name with
      | Rule.Delta(cst) -> List.mem (md cst) (List.map mk_mident @@ module_files ())
      | Rule.Gamma(_,cst) -> mident_eq (md cst) meta_file
    in
    let red = let open Reduction in
      {
        beta = Config.beta ();
        select = Some filter;
        strategy = Snf;
        nb_steps = None;
      }
    in
    match Env.reduction ~red:red term with
    | OK t -> t
    | Err err -> failwith "normalization error"

  let normalize_encoding term =
    LF.encode_term term

  let normalize term =
    match meta_file () with
    | None -> normalize_encoding term
    | Some mf -> normalize_meta (mk_mident mf) term


  let prefix md = "meta_"^(string_of_mident md)
  let mk_prelude lc file =
    (* To avoid circular dependencies if the meta files depends on this module *)
    let file' = prefix file in
    ignore(Env.init file');
    match meta_file () with
    | None -> Debug.debug Debug.d_notice "No meta file given"
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
(*
module P = Parser.Make(Meta)

let args =
  let open Config in
  ["--meta-file", Arg.String set_meta_file, "The file containing the meta rules. It has to be typed checked";
   "--encode", Arg.String set_encoding, "Encoding the Dedukti file. Only LF encoding is supported right now";
   "--switch-beta-off", Arg.Unit switch_beta_off, "switch off beta while normalizing terms";
   "--module", Arg.String add_module_file, "Normalize against constants defined in an other module";
   "--non-linear", Arg.Unit use_non_linear, "Allows to have non-linear meta-rules";
   "--output", Arg.String set_output_file, "Output file";
   "-I"      , Arg.String Basic.add_path         , "Add a directory to load path";
   "-nl"     , Arg.Set    Rule.allow_non_linear  , "Allow non left-linear rewrite rules";
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
        *)
