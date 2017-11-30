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
end

module Config:CONFIG =
struct

  let _meta_file = ref None
  let _module_files = ref []
  let _beta = ref true
  let _output_file = ref None

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

  let print_version =
    Version.print_version
end


module Meta =
struct
  open Config
  open Basic

  let normalize term =
    let filter name =
      let meta_file = mk_mident (match meta_file () with None -> "" | Some s -> s) in
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

  let mk_prelude lc id =
    Env.init id;
    match meta_file () with
    | None -> debug 0 "No meta file given"
    | Some s ->
      let s = mk_mident s in
      match Env.import lc s with
      | OK () -> ()
      | Err err -> failwith "cannot import the meta file, has it been checked?"

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
   "--switch-beta-off", Arg.Unit switch_beta_off, "switch off beta while normalizing terms";
   "--module", Arg.String add_module_file, "Normalize against constants defined in an other module";
   "--non-linear", Arg.Unit use_non_linear, "Allows to have non-linear meta-rules";
   "--output", Arg.String set_output_file, "Output file";
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
