(* TODO:
   - split the project in several modules
   - Make an API so that it can be use as a library
   - Use cmdliner
*)

open Basic


let run_on_file is_meta file =
  let import md =
    match Env.import Basic.dloc md with
    | OK _ -> ()
    | Err err -> Errors.fail_signature_error err
  in
  let prelude md =
    if not is_meta then
      List.iter import (Config.meta_mds ())
    else
      Config.add_meta_md md
  in
  let postlude _ =
  if is_meta then
    Errors.success "File '%s' was successfully metaified." file
  else
    Errors.success "File '%s' was successfully checked." file;
  in
  let input = open_in file in
  let md = Env.init file in
  prelude md;
  Parser.handle_channel md (Meta.mk_entry md is_meta) input;
  postlude md;
  close_in input


let _ =
  let open Config in
  let run_on_stdin = ref None  in
  let options = Arg.align
    [ ( "-d"
      , Arg.String Debug.set_debug_mode
      , "flags enables debugging for all given flags" )
    ; ( "-v"
      , Arg.Unit (fun () -> Debug.set_debug_mode "w")
      , " Verbose mode (equivalent to -d 'w')" )
    ; ( "-q"
      , Arg.Unit (fun () -> Debug.set_debug_mode "q")
      , " Quiet mode (equivalent to -d 'q'" )
    ; ("-m"
      , Arg.String add_meta_file
      , "The file containing the meta rules. It has to be typed checked" )
    ; ("--encode"
      , Arg.String set_encoding
      , "Encoding the Dedukti file. Only LF encoding is supported right now")
    ; ("--switch-beta-off"
      , Arg.Unit switch_beta_off,
      "switch off beta while normalizing terms")
    ; ("--output"
      , Arg.String set_output_file
      , "Output file")
    ; ( "-stdin"
      , Arg.String (fun n -> run_on_stdin := Some(n))
      , "MOD Parses standard input using module name MOD" )
    ; ( "-version"
      , Arg.Unit (fun () -> Format.printf "Meta Dedukti %s@." Config.version)
      , " Print the version number" )
    ; ( "-I"
      , Arg.String Basic.add_path
      , "DIR Add the directory DIR to the load path" )
    ; ( "-errors-in-snf"
      , Arg.Set Errors.errors_in_snf
      , " Normalize the types in error messages" )
    ; ( "-nl"
      , Arg.Set Rule.allow_non_linear
      , " Allow non left-linear rewriting rules" )]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  try
    List.iter (run_on_file true) (Config.meta_files ());
    List.iter (run_on_file false) files;
    match !run_on_stdin with
    | None   -> ()
    | Some m ->
      let md = Env.init m in
      Parser.handle_channel md (Meta.mk_entry md false) stdin;
      Errors.success "Standard input was successfully checked.\n"
  with
  | Parser.Parse_error(loc,msg) -> Format.eprintf "Parse error at (%a): %s@." pp_loc loc msg; exit 1
  | Sys_error err        -> Format.eprintf "ERROR %s.@." err; exit 1
  | Exit                 -> exit 3


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
