open Kernel
open Basic
open Api

(* By default, we don't wont to fail if a symbol is not found in a signature. This is to simplify the use of dkmeta and to add in the signature only the definable meta symbols. *)
let _ = Signature.fail_on_symbol_not_found := false

let meta_files : string list ref = ref []

let add_meta_file s =
  meta_files := s::!meta_files

let meta_mds : Basic.mident list ref = ref []
let add_meta_md md =
  meta_mds := md::!meta_mds

let set_debug_mode opts =
  try  Env.set_debug_mode opts
  with Env.DebugFlagNotRecognized c ->
    if c = 'a' then
      Debug.enable_flag Dkmeta.D_meta
    else
      raise (Env.DebugFlagNotRecognized c)

let _ =
  let run_on_stdin = ref None  in
  let beta = ref true in
  let stats = ref false in
  let switch_beta_off () = beta := false in
  let encoding : (module Dkmeta.Encoding) option ref = ref None in
  let set_encoding enc =
    if enc = "lf" then
      encoding := Some (module Dkmeta.LF)
    else if enc = "prod" then
      encoding := Some (module Dkmeta.PROD)
    else if enc = "app" then
      encoding := Some (module Dkmeta.APP)
    else
      Errors.fail_exit ~file:"" ~code:"-1" (Some dloc) "Unknown encoding '%s'" enc
  in
  let options = Arg.align
    [ ( "-l"
      , Arg.Unit (fun () -> (set_debug_mode "a"))
      , " Active the debug flag specific to dkmeta")
    ; ( "-d"
      , Arg.String set_debug_mode
      , " flags enables debugging for all given flags" )
    ; ( "-q"
      , Arg.Unit (fun () -> Env.set_debug_mode "q")
      , " Quiet mode (equivalent to -d 'q'" )
    ; ("-m"
      , Arg.String add_meta_file
      , " The file containing the meta rules.")
    ; ("--encoding"
      , Arg.String set_encoding
      , " Encoding the Dedukti file.")
    ; ("--switch-beta-off"
      , Arg.Unit switch_beta_off,
      " switch off beta while normalizing terms")
    ; ( "-stdin"
      , Arg.String (fun n -> run_on_stdin := Some(n))
      , " MOD Parses standard input using module name MOD" )
    ; ( "-version"
      , Arg.Unit (fun () -> Format.printf "Meta Dedukti %s@." Dkmeta.version)
      , " Print the version number" )
    ; ( "--stats"
      , Arg.Unit (fun () -> stats := true)
      , " Print statistics" )
    ; ( "-I"
      , Arg.String Files.add_path
      , " DIR Add the directory DIR to the load path" )]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  let cfg = Dkmeta.(
    { default_config with
      beta = !beta;
      encoding = !encoding;
      env = Env.init (Parsers.Parser.input_from_string (Basic.mk_mident "meta") "")
    })
  in
  if !stats then
    begin
      List.iter Stats.run_on_meta_file !meta_files;
      List.iter Stats.run_on_file files
    end
  else
    begin
      let cfg = Dkmeta.meta_of_files ~cfg !meta_files in
      Errors.success "Meta files parsed.@.";
      let post_processing entry = Format.printf "%a@." Pp.Default.print_entry entry in
      let hook_after env exn =
        match exn with
        | None ->Errors.success
                   (Format.asprintf "File '%s' was successfully metaified." (Env.get_filename env))
        | Some(env,lc,exn) -> Env.fail_env_error env lc exn
      in
      Processor.handle_files files ~hook_after (Dkmeta.make_meta_processor cfg post_processing);
      match !run_on_stdin with
      | None   -> ()
      | Some m ->
        let input = Parsers.Parser.input_from_stdin (Basic.mk_mident m) in
        Api.Processor.handle_input input (Dkmeta.make_meta_processor cfg post_processing)
    end

(* let run_on_file cfg file =
 *   let import md = Env.import Basic.dloc md in
 *   let input = open_in file in
 *   let md = E.init file in
 *   List.iter import !meta_mds;
 *   let entries = Parser.Parse_channel.parse md input in
 *   Dkmeta.init file;
 *   let entries' = List.map (Dkmeta.mk_entry cfg md) entries in
 *   List.iter (Format.printf "%a@." Printer.print_entry) entries';
 *   Errors.success "File '%s' was successfully metaified." file;
 *   close_in input *)
