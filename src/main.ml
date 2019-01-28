open Basic
open Dkmeta

let _ = Signature.unsafe := true

let meta_files : string list ref = ref []
let add_meta_file s =
  meta_files := s::!meta_files

let meta_mds : Basic.mident list ref = ref []
let add_meta_md md =
  meta_mds := md::!meta_mds

let run_on_file cfg file =
  let import md = Env.import Basic.dloc md in
  let input = open_in file in
  let md = Env.init file in
  List.iter import !meta_mds;
  let entries = Parser.Parse_channel.parse md input in
  let entries' = List.map (Dkmeta.mk_entry cfg md) entries in
  List.iter (Format.printf "%a@." Pp.print_entry) entries';
  Errors.success "File '%s' was successfully metaified." file;
  close_in input

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
  let switch_beta_off () = beta := false in
  let encoding : (module Dkmeta.Encoding) option ref = ref None in
  let set_encoding enc =
    if enc = "lf" then
      encoding := Some (module Dkmeta.LF)
    else if enc = "prod" then
      encoding := Some (module Dkmeta.PROD)
    else
      Errors.fail_exit (-1) dloc "Unknown encoding '%s'" enc
  in
  let options = Arg.align
    [ ( "-d"
      , Arg.String set_debug_mode
      , " flags enables debugging for all given flags" )
    ; ( "-q"
      , Arg.Unit (fun () -> Env.set_debug_mode "q")
      , " Quiet mode (equivalent to -d 'q'" )
    ; ("-m"
      , Arg.String add_meta_file
      , " The file containing the meta rules. It has to be typed checked first")
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
    ; ( "-I"
      , Arg.String Basic.add_path
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
      sg = Signature.make "meta" (* the name is not relevant here *)
    })
  in
  try
    let cfg = List.fold_left Dkmeta.meta_of_file cfg !meta_files in
    Errors.success "Meta file parsed.@.";
    List.iter (run_on_file cfg) files;
    match !run_on_stdin with
    | None   -> ()
    | Some m ->
       let md = Env.init m in
       let mk_entry e =
         Format.printf "%a@." Pp.print_entry (Dkmeta.mk_entry cfg md e)
       in
       Parser.Parse_channel.handle md mk_entry stdin;
       Errors.success "Standard input was successfully checked.@."
  with
  | Signature.SignatureError sg -> Errors.fail_env_error dloc (Env.EnvErrorSignature sg)
  | Env.EnvError(l,e) -> Errors.fail_env_error l e
  | Sys_error err        -> Format.eprintf "ERROR %s.@." err; exit 1
  | Exit                 -> exit 3
