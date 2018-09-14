open Basic

let meta_files : string list ref = ref []
let add_meta_file s =
  meta_files := s::!meta_files

let meta_mds : Basic.mident list ref = ref []
let add_meta_md md =
  meta_mds := md::!meta_mds

let mk_entry md safe =
  let open Entry in
  let open Rule in
  let sg = Env.get_signature () in
  function
  | Decl(lc,id,st,ty) ->
    if safe then
      Env.declare lc id st ty
    else
      Signature.add_declaration sg lc id st ty
  | Def(lc,id,opaque,ty_opt,te) ->
    Config.add_meta_rule (Delta(mk_name md id));
    if safe then
      Env.define lc id opaque te ty_opt
    else
      let ty = match ty_opt with None -> assert false | Some ty -> ty in
      Signature.add_declaration sg lc id Signature.Definable ty;
      let cst = mk_name md id in
      let rule = {name = Delta(cst); ctx= []; pat = Pattern(lc, cst, []); rhs = te} in
      let rule = Rule.to_rule_infos rule in
      Signature.add_rules sg [rule]
  | Rules(lc,rs) ->
    begin
      if safe then
        ignore(Env.add_rules rs)
      else
        let ri = List.map Rule.to_rule_infos rs in
        Signature.add_rules sg ri
    end;
    List.iter (fun (r:untyped_rule) -> Config.add_meta_rule r.name) rs
  | _ -> ()

let run_on_meta_file safe file =
  let open Config in
  let input = open_in file in
  begin
  match config.encoding with
  | None -> ()
  | Some (module E) ->
    ignore(Env.init  "lf");
    List.iter (mk_entry E.md safe) (E.entries ())
  end;
  let md = Env.init file in
  add_meta_md md;
  Parser.handle_channel md (mk_entry md safe) input;
  Errors.success "File '%s' was successfully checked." file;
  Env.export ();
  close_in input

let run_on_file file =
  let import md = Env.import Basic.dloc md in
  let input = open_in file in
  let md = Env.init file in
  List.iter import !meta_mds;
  Parser.handle_channel md (Meta.mk_entry md) input;
  Errors.success "File '%s' was successfully metaified." file;
  close_in input

let set_debug_mode opts =
  try  Env.set_debug_mode opts
  with Env.DebugFlagNotRecognized c ->
    if c = 'a' then
      Debug.enable_flag Meta.D_meta
    else
      raise (Env.DebugFlagNotRecognized c)

let _ =
  let run_on_stdin = ref None  in
  let unsafe = ref false in
  let switch_beta_off () = Config.(config.beta <- false) in
  let switch_everything () = Config.(config.everything <- true) in
  let set_encoding enc =
    if enc = "lfp" then
      Config.(config.encoding <- Some (module Encoding.LFP))
    else if enc = "lf" then
      begin
        Config.(config.encoding <- Some (module Encoding.LF));
        unsafe := true
      end
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
      , " The file containing the meta rules. It has to be typed checked" )
    ; ("--encoding"
      , Arg.String set_encoding
      , " Encoding the Dedukti file. Only LF encoding is supported right now")
    ; ("--unsafe"
      , Arg.Set unsafe
      , " Meta files are not type checked")
    ; ("--everything"
      , Arg.Unit switch_everything
      , " Every rule are considered as meta. Subsume option -m")
    ; ("--switch-beta-off"
      , Arg.Unit switch_beta_off,
      " switch off beta while normalizing terms")
    ; ( "-stdin"
      , Arg.String (fun n -> run_on_stdin := Some(n))
      , " MOD Parses standard input using module name MOD" )
    ; ( "-version"
      , Arg.Unit (fun () -> Format.printf "Meta Dedukti %s@." Config.version)
      , " Print the version number" )
    ; ( "-I"
      , Arg.String Basic.add_path
      , " DIR Add the directory DIR to the load path" )
    ; ( "-errors-in-snf"
      , Arg.Set Errors.errors_in_snf
      , " Normalize the types in error messages" )]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  try
    List.iter (run_on_meta_file (not !unsafe)) !meta_files;
    List.iter run_on_file files;
    match !run_on_stdin with
    | None   -> ()
    | Some m ->
      let md = Env.init m in
      Parser.handle_channel md (Meta.mk_entry md) stdin;
      Errors.success "Standard input was successfully checked.\n"
  with
  | Env.EnvError(l,e) -> Errors.fail_env_error l e
  | Sys_error err        -> Format.eprintf "ERROR %s.@." err; exit 1
  | Exit                 -> exit 3
