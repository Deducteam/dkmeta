open Kernel
open Basic
open Api

(* By default, we don't wont to fail if a symbol is not found in a
   signature. This is to simplify the use of dkmeta. Otherwise, all
   the symbols that appears in the files which are normalized (and
   consequently all the dependencies) needs to be added type checked
   first and to be added in the signature (the load path should be
   changed accordingly). *)
let _ = Signature.fail_on_symbol_not_found := false

(* A list of meta files. Order matters (see Dkmeta.mli). *)
let meta_files : string list ref = ref []

let add_meta_file s = meta_files := s :: !meta_files

(* The letter 'a' with the -d option acivates dkmeta log messages *)
let set_debug_mode opts =
  try Env.set_debug_mode opts
  with Env.DebugFlagNotRecognized c ->
    if c = 'a' then Debug.enable_flag Dkmeta.debug_flag
    else raise (Env.DebugFlagNotRecognized c)

(* The processor declared by dkmeta when used with the command line *)
type _ Processor.t += Dkmeta : unit Processor.t

let equal (type a b) :
    a Processor.t * b Processor.t ->
    (a Processor.t, b Processor.t) Processor.Registration.equal option =
  function
  | Dkmeta, Dkmeta -> Some (Processor.Registration.Refl Dkmeta)
  | _ -> None

let _ =
  let run_on_stdin = ref None in
  let beta = ref true in
  let switch_beta_off () = beta := false in
  let quoting : (module Dkmeta.QUOTING) option ref = ref None in
  let set_quoting func =
    if func = "lf" then quoting := Some (module Dkmeta.LF)
    else if func = "prod" then quoting := Some (module Dkmeta.PROD)
    else if func = "ltyped" then quoting := Some (module Dkmeta.APP)
    else
      Errors.fail_exit ~file:"" ~code:"-1" (Some dloc)
        "Unknown quoting function '%s'" func
  in
  let register_before = ref false in
  let encode_meta_rules = ref false in
  let unquoting = ref true in
  let options =
    Arg.align
      [
        ( "-l",
          Arg.Unit (fun () -> set_debug_mode "a"),
          " Active the debug flag specific to dkmeta" );
        ( "-d",
          Arg.String set_debug_mode,
          " flags enables debugging for all given flags" );
        ( "-q",
          Arg.Unit (fun () -> Env.set_debug_mode "q"),
          " Quiet mode (equivalent to -d 'q'" );
        ("-m", Arg.String add_meta_file, " The file containing the meta rules.");
        ("--quoting", Arg.String set_quoting, " Encoding the Dedukti file.");
        ( "--no-quoting",
          Arg.Unit (fun () -> unquoting := false),
          " Terms are not decoded after. Usage is mainly for debugging purpose."
        );
        ( "--quoting-meta",
          Arg.Unit (fun () -> encode_meta_rules := true),
          " Meta rules are also encoded. However this does not work with \
           product encoding" );
        ( "--register-before",
          Arg.Unit (fun () -> register_before := true),
          " With a typed encoding, entries are registered before they are \
           metaified" );
        ( "--switch-beta-off",
          Arg.Unit switch_beta_off,
          " switch off beta while normalizing terms" );
        ( "-stdin",
          Arg.String (fun n -> run_on_stdin := Some n),
          " MOD Parses standard input using module name MOD" );
        ( "-version",
          Arg.Unit (fun () -> Format.printf "Meta Dedukti %s@." Dkmeta.version),
          " Print the version number" );
        ( "-I",
          Arg.String Files.add_path,
          " DIR Add the directory DIR to the load path" );
      ]
  in
  let usage = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [FILE]...\n" in
  let usage = usage ^ "Available options:" in
  let files =
    let files = ref [] in
    Arg.parse options (fun f -> files := f :: !files) usage;
    List.rev !files
  in
  (* Modified configuration with options given on the command line *)
  let cfg =
    Dkmeta.
      {
        default_config with
        beta = !beta;
        quoting = !quoting;
        register_before = !register_before;
        encode_meta_rules = !encode_meta_rules;
        unquoting = !unquoting;
        env =
          Env.init
            (Parsers.Parser.input_from_string (Basic.mk_mident "meta") "");
      }
  in
  let cfg = Dkmeta.meta_of_files ~cfg !meta_files in
  Errors.success "Meta files parsed.";
  let post_processing env entry =
    let (module Printer) = Env.get_printer env in
    Format.printf "%a" Printer.print_entry entry
  in
  let hook =
    {
      Processor.before = (fun _ -> ());
      after =
        (fun env exn ->
          match exn with
          | None ->
              Errors.success
                (Format.asprintf "File '%s' was successfully metaified."
                   (Env.get_filename env))
          | Some (env, lc, exn) -> Env.fail_env_error env lc exn);
    }
  in
  let processor = Dkmeta.make_meta_processor cfg ~post_processing in
  Processor.Registration.register_processor Dkmeta { equal } processor;
  match !run_on_stdin with
  | None -> Processor.handle_files files ~hook Dkmeta
  | Some m ->
      let input = Parsers.Parser.input_from_stdin (Basic.mk_mident m) in
      Api.Processor.handle_input input ~hook Dkmeta
