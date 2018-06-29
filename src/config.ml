let version = "0.1"

let _meta_file : string option ref = ref None
let _module_files : string list ref = ref []
let _beta = ref true
let _output_file : string option ref = ref None
let _encoding : string option ref = ref None


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
