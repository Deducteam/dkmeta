let version = "0.1"

let _meta_files : string list ref = ref []
let _meta_rules : Rule.rule_name list ref = ref []
let _beta = ref true
let _output_file : string option ref = ref None
let _encoding : string option ref = ref None


let add_meta_file s =
  _meta_files := s::!_meta_files

let meta_files () = !_meta_files

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

let add_meta_rule n = _meta_rules := n::!_meta_rules

let meta_rules () = !_meta_rules
