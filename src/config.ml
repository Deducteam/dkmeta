let version = "0.1"

type config =
  {
    mutable everything  : bool;
    mutable meta_rules  : Rule.rule_name list;
    mutable beta        : bool;
    mutable encoding    : (module Encoding.T) option;
  }

let config =
  {
    everything = false;
    meta_rules = [];
    beta = true;
    encoding = None;
  }

let red_cfg : unit -> Reduction.red_cfg = fun () ->
  let open Reduction in
    { default_cfg with
      beta = config.beta;
      target = Snf;
      select = Some
          (fun r ->
             if config.everything then true
             else match config.meta_rules with [] -> false | _ -> List.mem r config.meta_rules)
    }

let add_meta_rule r = config.meta_rules <- r::config.meta_rules
(*
let _meta_rules : Rule.rule_name list ref = ref []

let _beta = ref true
let _unsafe = ref true
let _output_file : string option ref = ref None
let _encoding : string option ref = ref None

let safety = _unsafe

let unsafe () = !_unsafe

let switch_beta_off () =
  _beta := false

let beta () = !_beta

let set_output_file s =
  _output_file := Some s

let output_file () =
  !_output_file

let set_encoding s =
  _encoding := Some s

let add_meta_rule n = _meta_rules := n::!_meta_rules

let meta_rules () = !_meta_rules
*)
