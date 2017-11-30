module Meta =
struct
end

module P = Parser.Make(Meta)

let set_meta_file = failwith "todo"

let switch_beta_off = failwith "todo"

let use_non_linear = failwith "todo"

let set_output_file = failwith "todo"

let print_version = failwith "todo"

let args =
  ["--meta-file", Arg.String set_meta_file, "The file containing the meta rules. It has to be typed checked";
   "--switch-beta-off", Arg.Set switch_beta_off, "switch off beta while normalizing terms";
   "--non-linear", Arg.Set use_non_linear, "Allows to have non-linear meta-rules";
   "--output", Arg.String set_output_file, "Output file";
   "-version", Arg.Unit print_version, "print the version number"]

let parse lexbuf =
  try
    P.prelude Lexer.token lexbuf ;
    while true do
      P.line Lexer.token lexbuf
    done;
  with
  | Tokens.EndOfFile -> ()
  | P.Error       -> Errors.fail (Lexer.get_loc lexbuf)
                         "Unexpected token '%s'." (Lexing.lexeme lexbuf)

let run_on_file file =
  let input = open_in file in
  failwith "todo";
  close_in input

let usage_msg () = "Usage: "^ Sys.argv.(0) ^" [options] files"


let _ = Arg.parse args run_on_file (usage_msg ())
