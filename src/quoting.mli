open Kernel
open Parsers

module type S = sig
  val md : Basic.mident
  (** module name of the encoding *)

  val entries : unit -> Entry.entry list
  (** List of declarations *)

  val safe : bool
  (** If [safe], the encoding needs type checking. Type checking is
     done before encoding. *)

  val signature : Signature.t
  (** Signature of the encoding. Redudant with [entries] *)

  val quote_term :
    ?sg:Signature.t -> ?ctx:Term.typed_context -> Term.term -> Term.term
  (** [quote_term sg ctx t] quotes a term [t]. [sg] and [ctx] are used
     only if [safe] is true *)

  val unquote_term : Term.term -> Term.term
  (** [unquote_term t] decodes a term [t] *)

  val encode_rule :
    ?sg:Signature.t -> Rule.partially_typed_rule -> Rule.partially_typed_rule
  (** [encode_rule sg r] encodes a rule [r]. [sg] is used only if
       [safe] is true *)
end

module LF : S
(** Prefixing each subterm with its construtor *)

module PROD : S
(** Quoting products with HOAS *)

module APP : S
(** Same as [LF] with type informations for application on product
   only. *)
