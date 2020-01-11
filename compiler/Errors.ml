open Ast

type eval_error =
    | Esynerr of string

let pp ff = function
    | Esynerr msg ->
        Format.fprintf ff "%s" msg

exception Error of Codemap.span * eval_error

let error sp err = raise (Error (sp, err))
