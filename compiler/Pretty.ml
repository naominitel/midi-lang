open Ast
open Format

let rec list sep pp ff = function
    | [] -> ()
    | [hd] -> pp ff hd
    | (hd :: tl) -> fprintf ff "%a%s%a" pp hd sep (list sep pp) tl

let ty = function
    | Tint -> "int"
    | Tpoly -> "poly"
    | Tmono -> "mono"
    | Tgate -> "gate"
    | Tstr -> "str"

let signal ff (s: signal) =
    fprintf ff "%s:  %s" (Ident.show s.name) (ty s.val_type)

let const ff = function
    | Cgate Gon -> fprintf ff "On"
    | Cgate Goff -> fprintf ff "Off"
    | Cgate Gtie -> fprintf ff "Tie"
    | Cbool true -> fprintf ff "True"
    | Cbool false -> fprintf ff "False"
    | Cnum n -> fprintf ff "%d" n
    | Cstr s -> fprintf ff "\"%s\"" s

let rec expr ff = function
    | Eif (cond, t, f) ->
        fprintf ff "if %a then %a else %a"
            expr cond expr t expr f
    | e -> expr0 ff e

and expr0 ff = function
    | Ebinop (e1, Ofby, e2) -> fprintf ff "%a -> %a" expr0 e1 expr0 e2
    | Ebinop (e1, Oand, e2) -> fprintf ff "%a && %a" expr0 e1 expr0 e2
    | Ebinop (e1, Oor,  e2) -> fprintf ff "%a || %a" expr0 e1 expr0 e2
    | e -> expr1 ff e

and expr1 ff = function
    | Ebinop (e1, Oeq,  e2) -> fprintf ff "%a == %a" expr1 e1 expr1 e2
    | Ebinop (e1, Oneq, e2) -> fprintf ff "%a != %a" expr1 e1 expr1 e2
    | Ebinop (e1, Ole,  e2) -> fprintf ff "%a <= %a" expr1 e1 expr1 e2
    | Ebinop (e1, Oge,  e2) -> fprintf ff "%a >= %a" expr1 e1 expr1 e2
    | Ebinop (e1, Olt,  e2) -> fprintf ff "%a < %a"  expr1 e1 expr1 e2
    | Ebinop (e1, Ogt,  e2) -> fprintf ff "%a > %a"  expr1 e1 expr1 e2
    | e -> expr2 ff e

and expr2 ff = function
    | Ebinop (e1, Oadd, e2) -> fprintf ff "%a + %a" expr2 e1 expr2 e2
    | Ebinop (e1, Osub, e2) -> fprintf ff "%a - %a" expr2 e1 expr3 e2
    | e -> expr3 ff e

and expr3 ff = function
    | Ebinop (e1, Omul, e2) -> fprintf ff "%a * %a"  expr3 e1 expr3 e2
    | Ebinop (e1, Odiv, e2) -> fprintf ff "%a / %a"  expr3 e1 expr4 e2
    | Ebinop (e1, Omod, e2) -> fprintf ff "%a %% %a" expr3 e1 expr4 e2
    | e -> expr4 ff e

and expr4 ff = function
    | Eunop (Ominus, e) -> fprintf ff "- %a" expr4 e
    | Eunop (Onot,   e) -> fprintf ff "! %a" expr4 e
    | Epre v -> fprintf ff "pre %s" @@ Ident.show v
    | Ecall (f, args) ->
        fprintf ff "%s(%a)"
            (Ident.show f)
            (list ", " expr) args
    | Evar v -> fprintf ff "%s" @@ Ident.show v
    | Econst c -> const ff c
    | Eindex (e1, e2) -> fprintf ff "%a[%a]" expr4 e1 expr e2
    | Efield (e1, f) -> fprintf ff "%a.%s" expr4 e1 @@ Ident.show f
    | Epoly s -> fprintf ff "[%a]" (list ", " mono) s
    | Emono m -> fprintf ff "%a" mono m
    | e -> fprintf ff "(%a)" expr e

and mono ff (e1, e2, e3) = fprintf ff "%a => %a, %a" expr e1 expr e2 expr e3

let incr_p pfx = "    " ^ pfx

let locals ff loc =
    fprintf ff " // { %a }"
        (list ", " (fun ff v -> fprintf ff "%s" @@ Ident.show v)) loc

let rec stmt pfx ff = function
    | Seq { lhs ; rhs } ->
        fprintf ff "%s%s = %a;" pfx (Ident.show lhs) expr rhs
    | Sif (cond, t, f, _) ->
        fprintf ff "%sif %a {%a\n%a\n%s} else {%a\n%a\n%s}"
            pfx expr cond locals t.locals
            (block (incr_p pfx)) t pfx locals f.locals
            (block (incr_p pfx)) f pfx

and block pfx ff blk = list "\n" (stmt pfx) ff blk.stmts

let def ff d =
    fprintf ff
        "node %s (%a) (%a) = let%a\n%a\ntel\n\n"
        (Ident.show d.name)
        (list ", " signal) d.inputs
        (list ", " signal) d.outputs locals d.body.locals
        (block "    ") d.body

let pp ff =
    List.iter
        (function
            | Cdef d -> def ff d
            | Cget v -> fprintf ff "#get %s;;\n\n" @@ Ident.show v
            | Cstop -> fprintf ff "#start;;\n\n"
            | Cstart -> fprintf ff "#start;;\n\n")
