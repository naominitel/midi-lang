type binop =
    | Ofby
    | Oadd | Osub | Omul | Odiv | Omod
    | Oeq  | Oneq | Olt  | Ogt  | Ole | Oge
    | Oand | Oor

type unop =
    | Ominus | Onot

type gate =
    | Gon
    | Goff
    | Gtie

type const =
    | Cnum of int
    | Cbool of bool
    | Cstr of string
    | Cgate of gate

type prop_arg =
    | PA_id of Ident.t
    | PA_cst of const

type attribute =
    { pname: Ident.t ;
      args: prop_arg list }

type expr =
    | Ebinop of expr * binop * expr
    | Eunop  of unop * expr
    | Eif    of expr * expr * expr
    | Epre   of Ident.t
    | Ecall  of Ident.t * expr list
    | Evar   of Ident.t
    | Econst of const
    | Eindex of expr * expr
    | Efield of expr * Ident.t
    | Epoly  of (expr * expr * expr) list
    | Emono  of (expr * expr * expr)

type equ = {
    lhs: Ident.t ;
    rhs: expr
}

type statement =
    | Sif of expr * block * block * Ident.t list ref
    | Seq of equ

and block = {
    stmts: statement list ;
    mutable locals: Ident.t list
}

type value_type =
    | Tint
    | Tgate
    | Tpoly
    | Tmono
    | Tstr

type signal_type =
    | Sig_in
    | Sig_out
    | Sig_local

type signal = {
    sig_type: signal_type ;
    val_type: value_type ;
    name: Ident.t
}

type definition = {
    attrs: attribute list ;
    name: Ident.t ;
    (*clk: Ident.t ;*)
    inputs: signal list ;
    outputs: signal list ;
    body: block ;
}

type command =
    | Cstart
    | Cstop
    | Cget of Ident.t
    | Cdef of definition

type file = command list
