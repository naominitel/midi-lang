open Ast

let _UPDATE = 0x10

(* Instruction Opcodes *)

let _NOP  = 0x00
let _CONS = 0x01
let _GLOB = 0x04
let _PRE  = 0x08
let _ADD  = 0x10
let _SUB  = 0x11
let _MUL  = 0x12
let _DIV  = 0x13
let _MOD  = 0x14
let _MIN  = 0x17
let _EQ   = 0x18
let _NEQ  = 0x19
let _GE   = 0x1A
let _LE   = 0x1B
let _GT   = 0x1C
let _LT   = 0x1D
let _AND =  0x20
let _OR   = 0x21
let _NOT  = 0x27
let _FBY  = 0x28
let _IF   = 0x40
let _CALL = 0x50
let _INDX = 0x54
let _FLD  = 0x58
let _POLY = 0x60
let _MONO = 0x68

(* Constant types *)

let _INT  = 0x01
let _BOOL = 0x02
let _GATE = 0x08
let _STR  = 0x10

let add_u8 buf u8 =
    Buffer.add_char buf @@ char_of_int u8

let add_u16 buf u16 =
    add_u8 buf ( u16        land 0xFF) ;
    add_u8 buf ((u16 lsr 8) land 0xFF)

let add_u32 buf u32 =
    add_u8 buf ( u32           land 0xFF) ;
    add_u8 buf ((u32 lsr 0x08) land 0xFF) ;
    add_u8 buf ((u32 lsr 0x10) land 0xFF) ;
    add_u8 buf ((u32 lsr 0x18) land 0xFF)

let add_i64 buf i64 =
    add_u8 buf ( i64           land 0xFF) ;
    add_u8 buf ((i64 asr 0x08) land 0xFF) ;
    add_u8 buf ((i64 asr 0x10) land 0xFF) ;
    add_u8 buf ((i64 asr 0x18) land 0xFF) ;
    add_u8 buf ((i64 asr 0x20) land 0xFF) ;
    add_u8 buf ((i64 asr 0x28) land 0xFF) ;
    add_u8 buf ((i64 asr 0x30) land 0xFF) ;
    add_u8 buf ((i64 asr 0x38) land 0xFF)

let add_pad buf byte count =
    for i = 1 to count do
        add_u8 buf byte
    done

let add_reloc relocs buf str =
    let seek = Buffer.length buf in
    add_u32 buf 0x00 ;
    relocs := (seek, str) :: !relocs

(* FIXME: completely inefficient.
 * should be pre-computed during toposort *)
let varnum v def =
    let equs = def.body.stmts in
    let v = match v with
        | Evar v -> v
        | _ -> assert false
    in let rec search equs = match equs with
        | [] ->
            (* FIXME: eww *)
            let rec search2 (inpts: signal list) = match inpts with
                | [] -> assert false
                | (hd :: tl) ->
                    if hd.name = v then 0
                    else 1 + search2 tl
            in search2 def.inputs
        | (Sif _ :: _) -> assert false
        | (Seq { lhs ; _ } :: tl) ->
            if lhs = v then 0
            else 1 + search tl
    in
    search equs

let add_align buf align =
    while not ((Buffer.length buf mod align) == 0) do
        add_u8 buf 0
    done

(* FIXME: we should not have to carry def along like that... *)
let add_simple_op buf opc args def =
    add_u8  buf opc ;
    add_u8  buf 0x0 ;
    List.iter (fun e -> add_u16 buf (varnum e def)) args ;
    add_align buf 8

let add_expr relocs buf e def = match e with
    | Ebinop (e1, Ofby, e2) -> add_simple_op buf _FBY [e1 ; e2] def
    | Ebinop (e1, Oadd, e2) -> add_simple_op buf _ADD [e1 ; e2] def
    | Ebinop (e1, Osub, e2) -> add_simple_op buf _SUB [e1 ; e2] def
    | Ebinop (e1, Omul, e2) -> add_simple_op buf _MUL [e1 ; e2] def
    | Ebinop (e1, Odiv, e2) -> add_simple_op buf _DIV [e1 ; e2] def
    | Ebinop (e1, Omod, e2) -> add_simple_op buf _MOD [e1 ; e2] def
    | Ebinop (e1, Oeq,  e2) -> add_simple_op buf _EQ  [e1 ; e2] def
    | Ebinop (e1, Oneq, e2) -> add_simple_op buf _NEQ [e1 ; e2] def
    | Ebinop (e1, Oge,  e2) -> add_simple_op buf _GE  [e1 ; e2] def
    | Ebinop (e1, Ole,  e2) -> add_simple_op buf _LE  [e1 ; e2] def
    | Ebinop (e1, Ogt,  e2) -> add_simple_op buf _GT  [e1 ; e2] def
    | Ebinop (e1, Olt,  e2) -> add_simple_op buf _LT  [e1 ; e2] def
    | Ebinop (e1, Oand, e2) -> add_simple_op buf _AND [e1 ; e2] def
    | Ebinop (e1, Oor,  e2) -> add_simple_op buf _OR  [e1 ; e2] def
    | Eunop (Ominus, e) -> add_simple_op buf _MIN [e] def
    | Eunop (Onot, e)   -> add_simple_op buf _NOT [e] def
    | Eif (c, t, f) -> add_simple_op buf _IF [c ; t ; f] def
    | Epre e -> add_simple_op buf _PRE [Evar e] def
    | Ecall (f, args) ->
        add_u8  buf _CALL ;
        add_u8  buf 0x00 ;
        add_u16 buf (List.length args) ;
        add_reloc relocs buf (Ident.show f) ;
        List.iter (fun e -> add_u16 buf (varnum e def)) args ;
        add_align buf 8
    | Evar v ->
        if List.mem_assoc v Normalize.glob_env then begin
            add_u8 buf _GLOB ;
            add_pad buf 0x0 3 ;
            add_reloc relocs buf (Ident.show v)
        end else add_simple_op buf _NOP [Evar v] def
    | Econst (Cnum i) ->
        add_u8 buf _CONS ;
        add_u8 buf _INT ;
        add_pad buf 0x0 6 ;
        add_i64 buf i
    | Econst (Cbool b) ->
        add_u8 buf _CONS ;
        add_u8 buf _BOOL ;
        add_u8 buf @@ if b then 0x01 else 0x00 ;
        add_align buf 8
    | Econst (Cstr s) ->
        add_u8 buf _CONS ;
        add_u8 buf _STR ;
        add_pad buf 0x0 2 ;
        add_reloc relocs buf s
    | Econst (Cgate g) ->
        add_u8 buf _CONS ;
        add_u8 buf _GATE ;
        add_u8 buf @@
        begin match g with
            | Goff -> 0x00
            | Gon  -> 0x01
            | Gtie -> 0xFF
        end ;                   (*  *)
        add_align buf 8 ;
    | Eindex (e1, e2) -> add_simple_op buf _INDX [e1 ; e2] def
    | Efield (e, f) ->
        add_u8 buf _FLD ;
        add_u8 buf 0x0 ;
        add_u16 buf (varnum e def) ;
        (* TODO: fields should be
         * compiled to int indices *)
        add_reloc relocs buf (Ident.show f)
    | Epoly s ->
        add_u8 buf _POLY ;
        add_u8 buf (List.length s) ;
        add_pad buf 0x0 6 ;
        List.iter
            (fun (e1, e2, e3) ->
                 add_u16 buf (varnum e1 def) ;
                 add_u16 buf (varnum e2 def) ;
                 add_u16 buf (varnum e3 def) ;
                 add_u16 buf 0x0)
            s
    | Emono (e1, e2, e3) ->
        add_u8 buf _MONO ;
        add_u8 buf 0x0 ;
        add_u16 buf (varnum e1 def) ;
        add_u16 buf (varnum e2 def) ;
        add_u16 buf (varnum e3 def)

let def definition =
    let msg = Buffer.create 512 in
    let relocs = ref [] in

    (* temporary msg size, will be filled later *)
    add_u32 msg 0 ;
    add_u32 msg _UPDATE ;
    add_reloc relocs msg (Ident.show definition.name) ;

    add_u8  msg (List.length definition.inputs) ;
    add_u8  msg (List.length definition.outputs) ;
    add_u16 msg (List.length definition.body.locals) ;

    (* var number of outputs, will be filled later *)
    (* 16 bits + 32 bits reserved *)
    add_pad msg 0x00 0x30 ;

    let equ_begin = Buffer.length msg in

    List.iter
        (function
            | Seq { rhs ; _ } -> add_expr relocs msg rhs definition
            | Sif _ -> assert false)
        definition.body.stmts ;

    let equ_size = Buffer.length msg - equ_begin in
    add_align msg 16 ;

    (* locals var nums (entry points) *)
    List.iter
        (fun v -> add_u16 msg (varnum (Evar v) definition))
        definition.body.locals ;

    add_align msg 16 ;
    let cst_begin = Buffer.length msg in

    (* constant data *)
    let relocs = List.fold_left
        (fun relocs (seek, str) ->
             let new_seek = Buffer.length msg in
             for i = 0 to String.length str - 1 do
                 Buffer.add_char msg str.[i]
             done ;
             Buffer.add_char msg (char_of_int 0) ;
             add_align msg 16 ;
             (seek, new_seek) :: relocs)
        [] !relocs
    in

    let cst_size = Buffer.length msg - cst_begin in

    (* freeze message, write relocations *)
    let size = Buffer.length msg in
    let msg = Buffer.to_bytes msg in

    let write_u16 addr u16 =
        Bytes.set msg  addr      @@ char_of_int @@  u16        land 0xFF ;
        Bytes.set msg (addr + 1) @@ char_of_int @@ (u16 lsr 8) land 0xFF ;
    in

    let write_u32 addr u32 =
        Bytes.set msg  addr      @@ char_of_int @@  u32           land 0xFF ;
        Bytes.set msg (addr + 1) @@ char_of_int @@ (u32 lsr 0x08) land 0xFF ;
        Bytes.set msg (addr + 2) @@ char_of_int @@ (u32 lsr 0x10) land 0xFF ;
        Bytes.set msg (addr + 3) @@ char_of_int @@ (u32 lsr 0x18) land 0xFF
    in

    (* message size *)
    write_u32 0x0 size ;

    (* outputs *)
    List.iteri
        (fun i (v: signal) ->
             write_u16 (0x10 + (i lsl 1)) (varnum (Evar v.name) definition))
        definition.outputs ;

    (* section sizes *)
    write_u32 0x20 equ_size ;
    write_u32 0x24 cst_size ;

    (* constant data addresses *)
    List.iter
        (fun (seek, new_seek) -> write_u32 seek new_seek)
        relocs ;
    msg

let command = function
    | Cdef d -> def d
    | _ -> failwith "unsupported message kind"

let to_ir = List.map command

