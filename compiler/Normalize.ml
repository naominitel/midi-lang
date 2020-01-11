open Ast

(* normalization step 1: flattening (removal of if blocks)
 * (if blocks are not supposed to appear in AST after this step)
 * when encountering an if block:
 * - variables which are local to the parent block (i.e. bound on
 *   both sides) are compiled with an if
 * - other variables are just left in place with a new unique name *)

let rec flatten_expr env expr =
    let _rec = flatten_expr env in
    match expr with
        | Econst c -> Econst c
        | Evar v ->
            begin try Evar (List.assoc v env)
                with Not_found ->
                    failwith (Format.sprintf "unbound variable %s"
                              @@ Ident.show v)
            end
        | Epre v ->
            begin try Epre (List.assoc v env)
                with Not_found ->
                    failwith (Format.sprintf "unbound variable %s"
                              @@ Ident.show v)
            end
        | Ebinop (e1, op, e2) -> Ebinop (_rec e1, op, _rec e2)
        | Eunop (op, e) -> Eunop (op, _rec e)
        | Eif (cond, et, ef) -> Eif (_rec cond, _rec et, _rec ef)
        | Ecall (f, el) -> Ecall (f, List.map _rec el)
        | Eindex (e1, e2) -> Eindex (_rec e1, _rec e2)
        | Efield (e, f) -> Efield (_rec e, f)
        | Epoly e ->
            Epoly (List.map (fun (p, g, v) -> (_rec p, _rec g, _rec v)) e)
        | Emono (p, g, v) -> Emono (_rec p, _rec g, _rec v)

let undef = Evar (Ident.intern "undef")

let rec flatten_block env acc blk =
    let (locals, env) =
        List.fold_left
            (fun (locals, env) v ->
                 let vn = Ident.gensym (Ident.show v) in
                 (vn :: locals, (v, vn) :: env))
            ([], env) blk.locals in
    blk.locals <- locals ;
    let stmts = List.fold_left
        (fun acc equ -> match equ with
            | Seq { lhs ; rhs } ->
                let lhs =
                    try List.assoc lhs env
                    with Not_found ->
                        failwith (Format.sprintf "unbound variable %s"
                                  @@ Ident.show lhs)
                in

                let rhs = flatten_expr env rhs in
                Seq { lhs ; rhs } :: acc
            | Sif (cond, blk_t, blk_f, common) ->
                let cond = flatten_expr env cond in
                let (env_t, flat_t) = flatten_block env [] blk_t in
                let (env_f, flat_f) = flatten_block env [] blk_f in

                let rec find_equ lhs flat = match flat with
                    | [] -> (None, [])
                    | (Sif _ :: _) -> assert false
                    | (Seq { lhs = v ; rhs = rhs } :: tl) ->
                        if v = lhs then (Some rhs, tl)
                        else
                            let (res, tl) = find_equ lhs tl in
                            (res, Seq { lhs = v ; rhs = rhs } :: tl)
                in

                (* introduce new local vars in each branch
                 * for common vars *)
                let (acc, flat_f) =
                    List.fold_left
                        (fun (acc, flat_f) equ ->
                             let (lhs, rhs_t) = match equ with
                                 | Seq { lhs ; rhs } -> (lhs, rhs)
                                 | Sif _ -> assert false
                             in match find_equ lhs flat_f with
                                 | (Some rhs_f, flat_f) ->
                                     let v_t = Ident.gensym (Ident.show lhs) in
                                     let v_f = Ident.gensym (Ident.show lhs) in
                                     (Seq { lhs = v_t ; rhs = rhs_t } ::
                                      Seq { lhs = v_f ; rhs = rhs_f } ::
                                      Seq { lhs = lhs ;
                                            rhs = Eif (cond,
                                                       Evar v_t,
                                                       Evar v_f) } :: acc,
                                     flat_f)
                                 | (None, flat_f) ->
                                     let v_t = Ident.gensym (Ident.show lhs) in
                                     blk.locals <- lhs :: blk.locals ;
                                     (Seq { lhs = v_t ; rhs = rhs_t } ::
                                      Seq { lhs = lhs ;
                                            rhs = Eif (cond,
                                                       Evar v_t,
                                                       undef) } :: acc,
                                      flat_f))
                        (acc, flat_f) flat_t
                in

                let acc =
                    List.fold_left
                        (fun acc equ ->
                             let (lhs, rhs) = match equ with
                                 | Sif _ -> assert false
                                 | Seq { lhs ; rhs } -> (lhs, rhs)
                             in let v_f = Ident.gensym (Ident.show lhs) in
                             (Seq { lhs = v_f ; rhs = rhs } ::
                              Seq { lhs = lhs ;
                                    rhs = Eif (cond, undef, Evar v_f) } :: acc))
                        acc flat_f
                in

                acc)
        acc blk.stmts
    in (env, stmts)

let glob_env =
    List.map (fun v -> (v, v)) [ Ident.intern "MONO_OFF" ;
                                 Ident.intern "undef"]

let flatten_def def =
    let ini_env =
        List.map
            (fun (v: signal) -> (v.name, Ident.gensym (Ident.show v.name)))
            (def.inputs) @ glob_env
    in

    let (env, stmts) = flatten_block ini_env [] def.body in

    { def with
          body =
              { def.body with stmts = stmts } ;
          inputs =
              List.map
                  (fun (v: signal) -> { v with name = List.assoc v.name env })
                  def.inputs ;
          outputs =
              List.map
                  (fun (v: signal) -> { v with name = List.assoc v.name env })
                  def.outputs }

let flatten = List.map Ast.(function Cdef d -> Cdef (flatten_def d) | c -> c)

(* normalize step 2:
 * - all equs are of the form
 *       x = y + z     where y and z are variables (no sub-exprs) *)

let rec normalize var expr equs = match expr with
    | Evar v ->
        (* make sure glob access can be compiled properly *)
        if List.mem_assoc v glob_env then
            let id = Ident.gensym (Ident.show var) in
            (Evar id, Seq { lhs = id ; rhs = Evar v } :: equs)
        else (expr, equs)
    | e ->
        let id = Ident.gensym (Ident.show var) in
        (Evar id, normalize_equ id expr equs)

and normalize_equ var rhs equs =
    let normalize_expr = normalize var in
    let (rhs, equs) = match rhs with
        (* non-rec *)
        | Evar _ | Econst _ | Epre _ -> (rhs, equs)
        (* rec *)
        | Ebinop (e1, op, e2) ->
            let (e1, equs) = normalize_expr e1 equs in
            let (e2, equs) = normalize_expr e2 equs in
            (Ebinop (e1, op, e2), equs)
        | Eunop (op, e) ->
            let (e, equs) = normalize_expr e equs in
            (Eunop (op, e), equs)
        | Eif (e1, e2, e3) ->
            let (e1, equs) = normalize_expr e1 equs in
            let (e2, equs) = normalize_expr e2 equs in
            let (e3, equs) = normalize_expr e3 equs in
            (Eif (e1, e2, e3), equs)
        | Ecall (fn, args) ->
            let (args, equs) =
                List.fold_right
                    (fun arg (args, equs) ->
                         let (arg, equs) = normalize_expr arg equs in
                         (arg :: args, equs))
                    args ([], equs)
            in (Ecall (fn, args), equs)
        | Eindex (e1, e2) ->
            let (e1, equs) = normalize_expr e1 equs in
            let (e2, equs) = normalize_expr e2 equs in
            (Eindex (e1, e2), equs)
        | Efield (e, field) ->
            let (e, equs) = normalize_expr e equs in
            (Efield (e, field), equs)
        | Epoly exprs ->
            let (exprs, equs) =
                List.fold_right
                    (fun (e1, e2, e3) (exprs, equs) ->
                         let (e1, equs) = normalize_expr e1 equs in
                         let (e2, equs) = normalize_expr e2 equs in
                         let (e3, equs) = normalize_expr e3 equs in
                         ((e1, e2, e3) :: exprs, equs))
                    exprs ([], equs)
            in (Epoly exprs, equs)
        | Emono (e1, e2, e3) ->
            let (e1, equs) = normalize_expr e1 equs in
            let (e2, equs) = normalize_expr e2 equs in
            let (e3, equs) = normalize_expr e3 equs in
            (Emono (e1, e2, e3), equs)
    in Seq { lhs = var ; rhs = rhs } :: equs

let normalize_def def =
    { def with
          body = {
              def.body with
                  stmts =
                      List.fold_left
                          (fun norm next -> match next with
                               | Sif _ -> assert false
                               | Seq equ -> normalize_equ equ.lhs equ.rhs norm)
                          [] def.body.stmts } }

let normalize =
    List.map (function Cdef def -> Cdef (normalize_def def) | cmd -> cmd)
