open Ast

(* TODO: shadowing (aka: multiple assignment) *)
let rec scope_block blk =
    List.fold_left
        (fun (locals, bound) stmt -> match stmt with
            | Sif (_, blk_t, blk_f, blk_common) ->
                let (locals_t, bound_t) = scope_block blk_t in
                let (locals_f, bound_f) = scope_block blk_f in

                (* filter bound_f: those bound in both sides
                 * are removed and added to common. *)
                let (locals_f, common) =
                    List.fold_left
                        (fun (locals, common) bound ->
                             if List.mem bound bound_t
                             then (locals, bound :: common)
                             else (bound :: locals, common))
                        (locals_f, []) bound_f
                in
                (* same for bound_t except common are
                 * already computed *)
                let locals_t =
                    List.fold_left
                        (fun locals bound ->
                             if List.mem bound bound_f
                             then locals else bound :: locals)
                        locals_t bound_t
                in
                blk_t.locals <- locals_t ;
                blk_f.locals <- locals_f ;
                blk_common := common ;
                (common @ locals, bound)
            | Seq { lhs ; _ } -> (locals, lhs :: bound))
        ([], []) blk.stmts

let scope_def def =
    let (locals, bound) = scope_block def.body in
    (* FIXME: is this necessary? *)
    (* let locals =
     *     List.fold_left
     *         (fun locals (v: signal) ->
     *              if not @@ List.mem v.name locals
     *              then v.name :: locals else locals)
     *         (locals @ bound) (def.outputs @ def.inputs)
     * in def.body.locals <- locals *)
    def.body.locals <- locals @ bound

let scope =
    List.iter (function Cdef d -> scope_def d | _ -> ())
