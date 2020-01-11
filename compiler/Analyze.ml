open Ast

let rec (++) seq1 seq2 =
    fun () -> match seq1 () with
        | Seq.Cons (el, tail) -> Seq.Cons (el, tail ++ seq2)
        | Seq.Nil -> seq2 ()

let rec vars_of_expr deps = function
    | Ebinop (e1, _, e2) -> vars_of_expr deps e1 @ vars_of_expr deps e2
    | Eunop (_, e) -> vars_of_expr deps e
    | Eif (e1, e2, e3) ->
        vars_of_expr deps e1 @ vars_of_expr deps e2 @ vars_of_expr deps e3
    | Epre _ -> []
    | Ecall (_, args) -> List.flatten @@ List.map (vars_of_expr deps) args
    | Evar v ->
        begin match Hashtbl.find deps v with
            | _ -> [v]
            | exception Not_found ->
                failwith @@
                Format.sprintf "reference of undefined var %s" (Ident.show v)
        end
    | Econst _ -> []
    | Eindex (e1, e2) -> vars_of_expr deps e1 @ vars_of_expr deps e2
    | Efield (e, _) -> vars_of_expr deps e
    | Epoly s ->
        List.flatten @@
        List.map
            (fun (e1, e2, e3) ->
                 vars_of_expr deps e1 @
                 vars_of_expr deps e2 @
                 vars_of_expr deps e3)
            s
    | Emono (e1, e2, e3) ->
        vars_of_expr deps e1 @ vars_of_expr deps e2 @ vars_of_expr deps e3

type signal_descriptor = {
    mutable deps: Ident.t list ;
    sig_type: Ast.signal_type ;
    mutable equ: Ast.statement option
}

let var_dependencies def =
    let deps = Hashtbl.create 64 in

    let add_signals sig_type signals =
        List.iter
            (fun (s: Ast.signal) -> match Hashtbl.find deps s.name with
                 | v ->
                     failwith @@
                     Format.sprintf "duplicate definition of %s"
                         (Ident.show s.name)
                | exception Not_found ->
                    Hashtbl.add deps s.name
                        { deps = [] ;
                          sig_type = s.sig_type ;
                          equ = None })
            signals
    in

    add_signals Sig_in def.inputs ;
    add_signals Sig_out def.outputs ;

    List.iter
        (fun (v, _) ->
             Hashtbl.add deps v { deps = [] ;
                                  sig_type = Sig_local ;
                                  equ = None })
        Normalize.glob_env ;

    (* FIXME: could be more intelligent... *)
    List.iter
        (fun equ -> match equ with
             | Sif _ -> assert false
             | Seq { lhs ; rhs } ->
                 Hashtbl.add deps lhs { deps = [] ;
                                        sig_type = Sig_local ;
                                        equ = None })
        def.body.stmts ;

    List.iter
        (fun equ -> match equ with
             | Seq eq ->
                 begin match Hashtbl.find deps eq.lhs with
                     | { equ = Some _ ; _ } ->
                         failwith @@
                         Format.sprintf "duplicate assignation to %s"
                             (Ident.show eq.lhs)
                     | r ->
                         begin if r.sig_type == Sig_in then
                            failwith @@
                            Format.sprintf "assigning to input %s"
                                (Ident.show eq.lhs)
                         end ;
                         let vars = vars_of_expr deps eq.rhs in
                         r.deps <- vars ;
                         r.equ <- Some equ
                     | exception Not_found ->
                         let vars = vars_of_expr deps eq.rhs in
                         Hashtbl.add deps eq.lhs
                             { deps = vars ;
                               sig_type = Sig_local ;
                               equ = Some equ }
                 end
             | Sif _ -> assert false)
        def.body.stmts ;

    let rec check =
        List.iter
            (fun (s: signal) ->
                if (Hashtbl.find deps s.name).equ == None then
                    failwith @@
                    Format.sprintf "output is not assigned to: %s"
                        (Ident.show s.name))
    in 

    check def.outputs ;
    deps

let analyze_def definition =
    let deps = var_dependencies definition in
    (* topo sort *)
    let nodes = List.of_seq @@ Hashtbl.to_seq deps in
    let rec topo_sort nodes sorted =
        if nodes == [] then
            List.rev sorted
        else begin
            let (direct, indirect) =
                List.fold_left
                    (fun (g1, g2) (var, r) -> match r.deps with
                        | [] -> ((var, r) :: g1, g2)
                        |  _ -> (g1, (var, r) :: g2))
                    ([], []) nodes
            in

            begin if direct == [] then begin
                    List.iter
                        (fun (v, r) -> Format.printf "%s: " @@ Ident.show v ;
                            List.iter
                                (fun v -> Format.printf "%s, " @@ Ident.show v)
                                r.deps ;
                            Format.printf "\n")
                        indirect ;
                failwith @@ Format.sprintf "causality cycle" end
            end ;
            List.iter
                (fun (var, r) ->
                     r.deps <-
                         List.filter
                             (fun v -> not @@ List.mem_assoc v direct)
                             r.deps)
                indirect ;
            topo_sort indirect (direct @ sorted)
        end
    in
    let sorted = topo_sort nodes [] in
    { definition with
          body = { definition.body with
                       stmts = List.fold_right
                               (fun (var, r) acc -> match r.equ with
                                    | Some equ -> equ :: acc
                                    | None -> acc)
                               sorted [] } }

let analyze =
    List.map (function Cdef f -> Cdef (analyze_def f) | c -> c)
