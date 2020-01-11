type t = {
    (* original name in source *)
    name: string ;
}

let gensym =
    let next_id = ref 0 in
    fun base ->
        let uid = !next_id in
        next_id := !next_id + 1 ;
        { name = Format.sprintf "%s_%d" base uid }

let show ident = ident.name

let intern =
    let interner = Hashtbl.create 100 in
    fun name ->
        try Hashtbl.find interner name
        with Not_found ->
            let id = { name } in
            Hashtbl.add interner name { name } ;
            id
