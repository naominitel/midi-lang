open Ast
open Format
open Session

let process_phrase cm e env =
    try
        let () = Scope.scope e in
        (* Format.printf "%a" Pretty.pp e ; *)
        let f = Normalize.flatten e in
        (* Format.printf "%a" Pretty.pp f ; *)
        let f = Normalize.normalize f in
        (* Format.printf "%a" Pretty.pp f ; *)
        let f = Analyze.analyze f in
        (* Format.printf "%a" Pretty.pp f ; *)
        let f = IR.to_ir f in () ;
        List.iter (fun b -> print_bytes b) f ;
        ()
    with Errors.Error (sp, e) ->
        span_err (Format.err_formatter) cm sp e ;
        env ;
        ()

let process_file filename =
    let ic = open_in filename in
    match Session.parse Parser.file ic filename with
        | None -> close_in ic ; exit 2
        | Some (cm, el) ->
                        process_phrase cm el []

(* Main entry point. *)

let () =
    Printexc.record_backtrace true ;

    if Array.length Sys.argv > 1 then
        for i = 1 to Array.length Sys.argv - 1 do
            process_file Sys.argv.(i)
        done
    else Format.printf "usage: %s FILE\n" Sys.argv.(0)
