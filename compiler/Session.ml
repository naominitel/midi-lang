(* Lexing and parsing *)

(* Display a pretty error message along with the errorneous piece of code *)

let span_err ff cm sp err =
    let open Codemap in
    let open Lexing in
    let open Format in
    let (s, e) = sp in
    let file = Hashtbl.find cm s.pos_fname in
    let scol = s.pos_cnum - s.pos_bol + 1 in
    let ecol = e.pos_cnum - e.pos_bol + 1 in
    fprintf ff "%s:%d:%d: %d:%d \x1b[1;31merror: \x1b[1;37m%a\x1b[0m\n"
                    file.name s.pos_lnum scol e.pos_lnum ecol Errors.pp err ;
    if s.pos_lnum = e.pos_lnum then
        (* The errorneous expression spans over a single line
         * Draw an arrow to point the exact position *)
        let line = pos_line cm s in
        let pre  = sprintf "%s:%d" file.name s.pos_lnum in
        let pad  = String.make (String.length pre + scol) ' ' in
        let arr  =
            if scol = ecol then ""
            else String.make (ecol - scol - 1) '~'
        in
        fprintf ff "%s %s\n" pre line ;
        fprintf ff "%s\x1b[1;31m^%s\x1b[0m\n" pad arr
    else for i = s.pos_lnum to e.pos_lnum do
        let line = line cm s.pos_fname i in
        fprintf ff "%s:%d %s\n" file.name i line
    done

(* Tries to parse an input sentence according to entrypoint from input ic.
 * filname is given for error reporting.
 * Returns Some (cm, ast) in case of success, where cm is the codemap (used to
 * report errors encountered later), and ast is the parse tree.
 * Returns None in case of lexing or parsing failure. *)

let parse entrypoint ic filename =
    let lexmap = Lexer.new_lexmap () in
    let lexbuf = Lexing.from_function (Lexer.read ic lexmap) in
    Lexing.(lexbuf.lex_curr_p <- {
        pos_fname = filename ; pos_lnum = 1;
        pos_bol = 0 ; pos_cnum = 0
    }) ;

    (* creates a codemap from the lexmap *)
    let codemap_of lexmap filename =
        let cm = Hashtbl.create 10 in
        Hashtbl.add cm filename {
            Codemap.name = filename ;
            Codemap.contents = lexmap.Lexer.contents ;
            Codemap.lines = lexmap.Lexer.lines
        } ; cm
    in

    (* report an early error (lexing or parsing err)
     * that arises before the codemap is built *)
    let early_error msg =
        span_err
            (Format.err_formatter) (codemap_of lexmap filename)
            (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)
            (Errors.Esynerr msg)
    in

    try
        let ast = entrypoint (Lexer.token lexmap) lexbuf in
        let cm = codemap_of lexmap filename in
        Some (cm, ast)
    with
        | Lexer.Error msg -> early_error msg ; None
        | Parser.Error    -> early_error "syntax error" ; None
