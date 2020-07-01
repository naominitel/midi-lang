(* In memory "map" of a source file
 * Remembers the name of the file and its contents,
 * as well as the position of the beginning of each
 * line, to allow printing errorneous lines *)
type filemap = {
    name:     string    ;
    contents: string    ;
    lines:    int array ;
}

(* The map of the whole program
 * basically maps of each source files *)
type codemap = (string, filemap) Hashtbl.t

(* A span is a region of the code spanned by a
 * token or an non-terminal piece of code.
 * Contains the beginning and the end of the region *)
type span = (Lexing.position * Lexing.position)

let dummy = (Lexing.dummy_pos, Lexing.dummy_pos)

(* Returns the position of the beginning of the line
 * that contains the given position
 * The position is returned as a byte offset, starting
 * at 0 (the beginning of the file) *)
let line_pos cm pos =
    let (file, i) = (pos.Lexing.pos_fname, pos.Lexing.pos_lnum) in
    let file = Hashtbl.find cm file in
    file.lines.(i - 1)

(* Returns the end of the line, i.e. the last character
 * before the beginning of the next line *)
let line_end cm pos =
    let (file, i) = (pos.Lexing.pos_fname, pos.Lexing.pos_lnum) in
    let file = Hashtbl.find cm file in
    if i >= Array.length file.lines
        then match String.index_from_opt file.contents
                                         file.lines.(i-1) '\n' with
             | Some pos -> pos + 1
             | None -> String.length file.contents
        else file.lines.(i)

(* Returns the line containg the given position *)
let pos_line cm pos =
    let spos = line_pos cm pos in
    let epos = line_end cm pos in
    let file = Hashtbl.find cm pos.Lexing.pos_fname in
    String.sub file.contents spos (epos - spos - 1)

(* Returns the ith line of the given file *)
let line cm f i =
    pos_line cm {
        Lexing.pos_fname = f ;
        Lexing.pos_lnum  = i ;
        Lexing.pos_cnum  = 1 ;
        Lexing.pos_bol   = 0
    }
