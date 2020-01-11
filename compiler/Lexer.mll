{
    open Printf
    open Parser

    (* Temporary representation of a codemap
     * being built. The lexmap contains:
     *  - a copy of the whole program, filled by the read function when
     *    reading from an input_chan, or pre-filled when reading from a string
     *  - an offset to know where to read when we read from a string
     *  - the positions of the beginning of the lines, updated by the lexer. *)
    type lexmap = {
        mutable contents: string ;
        mutable lines:    int array ;
        mutable offset:   int ;
    }

    let new_lexmap () = {
        contents = "" ; lines = [| 0 |] ; offset = 0
    }

    (* Used as input function by the lexer. Stores the
     * input in the code map for later use for error
     * reporting and sends it back to the lexer. *)
    let read ic (lexmap: lexmap) buf len =
        let count = input ic buf 0 len in
        let data = String.sub (Bytes.to_string buf) 0 count in
        (* TODO: this can probably be more efficient *)
        lexmap.contents <- lexmap.contents ^ data ;
        count

    exception Error of string
}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let ident = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
let int_literal = ['0'-'9'] ['0'-'9' '_']*
let str_literal = '"' (_ | [^ '\\' '"'])* '"'

rule token lexmap = parse
    | eof         { EOF }
    | blank +     { token lexmap lexbuf }
    | newline     {
                      let pos = lexbuf.Lexing.lex_curr_p in
                      lexmap.lines <-
                          Array.append lexmap.lines [| pos.Lexing.pos_cnum |] ;
                      lexbuf.Lexing.lex_curr_p <- { pos with
                          Lexing.pos_lnum = pos.Lexing.pos_lnum + 1 ;
                          Lexing.pos_bol = pos.Lexing.pos_cnum ;
                      } ;
                      token lexmap lexbuf
                  }

    (* comments *)
    | "--"        { comment lexmap lexbuf ; token lexmap lexbuf }

    (* sigils *)
    | "#"         { HASH }
    | "="         { EQ }
    | "=>"        { DOUBLEARROW }
    | "{"         { BROP }
    | "}"         { BRCL }
    | "["         { SQOP }
    | "]"         { SQCL }
    | "let"       { LET }
    | "tel"       { TEL }
    | "("         { OP }
    | ")"         { CL }
    | ":"         { COL }
    | ";"         { SEMI }
    | ","         { COMMA }
    | "."         { DOT }

    (* notes *)
    | (['C' 'D' 'F' 'G'] as n) ('#'? as s) ((['0'-'9'] | "-1") as o)
    | (['D' 'E' 'G'] as n) ('b'? as s) ((['0'-'9'] | "-1") as o)
    | (['A'] as n) ('#'? as s) ((['0'-'8'] | "-1") as o)
    | (['A'] as n) ('b'? as s) ((['0'-'8'] | "-1") as o)
    | (['B'] as n) ('b'? as s) ((['0'-'8'] | "-1") as o)
      {
          let base = ((int_of_string o) + 2) * 12 in
          let off = List.assoc n [('C', 0) ; ('D', 2) ; ('E', 4) ;
                                  ('F', 5) ; ('G', 7) ; ('A', 9) ; ('B', 11)] in
          let alt = match s with
              | "#" -> 1
              | "b" -> -1
              | _ -> 0
          in NUM (base + off + alt)
      }

    (* operators *)
    | "+"         { PLUS }
    | "-"         { MINUS }
    | "*"         { TIMES }
    | "/"         { DIV }
    | "mod"       { MOD }
    | "&&"        { AND }
    | "||"        { OR }
    | "=="        { EQEQ }
    | "!="        { NEQ }
    | "!"         { NOT }
    | ">="        { GE }
    | "<="        { LE }
    | ">"         { GT }
    | "<"         { LT }
    | "->"        { FBY }

    | "if"        { IF }
    | "then"      { THEN }
    | "else"      { ELSE }
    | "pre"       { PRE }

    (* keywords *)
    | "node"      { NODE }
    | "int"       { INT }
    | "gate"      { GATE }
    | "mono"      { MONO }
    | "poly"      { POLY }
    | "On"        { LGATE Ast.Gon }
    | "Off"       { LGATE Ast.Goff }
    | "Tie"       { LGATE Ast.Gtie }
    | "True"      { BOOL true }
    | "False"     { BOOL false }
    (*| "in"        { IN }
    | "out"       { OUT }*)

    (* commands *)
    | "start"
    | "stop"
    | "get"

    | ident as s         { IDENT (Ident.intern s) }
    | int_literal as s   { NUM (int_of_string s) }
    | str_literal as s   { STRING (String.sub s 1 ((String.length s) - 2)) }
    | _ as c             { raise (Error (sprintf "illegal character %C" c)) }

and comment lexmap = parse
    | newline     {
                      let pos = lexbuf.Lexing.lex_curr_p in
                      lexmap.lines <- Array.append lexmap.lines [| pos.Lexing.pos_cnum |] ;
                      lexbuf.Lexing.lex_curr_p <- { pos with
                          Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
                          Lexing.pos_bol = pos.Lexing.pos_cnum;
                      } ; ()
                  }
    | _           { comment lexmap lexbuf }
