type info_elt = {token: string; loc: int * int}

let tag_of_token (tok : Ocaml_parser.token) =
  match tok with
  | AMPERAMPER ->
      "AMPERAMPER"
  | AMPERSAND ->
      "AMPERSAND"
  | AND ->
      "AND"
  | AS ->
      "AS"
  | ASSERT ->
      "ASSERT"
  | BACKQUOTE ->
      "BACKQUOTE"
  | BANG ->
      "BANG"
  | BAR ->
      "BAR"
  | BARBAR ->
      "BARBAR"
  | BARRBRACKET ->
      "BARRBRACKET"
  | BEGIN ->
      "BEGIN"
  | CHAR _ ->
      "CHAR"
  | CLASS ->
      "CLASS"
  | COLON ->
      "COLON"
  | COLONCOLON ->
      "COLONCOLON"
  | COLONEQUAL ->
      "COLONEQUAL"
  | COLONGREATER ->
      "COLONGREATER"
  | COMMA ->
      "COMMA"
  | COMMENT _ ->
      "COMMENT"
  | CONSTRAINT ->
      "CONSTRAINT"
  | DO ->
      "DO"
  | DOCSTRING _ ->
      "DOCSTRING"
  | DONE ->
      "DONE"
  | DOT ->
      "DOT"
  | DOTDOT ->
      "DOTDOT"
  | DOWNTO ->
      "DOWNTO"
  | ELSE ->
      "ELSE"
  | END ->
      "END"
  | EOF ->
      "EOF"
  | EOL ->
      "EOL"
  | EQUAL ->
      "EQUAL"
  | EXCEPTION ->
      "EXCEPTION"
  | EXTERNAL ->
      "EXTERNAL"
  | FALSE ->
      "FALSE"
  | FLOAT _ ->
      "FLOAT"
  | FOR ->
      "FOR"
  | FUN ->
      "FUN"
  | FUNCTION ->
      "FUNCTION"
  | FUNCTOR ->
      "FUNCTOR"
  | GREATER ->
      "GREATER"
  | GREATERRBRACE ->
      "GREATERRBRACE"
  | GREATERRBRACKET ->
      "GREATERRBRACKET"
  | IF ->
      "IF"
  | IN ->
      "IN"
  | INCLUDE ->
      "INCLUDE"
  | INFIXOP0 _ ->
      "INFIXOP0"
  | INFIXOP1 _ ->
      "INFIXOP1"
  | INFIXOP2 _ ->
      "INFIXOP2"
  | INFIXOP3 _ ->
      "INFIXOP3"
  | INFIXOP4 _ ->
      "INFIXOP4"
  | INHERIT ->
      "INHERIT"
  | INITIALIZER ->
      "INITIALIZER"
  | INT _ ->
      "INT"
  | LABEL _ ->
      "LABEL"
  | LAZY ->
      "LAZY"
  | LBRACE ->
      "LBRACE"
  | LBRACELESS ->
      "LBRACELESS"
  | LBRACKET ->
      "LBRACKET"
  | LBRACKETAT ->
      "LBRACKETAT"
  | LBRACKETATAT ->
      "LBRACKETATAT"
  | LBRACKETATATAT ->
      "LBRACKETATATAT"
  | LBRACKETBAR ->
      "LBRACKETBAR"
  | LBRACKETGREATER ->
      "LBRACKETGREATER"
  | LBRACKETLESS ->
      "LBRACKETLESS"
  | LBRACKETPERCENT ->
      "LBRACKETPERCENT"
  | LBRACKETPERCENTPERCENT ->
      "LBRACKETPERCENTPERCENT"
  | LESS ->
      "LESS"
  | LESSMINUS ->
      "LESSMINUS"
  | LET ->
      "LET"
  | LIDENT "failwith" ->
      "failwith"
  | LIDENT _ ->
      "LIDENT"
  | LPAREN ->
      "LPAREN"
  | MATCH ->
      "MATCH"
  | METHOD ->
      "METHOD"
  | MINUS ->
      "MINUS"
  | MINUSDOT ->
      "MINUSDOT"
  | MINUSGREATER ->
      "MINUSGREATER"
  | MODULE ->
      "MODULE"
  | MUTABLE ->
      "MUTABLE"
  | NEW ->
      "NEW"
  | NONREC ->
      "NONREC"
  | OBJECT ->
      "OBJECT"
  | OF ->
      "OF"
  | OPEN ->
      "OPEN"
  | OPTLABEL _ ->
      "OPTLABEL"
  | OR ->
      "OR"
  | PERCENT ->
      "PERCENT"
  | PLUS ->
      "PLUS"
  | PLUSDOT ->
      "PLUSDOT"
  | PLUSEQ ->
      "PLUSEQ"
  | PREFIXOP _ ->
      "PREFIXOP"
  | PRIVATE ->
      "PRIVATE"
  | QUESTION ->
      "QUESTION"
  | QUOTE ->
      "QUOTE"
  | RBRACE ->
      "RBRACE"
  | RBRACKET ->
      "RBRACKET"
  | REC ->
      "REC"
  | RPAREN ->
      "RPAREN"
  | SEMI ->
      "SEMI"
  | SEMISEMI ->
      "SEMISEMI"
  | SIG ->
      "SIG"
  | STAR ->
      "STAR"
  | STRING _ ->
      "STRING"
  | STRUCT ->
      "STRUCT"
  | THEN ->
      "THEN"
  | TILDE ->
      "TILDE"
  | TO ->
      "TO"
  | TRUE ->
      "TRUE"
  | TRY ->
      "TRY"
  | TYPE ->
      "TYPE"
  | UIDENT _ ->
      "UIDENT"
  | UNDERSCORE ->
      "UNDERSCORE"
  | VAL ->
      "VAL"
  | VIRTUAL ->
      "VIRTUAL"
  | WHEN ->
      "WHEN"
  | WHILE ->
      "WHILE"
  | WITH ->
      "WITH"
  | HASH ->
      "HASH"
  | HASHOP _ ->
      "HASHOP"
  | DOTOP _ ->
      "DOTOP"
  | QUOTED_STRING_EXPR _ ->
      "QUOTED_STRING_EXPR"
  | QUOTED_STRING_ITEM _ ->
      "QUOTED_STRING_ITEM"
  | ANDOP _ ->
      "ANDOP"
  | LETOP _ ->
      "LETOP"

let syntax_highlighting_locs src =
  let lexbuf = Lexing.from_string ~with_positions:true src in
  let rec collect lexbuf =
    let tok = Ocaml_lexer.token_with_comments lexbuf in
    let loc_start, loc_end = (lexbuf.lex_start_p, lexbuf.lex_curr_p) in
    let tag = tag_of_token tok in
    match tok with
    | EOF ->
        []
    | DOCSTRING (_, loc) | COMMENT (_, loc) ->
        {token= tag; loc= (loc.loc_start.pos_cnum, loc.loc_end.pos_cnum)}
        :: collect lexbuf
    | _ ->
        {token= tag; loc= (loc_start.pos_cnum, loc_end.pos_cnum)}
        :: collect lexbuf
  in
  collect lexbuf

type elt = {tag: string option; content: string}

type t = elt list

let extract ~infos src =
  let get_src a b =
    let in_bound x = min (max x 0) (String.length src) in
    let a = in_bound a and b = in_bound b in
    if b - a <= 0 then "" else String.sub src a (b - a)
  in
  let plain_code = function "" -> [] | content -> [{tag= None; content}] in
  let rec loop infos acc =
    match infos with
    | [] ->
        acc
    | {token; loc= from, to_} :: ({token= _; loc= from', _to_'} :: _ as infos)
      ->
        let elt = {tag= Some token; content= get_src from to_} in
        let in_between = plain_code (get_src to_ from') in
        loop infos (in_between @ [elt] @ acc)
    | [{token; loc= from, to_}] ->
        let elt = {tag= Some token; content= get_src from to_} in
        let in_between = plain_code (get_src to_ (String.length src)) in
        in_between @ [elt] @ acc
  in
  List.rev (loop infos [])

let of_string src =
  let infos = syntax_highlighting_locs src in
  extract ~infos src

let unescape str = Html_escaping.unescape_string str
