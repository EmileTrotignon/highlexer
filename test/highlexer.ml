let big_sources =
  [ {|type info_elt = {token: string; loc: int * int}

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
  |}
  ; {|
  (** This library provides a way to highlight OCaml source. It is designed for
  use with js-of-ocaml, but does not have to be used this way. 

  Because the jsoo code elimination does not work well with the compiler-libs,
  this library uses a modified version of the OCaml lexer. This version has
  error-handling disabled, and docstrings simplified, because these features
  required vendoring much more files from the OCaml source code.
  
  Here is an example of how to use [highlexer] with [js-of-ocaml] and [brr] :

{[
open Brr

let ( .%{} ) obj prop = Jv.get obj prop

let ( !% ) = Jstr.of_string

let highlight_snippet el =
  Console.log [!%"highlighting"; el] ;
  let src =
    (El.to_jv el).%{"innerHTML"} |> Jv.to_string |> Highlexer.unescape
  in
  let annotated = Highlexer.of_string src in
  let html =
    List.map
      (fun Highlexer.{tag; content} ->
        match tag with
        | Some tag ->
            El.(span ~at:At.[class' !%tag] [txt !%content])
        | None ->
            El.txt !%content )
      annotated
  in
  El.set_children el html

let () =
  let ocaml_snippets = Brr.El.find_by_class !%"language-ocaml" in
  Console.log ["ocaml snippets"; ocaml_snippets] ;
  List.iter highlight_snippet ocaml_snippets
]}

    This example had a size of 212K. This may improve with new versions of
    [js-of-ocaml].
*)

(** In [ {tag; content} ], [tag] is the name of the token associated with 
    [content]. It is an option because whitespaces are not represented by a token.
     *)
type elt = {tag: string option; content: string}

type t = elt list

val of_string : string -> elt list
(** [of_string src] Is the lexed version of [src] where a annotation is added 
    for each token.  
    {[(of_string src) |> List.map (fun {content;_} -> content) |> String.concat ""]}
    is equal to [src].
     *)

val unescape : string -> string
(** [unescape src] is [src] with HTML elements such as [%gt] replaced by their 
    associated characters. It is useful because the lexer does understand 
    HTML elements. *)
|}
  ]

(* source with expected result. Short because the result needs to be in the test *)
let small_sources =
  [ ( "Alcotest.(check (list int))"
    , [ (Some "UIDENT", "Alcotest")
      ; (Some "DOT", ".")
      ; (Some "LPAREN", "(")
      ; (Some "LIDENT", "check")
      ; (None, " ")
      ; (Some "LPAREN", "(")
      ; (Some "LIDENT", "list")
      ; (None, " ")
      ; (Some "LIDENT", "int")
      ; (Some "RPAREN", ")")
      ; (Some "RPAREN", ")") ] )
  ; ( {|val unescape : string -> string
    (** [unescape src] is [src] with HTML elements such as [%gt] replaced by their 
        associated characters. It is useful because the lexer does understand 
        HTML elements. *)|}
    , [ (Some "VAL", "val")
      ; (None, " ")
      ; (Some "LIDENT", "unescape")
      ; (None, " ")
      ; (Some "COLON", ":")
      ; (None, " ")
      ; (Some "LIDENT", "string")
      ; (None, " ")
      ; (Some "MINUSGREATER", "->")
      ; (None, " ")
      ; (Some "LIDENT", "string")
      ; (Some "EOL", "\n")
      ; (None, "    ")
      ; ( Some "DOCSTRING"
        , "(** [unescape src] is [src] with HTML elements such as [%gt] \
           replaced by their \n\
          \        associated characters. It is useful because the lexer does \
           understand \n\
          \        HTML elements. *)" ) ] ) ]

let highlight src =
  let open Highlexer in
  src |> of_string |> List.map (fun {tag; content} -> (tag, content))

let highlight_and_remove src =
  let open Highlexer in
  src |> of_string
  |> List.map (fun {tag= _; content} -> content)
  |> String.concat ""

let equation_tests =
  List.mapi
    (fun i src ->
      Alcotest.(
        test_case (Printf.sprintf "big source #%i" i) `Quick (fun () ->
            (check string) "same string" src (highlight_and_remove src) ) ) )
    big_sources
  @ List.mapi
      (fun i (src, _li) ->
        Alcotest.(
          test_case (Printf.sprintf "small source #%i" i) `Quick (fun () ->
              (check string) "same string" src (highlight_and_remove src) ) ) )
      small_sources

let highlighting_tests =
  List.mapi
    (fun i (src, li) ->
      Alcotest.(
        test_case (Printf.sprintf "small source #%i" i) `Quick (fun () ->
            check
              (list (pair (option string) string))
              "same list" li (highlight src) ) ) )
    small_sources

(* Run it *)
let () =
  let open Alcotest in
  run "Highlexer"
    [("Equation", equation_tests); ("Highlighting", highlighting_tests)]
