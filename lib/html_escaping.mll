(* The goal of this lexer is to remove html encoding from strings, so that
   they display nicely on the command-line. The only encodings included are the
   one actually used. Because this executable is mainly used for testing, this
   is fine. *)
rule unescape_buffer b = parse
| "&amp;" { Buffer.add_char b '&'; unescape_buffer b lexbuf } 
| "&lt;" { Buffer.add_char b '<'; unescape_buffer b lexbuf } 
| "&gt;" { Buffer.add_char b '>'; unescape_buffer b lexbuf } 
| "&gt" { Buffer.add_char b '>'; unescape_buffer b lexbuf } 
| "&gt;" { Buffer.add_char b '>'; unescape_buffer b lexbuf } 
| "&quot;" { Buffer.add_char b '>'; unescape_buffer b lexbuf } 
| "&#x27;" { Buffer.add_char b '\''; unescape_buffer b lexbuf } 
| "&#45" { Buffer.add_char b '-'; unescape_buffer b lexbuf } 
| eof { () }
| _ { Buffer.add_string b (Lexing.lexeme lexbuf) ; unescape_buffer b lexbuf }

{
let unescape_string str =
  let lexbuf = Lexing.from_string str in
  let b = Buffer.create (String.length str) in
  unescape_buffer b lexbuf ;
  Buffer.contents b
}