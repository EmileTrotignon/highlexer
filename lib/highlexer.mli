(** This library provides a way to highlight OCaml source. It is designed for
  use with js_of_ocaml, but does not have to be used this way. 

  Because the jsoo code elimination does not work well with the compiler-libs,
  this library uses a modified version of the OCaml lexer. This version has
  worse error-handling, and docstrings simplified, because these features
  required vendoring much more files from the OCaml source code.
  
  Here is an example of how to use [highlexer] with [js_of_ocaml] and [brr] :

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

    This example had a size of 168K. This may improve a little bit with new versions of
    [js_of_ocaml], but the lexing table itself is 104K, so it will never be
    super small like a highlighter with just a few regexp could.

    If you are bothered by the size, it is perfectly fine to use this in the
    backend instead.
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
