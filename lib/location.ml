(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** This version of location does not have a printer *)

open Lexing

type t = {loc_start: position; loc_end: position; loc_ghost: bool}

let in_file name =
  let pos = {Lexing.dummy_pos with pos_fname= name} in
  {loc_start= pos; loc_end= pos; loc_ghost= true}

let none = in_file "_none_"

let is_none l = l = none

let curr lexbuf =
  {loc_start= lexbuf.lex_start_p; loc_end= lexbuf.lex_curr_p; loc_ghost= false}

let init lexbuf fname =
  lexbuf.lex_curr_p <- {pos_fname= fname; pos_lnum= 1; pos_bol= 0; pos_cnum= 0}

let symbol_rloc () =
  { loc_start= Parsing.symbol_start_pos ()
  ; loc_end= Parsing.symbol_end_pos ()
  ; loc_ghost= false }

let symbol_gloc () =
  { loc_start= Parsing.symbol_start_pos ()
  ; loc_end= Parsing.symbol_end_pos ()
  ; loc_ghost= true }

let rhs_loc n =
  { loc_start= Parsing.rhs_start_pos n
  ; loc_end= Parsing.rhs_end_pos n
  ; loc_ghost= false }

let rhs_interval m n =
  { loc_start= Parsing.rhs_start_pos m
  ; loc_end= Parsing.rhs_end_pos n
  ; loc_ghost= false }

(* return file, line, char from the given position *)
let get_pos_info pos = (pos.pos_fname, pos.pos_lnum, pos.pos_cnum - pos.pos_bol)

type 'a loc = {txt: 'a; loc: t}

let mkloc txt loc = {txt; loc}

let mknoloc txt = mkloc txt none

(******************************************************************************)
(* Input info *)

let input_name = ref "_none_"

let input_lexbuf = ref (None : lexbuf option)

let input_phrase_buffer = ref (None : Buffer.t option)

(******************************************************************************)
(* Terminal info *)


(******************************************************************************)
(* Printing locations, e.g. 'File "foo.ml", line 3, characters 10-12' *)

(******************************************************************************)
(* An interval set structure; additionally, it stores user-provided information
   at interval boundaries.

   The implementation provided here is naive and assumes the number of intervals
   to be small, but the interface would allow for a more efficient
   implementation if needed.

   Note: the structure only stores maximal intervals (that therefore do not
   overlap).
*)

module ISet : sig
  type 'a bound = 'a * int

  type 'a t

  (* bounds are included *)
  val of_intervals : ('a bound * 'a bound) list -> 'a t

  val mem : 'a t -> pos:int -> bool

  val find_bound_in : 'a t -> range:int * int -> 'a bound option

  val is_start : 'a t -> pos:int -> 'a option

  val is_end : 'a t -> pos:int -> 'a option

  val extrema : 'a t -> ('a bound * 'a bound) option
end = struct
  type 'a bound = 'a * int

  (* non overlapping intervals *)
  type 'a t = ('a bound * 'a bound) list

  let of_intervals intervals =
    let pos =
      List.map
        (fun ((a, x), (b, y)) ->
          if x > y then [] else [((a, x), `S); ((b, y), `E)] )
        intervals
      |> List.flatten
      |> List.sort (fun ((_, x), k) ((_, y), k') ->
             (* Make `S come before `E so that consecutive intervals get merged
                together in the fold below *)
             let kn = function `S -> 0 | `E -> 1 in
             compare (x, kn k) (y, kn k') )
    in
    let nesting, acc =
      List.fold_left
        (fun (nesting, acc) (a, kind) ->
          match (kind, nesting) with
          | `S, `Outside ->
              (`Inside (a, 0), acc)
          | `S, `Inside (s, n) ->
              (`Inside (s, n + 1), acc)
          | `E, `Outside ->
              assert false
          | `E, `Inside (s, 0) ->
              (`Outside, (s, a) :: acc)
          | `E, `Inside (s, n) ->
              (`Inside (s, n - 1), acc) )
        (`Outside, []) pos
    in
    assert (nesting = `Outside) ;
    List.rev acc

  let mem iset ~pos =
    List.exists (fun ((_, s), (_, e)) -> s <= pos && pos <= e) iset

  let find_bound_in iset ~range:(start, end_) =
    List.find_map
      (fun ((a, x), (b, y)) ->
        if start <= x && x <= end_ then Some (a, x)
        else if start <= y && y <= end_ then Some (b, y)
        else None )
      iset

  let is_start iset ~pos =
    List.find_map (fun ((a, x), _) -> if pos = x then Some a else None) iset

  let is_end iset ~pos =
    List.find_map (fun (_, (b, y)) -> if pos = y then Some b else None) iset

  let extrema iset =
    if iset = [] then None
    else Some (fst (List.hd iset), snd (List.hd (List.rev iset)))
end


type input_line = {text: string; start_pos: int}

(* Takes a list of lines with possibly missing line numbers.

   If the line numbers that are present are consistent with the number of lines
   between them, then infer the intermediate line numbers.

   This is not always the case, typically if lexer line directives are
   involved... *)
let infer_line_numbers (lines : (int option * input_line) list) :
    (int option * input_line) list =
  let _, offset, consistent =
    List.fold_left
      (fun (i, offset, consistent) (lnum, _) ->
        match (lnum, offset) with
        | None, _ ->
            (i + 1, offset, consistent)
        | Some n, None ->
            (i + 1, Some (n - i), consistent)
        | Some n, Some m ->
            (i + 1, offset, consistent && n = m + i) )
      (0, None, true) lines
  in
  match (offset, consistent) with
  | Some m, true ->
      List.mapi (fun i (_, line) -> (Some (m + i), line)) lines
  | _, _ ->
      lines

(* [get_lines] must return the lines to highlight, given starting and ending
   positions.

   See [lines_around_from_current_input] below for an instantiation of
   [get_lines] that reads from the current input.
*)

let lines_around ~(start_pos : position) ~(end_pos : position)
    ~(seek : int -> unit) ~(read_char : unit -> char option) : input_line list =
  seek start_pos.pos_bol ;
  let lines = ref [] in
  let bol = ref start_pos.pos_bol in
  let cur = ref start_pos.pos_bol in
  let b = Buffer.create 80 in
  let add_line () =
    if !bol < !cur then (
      let text = Buffer.contents b in
      Buffer.clear b ;
      lines := {text; start_pos= !bol} :: !lines ;
      bol := !cur )
  in
  let rec loop () =
    if !bol >= end_pos.pos_cnum then ()
    else
      match read_char () with
      | None ->
          (* end of input *)
          add_line ()
      | Some c -> (
          incr cur ;
          match c with
          | '\r' ->
              loop ()
          | '\n' ->
              add_line () ; loop ()
          | _ ->
              Buffer.add_char b c ; loop () )
  in
  loop () ; List.rev !lines

(* Attempt to get lines from the lexing buffer. *)
let lines_around_from_lexbuf ~(start_pos : position) ~(end_pos : position)
    (lb : lexbuf) : input_line list =
  (* Converts a global position to one that is relative to the lexing buffer *)
  let rel n = n - lb.lex_abs_pos in
  if rel start_pos.pos_bol < 0 then
    (* Do nothing if the buffer does not contain the input (because it has been
       refilled while lexing it) *)
    []
  else
    let pos = ref 0 in
    (* relative position *)
    let seek n = pos := rel n in
    let read_char () =
      if !pos >= lb.lex_buffer_len then (* end of buffer *) None
      else
        let c = Bytes.get lb.lex_buffer !pos in
        incr pos ; Some c
    in
    lines_around ~start_pos ~end_pos ~seek ~read_char

(* Attempt to get lines from the phrase buffer *)
let lines_around_from_phrasebuf ~(start_pos : position) ~(end_pos : position)
    (pb : Buffer.t) : input_line list =
  let pos = ref 0 in
  let seek n = pos := n in
  let read_char () =
    if !pos >= Buffer.length pb then None
    else
      let c = Buffer.nth pb !pos in
      incr pos ; Some c
  in
  lines_around ~start_pos ~end_pos ~seek ~read_char

(* A [get_lines] function for [highlight_quote] that reads from the current
   input. *)
let lines_around_from_current_input ~start_pos ~end_pos =
  match (!input_lexbuf, !input_phrase_buffer, !input_name) with
  | _, Some pb, "//toplevel//" ->
      lines_around_from_phrasebuf pb ~start_pos ~end_pos
  | Some lb, _, _ ->
      lines_around_from_lexbuf lb ~start_pos ~end_pos
  | None, _, _ ->
      []

let is_dummy_loc loc =
  (* Fixme: this should be just [loc.loc_ghost] and the function should be
     inlined below. However, currently, the compiler emits in some places ghost
     locations with valid ranges that should still be printed. These locations
     should be made non-ghost -- in the meantime we just check if the ranges are
     valid. *)
  loc.loc_start.pos_cnum = -1 || loc.loc_end.pos_cnum = -1

(* It only makes sense to highlight (i.e. quote or underline the corresponding
   source code) locations that originate from the current input.

   As of now, this should only happen in the following cases:

   - if dummy locs or ghost locs leak out of the compiler or a buggy ppx;

   - more generally, if some code uses the compiler-libs API and feeds it
   locations that do not match the current values of [!Location.input_name],
   [!Location.input_lexbuf];

   - when calling the compiler on a .ml file that contains lexer line directives
   indicating an other file. This should happen relatively rarely in practice --
   in particular this is not what happens when using -pp or -ppx or a ppx
   driver.
*)
let is_quotable_loc loc =
  (not (is_dummy_loc loc))
  && loc.loc_start.pos_fname = !input_name
  && loc.loc_end.pos_fname = !input_name
