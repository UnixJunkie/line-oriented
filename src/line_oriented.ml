
open Printf

module A = BatArray
module L = BatList

type filename = string

let with_out_file (fn: filename) (f: out_channel -> 'a): 'a =
  let output = open_out_bin fn in
  let res = f output in
  close_out output;
  res

let with_in_file (fn: filename) (f: in_channel -> 'a): 'a =
  let input = open_in_bin fn in
  let res = f input in
  close_in input;
  res

let with_infile_outfile (in_fn: filename) (out_fn: filename)
    (f: in_channel -> out_channel -> 'a): 'a =
  let input = open_in_bin in_fn in
  let output = open_out_bin out_fn in
  let res = f input output in
  close_in input;
  close_out output;
  res

let lines_of_file (fn: filename): string list =
  with_in_file fn (fun input ->
      let res, exn = L.unfold_exc (fun () -> input_line input) in
      if exn <> End_of_file then
        raise exn
      else res
    )

let lines_to_file fn l =
  with_out_file fn (fun out ->
      L.iter (fprintf out "%s\n") l
    )

(* call f on lines of file *)
let iter_on_lines_of_file fn f =
  let input = open_in_bin fn in
  try
    while true do
      f (input_line input)
    done
  with End_of_file -> close_in input

let iteri_on_lines_of_file fn f =
  let i = ref 0 in
  let input = open_in_bin fn in
  try
    while true do
      f !i (input_line input);
      incr i
    done
  with End_of_file -> close_in input

(* map f on lines of file *)
let map_on_lines_of_file (fn: filename) (f: string -> 'a): 'a list =
  with_in_file fn (fun input ->
      let res, exn = L.unfold_exc (fun () -> f (input_line input)) in
      if exn = End_of_file then res
      else raise exn
    )

let mapi_on_lines_of_file (fn: filename) (f: int -> string -> 'a): 'a list =
  with_in_file fn (fun input ->
      let i = ref 0 in
      let res, exn =
        L.unfold_exc (fun () ->
            let curr = f !i (input_line input) in
            incr i;
            curr
          ) in
      if exn = End_of_file then res
      else raise exn
    )

let write_lines (lines: string list) (output_fn: filename): unit =
  with_out_file output_fn (fun out ->
      List.iter (fprintf out "%s\n") lines
    )

(* keep only lines that satisfy p *)
let filter_lines_of_file fn p =
  L.filter p (lines_of_file fn)

let count_lines_of_file (fn: string): int =
  let count = ref 0 in
  iter_on_lines_of_file fn (fun _line -> incr count);
  !count
