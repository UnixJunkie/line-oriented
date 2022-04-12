
module L = BatList

type filename = string

let with_in_file fn f =
  let input = open_in_bin fn in
  let res = f input in
  close_in input;
  res

let with_out_file fn f =
  let output = open_out_bin fn in
  let res = f output in
  close_out output;
  res

let with_infile_outfile in_fn out_fn f =
  with_in_file in_fn (fun input ->
      with_out_file out_fn (fun output ->
          f input output
        )
    )

let lines_of_file fn =
  with_in_file fn (fun input ->
      let res, exn = L.unfold_exc (fun () -> input_line input) in
      if exn = End_of_file then res
      else raise exn
    )

let rev_lines_of_file fn =
  with_in_file fn (fun input ->
      let res = ref [] in
      (try
         while true do
           res := (input_line input) :: !res
         done
       with End_of_file -> ()
      );
      !res
    )

let win_EOL = "\r\n"
let unix_EOL = '\n'

let output_EOL =
  if Sys.os_type = "Win32" then
    (fun out -> output_string out win_EOL)
  else
    (fun out -> output_char out unix_EOL)

let append_EOL =
  if Sys.os_type = "Win32" then
    (fun buff -> Buffer.add_string buff win_EOL)
  else
    (fun buff -> Buffer.add_char buff unix_EOL)

let to_string fn =
  let size =
    let stats = Unix.stat fn in
    stats.st_size in
  let buff = Buffer.create size in
  with_in_file fn (fun input ->
      try
        while true do
          let line = input_line input in
          Buffer.add_string buff line;
          append_EOL buff
        done;
        assert(false)
      with End_of_file -> Buffer.contents buff
    )

let lines_to_file fn lines =
  with_out_file fn (fun out ->
      L.iter (fun line ->
          output_string out line;
          output_EOL out
        ) lines
    )

let iter fn f =
  with_in_file fn (fun input ->
      try
        while true do
          f (input_line input)
        done
      with End_of_file -> ()
    )

let iteri fn f =
  let i = ref 0 in
  let g x =
    let y = f !i x in
    incr i;
    y in
  iter fn g

let map fn f =
  with_in_file fn (fun input ->
      let res, exn = L.unfold_exc (fun () -> f (input_line input)) in
      if exn = End_of_file then res
      else raise exn
    )

let fold fn f init =
  with_in_file fn (fun input ->
      let acc = ref init in
      (try
         while true do
           acc := f !acc (input_line input)
         done
       with End_of_file -> ()
      );
      !acc
    )

let rev_map fn f =
  let res = ref [] in
  let g line =
    res := (f line) :: !res in
  iter fn g;
  !res

let mapi fn f =
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

let list_rev_filter p l =
  let rec loop acc = function
    | [] -> acc
    | x :: xs ->
      loop
        (if p x then x :: acc else acc)
        xs in
  loop [] l

let filter fn p =
  list_rev_filter p (rev_lines_of_file fn)

(* count lines *)
let count fn =
  let count = ref 0 in
  iter fn (fun _line -> incr count);
  !count

(* alias *)
let length = count

(* marshal to file *)
let save fn x =
  with_out_file fn (fun out ->
      Marshal.to_channel out x [Marshal.No_sharing]
    )

(* unmarshal from file *)
let restore fn =
  with_in_file fn Marshal.from_channel
