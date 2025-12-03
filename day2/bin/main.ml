module StringSet = Set.Make(String);;

let parse_file filepath = 
  let file = open_in filepath in
  let tryread () = 
    try Some(input_line file)
    with End_of_file -> None in 
  let rec get_lines acc = 
    match tryread () with
    | Some(e) -> get_lines (e::acc)
    | None -> List.rev acc in
  get_lines [];;

let power x n = 
  let rec power_aux x n acc = 
    if n = 0 then acc 
    else (power_aux x (n-1) (acc * x)) in 
  power_aux x n 1;;

let chunks str n = 
  let rec aux str_cur acc =
    if (Seq.length str_cur) > 0
      then 
      aux (Seq.drop n str_cur) ((String.of_seq (Seq.take n str_cur))::acc)
    else (
        (List.rev acc)
    ) in 
  aux (String.to_seq str) [];;

let test x =
  let strx = (string_of_int x) and
  strilen = (String.length (string_of_int x)) in
  let rec aux i n = 
    let parts = (chunks strx i) in 
    let res = (List.length (StringSet.to_list (StringSet.of_list parts))) = 1 in 
    if i > n 
      then false
    else if res 
      then true
    else (aux (i+1) n) in 
    aux 1 (strilen / 2);;

let check_range lowb highb = 
  let rec check_range_aux lowb highb i acc =
    if i > highb then acc 
    else (
      if (test i)
        then (check_range_aux lowb highb (i+1) (i::acc))
      else (check_range_aux lowb highb (i+1) acc)
    ) in 
  check_range_aux lowb highb lowb [];;

let process_range range =
  let bounds = (String.split_on_char '-' range) in 
  match bounds with
  | e1::e2::[] -> (
    let lowb = (int_of_string e1) and highb = (int_of_string e2) in 
    (check_range lowb highb)@[]
    )
  | _ -> failwith "wrong bounds";;

let process_line line = 
  let rec ranges = (String.split_on_char ',' line) and
  each_range invalidIDs ranges =
  match ranges with
  | e::l -> each_range ((process_range e)@invalidIDs) l
  | [] -> invalidIDs in 
  each_range [] ranges;;

let () = 
  let input = (parse_file "res/input.txt") and
  each_lines lines =
    match lines with
    | e::[] -> (List.fold_left (fun acc x -> acc + x) 0 (process_line e))
    | _ -> failwith "more than one line" in
  print_int (each_lines input); print_newline ();;

