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

let max_first bank =
  let rec aux bank max =
    match bank with
    | e1::_::[] -> if e1 > max then e1 else max
    | e::l -> if e > max then (aux l e) else (aux l max)
    | [] -> max in
  aux bank 0;;

let max_second bank =
  let rec aux bank max =
    match bank with
    | e::l -> if e > max then (aux l e) else (aux l max)
    | [] -> max in
  aux bank 0;;

let line_to_list line =
  (List.map (fun x -> (int_of_char x)) (List.of_seq (String.to_seq line)));;

let sublist start list = 
  let rec aux i l acc = 
    match l with
    | e::l'  -> (
      if i <= start 
        then aux (i+1) l' acc 
      else 
        aux (i+1) l' (e::acc)
    )
    | _ -> (List.rev acc) in 
  aux 0 list [];;

let process_line line = 
  let bank = (line_to_list line) in 
  let first = (max_first bank) in 
  let index = 
    match (List.find_index (fun x -> x = first) bank) with 
    | Some(i) -> i
    | _ -> failwith "no index" in 
  let second = (max_second (sublist index bank)) in
  (int_of_string (String.of_seq (List.to_seq (List.map (fun x -> (char_of_int x))(first::second::[])))));;

let () = 
  let input = (parse_file "res/input.txt") in 
  print_endline (string_of_int (List.fold_left (fun acc x -> acc + (process_line x)) 0 input));;
