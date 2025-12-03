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

let max bank digits_left =
  let rec aux bank max =
    match bank with
    | e::l -> if (List.length l) < digits_left 
      then 
        (if e > max then e else max)
      else 
        (if e > max then (aux l e) else (aux l max))
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
  let rec aux newbank i acc =
    if i <= 0 then (List.rev acc) else 
    let newdigit = (max newbank i) in 
    let index = 
      match (List.find_index (fun x -> x = newdigit) newbank) with 
      | Some(index) -> index
      | _ -> failwith "no index" in 
    aux (sublist index newbank) (i-1) (newdigit::acc) and
    bank = (line_to_list line) in
  let value_as_list = aux bank 12 [] in 
  (int_of_string (String.of_seq (List.to_seq (List.map (fun x -> (char_of_int x)) (value_as_list)))));;

let rec process_lines lines = 
  match lines with 
  | e::l -> (process_line e)::(process_lines l)
  | [] -> []

let () = 
  let input = (parse_file "res/input.txt") in 
  print_int (List.fold_left (fun acc x -> acc + x) 0 (process_lines input));
