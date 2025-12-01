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

let modulo x y =
  let modi = x mod y in 
  if modi < 0 
    then modi + y 
  else modi

let update_dial dial (line: string) =
  let parts = 
    match (List.of_seq (String.to_seq line)) with
    | e::l -> (e, (String.of_seq (List.to_seq l)))
    | _ -> failwith "wrong format" in
  if (fst parts) = 'L' 
    then (modulo (dial - (int_of_string (snd parts))) 100) 
  else if (fst parts) = 'R' 
    then (modulo (dial + (int_of_string (snd parts))) 100)
  else failwith "wrong direction"

let apply_instructions lines =
  let rec loop l dial acc =
  match l with 
  | e::l' -> (
        let new_dial = (update_dial dial e) in 
        if new_dial = 0 
          then (loop l' new_dial (acc + 1))
          else (loop l' new_dial acc)
      )
  | [] -> acc in 
  loop lines 50 0;;

let () = 
  let input = (parse_file "res/input.txt") in 
  print_int (apply_instructions input);;
