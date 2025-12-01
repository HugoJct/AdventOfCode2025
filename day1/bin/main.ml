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

let update_dial dial acc (line: string) =
  let rec parts = 
    match (List.of_seq (String.to_seq line)) with
    | e::l -> (e, (String.of_seq (List.to_seq l)))
    | _ -> failwith "wrong format"
  and loop dial acc offset = 
    (* check on offset different from zero to avoid counting it twice 
       when dial = offset = 0
     * - once at the last loop iteration 
     * - once at the first iteration of the next loop
     *)
    let new_acc = if dial = 0 && offset <> 0 then (acc+1) else acc in
    if offset > 0 then loop (modulo (dial + 1) 100) new_acc (offset - 1)
    else if offset < 0 then loop (modulo (dial - 1) 100) new_acc (offset + 1)
    else (dial, (new_acc)) in 
  if (fst parts) = 'L' 
    then loop dial acc (-(int_of_string (snd parts)))
  else if (fst parts) = 'R' 
    then loop dial acc (int_of_string (snd parts))
  else failwith "wrong direction"

let apply_instructions lines =
  let rec loop l dial acc =
  match l with 
  | e::l' -> (
        let updated = (update_dial dial acc e) in 
        let new_dial = (fst updated) and new_acc = (snd updated) in
          loop l' new_dial new_acc
      )
  | [] -> acc in 
  loop lines 50 0;;

let () = 
  let input = (parse_file "res/input.txt") in 
  print_int (apply_instructions input);;
