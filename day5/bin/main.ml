module IntSet = Set.Make(Int)

let parse_file filepath = 
  let file = open_in filepath in
  let tryread () = 
    try Some(input_line file)
    with End_of_file -> None in 
  let rec get_lines acc = 
    begin 
      match tryread () with
      | Some(e) -> get_lines (e::acc)
      | None -> List.rev acc
    end in 
  get_lines [];;

let build_range line = 
  let split = (String.split_on_char '-' line) in 
  let startb = (List.hd split) |> int_of_string in 
  let endb = (List.nth split 1) |> int_of_string in 
  (startb, endb);;

let split_ranges lines  =
  let rec aux lines acc =
    begin
      match lines with
      | e::l -> 
        begin
          if (String.length e) = 0 
            then ((List.rev (List.map (fun x -> build_range x) acc)),  (List.map (fun x -> int_of_string x) l)) 
            else (aux l (e::acc))
        end
      | _ -> failwith "empty line not found"
    end in 
  aux lines [];;

let () = 
  let input = parse_file "res/input.txt" in 
  let (fresh_ids, values) = split_ranges input in
  print_int (List.fold_left (fun acc x -> 
    begin
      if (List.exists (fun (lowb, highb) -> x >= lowb && x <= highb) fresh_ids)
        then acc + 1 else acc
    end
  ) 0 values) ; print_newline ();;
