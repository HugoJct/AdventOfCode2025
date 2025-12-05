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
            then (List.rev (List.map (fun x -> build_range x) acc))
            else (aux l (e::acc))
        end
      | _ -> failwith "empty line not found"
    end in 
  aux lines [];;

let combine_ranges ranges =
  let rec aux ranges acc =
  match ranges with
    | (lowb1, highb1)::(lowb2, highb2)::l -> 
      begin 
        if lowb2 >= lowb1 && highb2 <= highb1 (* if second range inside of first *)
          then aux ((lowb1, highb1)::l) acc
        else if lowb1 >= lowb2 && highb1 <= highb2  (* if first range inside of second *)
          then aux ((lowb2, highb2)::l) acc
        else if lowb1 <= lowb2 && highb1 >= lowb2 
          then aux ((lowb1, highb2)::l) acc
        else if lowb2 <= lowb1 && highb1 >= lowb2 
          then aux ((lowb2, highb1)::l) acc
        else aux ((lowb2, highb2)::l) ((lowb1, highb1)::acc)
      end
    | e::[] -> List.rev (e::acc)
    | [] -> List.rev acc in 
  aux ranges [];;

let () = 
  let input = parse_file "res/input.txt" in 
  let fresh_ids = split_ranges input in
  let sorted_fresh_ids = (List.sort (fun (lowa, _) (lowb, _) -> 
    begin
      if lowa > lowb 
        then 1 
      else if lowb > lowa 
        then -1 
      else 
        0
    end) fresh_ids) in 
  let fresh_ids_combined = (combine_ranges sorted_fresh_ids) in 
  print_int (List.fold_left (fun acc (l, h) -> acc + (h - l + 1) ) 0 fresh_ids_combined) ; print_newline ();;
