module IntSet = Set.Make(Int)

let parse_file filepath = 
  let file = open_in filepath in
  let tryread () = 
    try Some (input_line file)
    with End_of_file -> None in 
  let rec get_lines acc = 
    begin 
      match tryread () with
      | Some(e) -> get_lines ((List.of_seq (String.to_seq e))::acc)
      | None -> List.rev acc
    end in 
  get_lines [];;

let uniq_cons x xs = if List.mem x xs then xs else x :: xs;;

let rec process_lines lines indices acc =
  let newacc = ref acc in
  match lines with
  | _::[] -> acc
  | e::l -> 
    begin
      let new_indices_opt = (List.mapi (fun i x -> (
        begin
          if x = '^' && (List.mem i indices) 
            then begin
              newacc := !newacc +1 ; 
              Some((i-1)::(i+1)::[])
            end
          else if (List.mem i indices) 
            then Some (i::[])
          else None
        end
        )) e) in 
      let new_indices = (List.fold_right uniq_cons (List.flatten (List.filter_map (fun x -> x) new_indices_opt)) []) in 
      process_lines (List.tl l) new_indices !newacc
    end
  | [] -> acc;;

let () = 
  let input = (parse_file "res/input.txt") in
  let start = 
    begin
      match (List.find_index (fun x -> x = 'S') (List.hd input)) with
      | Some i -> i 
      | None -> failwith "no start"
    end in
  print_int (process_lines (List.tl (List.tl input)) (start::[]) 0);;
