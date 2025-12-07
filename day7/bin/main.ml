let parse_file filepath = 
  let file = open_in filepath in
  let tryread () = 
    try Some (input_line file)
    with End_of_file -> None in 
  let rec get_lines acc = 
    begin 
      match tryread () with
      | Some(e) -> get_lines ((List.of_seq (String.to_seq e))::acc)
      | None -> acc
    end in 
  get_lines [];;

(* Assumes that no line starts nor ends with a split and that two splits cannot *)
(* be adjacent *)
let rec process_lines lines possibilities =
  match lines with
  | e::[] -> 
    begin
      let start_index = 
        begin 
          match (List.find_index (fun x -> x = 'S') e) with
          | Some i -> i
          | None -> failwith "no start" 
        end in 
      possibilities.(start_index) 
    end 
  | e::l -> 
    begin 
      (List.iteri (fun i x -> 
        begin 
          if x = '^'
            then possibilities.(i) <- (possibilities).(i-1) + possibilities.(i+1)
        end) e);
      process_lines l possibilities 
    end
  | [] -> failwith "impossible";;

let () = 
  let input = (parse_file "res/input.txt") in
  print_int (process_lines input (Array.of_list (List.init (List.length (List.hd input)) (fun _ -> 1))) );
  print_newline ();
