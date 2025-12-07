let parse_file filepath = 
  let file = open_in filepath in
  let tryread () = 
    try Some (input_line file)
    with End_of_file -> None in 
  let rec get_lines acc = 
    begin 
      match tryread () with
      | Some(e) -> get_lines (e::acc)
      | None -> acc
    end in 
  get_lines [];;

let parse_numbers lines =
  let numbers_grouped = ref [] in 
  let current_group = ref [] in 
  for column = 0 to (Array.length lines.(0)) - 1
    do begin
      let number = Array.make (Array.length lines) ' ' in 
      for line = 0 to (Array.length lines) - 1
        do begin
          number.(line) <- lines.(line).(column)
        end done ;
      try 
        current_group := (number |> Array.to_seq |> String.of_seq |> String.trim |> int_of_string)::(!current_group)
      with Failure _ -> 
        begin
          numbers_grouped := (!current_group)::!numbers_grouped ; 
          current_group := []
        end
    end done ;
    numbers_grouped := (!current_group)::!numbers_grouped ; 
  List.rev !numbers_grouped;;

let () = 
  let input = (parse_file "res/input.txt") in 
  let arr = (List.map (fun x -> (List.of_seq (String.to_seq x)) |> List.rev |> Array.of_list) (List.tl input)) |> List.rev |> Array.of_list in 
  let numbers = (parse_numbers arr) in 
  let regex = Str.regexp {|[ \t]+|} in 
  let operators = (Str.split regex (List.hd input)) |> List.rev in 
  let values = (List.mapi (fun i x -> 
    begin
      match x with
      | "*" -> (List.fold_left (fun acc x -> acc * x) 1 (List.nth numbers i))
      | "+" -> (List.fold_left (fun acc x -> acc + x) 0 (List.nth numbers i))        
      | _ -> failwith "impossible"
    end
  ) operators) in 
  print_int (List.fold_left (fun acc x -> acc + x) 0 values) ; print_newline ();
