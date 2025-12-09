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

let line_to_vals line = 
  match (String.split_on_char ',' line) with
    | x::y::[] -> (x |> int_of_string, y |> int_of_string)
    | _ -> failwith "impossible";;

let area (x1, y1) (x2, y2) =
  ((x2 - x1 + 1) * (y2 - y1 + 1)) |> abs;;

let () = 
  let input = (parse_file "res/input.txt") in 
  let values = List.map line_to_vals input in 
  let max = ref 1 in 
  for i = 0 to (List.length values) - 1
    do begin
      for j = (i+1) to (List.length values) - 1
        do begin
          let area = (area (List.nth values i) (List.nth values j)) in 
          if area > !max then max := area
        end done
    end done ; 
  print_int !max ; print_newline ();
