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

let () = 
  let regex = Str.regexp {|[ \t]+|} in 
  let input = (List.map 
                (fun x -> (Str.split regex x) |> Array.of_list) 
                (parse_file "res/input.txt")) 
    |> Array.of_list in 
  let total = ref 0 in 
  let j = ref 0 in 
  while !j < (Array.length input.(0))
    do begin
      let result = ref 0 in
      for i = 1 to (Array.length input) - 1
        do begin 
        let operator = input.(0).(!j) in
        if operator = "*"
          then begin
            if operator = "*" && !result = 0 then result := 1 ;
            result := !result * (int_of_string input.(i).(!j))
          end
        else result := !result + (int_of_string input.(i).(!j))
        end done ;
      j := !j + 1;
      total := !total + !result;
    end done;
  print_int !total ; print_newline ();;
