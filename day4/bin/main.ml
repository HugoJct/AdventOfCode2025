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

let list_to_array l = 
  let border = (Array.make ((List.length l) + 2) '$')::[] in 
  let data = (List.map (fun x -> ("$"^x^"$") |> String.to_seq |> Array.of_seq ) l) in  
  border@data@border
  |> List.to_seq 
  |> Array.of_seq;;

let check_spot x y arr = 
  let count = ref 0 in
  for i = (-1) to 1 
    do begin
      for j = (-1) to 1
        do begin
          if i = 0 && j = 0 
            then ()
          else if arr.(x+i).(y+j) = '@' 
            then count := (!count + 1)
        end done
    end done ; 
  (!count < 4);;

let process arr = 
  let count = ref 0 in
  for i = 1 to (Array.length arr) - 2
    do begin
      for j = 1 to (Array.length arr) - 2
        do begin
          if arr.(i).(j) <> '@' then ()
          else if (check_spot i j arr) then count := (!count + 1)
        end done
    end done ;
  (!count);;

let () = 
  let input = (parse_file "res/input.txt") in
  let arr = (list_to_array input) in 
  print_int (process arr) ; print_newline ();;
