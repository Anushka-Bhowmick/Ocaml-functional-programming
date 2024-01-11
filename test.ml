let filter_list lst =
  let rec remove_multiples n acc lst =
    match lst with
    | [] -> List.rev acc
    | hd :: tl ->
        if n mod 2 = 0 || n mod 3 = 0 then
          remove_multiples (n + 1) acc tl
        else
          remove_multiples (n + 1) (hd :: acc) tl
  in

  let length = List.length lst in
  if length mod 10 <> 0 then
    failwith "Error: The list length must be a multiple of 10.";

  remove_multiples 0 [] lst

let () =
  print_endline "Enter a list of integers (separated by spaces):";
  let input_list = List.map int_of_string (String.split_on_char ' ' (read_line ())) in

  try
    let result = filter_list input_list in
    Printf.printf "Filtered List: [%s]\n" (String.concat "; " (List.map string_of_int result))
  with
  | Failure msg -> print_endline msg
