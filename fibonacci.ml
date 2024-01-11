let rec fibonacci n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fibonacci (n - 1) + fibonacci (n - 2)

let rec print_fibonacci_sequence n current =
  if current <= n then begin
    Printf.printf "%d " (fibonacci current);
    print_fibonacci_sequence n (current + 1)
  end

let () =
  print_endline "Enter the number of terms for Fibonacci sequence:";
  let n = read_int () in
  print_fibonacci_sequence n 0;
  print_endline ""
