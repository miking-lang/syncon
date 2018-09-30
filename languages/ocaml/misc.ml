let rec foo n = match n with
  | 0 -> ()
  | _ -> print_endline "foo"; p_int n; bar (n-1)
and bar n = match n with
  | 0 -> ()
  | _ -> print_endline "bar"; p_int n; foo (n-1)

let main =
  let mult = (fun a b -> a * b) in
  let and_bool a b = match a with
    | true -> b
    | false -> false in
  let bools = match and_bool true false with
    | true -> "true"
    | false -> "false" in
  let _strings = match bools with
    | "true" -> print_endline "got true"
    | "false" -> print_endline "got false"
    | e -> print_endline e in
  let num_elements l = match l with
    | _ :: _ :: _ -> "two or more"
    | _ :: _ -> "one or more"
    | _ -> "any number" in
  p_int (mult 2 3);
  print_endline bools;
  print_endline (num_elements (1 :: 2 :: 3 :: []));
  print_endline (num_elements (2 :: 3 :: []));
  print_endline (num_elements (3 :: []));
  print_endline (num_elements ([]));
  foo 10
