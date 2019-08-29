 let fizzbuzz i = match ((i mod 3), (i mod 5)) with
    | (0,0) -> "fizzbuzz"
    | (_,0) -> "buzz"
    | (0,_) -> "fizz"
    | (_,_) -> string_of_int i ;;

 let do_fizzbuzz n = for i = 1 to n do
                        Printf.printf "%s\n" (fizzbuzz i)
                      done ;;