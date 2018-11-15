let lst = [1;2;3;4]

let rec print_list l =
  match l with
    [] -> Printf.printf "\n"
  | [e] -> Printf.printf "Ultimo elemento: %d\n" e
  | h::t -> Printf.printf "%d " h; print_list t


let () = print_list lst 