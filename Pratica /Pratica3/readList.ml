let rec print_list l =
  match l with
    [] -> Printf.printf "\n"
  | [e] -> Printf.printf "Ultimo elemento: %d\n" e
  | h::t -> Printf.printf "%d" h; print_list l


let rec read_list n acc =
  if n>=0 then read_list (n-1) (read_int()::acc) else acc


let () = let mylist = read_list 3 [] in print_list mylist
