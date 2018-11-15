let lst = [1;2;3;4]


let rec list_length l =
  match l with
    [] -> 0
  | h::t -> 1 + list_length t

let rec list_length_tr l acc =
  match l with
  | [] -> acc
  | h::t -> list_length_tr t (1+acc)

let rec read_list n acc =
  if n >= 0 then read_list (n-1) (read_int()::acc) else acc


let () = Printf.printf "Ocaml: %d\nNormal Recursion: %d\nTail Recursion: %d\n" 
    (List.length lst)
    (list_length lst)
    (list_length_tr lst 0)