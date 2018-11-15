let rec read_list n acc =
  if n >= 0 then read_list (n-1) (read_float()::acc) else List.rev acc

let rec print_list l =
  match l with
    [] -> Printf.printf "\n"
  | h::t -> Printf.printf "%f " h; print_list t

let rec read_poly () =
  let () = Printf.printf "Insira o grau do polinómio:\n" in
  let g = read_int() in
  Printf.printf "Introduza os coeficientes (do maior para o menor) separados por um enter\n";
  read_list g [] 


let rec horner dgr x pol =
