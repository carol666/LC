let rec read_list n acc =
  if n>=0 then read_list (n-1) (read_float()::acc) else List.rev acc

let rec to_string degree pol =
  match pol with
  |[] -> ""
  | [el] when el=0. -> ""
  | [el] -> string_of_float el
  | el1::el2::li -> (*Se tiver apenas 2 elementos o 3 é uma lista vazia*)
    let start = if el1=0. then ""
      else
        let coef = if el1 = 1. then "" else (string_of_float el1) in
        let dgr = if degree = 0 then ""
          else if degree = 1 then "x" else "x^"^(string_of_int degree) in
        coef^dgr
    in
    let stop = if el2=0. then "" else " + " in
    start^stop^(to_string (degree-1) (el2::li))

let rec read_poly () =
  let () = Printf.printf "Insira o grau do polinómio:\n" in
  let g = read_int() in
  Printf.printf "Introduza os coeficientes (do maior para o menor) separados por um enter\n";
  g, read_list g [] 


let () = let grau,polinomio = read_poly () in print_endline (to_string grau polinomio)
