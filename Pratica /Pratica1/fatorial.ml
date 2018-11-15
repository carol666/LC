open Printf

let rec f n =
  if n<0 then failwith "Error: factorial só para positivos"
  else if n<1 then 1 else n*f(n-1)

let () = print_string "Introduza um valor:"

let valor = read_int()

let () = printf "Factorial de %d e %d\nACABOU\n" valor (f valor)

