open Printf

let rec f n = if n<0 then failwith "Erro: tem que ser positivo"
              else if n>1 then f(n-1)+f(n-2) else 1

let valor = int_of_string Sys.argv.(1)

let () = printf "Fibonacci de %d é %d\n" valor (f valor)
