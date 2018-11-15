open Printf

let rec f n =
if n>1 then f (n-1) + f  (n-2) else 1


let rec leitura () =
let () = print_string "Introduza um valor:" in
let v = read_int() in
if v<0 then
let () = printf "O valor tem que ser positivo!\n" in
leitura ()
else v

let valor = leitura ()

let () = printf "Fibonacci de %d é %d\n" valor (f valor)

