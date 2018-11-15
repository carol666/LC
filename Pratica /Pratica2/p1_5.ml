open Printf

exception Input_Negativo

let rec leitura () =
  let () = print_string "Introduza um valor:" in
  let v = read_int() in

  if v<0 then
    raise Input_Negativo
  else v

let fib n = 
  let f0 = ref 0 in
  let f1 = ref 0 in
  let temp = ref 0 in
  for v=2 to n do
    temp := !f0;
    f0 := !f1;
    f1 := !temp + !f0
  done;
  !f1;;


let resposta () =
  let valor = leitura () in
  let () = printf "Fibonacci de %d é %d\n", valor, (fib valor)