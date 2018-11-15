open Printf


let rec leitura () =
  let () = print_string "Introduza um valor:" in
  let v = read_int() in
  if v<0 then
    let () = printf "O valor tem que ser positivo!\n" in
    leitura ()
  else v

let valor = leitura ()

let f0 = ref 1
let f1 = ref 1
let temp = ref 0

    for v=2 to valor do
      temp := !f0
          f0 := !f1
          f1 := !temp + !f0
    done;
  !f1;;


let () = printf "Fibonacci de %d é %d\n" valor f1 

    try 


