open Printf
open Scanf
open Hashtbl

let possivelMover n x y tabuleiro =
  if x >= 0 && y >= 0 && x < n && y < n && (tabuleiro.(x).(y) = 0) then true
  else false

let numSaltos x y tabuleiro n =
  let saltos = ref 0 in
  let xPos = [|2; 1; -2; -1; 2; 1; -2; -1|] in
  let yPos = [|1; 2; -1; -2; -1; -2; 1; 2|] in


  if not (possivelMover n x y tabuleiro)  then 9 else
    let () =
      for i=0 to 7 do 
        if (possivelMover n (x + xPos.(i)) (y + yPos.(i)) tabuleiro) then
          saltos := !saltos + 1 
      done 
    in
    !saltos

let tudoOcupado tabuleiro n =
  let flag = ref 1 in
  let () =
    for i = 0 to n-1 do
      for j = 0 to n-1 do 
        if tabuleiro.(i).(j) = 0 then flag := 0; 
      done
    done 
  in

  if !flag = 1 then true else false

let posOrdenadas x y xPos yPos tabuleiro n = 
  let pos = ref 1 in
  let auxX = ref 1 in
  let auxY = ref 1 in
  let min = ref 1 in

  for i = 0 to 6 do

    min := numSaltos (x + xPos.(i)) (y + yPos.(i)) tabuleiro n ;

    pos :=i;

    for j = i+1 to 7 do
      if numSaltos (x + xPos.(j)) (y + yPos.(j)) tabuleiro n < !min
      then 
        (
          min := numSaltos (x + xPos.(j)) (y + yPos.(j)) tabuleiro n;
          pos := j)
    done;
    if(!pos <> i) then
      (
        auxX := xPos.(!pos);
        xPos.(!pos) <- xPos.(i);
        xPos.(i) <- !auxX;
        auxY := yPos.(!pos);
        yPos.(!pos) <- yPos.(i);
        yPos.(i) <- !auxY )
  done

let rec solucao tabuleiro n x y =

  let xPos = [|2; 1; -2; -1; 2; 1; -2; -1|] in
  let yPos = [|1; 2; -1; -2; -1; -2; 1; 2|] in

  let rec auxFor i =
    let ans = ref 0 in
    let () =
      if i<8 then
        begin
          if possivelMover n (x+xPos.(i)) (y+yPos.(i)) tabuleiro then
            (
              tabuleiro.(x+xPos.(i)).(y+yPos.(i)) <- 1 ;
              if solucao tabuleiro n (x+xPos.(i)) (y+yPos.(i)) = 1 then ans :=1
              else tabuleiro.(x+xPos.(i)).(y+yPos.(i)) <- 0
            )
        end
    in
    if (!ans =1) || (i = 8) then !ans else auxFor (i+1) 
  in

  posOrdenadas x y xPos yPos tabuleiro n;

  if tudoOcupado tabuleiro n then 1
  else auxFor 0


let () =
  let x1 = ref 0 in
  let y1 = ref 0 in
  let (n,x,y,k) = scanf" %d %d %d %d"(fun a b c d -> (a,b,c,d)) in

  let tabuleiro = Array.make_matrix n n 0 in
  tabuleiro.(x).(y) <- 1;

  let () =
    for i=0 to k-1 do
      x1 := scanf" %d"(fun x -> x);
      y1 := scanf" %d"(fun x -> x);
      tabuleiro.(!x1).(!y1) <- 1;
    done
  in 

  if solucao tabuleiro n x y  = 1 then printf"YES\n" else printf"NO\n"