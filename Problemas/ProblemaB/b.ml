let rec cavaleiro tabuleiro n x y =
  let xPos = [|2; 1; -2; -1; 2; 1; -2; -1|] in
  let yPos = [|1; 2; -1; -2; -1; -2; 1; 2|] in


  let rec auxFor i =
    if i<8 then
      let () =
        if possivelMover n (x+xPos.(i)) (y+yPos.(i)) tabuleiro then
          tabuleiro.(x+xPos.(i)).(y+yPos.(i)) <- 1;
        if cavaleiro tabuleiro n (x+xPos.(i)) (y+yPos.(i)) then
          true
        else
          tabuleiro.(x+xPos.(i)).(y+yPos.(i)) <- 0
      in auxFor (i+1)
    else false
  in


  let () = posOrdenadas x y xPos yPos tabuleiro n in

  auxFor 0;
  false