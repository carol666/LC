let rec print_matrix n matrix i j = 

  if i = n then

    Printf.printf "\n"

  else if j = n then

    let () = 

      Printf.printf "\n";

      print_matrix n matrix (i+1) 0

    in ()

  else

    let () = 

      Printf.printf "%2d " matrix.(i).(j);

      print_matrix n matrix i (j+1)

    in ()


let () = 

  (* Reading input *)

  let n = Scanf.scanf " %d" (fun x -> x) in 

  let matrix = Array.make_matrix n n 0 in 

  let (i, j) = Scanf.scanf " %d %d" (fun x y -> x, y) in

  let k = Scanf.scanf " %d" (fun x -> x) in 

  let x = ref 0 in
  let y = ref 0 in

  let () =

    for a = 1 to k do

      x := Scanf.scanf " %d" (fun x -> x);
      y := Scanf.scanf " %d" (fun x -> x);
      matrix.(!x).(!y) <- -1

    done;

  in

  (* --- *)

  let () = Printf.printf "Start: (%d, %d)\n\n" i j in

  print_matrix n matrix 0 0