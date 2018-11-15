open Printf
open Scanf
open Hashtbl

let memo = create 97

let rec comb n k =
  try find memo (n,k)
  with Not_found -> 
    if(n = k || k = 0) then (add memo (n,k) 1; 1)
    else let res =(comb (n-1) (k-1))+(comb (n-1) (k)) in (add memo (n,k) res; res)

let sol n k =
  let res = (comb n k)*(comb n (k-1))/n in
  res

let (n,k) = scanf " %d %d" (fun a b -> a,b)
let () = printf "%d\n" (sol n k)
