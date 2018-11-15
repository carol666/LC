open Printf
open Hashtbl

let memo = create 97


let rec f_aux x y ant k n picos =
  try find memo (x,y,ant,k,n,picos)
  with Not_found ->
    if (((x = 0) && (y = 0) && (picos = k)) || (y = 0 && (x = (2 * (k - picos))))) 
    then (add memo (x,y,ant,k,n,picos) 1; 1)
    else if ((picos = k && x <> y) || picos > k || y > (n - k + 1) || x < y || ((ant == 1) && x < (k - picos) * 2 + y - 1)) 
    then (add memo (x,y,ant,k,n,picos) 0; 0)
    else if (y = 0) 
    then let res = (f_aux (x - 1) (y + 1) 0 (k-picos) n 0) in (add memo (x,y,ant,k,n,picos) res; res)
    else if (ant = 0) 
    then let res = (f_aux (x - 1) (y + 1) 0 k n picos) + (f_aux (x - 1) (y - 1) 1 k n (picos + 1)) in (add memo (x,y,ant,k,n,picos) res; res)
    else let res = (f_aux (x - 1) (y + 1) 0 k n picos) + (f_aux (x - 1) (y - 1) 1 k n picos) in (add memo (x,y,ant,k,n,picos) res; res)


let f n k = f_aux (2 * n) 0 0 k n 0  

let (n,k) = Scanf.scanf " %d %d" (fun a b -> a,b)
let () = printf "n=%d k=%d\nsolucoes:%d\n" n k  (f k n) 