open Printf

let rec f x y ant k n picos =
  if (x == 0) && (y == 0) && (picos == 0) then 1
  else if (x <= 0) || (y < 0) || (picos < 0) || (y > (n - k - 1)) then 0
  else if (y == 0) then (f (x - 1) (y + 1) 0 k n picos)
  else if (ant == 0) then (f (x - 1) (y + 1) 0 k n picos) + (f (x - 1) (y - 1) 1 k n (picos - 1))
  else (f (x - 1) (y + 1) 0 k n picos) + (f (x - 1) (y - 1) 1 k n picos)



let n = read_int()
let k = read_int()
let () = printf "n=%d k=%d\nsolucoes:%d\n" n k (f (2 * n) 0 0 k n k)