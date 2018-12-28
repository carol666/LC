open Stream
open Genlex

(* É necessário instalar previamente o camlp4  
   (via o comando: brew install opam)                     *)
(* ocamlc str.cma -pp camlp4o ....... *)
(* ou  melhor ainda:  ocamlopt str.cmxa -pp camlp4o ....... *)

type variavel = string
type formula =
  | Implica of formula*formula
  | Equivale of formula*formula
  | Ou of formula*formula
  | E of formula*formula
  | Nand of formula*formula
  | Nao of formula
  | Var of variavel
  | Verdade
  | Falso

 (*
E  ::= T E'
E' ::= -> T E'
E' ::= <-> T E'
E' ::= \epsilon
T  ::= F T'
T' ::= & F T'
T' ::= | F T'
T' ::= \epsilon
F  ::= N
F  ::= V
F  ::= ! E
F  ::= ( E )
*)


let lexer = Genlex.make_lexer ["("; ")"; "<->"; "->"; "|"; "&" ; "!"; "TRUE"; "FALSE"]

let rec parse_expr = parser (* corresponde a entrada E da gramatica *)
    [< e1 = parse_conj; e = parse_more_imps e1 >] -> e
and parse_more_imps e1 = parser (* corresponde a entrada E' da gramatica *)
    [< 'Kwd "->"; e2 = parse_conj; e = parse_more_imps (Implica(e1, e2)) >] -> e
                       | [< 'Kwd "<->"; e2 = parse_conj; e = parse_more_imps (Equivale(e1, e2)) >] -> e
                       | [< >] -> e1
and parse_conj = parser (* corresponde a entrada T da gramatica *)
    [< e1 = parse_simple; e = parse_more_conjs e1 >] -> e
and parse_more_conjs e1 = parser (* corresponde a entrada T' da gramatica *)
    [< 'Kwd "&"; e2 = parse_simple; e = parse_more_conjs (E(e1, e2)) >] -> e
                        | [< 'Kwd "|"; e2 = parse_simple; e = parse_more_conjs (Ou(e1, e2)) >] -> e
                        | [< >] -> e1
and parse_simple = parser (* corresponde a entrada F da gramatica *)
    [< 'Ident s >] -> Var s
                 | [< 'Kwd "TRUE" >] -> Verdade
                 | [< 'Kwd "FALSE" >] -> Falso
                 | [< 'Kwd "!";  'Kwd "("; e = parse_expr; 'Kwd ")" >] -> Nao e
                 | [< 'Kwd "("; e = parse_expr; 'Kwd ")" >] -> e;;




let parse_expression = parser [< e = parse_expr; _ = Stream.empty >] -> e;;

let formula_of_string s =
  parse_expression
    (lexer
       (Stream.of_string (Str.global_replace (Str.regexp "!") "! " s)
       ))


let read_formula () =  formula_of_string (read_line ())


let rec string_of_formula form =
  match form with
  | Var v          ->  v
  | Verdade        -> "TRUE"
  | Falso          -> "FALSE"
  | Implica(f, g)  -> ("("^ string_of_formula f ^ " -> " ^ string_of_formula g ^")")
  | Equivale(f, g) -> ("("^ string_of_formula f ^ " <-> " ^ string_of_formula g ^")")
  | E(f, g)        -> ("("^ string_of_formula f ^ " & " ^ string_of_formula g ^")")
  | Nand(f,g)      -> ("("^ string_of_formula f ^ " % " ^ string_of_formula g ^")")
  | Ou(f, g)       -> ("("^ string_of_formula f ^ " | " ^ string_of_formula g ^")")
  | Nao (Var v)          ->  ("!("^ v ^")" )
  | Nao f          ->  ("!"^ string_of_formula f)

let input =  read_formula ()


let rec nand_formula form =
  match form with 
  | Var v -> form
  | Verdade -> let z = Var "Z" in Nand(z , Nand(z, z))
  | Falso -> Nand(nand_formula Verdade, nand_formula Verdade)
  | Nao(E(f,g)) -> Nand(nand_formula f, nand_formula g)
  | Nao f -> Nand(nand_formula f, nand_formula f)
  | E (f, g) -> Nand(Nand(nand_formula f, nand_formula g), Nand(nand_formula f, nand_formula g))
  | Nand (f,g) -> Nand(nand_formula f, nand_formula g)
  | Ou (f, g) -> Nand(Nand(nand_formula f, nand_formula f), Nand(nand_formula g, nand_formula g))
  | Implica (f, g) -> nand_formula (Ou(Nao f, g))
  | Equivale (f, g) -> nand_formula (E(Implica(f, g), Implica(g, f)))


let sol = nand_formula input

let () =  Format.printf "%s@."  (string_of_formula sol)
(*
 * Local Variables:
 * compile-command: "ocamlopt str.cmxa -pp camlp4o -o probD esqueletoD.ml"
 * End:
 *)



