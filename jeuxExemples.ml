#use "polymod.ml";;


(************************* Exemples de Z/nZ *************************)

(* N3 = module servant pour le modulo de Entiermod *)
module N3 = Entier.Make (
  struct let value = Entier.entier_of_string "3" end);;
(* E3 = Z/3Z *)
module E3 = Entiermod(Entier)(N3);;
(* E5 = Z/5Z *)
module E5 = Entiermod(Entier)(Entier.Make (
  struct let value = Entier.entier_of_string "5" end));;

(* Exemple de somme, multiplication, inverses dans 3Z et 5Z *)

E3.("3" $$+ "1");;
E3.("3" $$* "3");;
E3.inv "2";;

E3.("2" $$* (E3.inv "2"));;

E5.("5" $$+ "0");;

E5.("3" $$+ "2");;
(* string = "0" *)
E5.($$+) "3" "3";;
(* string = "1" *)
E5.inv (E5.("3" $$+ "1324"));;
(* string = "-2" *)

E5.((E5.("3" $$+ "1324")) $$* (E5.inv (E5.("3" $$+ "1324"))));;

E5.("3" $$+ "2");;
E5.($$+) "3" "3";;

(* 100Z *)

module E100 = Entiermod(Entier)(Entier.Make (struct let value = Entier.entier_of_string "100" end));;


(********************** Exemples de Zn[X]/P(X) **********************)

(* Z[X] : polynomes a coefficients entiers *)
module P = Polynome(Coeff_Z);;
let p = P.poly_of_polyStr [(0, "-3"); (3, "7")];;
let q = P.poly_of_polyStr [(0, "1"); (6, "-12")];;

P.string_of_poly p;;
P.string_of_poly q;;

P.string_of_poly (P.mul p q);;


(* 100Z[X] : polynomes a coefficients dans 100Z *)
module P100 = Polynome(E100);;


(* M contient le polynome irreductible x^2 + x + 1 *)
module M = P100.MakePolynome(
  struct 
    type polynome = P100.polynome
    let value = P100.poly_of_polyStr [(0,"1"); (1,"1"); (2,"1")]
  end);;

(* P module de polynomes a coefficient dans 100Z modulo M *)
module P100ModM = Polymod(P100)(M);;
P100ModM.somme [0,"15"] [(1,"11");(2,"22")];;

(* N contient le polynome x^2 + 3x + 1 (non ireductible delta >= 0) *)
module N = P100.MakePolynome(
  struct type polynome = P100.polynome
	 let value = P100.poly_of_polyStr [(0,"1");(1,"3");(2,"1")]
  end);;

module P100ModN = Polymod(P100)(N);;

(* A savoir :
   x4 - x 3 + x2 - x + 8 = (x2 + 3x + 1)(x2 - 4x + 12) - 33x - 4 *)
P100ModN.somme [(0, "7"); (1, "-1"); (2, "1"); (3, "-1"); (4, "1")] [(0, "1")];;

P100ModN.mul [(0, "7"); (1, "-1"); (2, "1"); (3, "-1"); (4, "1")] [(0, "1")];;
