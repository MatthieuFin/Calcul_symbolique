#use "polynome.ml";;


  
(* P est un ens de polynomes a coeff dans Zn 
   et M est un polynome fixé a coeff dans Zn *)
module Polymod (P : SigOperationPolynome) (M : SigMakePolynome) =
struct
  type polynome = P.polynome;;
  type polyStr = P.polyStr;;
  let somme (a : polyStr) (b : polyStr) =
    P.polyStr_of_poly
      (snd (P.div
	      (P.somme
		 (P.poly_of_polyStr a)
		 (P.poly_of_polyStr b))
	      M.value));;
  let mul (a : polyStr) (b : polyStr) =
    P.polyStr_of_poly
      (snd (P.div
	      (P.mul
		 (P.poly_of_polyStr a)
		 (P.poly_of_polyStr b))
	      M.value));;
  let inv (a : polyStr) =
      P.polyStr_of_poly (P.inv (P.poly_of_polyStr a) M.value);;
end


module E100 = Entiermod(Entier)(Entier.Make (struct let value = Entier.entier_of_string "100" end));;
module P100 = Polynome(E100);;

(* M contient le polynome irreductible x^2 + x + 1 *)
module M = P100.MakePolynome(
  struct 
    type polynome = P100.polynome
    let value = P100.poly_of_polyStr [(0,"1"); (1,"1"); (2,"1")]
  end);;
(* P module de polynomes a coefficient dans 100Z modulo M *)
module P100ModM = Polymod(P100)(M);;
open P100ModM;;
somme [0,"15"] [(1,"11");(2,"22")];;

(* N contient le polynome x^2 + 3x + 1 (non ireductible delta >= 0) *)
module N = P100.MakePolynome(
  struct type polynome = P100.polynome
	 let value = P100.poly_of_polyStr [(0,"1");(1,"3");(2,"1")]
  end);;

module P100ModN = Polymod(P100)(N);;
(* A savoir :
   x4 - x 3 + x2 - x + 8 = (x2 + 3x + 1)(x2 - 4x + 12) - 33x - 4 *)
P100ModN.somme [(0, "7"); (1, "-1"); (2, "1"); (3, "-1"); (4, "1")] [(0, "1")];;


module E4 = Entiermod(Entier)(Entier.Make (struct let value = Entier.entier_of_string "4" end));;

module P4 = Polynome(E4);;
module Q = P4.MakePolynome(
  struct type polynome = P4.polynome
	 let value = P4.poly_of_polyStr [(0,"1");(1,"2")]
  end);;
module P4ModQ = Polymod(P4)(Q);;

open P4ModQ;;
