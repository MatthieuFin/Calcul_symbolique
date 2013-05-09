#use "polynome.ml";;



module type SigPolymod =
sig
  type polynome;;
  type polyStr;;
  val somme : polynome -> polynome -> polynome;;
end

  
(* P est un ens de polynomes a coeff dans Zn 
   et M est un polynome fixé a coeff dans Zn *)
module Polymod (P : SigPolynome) (M : SigMakePolynome) (*: SigPolymod*)=
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
end


module E100 = Entiermod(Entier)(Entier.Make (struct let value = Entier.entier_of_string "100" end));;
module P100 = Polynome(E100);;

(* M contient le polynome irreductible x^2 + x + 1 *)
module M = P100.MakePolynome(struct type polynome = P100.polynome let value = P100.poly_of_polyStr [(0,"1"); (1,"1"); (2,"1")] end);;
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
