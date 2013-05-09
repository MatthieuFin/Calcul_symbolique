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
    P.polyStr_of_poly (snd (P.div (P.somme (P.poly_of_polyStr a) (P.poly_of_polyStr b)) M.value));;
(*  (P.somme (P.poly_of_polyStr a) (P.poly_of_polyStr b));;*)
end


module E100 = Entiermod(Entier)(Entier.Make (struct let value = Entier.entier_of_string "100" end));;
module P100 = Polynome(E100);;

module M = P100.MakePolynome(struct type polynome = P100.polynome let value = P100.poly_of_polyStr [(0,"1"); (1,"1"); (2,"1")] end);;
module P = Polymod(P100)(M);;
open P;;
somme [0,"15"] [(1,"11");(2,"22")];;
