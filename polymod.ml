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
