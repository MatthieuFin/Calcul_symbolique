(**
   Module permettant de travailler sur des polynomes a coefficients generiques.
*)
module type coeff_poly =
sig
  type t = Gdnb.Gdnb.gdnb
  (**
     type des coefficients d'un polynome.
  *)
  val zero : t
  (**
     Valeur correspondant a 0.
  *)
  val unit : t
  (**
     Valeur correspondant a 1.
  *)
  val compare : t -> t -> int
  (**
     Params -: a; b
     @return 1 si a > b -1 si b > a 0 si a = b.
  *)
  val oppose : t -> t
  (**
     Params -: a
     @return -a
  *)
  val somme : t -> t -> t
  (**
     Params -: a; b
     @return a + b
  *)
  val div : t -> t -> t * t
  (**
     Params -: a; b
     @return le couple (quotient,reste) de la division entiere de a par b.
  *)
  val mul : t -> t -> t
(**
   Params -: a; b
   @return a * b
*)
  val string_of_entier : t -> string
  (**
     Params -: a
     @return la chaine correspondant a l'entier a.
  *)
  val entier_of_string : string -> t
(**
   Params -: s
   @return l'entier correspondant a la chaine s.
*)
end
(**
   Signature que modules servant de coefficient aux polynomes doivent respecter.
*)
module type SigOperationPolynome =
sig
  type polynome = (int * Gdnb.Gdnb.gdnb) list
  type polyStr = (int * string) list
  val poly_of_polyStr : polyStr -> polynome
  (**
     Params -: ps
     @return le polynome correspondant a ps.
  *)
  val polyStr_of_poly : polynome -> polyStr
  (**
     Params -: p
     @return le polyStr correspondant au polynome p.
  *)
  val string_of_poly : polynome -> string
  (**
     Params -: p
     @return une representation usuel du polynome p.
  *)
  val pol_is_equal : polynome -> polynome -> bool
  (**
     Params -: a; b
     @return a = b
  *)
  val somme : polynome -> polynome -> polynome
  (**
     Params -: a; b
     @return a + b
  *)
  val difference : polynome -> polynome -> polynome
  (**
     Params -: a; b
     @return a - b
  *)
  val deg : polynome -> int
  (**
     Params -: p
     @return le degre du polynome p.
  *)
  val mul : polynome -> polynome -> polynome
  (**
     Params -: a; b
     @return a * b
  *)
  val div : polynome -> polynome -> polynome * polynome
  (**
     Params -: a; b
     @return le couple (quotient,reste) de la division de a par b.
  *)
  val euclide : polynome -> polynome -> polynome * polynome * polynome
  (**
     Params -: a; b
     @return (p,u,v) tels que p = a*u + b*v
  *)
  val pgcd : polynome -> polynome -> polynome
  (**
     Params -: a; b
     @return pgcd(a,b)
  *)
  val inv : polynome -> polynome -> polynome
  (**
     Params -: a; b
     @return l'inverse de a modulo b.
  *)
  val eval_poly : polynome -> string -> string
  (**
     Params -: p; n
     @return l'evaluation du polynome p au point n.
  *)
end
(**
   Signature specifiant les operations disponnibles sur les polynomes.
*)
module type SigMakePolynome =
sig
  type polynome = (int * Gdnb.Gdnb.gdnb) list
  val value : polynome
  (**
     valeur contenue dans le module.
  *)
end
(**
   Module contenant une valeur de type polynome.
*)
module type SigPolynome =
  functor (Coeff : coeff_poly) ->
sig
  type polynome = (int * Coeff.t) list
  type polyStr = (int * string) list
  module MakePolynome :
    functor
      (V : sig
        type polynome = (int * Coeff.t) list
        val value : polynome
      end) ->
  sig type polynome = V.polynome val value : V.polynome end
    (**
       Module contenant une valeur constante d'un polynome.
    *)
  val poly_of_polyStr : polyStr -> polynome
    (**
       Params -: a
       @return le polynome correspondant au polyStr a.
    *)
  val polyStr_of_poly : polynome -> polyStr
    (**
       Params -: a
       @return le polyStr correspondant au polynome a.
    *)
  val string_of_poly : polynome -> string
    (**
       Params -: a
       @return une representation usuel du polynome a.
    *)
  val pol_is_equal : polynome -> polynome -> bool
    (**
       Params -: a; b
       @return a = b
    *)
  val somme : polynome -> polynome -> polynome
    (**
       Params -: a; b
       @return a + b
    *)
  val difference : polynome -> polynome -> polynome
    (**
       Params -: a; b
       @return a - b
    *)
  val deg : polynome -> int
    (**
       Params -: a
       @return le degre de a.
    *)
  val mul : polynome -> polynome -> polynome
    (**
       Params -: a; b
       @return a * b
    *)
  val div : polynome -> polynome -> polynome * polynome
    (**
       Params -: a; b
       @return le couple (quotient,reste) de la division de a par b.
    *)
  val euclide : polynome -> polynome -> polynome * polynome * polynome
  (**
     Params -: a; b
     @return (p,u,v) tels que p = a*u + b*v
  *)
  val pgcd : polynome -> polynome -> polynome
  (**
     Params -: a; b
     @return pgcd(a,b)
  *)
  val inv : polynome -> polynome -> polynome
  (**
     Params -: a; b
     @return l'inverse de a modulo b.
  *)
  val eval_poly : polynome -> string -> string
  (**
     Params -: p; n
     @return l'evaluation du polynome p au point n.
  *)
end
(**
   Signature complete du module Polynome servant a implanter des polynomes a
   coefficients generique.
*)
module Coeff_Z : coeff_poly
(**
   Module permettant d'implanter des coefficients dans Z.
*)
module Polynome : SigPolynome
(**
   Module implantant des polynomes a coefficients generique.
*)



module Polymod :
  functor (P : SigOperationPolynome) ->
    functor (M : SigMakePolynome) ->
sig
  type polynome = P.polynome
  type polyStr = P.polyStr
  val somme : polyStr -> polyStr -> P.polyStr
  (**
     Params -: a; b
     @return a + b (somme dans Zn(X)/P(X)).
  *)
  val mul : polyStr -> polyStr -> P.polyStr
  (**
     Params -: a; b
     @return a * b (multiplication dans Zn(X)/P(X)).
  *)
  val inv : polyStr -> P.polyStr
  (**
     Params -: a
     @return l'inverse de a dans Zn(X)/P(X).
  *)
end
(**
   Module implantant Zn(X)/P(X), P(X) contenu dans M.value.
*)
