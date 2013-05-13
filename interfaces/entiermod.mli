(**
   Module permettant d'implanter Z/nZ.
*)
module type MakeEntier =
sig
  type t = Gdnb.Gdnb.gdnb
  val value : t
(**
  Valeur du module. 
*)
end
(**
   Module contenant une valeur de type t.
*)
module type SigEntier =
  sig
    module Make :
      functor (V : sig val value : Gdnb.Gdnb.gdnb end) -> MakeEntier
	(**
	   Module contenant une valeur entiere.
	*)
    type t = Gdnb.Gdnb.gdnb
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
    val somme : t -> t -> t
    (**
       Params -: a; b
       @return a + b
    *)
    val inv : t -> t -> t
      (**
	 Params -: a; b
	 @return retourne l'inverse de a modulo b.
      *)
    val oppose : t -> t
      (**
	 Params -: a : gdnb
	 @return -a
      *)
  end
(**
   Signature des modules implantant des entiers.
*)
module Entier : SigEntier
(**
   Module implantant des entiers.
*)
module type SigEntiermod =
  sig
    type t = Gdnb.Gdnb.gdnb
    val zero : t
    (**
       Valeur correspondant a 0.
    *)
    val unit : t
    (**
       Valeur correspondant a 1.
    *)
    val somme : t -> t -> t
    (**
       Params -: a; b
       @return a + b (somme dans Z/nZ)
    *)
    val mul : t -> t -> t
    (**
       Params -: a; b
       @return a * b (multiplication dans Z/nZ)
    *)
    val inv : string -> string
    (**
       Params -: a
       @return l'inverse de a dans Z/nZ
    *)
    val ( $$+ ) : string -> string -> string
    (**
       Params -: a; b
       @return a + b (somme dans Z/nZ)
    *)
    val ( $$* ) : string -> string -> string
    (**
       Params -: a; b
       @return a * b (multiplication dans Z/nZ)
    *)
    val string_of_entier : t -> string
    (**
       Params -: a
       @return la chaine corresspondant a l'entier a.
    *)
    val entier_of_string : string -> t
    (**
       Params -: s
       @return l'entier correspondant a la chaine s.
    *)
    val compare : t -> t -> int
    (**
       Params -: a; b
       @return 1 si a > b -1 si b > a 0 si a = b
    *)
    val oppose : t -> t
    (**
       Params -: a
       @return -a
    *)
    val div : t -> t -> t * t
  (**
     Params -: a; b
     @return le couple (quotient,reste) de la division entiere de a par b.
  *)
  end
(**
   Signature des modules implantant Z/nZ
*)
module Entiermod :
  functor (E : SigEntier) -> functor (M : MakeEntier) -> SigEntiermod
(**
   Module implantant Z/nZ, la valeur de n etant contenue dans M.value.
*)
