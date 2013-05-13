module type gdnb =
(**
   Interface permettant de travailler sur des grands entiers.
*)
  sig
    val base : int
      (**
	 Base utiliser pour gerer les grands nombres.
      *)
    type gdnb
    (**
       type des grands nombres.
    *)
    val zero : gdnb
      (**
	 valeur de type gdnb representant la valeur 0.
      *)
    val unit : gdnb
      (**
	 valeur de type gdnb representant la valeur 1.
      *)
    val gdnb_of_string : string -> gdnb
      (**
	 Params -: s
	 @return la valeur correspondant a la chaine s.
      *)
    val string_of_gdnb : gdnb -> string
      (**
	 Params -: n
	 @return la chaine correspondant au grand nombre n.
      *)
    val compare_gdnb : gdnb -> gdnb -> int
      (**
	 Params -: a; b
	 @return 1 si a > b -1 si b > a 0 si a = b.
      *)
    val nb_rang : 'a -> ('a * int) list -> int
      (**
	 Params -: n : int ; a : gdnb
	 @return L'entier de puissance n servant a coder le grand nombre n.
      *)
    val soustraction : gdnb -> gdnb -> gdnb
      (**
	 Params -: a; b
	 @return a - b
      *)
    val somme : gdnb -> gdnb -> gdnb
      (**
	 Params -: a; b
	 @return a + b
      *)
    val oppose_gdnb : gdnb -> gdnb
      (**
	 Params -: a : gdnb
	 @return -a
      *)
    val deg : gdnb -> int
      (**
	 Params -: a : gdnb
	 @return la puissance du polynome servant a coder le gdnb n.
      *)
    val mul : gdnb -> gdnb -> gdnb
      (**
	 Params -: a; b
	 @return a * b
      *)
    val puissance : gdnb -> int -> gdnb
      (**
	 Params -: a : gdnb; n : int
	 @return a^n
      *)
    val div : gdnb -> gdnb -> gdnb * gdnb
      (**
	 Params -: a; b
	 @return le couple (quotient,reste) de la division entiere de a par b.
      *)
    val quotient : gdnb -> gdnb -> gdnb
      (**
	 Params -: a; b
	 @return le quotient de la division entiere de a par b.
      *)
    val modulo : gdnb -> gdnb -> gdnb
      (**
	 Params -: a; b
	 @return le reste de la division entiere de a par b.
      *)
    val euclide : gdnb -> gdnb -> gdnb * gdnb * gdnb
      (**
	 Params -: a; b
	 @return (p,u,v) tels que p = a*u + b*v
      *)
    val pgcd : gdnb -> gdnb -> gdnb
      (**
	 Params -: a; b
	 @return pgcd(a,b)
      *)
    val inv : gdnb -> gdnb -> gdnb
      (**
	 Params -: a; b
	 @return l'inverse de a modulo b.
      *)
    val ( $/ ) : string -> string -> string
      (**
	 Params -: a; b
	 @return le quotient de la division entiere de a par b.
      *)
    val ( $% ) : string -> string -> string
      (**
	 Params -: a; b
	 @return le reste de la division entiere de a par b.
      *)
    val ( $$/ ) : string -> string -> string * string
      (**
	 Params -: a; b
	 @return le couple (quotient,reste) de la division entiere de a par b.
      *)
    val ( $+ ) : string -> string -> string
      (**
	 Params -: a; b
	 @return a + b
      *)
    val ( $- ) : string -> string -> string
      (**
	 Params -: a; b
	 @return a + b
      *)
    val ( $* ) : string -> string -> string
      (**
	 Params -: a; b
	 @return a * b
      *)
    val ( $^ ) : string -> int -> string
      (**
	 Params -: a, n
	 @return a^n
      *)
    val ( $< ) : string -> string -> bool
      (**
	 Params -: a; b
	 @return a < b
      *)
    val ( $> ) : string -> string -> bool
      (**
	 Params -: a; b
	 @return a > b
      *)
    val ( $= ) : string -> string -> bool
      (**
	 Params -: a; b
	 @return a = b
      *)
    val ( $<= ) : string -> string -> bool
      (**
	 Params -: a; b
	 @return a <= b
      *)
    val ( $>= ) : string -> string -> bool
      (**
	 Params -: a; b
	 @return a >= b
      *)
  end
(**
   Module type des grands nombres.
*)
module Gdnb : gdnb
(**
   Module implantant les grands nombre et respectant la signature gdnb.
*)
