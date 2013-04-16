(***********************************************************************)
(*                                                                     *)
(*                             Projet de CS                            *)
(*                                                                     *)
(* Fichier : gdnb.mli                                                  *)
(* Auteur : Matthieu Fin                                               *)
(* Date : 16/04/13                                                     *)
(*                                                                     *)
(*                          Licence informatique 2eme année 2012/2013  *)
(*                                  UFR-Sciences et Techniques         *)
(*                                     Université de Rouen             *)
(*                                                                     *)
(***********************************************************************)

module type gdnb =
sig
  val base : int
  type gdnb = { signe : bool; abs : (int * int) list; }
  val gnbr : gdnb
  val gdnb_of_string : string -> gdnb
  val string_of_gdnb : gdnb -> string
  val compare_gdnb : gdnb -> gdnb -> int
  val nb_rang : 'a -> ('a * int) list -> int
  val difference : gdnb -> gdnb -> gdnb
  val som : gdnb -> gdnb -> gdnb
  val somme : gdnb -> gdnb -> gdnb
  val oppose_gdnb : gdnb -> gdnb
  val separe : gdnb -> int -> gdnb * gdnb
  val deg : gdnb -> int
  val degPair : gdnb -> int
  val max : int -> int -> int
  val ajoutDeg : gdnb -> int -> gdnb
  val mul_n : gdnb -> int * int -> gdnb
  val mult : gdnb -> gdnb -> gdnb
  val mul : gdnb -> gdnb -> gdnb
  val puissance : gdnb -> int -> gdnb
  val div : gdnb -> gdnb -> gdnb * gdnb
  val modulo : gdnb -> gdnb -> gdnb
  val euclide : gdnb -> gdnb -> gdnb * gdnb * gdnb
  val pgcd : gdnb -> gdnb -> gdnb
  val inv : gdnb -> gdnb -> gdnb
  val ( $/ ) : string -> string -> string
  val ( $% ) : string -> string -> string
  val ( $$/ ) : string -> string -> gdnb * gdnb
  val ( $+ ) : string -> string -> string
  val ( $- ) : string -> string -> string
  val ( $* ) : string -> string -> string
  val ( $^ ) : string -> int -> string
  val ( $$* ) : string -> string -> string
  val ( $< ) : string -> string -> bool
  val ( $> ) : string -> string -> bool
  val ( $= ) : string -> string -> bool
  val ( $<= ) : string -> string -> bool
  val ( $>= ) : string -> string -> bool
end
