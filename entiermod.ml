(***********************************************************************)
(*                                                                     *)
(*                             Projet de CS                            *)
(*                                                                     *)
(* Fichier : entiermod.ml                                              *)
(* Auteur : Fin Matthieu                                               *)
(* Date : 10/05/13                                                     *)
(*                                                                     *)
(*                          Licence informatique 2eme année 2012/2013  *)
(*                                  UFR-Sciences et Techniques         *)
(*                                     Université de Rouen             *)
(*                                                                     *)
(***********************************************************************)

#load "gdnb.cmo";;
open Gdnb;;

(* Type du module contenant la valeur du modulo *)
module type MakeEntier =
sig
  type t = Gdnb.gdnb
  val value : t
end

(* Type du module qui fourni les fonctions accessible dans Entiermod *)
module type SigEntier =
sig
  module Make (V : sig val value : Gdnb.gdnb end) : MakeEntier
  type t = Gdnb.gdnb
  val zero : t
  val unit : t
  val compare : t -> t -> int
  val string_of_entier : t -> string
  val entier_of_string : string -> t
  val div : t -> t -> t * t
  val mul : t -> t -> t
  val somme : t -> t -> t
  val inv : t -> t -> t
  val oppose : t -> t
end
  
(* Entier Fournit des fonctions sur les gdnb ainsi
   qu'une valeur modulo dans son module Entier.Make *)
module Entier : SigEntier =
struct
type t = Gdnb.gdnb;;
  module Make = functor (V : sig val value : Gdnb.gdnb end) ->
  struct
    type t = Gdnb.gdnb;;
    let value = V.value;;
  end
  let zero = Gdnb.zero;;
  let unit = Gdnb.unit;;
  let compare = Gdnb.compare_gdnb;;
  
  let string_of_entier = Gdnb.string_of_gdnb;;
  let entier_of_string = Gdnb.gdnb_of_string;;
  let div = Gdnb.div;;
  let mul = Gdnb.mul;;
  let somme = Gdnb.somme;;
  let inv = Gdnb.inv;;
  let oppose = Gdnb.oppose_gdnb;;
  
end

module type SigEntiermod =
sig
  type t = Gdnb.gdnb;;
  val zero : t;;
  val unit : t;;
  val somme : t -> t -> t
  val mul : t -> t -> t
  val inv : string -> string
  val ( $$+ ) : string -> string -> string
  val ( $$* ) : string -> string -> string
  val string_of_entier : t -> string
  val entier_of_string : string -> t
  val compare : t -> t -> int
  val oppose : t -> t
  val div : t -> t -> t * t
end


(* Entiermod est parametre par deux modules de type
   SigEntier et MakeEntier. Permettant de creer un module
   representant Z/M.valueZ *)
module Entiermod (E : SigEntier) (M : MakeEntier) : SigEntiermod =
struct
  type t = E.t;;
  let zero = E.zero;;
  let unit = E.unit;;
  let somme (a : t) (b : t) =
    snd (E.div (E.somme a b) M.value);;
  let mul   (a : t) (b : t) =
    snd (E.div (E.mul a b) M.value);;
  let inv   (a : string)              = (E.string_of_entier (E.inv (E.entier_of_string a) M.value));;
  
  let string_of_entier = E.string_of_entier;;
  let entier_of_string = E. entier_of_string;;
  let compare (a : t) (b : t) = E.compare (snd (E.div a M.value)) (snd (E.div b M.value));;
  let oppose = E.oppose;;
  let div (a : t) (b : t) =
    E.div
      (snd (E.div a M.value))
      (snd (E.div b M.value));;
  let ( $$+ ) (a : string) (b : string) = 
    let u = (E.entier_of_string a)
    and v = (E.entier_of_string b)
    in (E.string_of_entier (somme u v));;
  let ( $$* ) (a : string) (b : string) =
    let u = (E.entier_of_string a)
    and v = (E.entier_of_string b)
    in (E.string_of_entier (mul u v));;
end
