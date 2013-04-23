(***********************************************************************)
(*                                                                     *)
(*                             Projet de CS                            *)
(*                                                                     *)
(* Fichier : entiermod.ml                                              *)
(* Auteur : Matthieu Fin                                               *)
(* Date : 16/04/13                                                     *)
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
  val zero : Gdnb.gdnb
  val unit : Gdnb.gdnb
  val compare : Gdnb.gdnb -> Gdnb.gdnb -> int
  val string_of_entier : Gdnb.gdnb -> string
  val entier_of_string : string -> Gdnb.gdnb
  val div : Gdnb.gdnb -> Gdnb.gdnb -> Gdnb.gdnb * Gdnb.gdnb
  val mul : Gdnb.gdnb -> Gdnb.gdnb -> Gdnb.gdnb
  val somme : Gdnb.gdnb -> Gdnb.gdnb -> Gdnb.gdnb
  val inv : Gdnb.gdnb -> Gdnb.gdnb -> Gdnb.gdnb
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
  
  let ( $$+ ) (a : string) (b : string) = 
    let u = (E.entier_of_string a)
    and v = (E.entier_of_string b)
    in (E.string_of_entier (somme u v));;
  let ( $$* ) (a : string) (b : string) =
    let u = (E.entier_of_string a)
    and v = (E.entier_of_string b)
    in (E.string_of_entier (mul u v));;
end


(* Syntax pour creer des Entiermod : *)

(* N3 = module servant pour le modulo de Entiermod *)
module N3 = Entier.Make (struct let value = Entier.entier_of_string "3" end);;
(* P3 = Z/3Z *)
module P3 = Entiermod(Entier)(N3);;
(* P5 = Z/5Z *)
module P5 = Entiermod(Entier)(Entier.Make (struct let value = Entier.entier_of_string "5" end));;

P3.("3" $$* "3");;

P5.("5" $$+ "0");;

P5.("3" $$+ "2");;
(* string = "0" *)
P5.($$+) "3" "3";;
(* string = "1" *)
P5.inv (P5.("3" $$+ "1324"));;
(* string = "-2" *)



(*
module Entiermod = functor (Gdnb : gdnb) ->
struct
  module Em = 
  struct
    let n = Gdnb.gdnb_of_string "5";;
    let somme (a : Gdnb.gdnb) (b : Gdnb.gdnb) = snd (Gdnb.div (Gdnb.somme a b) n);;
    let mul (a : Gdnb.gdnb) (b : Gdnb.gdnb) = snd (Gdnb.div (Gdnb.mul a b) n);;
    let inv (a : Gdnb.gdnb) = Gdnb.inv a n;;
  end
  
  
  let somme = Em.somme;;
  let mul = Em.mul;;
  let inverse = Em.inv;;
  let inv (a : string) = (Gdnb.string_of_gdnb (Em.inv (Gdnb.gdnb_of_string a)));;
  let ( $$+ ) (a : string) (b : string) = Gdnb.string_of_gdnb (Em.somme (Gdnb.gdnb_of_string a) (Gdnb.gdnb_of_string b));;
  let ( $$* ) (a : string) (b : string) = Gdnb.string_of_gdnb (Em.mul (Gdnb.gdnb_of_string a) (Gdnb.gdnb_of_string b));;
end


module P5 = Entiermod(Gdnb);;

P5.("3" $$+ "2");;
P5.($$+) "3" "3";;
P5.inv (P5.("3" $$+ "1324"));;

*)
