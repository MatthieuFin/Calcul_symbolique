#use "gdnb.ml";;


module type MakeEntier =
sig
  type t = Gdnb.gdnb
  val value : t
end

module Entier =
struct
  type t = Gdnb.gdnb;;
  module Make = functor (V : sig val value : Gdnb.gdnb end) ->
    struct
      type t = Gdnb.gdnb;;
      let value = V.value;;
    end
  let string_of_entier = Gdnb.string_of_gdnb;;
  let entier_of_string = Gdnb.gdnb_of_string;;
  let div = Gdnb.div;;
  let mul = Gdnb.mul;;
  let somme = Gdnb.somme;;
  let inv = Gdnb.inv;;
end

module Entiermod = functor (M : MakeEntier) ->
struct
  open Entier
  type t = M.t;;
  let somme (a : string) (b : string) =
    (Entier.string_of_entier (snd (Entier.div (Entier.somme (Entier.entier_of_string a) (Entier.entier_of_string b)) M.value)))
  let mul   (a : string) (b : string) =
    (Entier.string_of_entier (snd (Entier.div (Entier.mul (Entier.entier_of_string a) (Entier.entier_of_string b)) M.value)));;
  let inv   (a : string)              = (Entier.string_of_entier (Entier.inv (Entier.entier_of_string a) M.value));;
  let ( $$+ ) = somme;;
  let ( $$* ) = mul;;
end

module N3 = Entier.Make (struct let value = Entier.entier_of_string "3" end);;
module P3 = Entiermod(N3);;
module P5 = Entiermod(Entier.Make (struct let value = Entier.entier_of_string "5" end));;

P3.("3" $$+ "3");;
P5.("3" $$+ "3");;

P5.("3" $$+ "2");;
(* string = "0" *)
P5.($$+) "3" "3";;
(* string = "1" *)
P5.inv (P5.("3" $$+ "1324"));;
(* string = "-2" *)



(*
module Entiermod = functor (M : MakeEntier) ->
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
