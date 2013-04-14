(*#use "gdnb.mli";;*)
#use "gdnb.ml";;

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
  let ($$+) (a : string) (b : string) = Gdnb.string_of_gdnb (Em.somme (Gdnb.gdnb_of_string a) (Gdnb.gdnb_of_string b));;
  let ($$*) (a : string) (b : string) = Gdnb.string_of_gdnb (Em.mul (Gdnb.gdnb_of_string a) (Gdnb.gdnb_of_string b));;
end
  
  
module P5 = Entiermod(Gdnb);;

P5.("3" $$+ "2");;
P5.($$+) "3" "3";;
P5.inv (P5.("3" $$+ "1324"));;


