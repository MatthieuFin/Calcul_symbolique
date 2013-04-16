(***********************************************************************)
(*                                                                     *)
(*                             Projet de CS                            *)
(*                                                                     *)
(* Fichier : polynome.ml                                               *)
(* Auteur : Matthieu Fin                                               *)
(* Date : 16/04/13                                                     *)
(*                                                                     *)
(*                          Licence informatique 2eme année 2012/2013  *)
(*                                  UFR-Sciences et Techniques         *)
(*                                     Université de Rouen             *)
(*                                                                     *)
(***********************************************************************)

(*# use "gdnb.ml";;*)
# use "entiermod.ml";;
(*
module type coeff_poly =
sig
  type t = Gdnb.gdnb;;
  val zero : t;;
  val unit : t;;
  val compare : t -> t -> int;;
  val somme : t -> t -> t;;
  val mul : t -> t -> t;;
  val string_of_entier : t -> string;;
  val entier_of_string : string -> t;;
end
*)

module Polynome (Ent : SigEntiermod) =
struct
  type polynome = (int * Ent.t) list;;
  
  let rec coeff (i : int) (p : polynome) = 
    match p with
	[] -> Ent.zero
      | (deg,coef)::l when deg == i -> coef
      | e::l -> coeff i l
  ;;
  
  let rec pol_is_equal (p1 : polynome) (p2 : polynome) =
    match p1, p2 with
	[], [] -> true
      | ([], _) -> false
      | (_, []) -> false
      | ((d1,c1)::l1),((d2,c2)::l2) when (d1 = d2  && (Ent.compare c1 c2) = 0) -> 
	(pol_is_equal l1 l2)
      | _ -> false
  ;;

let somme (p1 : polynome) (p2 : polynome) = 
  let rec aux p q res = 
    match p, q with
	[], [] -> List.rev res
      | [], e::rq -> aux [] rq (e::res)
      | e::rp, [] -> aux rp [] (e::res)
      | ((dp,cp)::rp), ((dq,cq)::rq) when dp < dq -> 
	aux rp q ((dp,cp)::res)
      | ((dp,cp)::rp), ((dq,cq)::rq) when dp > dq -> 
	aux p rq ((dq,cq)::res)
      | ((dp,cp)::rp), ((dq,cq)::rq) -> 
	let s = (Ent.somme cp cq) in
	if (Ent.compare s Ent.zero) = 0 then
	  aux rp rq res
	else
	  aux rp rq ((dp,s)::res)
  in
  aux p1 p2 []
;;

end

module E5 = Entiermod(Entier)(Entier.Make (struct let value = Entier.entier_of_string "5" end));;
module P5 = Polynome(E5);;
let p = [(0,(E5.entier_of_string "1"));(1,(E5.entier_of_string "2"))];;
