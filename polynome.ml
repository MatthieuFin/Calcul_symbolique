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

(* Polynome a coefficients dans Zn *)
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
  
  let difference (p1 : polynome) (p2 : polynome) =
    let rec opp p acc =
      match p with
	  []        -> List.rev acc
	| (d,c)::rp -> opp rp ((d, Ent.oppose c)::acc)
    in
    somme p1 (opp p2 []);;
  
  
(* kara *)
  
  let separe (p : polynome) n = 
    let rec aux (p : polynome) (p1 : polynome) (p2 : polynome) = 
      match p with
	  [] -> ((List.rev p1),(List.rev p2))
	| (dp,cp)::rp -> 
	  if dp < n/2
	  then
	    aux rp ((dp,cp)::p1) p2
	  else
	    aux rp p1 ((dp - n/2,cp)::p2)
    in
    aux p [] []
  ;;
  
  let deg (pol : polynome) =
    let rec aux p acc =
      match p with
	  [] -> acc
	| (dp,cp)::l when dp > acc -> aux l dp
	| (dp,cp)::l -> aux l acc
    in
    aux pol 0
  ;;
  
  let degPair (pol : polynome) = 
    let d = deg pol in
    if (d mod 2) == 0
    then
      d
    else
      d + 1
  ;;
  
  let max (a : int) (b : int) = 
    if a > b
    then a
    else b
  ;;
  
  let ajoutDeg (p : polynome) (n : int) =
    let rec aux (p : polynome) (p1 : polynome) (n : int) =
      match p with 
	  []        -> List.rev p1
	| (d, c)::p -> aux p ((d + n, c)::p1) n
    in aux p [] n
  ;;
  
  let rec mul (p1 : polynome) (p2 : polynome) =
    let n = max (degPair p1) (degPair p2) in
    if (n < 1) then
      let res = (Ent.mul (coeff n p1) (coeff n p2)) in
      if (Ent.compare res Ent.zero) = 0 then
	[]
      else
	[(n*n,res)]
    else
      let a = (separe p1 n) and b = (separe p2 n) in
      let c0 = mul (fst a) (fst b) and
	  c2 = mul (snd a) (snd b) in
      let c1 = (difference (difference (mul (somme (fst a) (snd a)) (somme (fst b) (snd b))) (c0)) (c2) ) in
      (somme (somme (c0) (ajoutDeg c1 (n/2))) (ajoutDeg c2 n))
  ;;
  
  (* Division de polynome methode de Newton *)
  
  let renv (k : int) (a : polynome) =
    let rec aux p acc =
      match p with
	  [] -> acc
	| (dp,cp)::l -> aux l ((k - dp,cp)::acc)
    in
    if k >= (deg a) then
      aux (a) []
    else
      failwith "il faut k >= (deg a) !!"
  ;;
  
  
  let pol_mod p q =
    let rec aux acc = function
    [] -> acc
      | (d,c)::l -> 
	if d < (deg q) then
	  (aux ((d,c)::acc) l)
	else 
	  aux acc l
    in
    aux [] (List.rev p)
  ;;
  
  let gi f borne = 
    let deux = (Ent.somme Ent.unit Ent.unit) in
    let rec aux i g =
      if (2.**i) >= borne then
	g
      else
	(aux (i +. 1.)
	   (difference (mul [0,deux] g) (mul f (mul g g)))
	)
    in
    aux 0. [(0,Ent.unit)]
  ;;
  
  let split a n =
    let rec aux acc p =
      match p with
	  [] -> acc
	| (d,c)::l -> 
	  if d < n then
	    (aux ((d,c)::acc) l)
	  else
	    (aux acc l)
    in
    aux [] (List.rev a)
  ;;
  
  let div (a : polynome) (b : polynome) =
    let m = deg a
    and n = deg b in
    let s = (gi (renv n b) (float_of_int (m - n + 1))) in
    let srA = mul s (renv m a) in
    let rQ = split srA (m-n+1) in
    let q = renv (m-n) rQ in
    let r = (difference a (mul b q)) in
    (q,r)
  ;;
  
end

module E100 = Entiermod(Entier)(Entier.Make (struct let value = Entier.entier_of_string "100" end));;
module P100 = Polynome(E100);;
open P100;;
let p = [(1,(E100.entier_of_string "3"));(3,(E100.entier_of_string "7"))];;
let pp = renv (deg p) p;;
E100.string_of_entier (P100.coeff 0 p);;
E100.string_of_entier (P100.coeff 3 p);;

E100.string_of_entier (P100.coeff 0 pp);;
E100.string_of_entier (P100.coeff 3 pp);;


let a = [(0,(E100.entier_of_string "3"));(1,(E100.entier_of_string "2"));(3,(E100.entier_of_string "1"))];;
let b = [(1,(E100.entier_of_string "1"));(2,(E100.entier_of_string "1"))];;

let pol = pol_mod (renv 2 b) [(2,(E100.entier_of_string "3"))];;
E100.string_of_entier (P100.coeff 0 pol);;
E100.string_of_entier (P100.coeff 1 pol);;
let d = div a b;;
E100.string_of_entier (P100.coeff 0 (fst d));;
E100.string_of_entier (P100.coeff 1 (fst d));;

E100.string_of_entier (P100.coeff 0 (snd d));;
E100.string_of_entier (P100.coeff 1 (snd d));;


(*  
module E5 = Entiermod(Entier)(Entier.Make (struct let value = Entier.entier_of_string "5" end));;
module P5 = Polynome(E5);;
let p = [(0,(E5.entier_of_string "3"));(1,(E5.entier_of_string "4"))];;
(*open P5;;*)
module E100 = Entiermod(Entier)(Entier.Make (struct let value = Entier.entier_of_string "100" end));;
module P100 = Polynome(E100);;
open P100;;

let p = [(0,(E100.entier_of_string "3"));(3,(E100.entier_of_string "7"))];;
let q = [(0,(E100.entier_of_string "1"));(6,(E100.entier_of_string "-12"))];;

let res = P100.mul p q;;
E100.string_of_entier (P100.coeff 0 res);;
E100.string_of_entier (P100.coeff 3 res);;
E100.string_of_entier (P100.coeff 6 res);;
E100.string_of_entier (P100.coeff 9 res);;

let p = [(0,(E100.entier_of_string "3"));(1,(E100.entier_of_string "2"))];;
let q = [(0,(E100.entier_of_string "1"));(2,(E100.entier_of_string "2"))];;
let res = P100.mul p q;;
E100.string_of_entier (P100.coeff 0 res);;
E100.string_of_entier (P100.coeff 1 res);;
E100.string_of_entier (P100.coeff 2 res);;
E100.string_of_entier (P100.coeff 3 res);;

let p = [(0,(E100.entier_of_string "3"));(1,(E100.entier_of_string "2"))];;
let q = [(2,(E100.entier_of_string "2"))];;
let res = P100.mul p q;;
E100.string_of_entier (P100.coeff 2 res);;
E100.string_of_entier (P100.coeff 3 res);;
*)
