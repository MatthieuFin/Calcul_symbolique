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

(* Type minimum des modules pouvant 
   servir de coefficients aux polynomes. *)
module type coeff_poly =
sig
  type t = Gdnb.gdnb;;
  val zero : t;;
  val unit : t;;
  val compare : t -> t -> int;;
  val oppose : t -> t
  val somme : t -> t -> t;;
  val div : t -> t -> t * t;;
  val mul : t -> t -> t;;
  val string_of_entier : t -> string;;
  val entier_of_string : string -> t;;
end
  
  
module type SigPolynome =
sig
  type polynome = (int * Gdnb.gdnb) list;;
  type polyStr = (int * string) list
  val poly_of_polyStr : polyStr -> polynome
  val polyStr_of_poly : polynome -> polyStr
  val string_of_poly : polynome -> string
  val pol_is_equal : polynome -> polynome -> bool
  val somme : polynome -> polynome -> polynome
  val difference : polynome -> polynome -> polynome
  val separe : polynome -> int -> polynome * polynome
  val deg : polynome -> int
  val degPair : polynome -> int
  val max : int -> int -> int
  val ajoutDeg : polynome -> int -> polynome
  val mul : polynome -> polynome -> polynome
  val renv : int -> polynome -> polynome
  val pol_mod : polynome -> polynome -> polynome
  val gi : polynome -> float -> polynome
  val split : polynome -> int -> polynome
  val div : polynome -> polynome -> polynome * polynome
  val euclide : polynome -> polynome -> polynome * polynome * polynome
  val pgcd : polynome -> polynome -> polynome
end
  (*
module type SigMakePolynome =
  functor (V : sig type polynome val value : polynome end) ->
sig val value : V.polynome end*)    

module type SigMakePolynome =
sig
  type polynome = (int * Gdnb.gdnb) list;;
  val value : polynome
end
  
(* Polynome a coefficients dans Zn *)
module Polynome (Ent : coeff_poly) (*: SigPolynome*)=
struct
  type polynome = (int * Ent.t) list;;
  module MakePolynome (*: SigMakePolynome*) =
    functor (V : sig type polynome =
			 (int * Ent.t) list val value : polynome end) ->
  struct
    type polynome = V.polynome;;
    let value = V.value;;
  end
  (* Affichage *)
  type polyStr = (int * string) list;;
  
  let poly_of_polyStr (p : polyStr) =
    let rec aux (q : polyStr) (acc : polynome) =
      match q with
	  [] -> List.rev acc
	| (d, c) :: qq ->
	  aux qq ((d, Ent.entier_of_string c)::acc)
    in
    ((aux p []) : polynome);;
  
  let polyStr_of_poly (a : polynome) =
    let rec aux p acc =
      match p with
	  [] -> acc
	| (i,n)::r -> aux r ((i,(Ent.string_of_entier n))::acc)
    in
    ((aux (List.rev a) []) : polyStr);;
  
  let string_of_poly (p : polynome)= 
    let absCoeff (e : Ent.t) =
      if (Ent.compare e Ent.zero) >= 0 then
	Ent.string_of_entier e
      else
	Ent.string_of_entier (Ent.oppose e)
    in
    let signe s c =
      match s, c with 
	  [], i when (Ent.compare i Ent.zero) > 0 -> ""
	| _, i  when (Ent.compare i Ent.zero) < 0 -> " - "
	| _, _                                    -> " + " 
    in
    let rec aux p acc =
      match p with
	  [] -> acc
	|(d, c)::q when d = 0 -> aux q acc^(signe q c)^(absCoeff c)
	|(d, c)::q when ((Ent.compare c Ent.unit) = 0
			|| (Ent.compare c (Ent.oppose Ent.unit)) = 0)
	    && d > 1 ->
	  aux q acc^(signe q c)^"x^"^(string_of_int d)
	|(d, c)::q when ((Ent.compare c Ent.zero) < 0) && d > 1 ->
	  aux q acc^(signe q c)^(absCoeff c)^"x^"^(string_of_int d)
	|(d, c)::q when ((Ent.compare c Ent.zero) > 0) && d > 1 ->
	  aux q acc^(signe q c)^(absCoeff c)^"x^"^(string_of_int d)
	|(d, c)::q when ((Ent.compare c Ent.unit) = 0
			|| (Ent.compare c (Ent.oppose Ent.unit)) = 0) ->
	  aux q acc^(signe q c)^(absCoeff c)^"x"
	|(d, c)::q when (Ent.compare c Ent.zero) < 0 ->
	  aux q acc^(signe q c)^(absCoeff c)^"x"
	|(d, c)::q when (Ent.compare c Ent.zero) > 0 ->
	  aux q acc^(signe q c)^(absCoeff c)^"x"
	|e::q -> aux q acc
    in
    let res = aux p "" in
    if res = "" then "0" else res;;
  
  (* Implentation *)
  
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
      | ((d1,c1)::l1),((d2,c2)::l2)
	when (d1 = d2  && (Ent.compare c1 c2) = 0) -> 
	(pol_is_equal l1 l2)
      | _ -> false
  ;;
  
  let somme (p1 : polynome) (p2 : polynome) = 
    let rec aux p q res = 
      match p, q with
	  [], [] -> ((List.rev res) : polynome)
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
	  []        -> ((List.rev acc) : polynome)
	| (d,c)::rp -> opp rp ((d, Ent.oppose c)::acc)
    in
    somme p1 (opp p2 []);;
  
  
(* kara *)
  
  let separe (p : polynome) (n : int) = 
    let rec aux (p : polynome) (p1 : polynome) (p2 : polynome) = 
      match p with
	  [] -> (((List.rev p1),(List.rev p2)) : (polynome * polynome))
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
	  []        -> ((List.rev p1) : polynome)
	| (d, c)::p -> aux p ((d + n, c)::p1) n
    in aux p [] n
  ;;
  
  let rec mul (p1 : polynome) (p2 : polynome) =
    if (p1 = []) || (p2 = []) then [] else
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
      let c1 = (difference
		  (difference
		     (mul (somme (fst a) (snd a)) (somme (fst b) (snd b)))
		     (c0))
		  (c2) ) in
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
      ((aux (a) []) : polynome)
    else
      failwith "il faut k >= (deg a) !!"
  ;;
  
  
  let pol_mod (p : polynome) (q : polynome) =
    let rec aux acc = function
    [] -> acc
      | (d,c)::l -> 
	if d < (deg q) then
	  (aux ((d,c)::acc) l)
	else 
	  aux acc l
    in
    ((aux [] (List.rev p)) : polynome)
  ;;
  
  let gi (f : polynome) (borne : float) = 
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
  
  let split (a : polynome) (n : int) =
    let rec aux acc p =
      match p with
	  [] -> acc
	| (d,c)::l -> 
	  if d < n then
	    (aux ((d,c)::acc) l)
	  else
	    (aux acc l)
    in
    ((aux [] (List.rev a)) : polynome )
  ;;
  (* @pre (coeff (deg b) b) == 1. *)
  let sub_div (a : polynome) (b : polynome) =
    let m = deg a
    and n = deg b in
    let s = (gi (renv n b) (float_of_int (m - n + 1))) in
    let srA = mul s (renv m a) in
    let rQ = split srA (m-n+1) in
    let q = renv (m-n) rQ in
    let r = (difference a (mul b q)) in
    (q,r)
  ;;
  
  let divCoeff (p : polynome) (coeffDom : Ent.t) =
    let rec aux p acc =
      match p with
	  []    -> acc
	| (d,c)::l ->
	  let qr = (Ent.div c coeffDom) in
	  if (snd qr) = Ent.zero then
	    aux l ((d,(fst (Ent.div c coeffDom)))::acc)
	  else
	    failwith "Polynome a coefficient non entier !"
    in
    aux (List.rev p) []
  ;;
  let mulCoeff (p : polynome) (coeff : Ent.t) = 
    let rec aux p acc =
      match p with
	  []       -> acc
	| (d,c)::l ->
	  aux l ((d,(Ent.mul c coeff))::acc)
    in
    aux (List.rev p) []
  ;;
  
  (*
    Pour que le resultat soit a coefficient entier il faut que
    b soit un polynome unitaire.
    Retourne le quotient et le reste de la division de deux polynomes
    Levée d'exception si le quotient ou le reste n'est pas a coefficient
    entier.
    throws : Exception: Failure "Polynome a coefficient non entier !"
  *)
  let div (a : polynome) (b : polynome) =
    let coeffDom = (coeff (deg b) b) in
    if ((Ent.compare coeffDom Ent.unit) = 0) then
      sub_div a b
    else
      let res = (sub_div (divCoeff a coeffDom) (divCoeff b coeffDom)) in
      (fst res),(mulCoeff (snd res) coeffDom)
  ;;
  
  (* Implantation d'euclide pour le calcul
     du pgcd et de l'inverse de polynomes. *)
  
  let rec euclide (a : polynome) (b : polynome) = 
    if b = [] then 
      (a, [(0,(Ent.entier_of_string "1"))], [])
    else
      let dd = div a b in
      let (dp, xp, yp) = euclide b (snd dd) in
      let (d, x, y) = (dp, yp, difference xp (mul (fst dd) yp)) in
      (d, x, y);;
  

  (**
     Retourne le pgcd de a et de b.
     @param a
     @param b
     @return pgcd(a, b)
  *)
  let pgcd (a : polynome) (b : polynome) =
    if b = [] then
      a
    else
      let (x, y, z) = try (euclide a b) with
	  | Failure "Polynome a coefficient non entier !" -> (euclide b a)
      in
      x;;
(*
  (**
     Calcul l'inverse de a modulo n.
  *)
  let rec inv (a : gdnb) (n : gdnb) =
    if (compare_gdnb a {signe=true;abs=[]} < 0)
    then (inv (somme a n) n) else 
    let (p, u, v) = (euclide a n) in
    match p with
	{signe=true;abs=[(0,1)]} -> u
      | _ -> failwith ((string_of_gdnb a)
                      ^" non inversible dans Z/"
                      ^(string_of_gdnb n)
                      ^"Z");;
  *)
end


module E100 = Entiermod(Entier)(Entier.Make (struct let value = Entier.entier_of_string "100" end));;
module P100 = Polynome(E100);;
(*
open P100;;
let p = [(1,(E100.entier_of_string "3"));(3,(E100.entier_of_string "7"))];;
let pp = renv (deg p) p;;
E100.string_of_entier (P100.coeff 0 p);;
E100.string_of_entier (P100.coeff 3 p);;

E100.string_of_entier (P100.coeff 0 pp);;
E100.string_of_entier (P100.coeff 3 pp);;

(*
let test_euclide = 
    let r = euclide (poly_of_polyStr [0,"3"]) (poly_of_polyStr [0,"9"]) in
    match r with
	a,b,c -> (string_of_poly a),(string_of_poly b),(string_of_poly c);;
*)
let test_euclide = 
    let r = euclide (poly_of_polyStr [(4,"2");(3,"-3");(1,"-3");(0,"-2")]) (poly_of_polyStr [(2,"1");(0,"-1")]) in
    match r with
	a,b,c -> (string_of_poly a),(string_of_poly b),(string_of_poly c);;


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

(* test affichage ... *)
string_of_poly (somme (poly_of_polyStr [(0,"1");(1,"3")]) (poly_of_polyStr [(1,"199")]));;
string_of_poly (somme (poly_of_polyStr [(0,"-3")]) (poly_of_polyStr [(0,"3")]));;






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


*)

(* Test dans Z *)

module Coeff_Z : coeff_poly =
struct
  type t = Gdnb.gdnb;;
  let zero = Gdnb.zero;;
  let unit = Gdnb.unit;;
  let compare = Gdnb.compare_gdnb;;
  let oppose = Gdnb.oppose_gdnb;;
  let somme = Gdnb.somme;;
  let div = Gdnb.div;;
  let mul = Gdnb.mul;;
  let string_of_entier = Gdnb.string_of_gdnb;;
  let entier_of_string = Gdnb.gdnb_of_string;;
end

module P = Polynome(Coeff_Z);;
let p = P.poly_of_polyStr [(0, "-3"); (3, "7")];;
let q = P.poly_of_polyStr [(0, "1"); (6, "-12")];;

P.string_of_poly p;;
P.string_of_poly q;;

P.string_of_poly (P.mul p q);;



