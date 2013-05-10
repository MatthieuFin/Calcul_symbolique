(***********************************************************************)
(*                                                                     *)
(*                             Projet de CS                            *)
(*                                                                     *)
(* Fichier : polynome.ml                                               *)
(* Auteurs : Matthieu Fin                                              *)
(*           Poinsot Clement                                           *)
(* Date : 10/05/13                                                     *)
(*                                                                     *)
(*                          Licence informatique 2eme année 2012/2013  *)
(*                                  UFR-Sciences et Techniques         *)
(*                                     Université de Rouen             *)
(*                                                                     *)
(***********************************************************************)

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
  
(* module type contenant uniquement les operation de Polynomes *)
module type SigOperationPolynome =
sig
  type polynome = (int * Gdnb.gdnb) list;;
  type polyStr = (int * string) list
  val poly_of_polyStr : polyStr -> polynome
  val polyStr_of_poly : polynome -> polyStr
  val string_of_poly : polynome -> string
  val pol_is_equal : polynome -> polynome -> bool
  val somme : polynome -> polynome -> polynome
  val difference : polynome -> polynome -> polynome
  val deg : polynome -> int
  val mul : polynome -> polynome -> polynome
  val div : polynome -> polynome -> polynome * polynome
  val euclide : polynome -> polynome -> polynome * polynome * polynome
  val pgcd : polynome -> polynome -> polynome
  val inv : polynome -> polynome -> polynome
  val eval_poly : polynome -> string -> string
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

(* module type complet de Polynome
avec son module integré Polynome.MakePolynome *)
module type SigPolynome = functor (Coeff : coeff_poly) ->
sig
  type polynome = (int * Coeff.t) list
  type polyStr = (int * string) list
  module MakePolynome :
    functor
      (V : sig
        type polynome = (int * Coeff.t) list
        val value : polynome
      end) ->
  sig type polynome = V.polynome val value : V.polynome end
  val poly_of_polyStr : polyStr -> polynome
  val polyStr_of_poly : polynome -> polyStr
  val string_of_poly : polynome -> string
  val pol_is_equal : polynome -> polynome -> bool
  val somme : polynome -> polynome -> polynome
  val difference : polynome -> polynome -> polynome
  val deg : polynome -> int
  val mul : polynome -> polynome -> polynome
  val div : polynome -> polynome -> polynome * polynome
  val euclide : polynome -> polynome -> polynome * polynome * polynome
  val pgcd : polynome -> polynome -> polynome
  val inv : polynome -> polynome -> polynome
  val eval_poly : polynome -> string -> string
end


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

  
(* Polynome a coefficients dans Zn *)
module Polynome : SigPolynome = functor (Ent : coeff_poly) ->
struct
  type polynome = (int * Ent.t) list;;
  module MakePolynome =
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
  
  
(* Karatsuba *)
  
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
  
  (* Division de polynome par la methode de Newton *)
  
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

  (**
     Calcul l'inverse de a modulo n.
  *)
  let inv (a : polynome) (n : polynome) =
    let (p, u, v) = (euclide a n) in
    if p = [(0,Ent.unit)]
    then
      u
    else
      failwith "polynome non inversible !";;

  let eval_poly (p : polynome) (x : string) = 
    let n = deg p in
    let e = (coeff n p) in
    let xx = (Ent.entier_of_string x) in
    let rec aux (i : int) (acc : Ent.t) = 
      if (i >= 0)
      then aux (i-1)
	(Ent.somme (coeff (i) p) (Ent.mul xx acc))
      else (Ent.string_of_entier acc)
    in
    aux (n-1) e
  ;;

end
