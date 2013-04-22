(***********************************************************************)
(*                                                                     *)
(*                             Projet de CS                            *)
(*                                                                     *)
(* Fichier : gdnb.ml                                                   *)
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
  val zero : gdnb
  val unit : gdnb
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
  val ( $$/ ) : string -> string -> string * string
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

module Gdnb : gdnb = 
struct
  
(* TP_CS sur les Grands nombre *)
  open String;;
  
(*let ($+) (a : string) (b : string) = string_of_gdnb (somme (gdnb_of_string a) (gdnb_of_string b));;*)
  
(**
   Base de codage des grand nombres.
*)
  let base = 10000;;
(**
   Type gdnb. Permettant de travailler sur les grands entiers.
*)
  type gdnb = {
    signe : bool;
    abs : (int*int)list
  };;
  
  let zero = {signe = true; abs = [];};;
  let unit = {signe = true; abs = [(0,1)];};;
  
(**
   Params - s : string (composé d'une suite de chiffre avec eventuellement un '+' ou un '-' en début de chaine).
   @return n : gdnb correspondant a la chaine s
*)
  let gdnb_of_string s =
    let signe_present s =
      (match get s 0 with
	  '-' -> true
	| '+' -> true
	| _ -> false)
    in
    let rec aux nb acc b sgn = 
      let n = (if length nb >= 4 then sub nb ((length nb) - 4) 4 else nb) in
      let add = (if int_of_string n = 0 then [] else [(b,(int_of_string n))])
      in
      (if length nb > 4
       then
	  aux (sub nb (0) ((length nb)-(4))) (acc@add) (b+1) sgn
       else
	  {signe = sgn; abs = (acc@add);})
    in
    let nbabs = sub s 1 ((length s)-1) in
    if signe_present s
    then
      (if get s 0 = '-'
       then
	  aux nbabs [] 0 false
       else 
	  aux nbabs [] 0 true)
    else 
      aux s [] 0 true
  ;;
  
(**
   Params - n : gdnb.
   @return une string correspondant au grand nombre n.
*)
  let string_of_gdnb n = 
    let chiffre_to_string n base =
      let rec aux ch b acc =
	if (b/10) > 0 && (ch*10) < base
	then 
	  aux (ch*10) (b/10) "0"^acc
	else
	  acc
      in
      aux n (base/10) (string_of_int n)
    in
    let rec aux nb acc i = 
      match nb with
	  {signe = s; abs = []} -> acc
	| {signe = s; abs = (e::l)}  when ((fst e) = (i)) ->
	  (match l with
	      [] -> 
		let signe = if s = false then "-" else "" in
		aux ({signe = s; abs = (l)}) (signe^((string_of_int (snd e))^acc)) (i+1)
	    | _  -> aux ({signe = s; abs = (l)}) ((chiffre_to_string (snd e) base)^acc) (i+1)
	  )
	| {signe = s; abs = (e::l)} -> 
	  aux ({signe = s; abs = (e::l)}) ((chiffre_to_string (0) base)^acc) (i+1)
    in
    if n.abs = [] then "0" else aux n "" 0
  ;;
  
(**
   Params - n1 : gdnb ; n2 : gdnb.
   @return -1 si n1 < n2
   0 si n1 = n2
   1 sinon
*)
  let compare_gdnb n1 n2 = 
    let a = {signe = n1.signe; abs= (List.rev n1.abs)}
    and b = {signe = n2.signe; abs= (List.rev n2.abs)} in
    if a < b then -1 else (if a = b then 0 else 1)
  ;;
  
(**
   Params - n : int ; l : (int*int)list.
   @return le coefficient correspondant au degre n
*)
  let nb_rang n l = 
    let rec aux li = 
      match li with
	  []    -> 0
	| e::lli when (fst e) = n -> snd e
	| e::lli -> aux lli
    in aux l
  ;;
  
  
  (*
    on suppose |p1| >= |p2|
  *)
  (**
     Param - p1 : gdnb ; p2 : gdnb.
     Precondition - |p1| >= |p2|.
     @return |p1| - |p2| : gdnb
  *)
  let difference (p1 : gdnb) (p2 : gdnb) = 
    let rec aux p q r res = 
      match p, q with
          [], [] -> List.rev res
	| [], e::rq -> aux [] rq r (e::res)
	| e::rp, [] -> aux rp [] r (e::res)
	| ((dp,cp)::rp), ((dq,cq)::rq) when dp < dq -> aux rp q 0 ((dp, cp)::res)
	  
	| ((dp,cp)::rp), ((dq,cq)::rq) when dp > dq -> aux p rq 0 res
	  
	| ((dp,cp)::rp), ((dq,cq)::[]) -> let di = (cp - cq - r) in
					  if (di >= 0) then 
					    if (di = 0) then
					      aux rp [] 0 res
					    else
					      aux rp [] 0 ((dp, di)::res)
					  else 
					    aux rp [(dp + 1, 1)] 0 ((dp, base + di)::res)
					      
	| ((dp,cp)::rp), ((dq,cq)::rq) -> let di = (cp - cq - r) in
					  if (di >= 0) then 
					    if (di = 0) then
					      aux rp rq 0 res
					    else
					      aux rp rq 0 ((dp, di)::res)
					  else 
					    aux rp rq 1 ((dp, base + di)::res)					    
    in
    let complete p =
      let rec aux q deg acc =
	match q with
	    (d,c)::qq when deg = d -> acc@q
	  |_ -> aux q (deg + 1) (acc@[(deg, 0)])
      in
      aux p 0 []
    in
    
    let arg = if ((compare_gdnb p2 p1) > 0) then (p2, p1) else (p1, p2) in
    let a = (fst arg) and b = (snd arg) in
    if (a.signe = b.signe) then
      {signe = a.signe; abs = aux (complete a.abs) b.abs 0 []}
    else
      {signe = true; abs = aux (a.abs) (b.abs) 0 []};;
  
(**
   Params - (p1 : gdnb) (p2 : gdnb).
   Precondition : |p1| > |p2|.
   @return p1 + p2 : gdnb
*)
  let som (p1 : gdnb) (p2 : gdnb) = 
    let rec aux p q r rg_ret res = 
      match p, q with
          [], [] -> List.rev res
	| [], e::rq -> aux [] rq 0 (rg_ret + 1) (e::res)
	| e::rp, [] -> aux rp [] 0 (rg_ret + 1) (e::res)
	| ((dp,cp)::rp), ((dq,cq)::rq) when dp < dq -> aux rp q 0 (rg_ret + 1) ((dp,cp)::res)
	| ((dp,cp)::rp), ((dq,cq)::rq) when dp > dq -> aux p rq 0 (rg_ret + 1) ((dq,cq)::res)
      (* dp = dq *)
	| ((dp,cp)::rp), ((dq, cq)::rq) when rg_ret < dp && r != 0 -> aux p q 0 (rg_ret + 1) ((rg_ret, 1)::res)
      (* cas 2 du cour *)
	| ((dp,cp)::rp), ((dq,cq)::[]) -> let di = if (rg_ret = dp) then (cp + cq + r) else (cp + cq) in
					  if (rg_ret = dp) then 
					    if (di >= base) then 
					      if (di - base > 0) then 
						aux rp [(dp + 1, 1)] 0 (rg_ret + 1) ((dp, di - base)::res)
					      else
						aux rp [(dp + 1, 1)] 0 (rg_ret + 1) res
					    else 
					      aux rp [] 0 (rg_ret + 1) ((dp, di)::res)
					  else 
					    aux rp [] 0 (rg_ret + 1) ((dp, di)::res)
      (* cas 1 du cour *)
	| ((dp,cp)::rp), ((dq,cq)::rq) -> let di = if (rg_ret = dp) then (cp + cq + r) else (cp + cq) in
					  if (di >= base) then 
					    if (di - base > 0) then
					      aux rp rq 1 (rg_ret + 1) ((dp, di - base)::res)
					    else
					      aux rp rq 1 (rg_ret + 1) res
					  else 
					    aux rp rq 0 (rg_ret + 1) ((dp, di)::res)
					      
					      
    in
    let args = if ((compare_gdnb p1 p2) > 0) then (p1,p2) else (p2,p1) in
    let a = (fst args) and b = (snd args) in
    if a.abs = [] && b.abs = [] then {signe=true;abs=[]} else
      if a.signe = b.signe then
	{signe = a.signe; abs=(aux p1.abs p2.abs 0 0 [])}
      else
	let x = {signe = true; abs = a.abs} and y = {signe = true; abs=b.abs} in
	let res = if ((compare_gdnb x y) > 0) then 
	    (a.signe, difference x y)
	  else
	    (b.signe, difference y x)
	in 
	if (snd res).abs = [] then
	  {signe = true; abs = (snd res).abs}
	else
	  {signe = (fst res); abs = (snd res).abs}
  ;;

  (**
     Params - (p1 : gdnb) (p2 : gdnb).
     @return p1 + p2 : gdnb
  *)
  let somme (a : gdnb) (b : gdnb) = 
(*    if (List.length a.abs) > (List.length b.abs) then*)
    if (compare_gdnb a b) > 0 then
      som a b
    else
      som b a;;
  
  (**
     Params - n : gdnb.
     @return -n : gdnb
  *)
  let oppose_gdnb (n : gdnb) = {signe = not n.signe; abs = n.abs};;
  
  
  
  (* Karatsuba *)
  
  (**
     Retourne le coupe des grands nombres de la separation de ga.
     Params - ga : gdnb.
     @return : le couple gdnb * gdnb
  *)  
  let separe (ga : gdnb) n = 
    let rec aux p p1 p2 = 
      match p with
	  [] -> {signe = ga.signe; abs = (List.rev p1)},{signe = ga.signe; abs = (List.rev p2)}
	| (dp,cp)::rp -> 
	  if dp < n/2
	  then
	    aux rp ((dp,cp)::p1) p2
	  else
	    aux rp p1 ((dp - n/2,cp)::p2)
    in
    aux ga.abs [] []
  ;;
  
(**
   Retourne le degre maximum de ga.
   Params - ga : gdnb.
   @return le degre maximum de ga
*)
  let deg (ga : gdnb) =
    let rec aux p acc =
      match p with
	  [] -> acc
	| (dp,cp)::l when dp > acc -> aux l dp
	| (dp,cp)::l -> aux l acc
    in
    aux ga.abs 0
  ;;
  
(**
   Params - ga : gdnb.
   @return (deg a) si (deg a) est pair (deg a) + 1 sinon
*)
  let degPair (ga : gdnb) = 
    let d = deg ga in
    if (d mod 2) == 0
    then
      d
    else
      d + 1
  ;;
  
(**
   Params - (a : int) ; (b : int).
   @return le maximum de a et b
*)
  let max (a : int) (b : int) = 
    if a > b
    then a
    else b
  ;;
  
  (**
     Ajoute n a chaque degre de ga (ce qui revient a multiplier ga par base^n).
     Params - (ga : gdnb) ; (n : int).
     @return ga * base^n
  *)
  let ajoutDeg (ga : gdnb) (n : int) =
    let rec aux p p1 n = match p with 
	[]        -> {signe = ga.signe; abs = List.rev p1}
      | (d, c)::p -> aux p ((d+n, c)::p1) n
    in aux ga.abs [] n
  ;;

  (* reste plus qu'a faire la multiplication ;) *)
(*
  let rec coeff i p = 
    match p with
	[] -> 0;
      | (deg,coef)::l when deg == i -> coef
      | e::l -> coeff i l
  ;;
*)
  let mul_n a b =
    let rec aux m n r deg acc =
      match m,n with
	  [],_ when r = 0 -> {signe = a.signe; abs = (List.rev acc)}
	| [],_ -> aux m n 0 (deg+1) ((deg+1,r)::acc)
	| (exp,v)::aa,(e2,v2) -> let d = v * v2 + r in
				 if (d < base) then
				   aux aa b 0 (exp+e2) ((exp+e2,d)::acc)
				 else
				   aux aa b (d / base) (exp+e2) ((exp+e2,d mod base)::acc)
    in aux a.abs b 0 0 [];;

  let mult a b = 
    let s = (a.signe == b.signe) in
    let rec aux b acc =
      match b with
	  [] -> acc
	| (e,v)::bb -> aux bb (somme acc (mul_n a (e,v)))
    in aux b.abs {signe = s; abs = []};;
  
(*
  let rec mul p1 p2 =
    (*let sgn = (p1.signe && p2.signe) || ((not p1.signe) && (not p2.signe)) in*)
    let n = max (degPair p1) (degPair p2) in
    if (n < 1) then
      (*let res = (coeff n p1.abs) * (coeff n p2.abs) in
      if (res = 0) then
	{signe=sgn; abs=[]}
      else*)
	mult p1 p2
    else
      let a = (separe p1 n) and b = (separe p2 n) in
      let c0 = mul (fst a) (fst b) and
	  c2 = mul (snd a) (snd b) in
      let c1 = (somme (somme (mul (somme (fst a) (snd a)) (somme (fst b) (snd b))) (oppose_gdnb c0)) (oppose_gdnb c2) ) in
      (somme (somme (c0) (ajoutDeg c1 (n/2))) (ajoutDeg c2 n))
  ;;
  *)

  (**
     Params - (p : gdnb) (q : gdnb).
     @return p*q : gdnb
  *)
  let mul (p : gdnb) (q : gdnb) =
    let rec aux_k (p1 : gdnb) (p2 : gdnb) =
      let n = max (degPair p1) (degPair p2) 
      in
      (* *)
      if p1.abs = [] || p2.abs = [] then {signe=true; abs = []} else
      (* *)
      if (n < 2) then
	mult p1 p2 
      else
	let a = separe p1 n and b = separe p2 n in
	let c0 = aux_k (fst a) (fst b) and
	    c2 = aux_k (snd a) (snd b) in
	let c1 = 
	  somme (somme (aux_k
			  (somme (fst a) (snd a)) 
			  (somme (fst b) (snd b))
	  ) (oppose_gdnb c0)
	  ) (oppose_gdnb c2)
	in
	somme (somme c0 (ajoutDeg c1 (n/ 2))) (ajoutDeg c2 n)
    in
    let res = aux_k p q in
    if (p.signe && q.signe || ((not p.signe) && (not q.signe))) then 
      {signe = true; abs = res.abs} 
    else 
      {signe = false; abs = res.abs}
  ;;  
  
  (**
     Puissance selon d'algorithme d'exponentiation dichotomique.
     Params - (a : gdnb) (n : int).
     @return a^n : gdnb
  *)
  let puissance (a : gdnb) (n : int) =
    let rec aux y =
      match y with
	  0                    -> {signe = true; abs = [0,1]}
	| _ when (y mod 2 = 0) -> let c = aux (y / 2) in
				  (mul c c)
	| _                    -> let c = aux ((y - 1) / 2) in
				  (mul a (mul c c))
    in
    aux n;;
  
  
  (* Division *)
  
  (* Division par methode binaire *)
  (** 
      Calcul le couple (quotient,reste) 
      de la division de deux gdnb positifs.
  *)
  let div (aa : gdnb) (b : gdnb) =
    let a = {signe = true; abs = aa.abs} in
    let rec aux y l =
      if (compare_gdnb y a) > 0 then
	l
      else
	aux (somme y y) (l @ [y])
    in
    let rec som l i accpgm acc2 = 
      match l with
	  [] -> accpgm,acc2
	| e::ll -> 
	  let res = (somme e accpgm) in
	  if (compare_gdnb res a) > 0 then
	    som ll (i-1) accpgm acc2
	  else
	    som ll (i-1) res (acc2 @ [(puissance {signe = true; abs = [0,2]} i)])
    in
    let rec somliste l acc =
      match l with
	  [] -> acc
	| e::ll -> somliste ll (somme acc e)
    in
    if b.abs = [] then failwith "Division par zero !" else
      if (compare_gdnb a zero) = 0 then 
	zero,zero
      else
	let li = (aux b []) in
	let pgm_q = (som (List.rev li) ((List.length li) - 1) ({signe = true; abs = []}) []) in
	let pgm = fst pgm_q in
	let q = somliste (snd pgm_q) {signe=true;abs=[]} in
	if (compare_gdnb aa zero) < 0 then
	  (oppose_gdnb (somme unit q)), (oppose_gdnb (somme {signe=true;abs=a.abs} (oppose_gdnb (mul b (somme unit q)))))
	else
	  q,(difference a pgm);;
  
  
  let modulo (a : gdnb) (b : gdnb) = snd (div a b);;    
  
  let rec euclide (a : gdnb) (b : gdnb) = 
    if ((compare_gdnb b {signe=true;abs=[]}) = 0) then
      (a, {signe=true;abs=[(0,1)]}, {signe=true;abs=[]})
    else
      let dd = div a b in
      let (dp, xp, yp) = euclide b (snd dd) in
      let (d, x, y) = (dp, yp, somme xp (oppose_gdnb (mul (fst dd) yp))) in
      (d, x, y);;


  (**
     Retourne le pgcd de a et de b.
     @param a
     @param b
     @return pgcd(a, b)
  *)
  let pgcd (a : gdnb) (b : gdnb) =
    if ((compare_gdnb b {signe=true;abs=[]}) = 0) then
      a
    else
      let (a, b, c) = euclide a b in
      a;;
  
  (**
     Calcul l'inverse de a modulo n.
  *)
  let rec inv (a : gdnb) (n : gdnb) =
    if (compare_gdnb a {signe=true;abs=[]} < 0) then (inv (somme a n) n) else 
    let (p, u, v) = (euclide a n) in
    match p with
	{signe=true;abs=[(0,1)]} -> u
      | _ -> failwith ((string_of_gdnb a)^" non inversible dans Z/"^(string_of_gdnb n)^"Z");;
  
  let ($/) (a : string) (b : string) = string_of_gdnb (fst (div (gdnb_of_string a) (gdnb_of_string b)));;
  let ($%) (a : string) (b : string) = string_of_gdnb (snd (div (gdnb_of_string a) (gdnb_of_string b)));;
  let ($$/) (a : string) (b : string) =
    let d = (div (gdnb_of_string a) (gdnb_of_string b)) in
    ((string_of_gdnb (fst d)), (string_of_gdnb (snd d)));;

  (* Operations sur les gdnb *)
  (**
     Somme de deux gdnb.
  *)
  let ($+) (a : string) (b : string) = string_of_gdnb (somme (gdnb_of_string a) (gdnb_of_string b));;
  (**
     Difference de deux gdnb.
  *)
  let ($-) (a : string) (b : string) = string_of_gdnb (somme (gdnb_of_string a) (oppose_gdnb(gdnb_of_string b)));;
  (**
     Multiplication Karatsuba.
  *)
  let ($*) (a : string) (b : string) = string_of_gdnb (mul (gdnb_of_string a) (gdnb_of_string b))
  ;;
  (**
     Exponentiation dichotomique.
  *)
  let ($^) (a:string) (n : int) = string_of_gdnb (puissance (gdnb_of_string a) n);;
  
  (**
     Multiplication naive.
  *)
  let ($$*) (a : string) (b : string) =
    string_of_gdnb (mult (gdnb_of_string a) (gdnb_of_string b));;

  (* Comparaisons sur les gdnb *)  
  let ($<) (a : string) (b : string) =  if (compare_gdnb (gdnb_of_string a) (gdnb_of_string b))  < 0 then true else false;;
  let ($>) (a : string) (b : string) =  if (compare_gdnb (gdnb_of_string a) (gdnb_of_string b))  > 0 then true else false;;
  let ($=) (a : string) (b : string) =  if (compare_gdnb (gdnb_of_string a) (gdnb_of_string b))  = 0 then true else false;;
  let ($<=) (a : string) (b : string) = if (compare_gdnb (gdnb_of_string a) (gdnb_of_string b)) <= 0 then true else false;;
  let ($>=) (a : string) (b : string) = if (compare_gdnb (gdnb_of_string a) (gdnb_of_string b)) >= 0 then true else false;;

end;;
(*
open Gdnb;;

"1234567890123456147258369" $$* "9876543210987654258963714";;

"2400015769" $+ "2400015769";;
(string_of_gdnb gnbr) $+ (string_of_gdnb gnbr);;
(string_of_gdnb {signe = false; abs = [(0, 5769); (2, 24)]}) $+ (string_of_gdnb {signe = false; abs = [(0, 5769); (2, 24)]});;
somme {signe = false; abs = [(0, 5769); (1,0); (2, 24)]} {signe = false; abs = [(0, 5769); (1,0); (2, 24)]};;

string_of_gdnb {signe = true; abs = [(0, 5769); (1, 24)]};;

string_of_gdnb ({signe = false; abs = [(0, 1538); (2, 25)]});;
(*- : string = "-2500001538"*)
string_of_gdnb gnbr;;
(*- : string = "-2400005769"*)
somme {signe = true; abs = [(0, 5769); (1, 24)]} {signe = true; abs = [(0, 5769); (1, 24)]};;
(*- : (int * int) list = [(0, 1538); (1, 25)]*)
string_of_gdnb {signe = true; abs = [(0, 5769); (1, 24)]};;
(*- : string = "245769"*)

(* tests *)
let s = "plop1234";;
String.get s 0;;


String.sub s 4 4 ;;

string_of_gdnb (gdnb_of_string "-12345678");;
string_of_gdnb (gdnb_of_string "123400005678");;
string_of_gdnb (gdnb_of_string "000012345678");;
string_of_gdnb (gdnb_of_string "12340005678");;
string_of_gdnb (gdnb_of_string "123400005678");;
string_of_gdnb (gdnb_of_string "1234078");;
string_of_gdnb (gdnb_of_string "12340078");;
string_of_gdnb (gdnb_of_string "-123400043078");;
*)
