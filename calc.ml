(***********************************************************************)
(*                                                                     *)
(*                             Projet de CS                            *)
(*                                                                     *)
(* Fichier : calc.ml                                                   *)
(* Auteurs : Fin Matthieu                                              *)
(*           Poinsot Clement                                           *)
(* Date : 10/05/13                                                     *)
(*                                                                     *)
(*                          Licence informatique 2eme année 2012/2013  *)
(*                                  UFR-Sciences et Techniques         *)
(*                                     Université de Rouen             *)
(*                                                                     *)
(***********************************************************************)
open Gdnb;;

print_string "Bienvenue dans la calculatrice magique !\n";;

let rec calc (s : string) = 
  match s with
      "exit" -> 
	let () = print_string "# Bye !"
	in print_newline ()
    | "add" | "+" | "somme" ->
      let a = read_line (print_string "-> ") in
      let b = read_line (print_string "-> ") in
      let res = Gdnb.($+) a b in
      print_newline (print_string ("# " ^ a ^ " + " ^ b ^ " = " ^ res));
      calc (read_line (print_string "$ "));
    | _ ->
      calc (read_line (print_string "# Commande inconnue !\n$ "))
	
;;


calc (read_line (print_string "$ "));;
