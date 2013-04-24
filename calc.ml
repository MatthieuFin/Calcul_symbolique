(*#load "gdnb.cmo";;*)
open Gdnb;;

print_string "Bienvenue dans la calculatrice magique !\n";;


let rec calc (s : string) = 
  if s = "exit" then
    let () = print_string "Bye !"
    in print_newline ()
  else
    begin
      print_newline (print_string s);
      calc (read_line ());
    end
;;

let rec calc (s : string) = 
  match s with
      "exit" -> 
	let () = print_string "# Bye !"
	in print_newline ()
    | "add" | "+" | "somme" ->
      let a = read_line (print_string "-> ") in
      let b = read_line (print_string "-> ") in
      let res = Gdnb.(a $+ b) in
      print_newline (print_string ("# " ^ a ^ " + " ^ b ^ " = " ^ res));
      calc (read_line ());
    | _ ->
	(*
      begin
	print_newline (print_string "# Commande inconnue !");
	calc (read_line (print_string "$ "));
      end*)
      calc (read_line (print_string "# Commande inconnue !\n$ "))
	
;;


calc (read_line (print_string "$ "));;
(*calc Gdnb.("3" $+ "3");;*)
(*calc (Gdnb.($+) "3" "3");;*)
