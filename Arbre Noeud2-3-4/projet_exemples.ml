#use "projet.ml";;

(** Voici les exemples de manipulation des arbres du projet 
		Il y un ou plusieurs test pour chaqu'une des fonctions du projet **)



(** II - Définition **)
(* 1.1 *)

let ex_arbre234 = Noeud2(10,Noeud2(5,Vide234,Vide234),
												   Noeud3(12,13,Vide234,Vide234,Vide234));;

let ex_faux_arbre234 = Noeud2(10,Noeud2(5,Vide234,Vide234),
												   			 Noeud3(7,13,Vide234,Vide234,Vide234));;


(* 1.2 est234 *)

est234 ex_arbre234;;
est234 ex_faux_arbre234;;



(* 1.3 recherche *)

let ex2_arbre234 = Noeud3(10,20,Noeud4(4,6,8,Vide234,Vide234,Vide234,Vide234),
                        				Noeud2(15,Vide234,Vide234),
                        				Noeud3(25,30,Vide234,Vide234,Vide234));;

research234 15 ex2_arbre234;; 
research234 14 ex2_arbre234;; 





(** III - Conversion **)

(* 2.3 *)

let arbre234_a = Noeud4(10,20,30,Noeud2(5,Vide234,Vide234),
																 Noeud3(13,17,Vide234,Vide234,Vide234),
																 Noeud3(23,27,Vide234,Vide234,Vide234),
																 Noeud4(32,37,40,Vide234,Vide234,Vide234,Vide234) );;

a234_vers_abic arbre234_a;;



let arbrebic_a = Noeud(17,Noir, Noeud(15,Noir,Noeud(10,Rouge,Vide,Vide),Vide),
																Noeud(253,Rouge,Noeud(22, Noir,Vide,Vide),Noeud(254, Noir,Vide,Vide) ) );;

abic_vers_a234 arbrebic_a;;




(** IV - Algorithmes **)

(* 4.1 Insertion *)


let ex3_arbre234 = Noeud2(20,(Noeud4(4,6,8,Noeud2(2,Vide234,Vide234),
                                     				  Noeud2(5,Vide234,Vide234),
                                      			  Noeud2(7,Vide234,Vide234),
                                      				Noeud2(9,Vide234,Vide234)) ),
                        				(Noeud2(25,Noeud3(22,23,Vide234,Vide234,Vide234),
                                   				 Noeud2(28,Vide234,Vide234)) ) );;

insert ex3_arbre234 1 ;;


creation_arbre234 10 15;; 



(* 4.2 Suppression *)


let ex4_arbre234 = Noeud3(10,20,(Noeud4(4,6,8,Noeud2(2,Vide234,Vide234),
                                     				  Noeud2(5,Vide234,Vide234),
                                      			  Noeud2(7,Vide234,Vide234),
                                      				Noeud2(9,Vide234,Vide234)) ),
                        				(Noeud2(15,Noeud3(12,13,Vide234,Vide234,Vide234),
                                   				 Noeud2(18,Vide234,Vide234)) ),
                        				(Noeud3(25,30,Noeud2(22,Vide234,Vide234),
                                      				Noeud2(27,Vide234,Vide234),
                                     				  Noeud3(32,34,Vide234,Vide234,Vide234)) ) );;

delete 15 ex4_arbre234;;




(** V - Manipulation d'ensembles **)

(* 5 - Fonctions ensemblistes *)



let ex5_arbre234 = Noeud2(10,Noeud2(5,Noeud2(3,Vide234,Vide234),
																			Noeud2(7,Vide234,Vide234)),
														 Noeud3(15,18,Noeud2(13,Vide234,Vide234),
																			 Noeud2(17,Vide234,Vide234),
																			Noeud2(19,Vide234,Vide234)));;

let ex5_arbre234_bis = Noeud2(10,Noeud2(5,Noeud2(3,Vide234,Vide234),
																			Noeud2(7,Vide234,Vide234)),
														 Noeud3(15,18,Noeud2(13,Vide234,Vide234),
																			 Noeud2(17,Vide234,Vide234),
																			Noeud2(19,Vide234,Vide234)));;

let ex6_arbre234 = Noeud2(20,Noeud2(15,Noeud2(13,Vide234,Vide234),
																			Noeud2(17,Vide234,Vide234)),
														 Noeud3(25,28,Noeud2(23,Vide234,Vide234),
																			 Noeud2(27,Vide234,Vide234),
																			 Noeud2(29,Vide234,Vide234) ));;

(* 5.1 union *)
union ex5_arbre234 ex6_arbre234;;

(* 5.2 intersection *)
intersection ex5_arbre234 ex6_arbre234;;


(* 5.3 différence *)
difference ex5_arbre234 ex6_arbre234;;


(* 5.4 différence symétrique*)
difference_symetrique ex5_arbre234 ex6_arbre234;;


(* 5.5 test d'égalité*)
est_egal ex5_arbre234 ex5_arbre234_bis;;
est_egal ex5_arbre234 ex6_arbre234;;


(* 5.6 test d'inclusion*)
est_inclus ex5_arbre234 ex6_arbre234;;
est_inclus ex5_arbre234 ex5_arbre234_bis;;








(* 6 - Fonctions de haut niveau *)



(* permet de faire la somme des éléments de l'arbre avec fold_left234 *)
let somme arbre234 = fold_left234 (+) 0 arbre234;;

somme ex5_arbre234;;

(* produit des éléments de l'arbre avec fold_left234 *)
let produit arbre234 = fold_left234 ( * ) 1 arbre234;;  

produit ex5_arbre234;; 

(* permet de faire la somme des éléments de l'arbre avec fold_right234 *)
let somme2 arbre234 = fold_right234 (+) arbre234 0;;

somme2 ex5_arbre234;;

(* produit des éléments de l'arbre avec fold_right234 *)
let produit2 arbre234 = fold_right234 ( * ) arbre234 1 ;; 

produit2 ex5_arbre234;;




(* 6.1 *)
cardinal ex5_arbre234;;

cardinal_vsup ex5_arbre234;;



(* 6.2 *)
separer (function y -> y < 10 ) ex5_arbre234;;
(* renvoie deux arbre, l'un contenant toutes les valeurs plus petites que 10 et un autre le reste de l'arbre ex5_arbre234 *)



(* 6.3 *)
filtrer (function y -> y < 10 ) ex5_arbre234;;
(* renvoie un arbre avec toutes les valeurs inférieurs à 10 de l'arbre ex5_arbre234  *)

