(** =====================================================		2 	===================================================================== **)
(** 2.1 **)

type element = int
type arbre234 = Vide234 | Noeud2 of ( element * arbre234 * arbre234 ) 
                    	  | Noeud3 of ( element * element * arbre234 * arbre234 * arbre234) 
                    	  | Noeud4 of ( element * element * element * arbre234 * arbre234 * arbre234 * arbre234 )
type couleur = Rouge | Noir
type ab = Vide | Noeud of ( element * couleur * ab * ab )

let comp = compare
let ($=$) x y = comp x y = 0
let ($<$) x y = comp x y < 0
let ($>$) x y = comp x y > 0


(** 2.2 **)
(* fonction qui renvoie true si x est dans l'arbre234, false sinon  *)
let research234 x arbre234 = 
	let rec aux x = function
    | Vide234 -> false
    | Noeud2 ( e1,a1,a2 ) when x = e1 -> true
    | Noeud2 ( e1,a1,a2 ) when x < e1 -> aux x a1 
    | Noeud2 ( e1,a1,a2 ) when x > e1 -> aux x a2
    
    | Noeud3 ( e1,e2,a1,a2,a3 ) when x = e1 || x = e2 -> true
    | Noeud3 ( e1,e2,a1,a2,a3 ) when x < e1 -> aux x a1 
    | Noeud3 ( e1,e2,a1,a2,a3 ) when x > e1 && x < e2 -> aux x a2
    | Noeud3 ( e1,e2,a1,a2,a3 ) when x > e2 -> aux x a3
    
    | Noeud4 ( e1,e2,e3,a1,a2,a3,a4 ) when x = e1 || x = e2 || x = e3 -> true
    | Noeud4 ( e1,e2,e3,a1,a2,a3,a4 ) when  x < e1 -> aux x a1
    | Noeud4 ( e1,e2,e3,a1,a2,a3,a4 ) when x > e1 && x < e2 -> aux x a2
    | Noeud4 ( e1,e2,e3,a1,a2,a3,a4 ) when  x > e2 && x < e3 -> aux x a3
    | Noeud4 ( e1,e2,e3,a1,a2,a3,a4 ) when x > e3 -> aux x a4
		| _ -> failwith"err"
	in aux x arbre234;;





(* RENVOIE TRUE SI L'ARBRE EST EQUILIBRE, FALSE SINON *)
let rec est_ordonne234 = function
    | Vide234 -> true
    | Noeud2 (_,Vide234,Vide234) -> true
    | Noeud3 (_,_,Vide234,Vide234,Vide234) -> true
    | Noeud4 (_,_,_,Vide234,Vide234,Vide234,Vide234) -> true 
		
    | Noeud2(e1,a1,a2) when e1 $>$ renv_rac2 a1 && e1 $<$ renv_rac1 a2 -> est_ordonne234 a1 && est_ordonne234 a2																								
																																											                                  
    | Noeud3(e1,e2,a1,a2,a3) when e1 $>$ renv_rac2 a1 && 
																  e1 $<$ renv_rac1 a2 && 
																	e2 $>$ renv_rac2 a2 && 
																	e2 $<$ renv_rac1 a3 					-> est_ordonne234 a1 && est_ordonne234 a2 && est_ordonne234 a3												
                                                                
    | Noeud4(e1,e2,e3,a1,a2,a3,a4) when e1 $>$ renv_rac2 a1 && 
                                        e1 $<$ renv_rac1 a2 && 
                                        e2 $>$ renv_rac2 a2 && 
                                    	  e2 $<$ renv_rac1 a3 && 
                                     	  e3 $>$ renv_rac2 a3 && 
                                     	  e3 $<$ renv_rac1 a4      -> est_ordonne234 a1 && est_ordonne234 a2 && est_ordonne234 a3 && est_ordonne234 a4
    | _ -> false;;



(* renvoie la plus petite racine *)
let renv_rac1 = function
    | Noeud2(e1,_,_) -> e1
    | Noeud3(e1,_,_,_,_) -> e1
    | Noeud4(e1,_,_,_,_,_,_) -> e1
    | _ -> failwith"pas un arbre 2-3-4";;

(* renvoie la plus grande racine *)
let renv_rac2 = function
  	| Noeud2(e1,_,_) -> e1
    | Noeud3(_,e2,_,_,_) -> e2
    | Noeud4(_,_,e3,_,_,_,_) -> e3
    | _ -> failwith"pas un arbre 2-3-4";;


(* renvoie true si la hauteur des sous-arbres est la même *)
let meme_hauteur234 = function
	| Noeud2(e1,a1,a2) when hauteur234 a1 = hauteur234 a2 -> true
	| Noeud3(e1,e2,a1,a2,a3) when hauteur234 a1 = hauteur234 a2 && hauteur234 a1 = hauteur234 a3 -> true
	| Noeud4(e1,e2,e3,a1,a2,a3,a4) when hauteur234 a1 = hauteur234 a2 && hauteur234 a3 = hauteur234 a4 && hauteur234 a1 = hauteur234 a3 -> true
	| _ -> false;;
	

(* calcule la hauteur d'un sous-arbre *)
let rec hauteur234 = function
	| Vide234 -> 0
	| Noeud2(e1,a1,a2) -> 1 + max(hauteur234 a1)( hauteur234 a2)
	| Noeud3(e1,e2,a1,a2,a3) -> 1 + max (max(hauteur234 a1) (hauteur234 a2)) (hauteur234 a3)
	| Noeud4(e1,e2,e3,a1,a2,a3,a4) -> 1 + max (max(hauteur234 a1)(hauteur234 a2))  (max(hauteur234 a3)(hauteur234 a4));;



(* RENVOIE TRUE SI L'ARBRE EST UN ARBRE234, FALSE SINON *)
let est234 arbre234 = est_ordonne234 arbre234 && meme_hauteur234 arbre234;;




(** ======================================================  3  ==================================================================== **)
    
 (** 3.3 **)   
    
(* transforme un arbre 2-3-4 en arbre bicolore *)
let a234_vers_abic a234 = 
	let  rec aux = function
  	| Vide234 -> Vide
    | Noeud2(e1,a1,a2)  -> Noeud(e1,Noir, aux a1, aux a2)
    | Noeud3(e1,e2,a1,a2,a3) -> Noeud(e2,Noir, Noeud(e1,Rouge,aux a1, aux a2), aux a3 )
    | Noeud4(e1,e2,e3,a1,a2,a3,a4) -> Noeud(e2,Noir, Noeud(e1,Rouge, aux a1, aux a2), Noeud(e3,Rouge, aux a3, aux a4) )
		| _ -> failwith "ce n'est pas un arbre 2-3-4"
  in aux a234;;


(* transforme un arbre bicolore en arbre 2-3-4 *)
let abic_vers_a234 a234 =
  let rec aux = function
    | Vide -> Vide234
    | Noeud(e2,Noir, Noeud(e1,Rouge, a1, a2) , Noeud(e3,Rouge, a3, a4)) -> Noeud4(e1,e2,e3, aux a1, aux a2, aux a3, aux a4)
    | Noeud(e2,Noir, Noeud(e1, Rouge, a1, a2) , a3) -> Noeud3(e1, e2, aux a1, aux a2, aux a3) 
    | Noeud(e2,Noir, a1, Noeud(e3, Rouge, a2, a3)) -> Noeud3(e2, e3, aux a1, aux a2, aux a3) 
    | Noeud(e1,Noir, a1, a2) -> Noeud2(e1, aux a1, aux a2)
    | _ -> failwith "ce n'est pas un arbre bicolore"
   in aux a234;;







(** ====================================================  4  ====================================================================== **)

(** ///////////////////////////////////////////////       4.1 INSERTION **)



(* Eclatement racine *) 
let eclater_racine  = function 
  |Noeud4(e1,e2,e3,a1,a2,a3,a4) -> Noeud2(e2,Noeud2(e1,a1,a2),Noeud2(e3,a3,a4))
  | arbre234 -> arbre234;;


(** Insertion dans arbre 234 (partiel) **)

(* Insertion dans un Noeud normal *)
let insert_234 arbre234 x = 
  let rec aux = function
    | Vide234 -> Noeud2(x,Vide234,Vide234)
    | Noeud2(e1, a1, a2) as ab when e1 = x -> ab
    | Noeud2(e1,Vide234,Vide234) when x < e1 -> Noeud3(x,e1,Vide234,Vide234,Vide234)
    | Noeud2(e1,Vide234,Vide234) when x > e1 -> Noeud3(e1,x,Vide234,Vide234,Vide234)
      
                    
	(*Insertion dans un Noeud 2 elements *)  
    | Noeud3(e1,e2,a1,a2,a3) as ab when x = e1 || x = e2 -> ab  
    | Noeud3(e1,e2,Vide234,Vide234,Vide234) when x < e1 					 -> Noeud4(x,e1,e2,Vide234,Vide234,Vide234,Vide234)
    | Noeud3(e1,e2,Vide234,Vide234,Vide234) when x > e2 					 -> Noeud4(e1,e2,x,Vide234,Vide234,Vide234,Vide234)
    | Noeud3(e1,e2,Vide234,Vide234,Vide234) when x > e1 && x < e2 -> Noeud4(e1,x,e2,Vide234,Vide234,Vide234,Vide234)

  (* Cassage et rééquilibrage *)
    | Noeud2(e1,Noeud4(e2,e3,e4,a1,a2,a3,a4),a5) -> aux (Noeud3(e3,e1,Noeud2(e2,a1,a2),Noeud2(e4,a3,a4),a5))
    | Noeud2(e1,a1,Noeud4(e2,e3,e4,a2,a3,a4,a5)) -> aux (Noeud3(e1,e3,a1,Noeud2(e2,a2,a3),Noeud2(e4,a4,a5)))
  
  (* Ceci n'est pas un cassage *)
    | Noeud2(e1, a1, a2)   when x > e1 -> Noeud2(e1, a1, aux a2)
    | Noeud2(e1, a1, a2)   when x < e1 -> Noeud2(e1, aux a1, a2) 
                                                                      
  (* reprise cassage *)
    |Noeud3(e1,e2,Noeud4(e3,e4,e5,a3,a4,a5,a6),a1,a2) -> aux (Noeud4(e4,e1,e2,Noeud2(e3,a3,a4),Noeud2(e5,a5,a6),a1,a2))
    |Noeud3(e1,e2,a1,Noeud4(e3,e4,e5,a3,a4,a5,a6),a2) -> aux (Noeud4(e1,e4,e2,a1,Noeud2(e3,a3,a4),Noeud2(e5,a5,a6),a2))
    |Noeud3(e1,e2,a1,a2,Noeud4(e3,e4,e5,a3,a4,a5,a6)) -> aux (Noeud4(e1,e2,e4,a1,a2,Noeud2(e3,a3,a4),Noeud2(e5,a5,a6)))
                                                            
             
  (* gestion quand Noeud 3 & 4 *)
    | Noeud3(e1, e2, a1, a2, a3) when x < e1 -> Noeud3(e1, e2, aux a1, a2, a3) 
    | Noeud3(e1, e2, a1, a2, a3) when x > e2 -> Noeud3(e1, e2, a1, a2, aux a3) 
    | Noeud3(e1, e2, a1, a2, a3) when x > e1 && x < e2  -> Noeud3(e1, e2, a1, aux a2, a3)

    | Noeud4(e1, e2, e3, a1, a2, a3, a4) as a when x = e1 || x = e2 || x = e3 -> a
    | Noeud4(e1, e2, e3, a1, a2, a3, a4) when x < e1 -> Noeud4(e1, e2, e3, aux a1, a2, a3, a4)
    | Noeud4(e1, e2, e3, a1, a2, a3, a4) when x > e3 -> Noeud4(e1, e2, e3, a1, a2, a3, aux a4)
    | Noeud4(e1, e2, e3, a1, a2, a3, a4) when x > e1 && x < e2  -> Noeud4(e1, e2, e3, a1, aux a2, a3, a4)
    | Noeud4(e1, e2, e3, a1, a2, a3, a4) when x > e2 && x < e3  -> Noeud4(e1, e2, e3, a1, a2, aux a3, a4)
   
    | _ -> failwith "err"
  in aux arbre234;;
  
let insert arbre234 x = insert_234 (eclater_racine arbre234) x;;




(** Création d'arbres aléatoires **)
(* crée un arbre de taille taille avec des valeurs allant jusqu'à x *)
let creation_arbre234 taille x = 
  let rec aux acc size = 
    if size = 0 then acc
    else
      aux  (insert acc (Random.int x)) (size-1)
  in aux Vide234 taille;;





(** //////////////////////////////////////////////////    4.2 SUPPRESSION  **)

(* supprime la valeur x dans l'arbre234, ou appelle equi_suppr pour l'équilibrer *)
let suppr x arbre234 = 
	let rec aux = function

		| Vide234 -> Vide234
		
	(* supprimer x s'il est égal à une racine dans un arbre sans descendance cf. Figure 7 *)
		| Noeud2(e1,Vide234,Vide234) when e1 = x -> Vide234

		| Noeud3(e1, e2,Vide234,Vide234,Vide234) when e1 = x -> Noeud2(e2,Vide234,Vide234)
		| Noeud3(e1, e2,Vide234,Vide234,Vide234) when e2 = x -> Noeud2(e1,Vide234,Vide234)

		| Noeud4(e1, e2, e3,Vide234,Vide234,Vide234,Vide234) when e1 = x -> Noeud3(e2, e3,Vide234,Vide234,Vide234)
		| Noeud4(e1, e2, e3,Vide234,Vide234,Vide234,Vide234) when e2 = x -> Noeud3(e1, e3,Vide234,Vide234,Vide234)
		| Noeud4(e1, e2, e3,Vide234,Vide234,Vide234,Vide234) when e3 = x -> Noeud3(e1, e2,Vide234,Vide234,Vide234)


(* supprimer x s'il est égal à une racine dans un arbre avec descendance *)
	  | (Noeud2(e1,a1,a2) as y) when e1 = x 																	-> equi_suppr x y
		| (Noeud3(e1,e2,a1,a2,a3) as y) when x = e1 || x = e2 									-> equi_suppr x y
  	| (Noeud4(e1,e2,e3,a1,a2,a3,a4) as y) when x = e1 || x = e2 || x = e3		-> equi_suppr x y


	(* supprimer x dans un arbre avec descendance *)
	  | Noeud2(e1, a1, a2) when x < e1 -> Noeud2(e1, aux ( equi_suppr x a1), a2)
  	| Noeud2(e1, a1, a2) when x > e1 -> Noeud2(e1, a1, aux ( equi_suppr x a2))
  		
  	| Noeud3(e1, e2, a1, a2, a3) when x < e1 						-> Noeud3(e1, e2, aux ( equi_suppr x a1), a2, a3) 
  	| Noeud3(e1, e2, a1, a2, a3) when x > e1 && x < e2  -> Noeud3(e1, e2, a1, aux ( equi_suppr x a2), a3)
  	| Noeud3(e1, e2, a1, a2, a3) when x > e2 						-> Noeud3(e1, e2, a1, a2, aux ( equi_suppr x a3))
  		
  	| Noeud4(e1, e2, e3, a1, a2, a3, a4) when x < e1 						-> Noeud4(e1, e2, e3, aux ( equi_suppr x a1), a2, a3, a4)
  	| Noeud4(e1, e2, e3, a1, a2, a3, a4) when x > e1 && x < e2  -> Noeud4(e1, e2, e3, a1, aux ( equi_suppr x a2), a3, a4)
  	| Noeud4(e1, e2, e3, a1, a2, a3, a4) when x > e2 && x < e3  -> Noeud4(e1, e2, e3, a1, a2, aux ( equi_suppr x a3), a4)
  	| Noeud4(e1, e2, e3, a1, a2, a3, a4) when x > e3 						-> Noeud4(e1, e2, e3, a1, a2, a3, aux ( equi_suppr x a4)) 


		| _ -> failwith "x n'est pas dans l'arbre"

		in aux arbre234 ;;


(* équilibre l'arbre pour pouvoir supprimer x *)
let equi_suppr x = function

		
(** ////// ROTATION ////// **)		
	
(** cas racine Noeud2 **)	
		(* 2 - 2|3	->	2 - 3|2*)
		| Noeud2(e1,Noeud2(e2,a1,a2),Noeud3(e3,e4,a3,a4,a5)) when x <= e1 -> Noeud2(e3,Noeud3(e2,e1,a1,a2,a3),Noeud2(e4,a4,a5))
		(* 2 - 2|4	->	2 - 3|3 *)
		| Noeud2(e1,Noeud2(e2,a1,a2),Noeud4(e3,e4,e5,a3,a4,a5,a6)) when x <= e1 -> Noeud2(e3,Noeud3(e2,e1,a1,a2,a3),Noeud3(e4,e5,a4,a5,a6))	
		
		(* 2 - 3|2	->	2 - 2|3 *)
		| Noeud2(e1,Noeud3(e2,e3,a1,a2,a3),Noeud2(e5,a5,a6)) when x >= e1 -> Noeud2(e3,Noeud2(e2,a1,a2),Noeud3(e1,e5,a3,a5,a6))
		(* 2 - 4|2	->	2 - 3|3 *)
		| Noeud2(e1,Noeud4(e2,e3,e4,a1,a2,a3,a4),Noeud2(e5,a5,a6)) when x >= e1  -> Noeud2(e4,Noeud3(e2,e3,a1,a2,a3),Noeud3(e1,e5,a4,a5,a6))
		

(** cas racine Noeud3 **)
		(* 3 - 2|3|x	->	3 - 3|2|x *)
		| Noeud3(e1,e2,Noeud2(e3,a2,a3),Noeud3(e4,e5,a4,a5,a6),a1) when x <= e1 -> Noeud3(e4,e2,Noeud3(e3,e1,a2,a3,a4),Noeud2(e5,a5,a6),a1)
  	(* 3 - 3|2|x	->	3 - 2|3|x *)	
  	| Noeud3(e1,e2,Noeud3(e3,e4,a2,a3,a4),Noeud2(e5,a5,a6),a1) when x >= e1 && x < e2 -> Noeud3(e4,e2,Noeud2(e3,a2,a3),Noeud3(e1,e5,a4,a5,a6),a1)
  	(* 3 - x|2|3	->	3 - x|3|2 *)					
  	| Noeud3(e1,e2,a1,Noeud2(e3,a2,a3),Noeud3(e4,e5,a4,a5,a6)) when x > e1 && x <= e2 -> Noeud3(e1,e4,a1,Noeud3(e3,e2,a2,a3,a4),Noeud2(e5,a5,a6))
  	(* 3 - x|3|2	->	3 - x|2|3 *)								
  	| Noeud3(e1,e2,a1,Noeud3(e3,e4,a2,a3,a4),Noeud2(e5,a5,a6)) when x >= e2 -> Noeud3(e1,e4,a1,Noeud2(e3,a2,a3),Noeud3(e2,e5,a4,a5,a6))	

								
		(* 3 - 2|4|x	->	3 - 3|3|x *)					
		| Noeud3(e1,e2,Noeud2(e3,a2,a3),Noeud4(e4,e5,e6,a4,a5,a6,a7),a1) when x <= e1 -> Noeud3(e4,e2,Noeud3(e3,e1,a2,a3,a4),Noeud3(e5,e6,a5,a6,a7),a1)
		(* 3 - 4|2|x	->	3 - 3|3|x *)
    | Noeud3(e1,e2,Noeud4(e3,e4,e5,a2,a3,a4,a5),Noeud2(e6,a6,a7),a1) when x >= e1 && x < e2 -> Noeud3(e5,e2,Noeud3(e3,e4,a2,a3,a4),Noeud3(e1,e6,a5,a6,a7),a1)
    (* 3 - x|2|4	->	3 - x|3|3 *)
		| Noeud3(e1,e2,a1,Noeud2(e3,a2,a3),Noeud4(e4,e5,e6,a4,a5,a6,a7)) when x > e1 && x <= e2 -> Noeud3(e1,e4,a1,Noeud3(e3,e2,a2,a3,a4),Noeud3(e5,e6,a5,a6,a7))
		(* 3 - x|4|2	->	3 - x|3|3 *)
		| Noeud3(e1,e2,a1,Noeud4(e3,e4,e5,a2,a3,a4,a5),Noeud2(e6,a6,a7)) when x >= e2 -> Noeud3(e1,e5,a1,Noeud3(e3,e4,a2,a3,a4),Noeud3(e2,e6,a5,a6,a7))



		(* 3 - 3|3|x 	-> 3 - 4|2|x *) 
		| Noeud3(e1,e2,Noeud3(e3,e4,a2,a3,a4),Noeud3(e5,e6,a5,a6,a7),a1) when x <= e1 -> Noeud3(e5,e2,Noeud4(e3,e4,e1,a2,a3,a4,a5),Noeud2(e6,a6,a7),a1)
		(* 3 - 3|3|x 	-> 3 - 2|4|x *) 
		| Noeud3(e1,e2,Noeud3(e3,e4,a2,a3,a4),Noeud3(e5,e6,a5,a6,a7),a1) when x >= e1 && x < e2 -> Noeud3(e4,e2,Noeud2(e3,a2,a3),Noeud4(e1,e5,e6,a4,a5,a6,a7),a1)
		(* 3 - x|3|3 	-> 3 - x|4|2*)
		| Noeud3(e1,e2,a1,Noeud3(e3,e4,a2,a3,a4),Noeud3(e5,e6,a5,a6,a7)) when x > e1 && x <= e2 -> Noeud3(e1,e5,a1,Noeud4(e3,e4,e2,a2,a3,a4,a5),Noeud2(e6,a6,a7))
		(* 3 - x|3|3 	-> 3 - x|2|4*)
		| Noeud3(e1,e2,a1,Noeud3(e3,e4,a2,a3,a4),Noeud3(e5,e6,a5,a6,a7)) when x >= e2 -> Noeud3(e1,e4,a1,Noeud2(e3,a2,a3),Noeud4(e2,e5,e6,a4,a5,a6,a7)) 



		(* 3 - 4|3|x	->  3 - 3|4|x *)
		| Noeud3(e1,e2,Noeud4(e3,e4,e5,a2,a3,a4,a5),Noeud3(e6,e7,a6,a7,a8),a1) when x <= e1 -> Noeud3(e5,e2,Noeud3(e3,e4,a2,a3,a4),Noeud4(e1,e6,e7,a5,a6,a7,a8),a1)
		(* 3 - 3|4|x	->  3 - 4|3|x *)
		| Noeud3(e1,e2,Noeud3(e3,e4,a2,a3,a4),Noeud4(e5,e6,e7,a5,a6,a7,a8),a1) when x >= e1 && x < e2 -> Noeud3(e5,e2,Noeud4(e3,e4,e1,a2,a3,a4,a5),Noeud3(e6,e7,a6,a7,a8),a1)
		(* 3 - x|4|3	->  3 - x|3|4 *)
		| Noeud3(e1,e2,a1,Noeud4(e3,e4,e5,a2,a3,a4,a5),Noeud3(e6,e7,a6,a7,a8)) when x > e1 && x <= e2 -> Noeud3(e1,e5,a1,Noeud3(e3,e4,a2,a3,a4),Noeud4(e2,e6,e7,a5,a6,a7,a8))
		(* 3 - x|3|4	->  3 - x|4|3 *)
		| Noeud3(e1,e2,a1,Noeud3(e3,e4,a2,a3,a4),Noeud4(e5,e6,e7,a5,a6,a7,a8)) when x >= e2 -> Noeud3(e1,e5,a1,Noeud4(e3,e4,e2,a2,a3,a4,a5),Noeud3(e6,e7,a6,a7,a8))				



(** cas racine Noeud4 **)

		(* 4 - 3|3|x|x	->	4 - 4|2|x|x  *)
		| Noeud4(e1,e2,e3,Noeud3(e4,e5,a3,a4,a5),Noeud3(e6,e7,a6,a7,a8),a1,a2) when x <= e1 -> Noeud4(e6,e2,e3,Noeud4(e4,e5,e1,a3,a4,a5,a6),Noeud2(e7,a7,a8),a1,a2)
		(* 4 - x|3|3|x	->	4 - x|4|2|x  *)
		| Noeud4(e1,e2,e3,a1,Noeud3(e4,e5,a3,a4,a5),Noeud3(e6,e7,a6,a7,a8),a2) when x >= e1 && x < e2 -> Noeud4(e1,e6,e3,a1,Noeud4(e4,e5,e2,a3,a4,a5,a6),Noeud2(e7,a7,a8),a2)
		(* 4 - x|3|3|x	->	4 - x|2|4|x  *)
		| Noeud4(e1,e2,e3,a1,Noeud3(e4,e5,a3,a4,a5),Noeud3(e6,e7,a6,a7,a8),a2) when x > e2 && x <= e3 -> Noeud4(e1,e5,e3,a1,Noeud2(e4,a3,a4),Noeud4(e5,e6,e7,a5,a6,a7,a8),a2)
		(* 4 - x|3|3|3	->	4 - x|x|2|4  *)
		| Noeud4(e1,e2,e3,a1,a2,Noeud3(e4,e5,a3,a4,a5),Noeud3(e6,e7,a6,a7,a8)) when x >= e3 -> Noeud4(e1,e2,e5,a1,a2,Noeud2(e4,a3,a4),Noeud4(e3,e6,e7,a5,a6,a7,a8))



		(* 4 - 4|3|x|x	->	4 - 3|4|x|x *)
		| Noeud4(e1,e2,e3,Noeud4(e4,e5,e6,a3,a4,a5,a6),Noeud3(e7,e8,a7,a8,a9),a1,a2) when x <= e1 -> Noeud4(e6,e2,e3,Noeud3(e4,e5,a3,a4,a5),Noeud4(e1,e7,e8,a6,a7,a8,a9),a1,a2)
		(* 4 - 3|4|x|x	->	4 - 4|3|x|x *)
		| Noeud4(e1,e2,e3,Noeud3(e4,e5,a3,a4,a5),Noeud4(e6,e7,e8,a6,a7,a8,a9),a1,a2) when x >= e1 && x < e2 -> Noeud4(e6,e2,e3,Noeud4(e4,e5,e1,a3,a4,a5,a6),Noeud3(e7,e8,a7,a8,a9),a1,a2)
		(* 4 - x|4|3|x	->	4 - x|3|4|x *)
		| Noeud4(e1,e2,e3,a1,Noeud4(e4,e5,e6,a3,a4,a5,a6),Noeud3(e7,e8,a7,a8,a9),a2) when x > e1 && x <= e2 -> Noeud4(e1,e6,e3,a1,Noeud3(e4,e5,a3,a4,a5),Noeud4(e2,e7,e8,a6,a7,a8,a9),a2)
		(* 4 - x|3|4|x	->	4 - x|4|3|x *)
		| Noeud4(e1,e2,e3,a1,Noeud3(e4,e5,a3,a4,a5),Noeud4(e6,e7,e8,a6,a7,a8,a9),a2) when x >= e2 && x < e3 -> Noeud4(e1,e6,e3,a1,Noeud4(e4,e5,e2,a3,a4,a5,a6),Noeud3(e7,e8,a7,a8,a9),a2)
		(* 4 - x|x|4|3	->	4 - x|x|3|4 *)
		| Noeud4(e1,e2,e3,a1,a2,Noeud4(e4,e5,e6,a3,a4,a5,a6),Noeud3(e7,e8,a7,a8,a9)) when x > e2 && x <= e3 -> Noeud4(e1,e2,e6,a1,a2,Noeud3(e4,e5,a3,a4,a5),Noeud4(e3,e7,e8,a6,a7,a8,a9))
		(* 4 - x|x|3|4	->	4 - x|x|4|3 *)
		| Noeud4(e1,e2,e3,a1,a2,Noeud3(e4,e5,a3,a4,a5),Noeud4(e6,e7,e8,a6,a7,a8,a9)) when x >= e3 -> Noeud4(e1,e2,e6,a1,a2,Noeud4(e4,e5,e3,a3,a4,a5,a6),Noeud3(e7,e8,a7,a8,a9))



		(* 4 - 2|3|x|x	 ->	 4 - 3|2|x|x *)
		| Noeud4(e1,e2,e3,Noeud2(e4,a3,a4),Noeud3(e5,e6,a5,a6,a7),a1,a2) when x <= e1 -> Noeud4(e5,e2,e3,Noeud3(e4,e1,a3,a4,a5),Noeud2(e6,a6,a7),a1,a2)
		(* 4 - 2|4|x|x	 ->	 4 - 3|3|x|x *)
		| Noeud4(e1,e2,e3,Noeud2(e4,a3,a4),Noeud4(e5,e6,e7,a5,a6,a7,a8),a1,a2) when x <= e1 -> Noeud4(e5,e2,e3,Noeud3(e4,e1,a3,a4,a5),Noeud3(e6,e7,a6,a7,a8),a1,a2)
		(* 4 - 3|2|x|x	 ->	 4 - 2|3|x|x *)
		| Noeud4(e1,e2,e3,Noeud3(e4,e5,a3,a4,a5),Noeud2(e6,a6,a7),a1,a2) when x >= e1 && x < e2 -> Noeud4(e5,e2,e3,Noeud2(e4,a3,a4),Noeud3(e1,e6,a5,a6,a7),a1,a2)
		(* 4 - 4|2|x|x	 ->	 4 - 3|3|x|x *)
		| Noeud4(e1,e2,e3,Noeud4(e4,e5,e6,a3,a4,a5,a6),Noeud2(e7,a7,a8),a1,a2) when x >= e1 && x < e2 -> Noeud4(e6,e2,e3,Noeud3(e4,e5,a3,a4,a5),Noeud3(e1,e7,a6,a7,a8),a1,a2)
				
				
		(* 4 - x|2|3|x	 ->	 4 - x|3|2|x *)
		| Noeud4(e1,e2,e3,a1,Noeud2(e4,a3,a4),Noeud3(e5,e6,a5,a6,a7),a2) when x > e1 && x <= e2 -> Noeud4(e1,e5,e3,a1,Noeud3(e4,e2,a3,a4,a5),Noeud2(e6,a6,a7),a2)
		(* 4 - x|2|4|x	 ->	 4 - x|3|3|x *)
		| Noeud4(e1,e2,e3,a1,Noeud2(e4,a3,a4),Noeud4(e5,e6,e7,a5,a6,a7,a8),a2) when x > e1 && x <= e2 -> Noeud4(e1,e5,e3,a1,Noeud3(e4,e2,a3,a4,a5),Noeud3(e6,e7,a6,a7,a8),a2)
		(* 4 - x|3|2|x	 ->	 4 - x|2|3|x *)
		| Noeud4(e1,e2,e3,a1,Noeud3(e4,e5,a3,a4,a5),Noeud2(e6,a6,a7),a2) when x >= e2 && x < e3 -> Noeud4(e1,e5,e3,a1, Noeud2(e4,a3,a4),Noeud3(e2,e6,a5,a6,a7),a2)
		(* 4 - x|4|2|x	 ->	 4 - x|3|3|x *)
		| Noeud4(e1,e2,e3,a1,Noeud4(e4,e5,e6,a3,a4,a5,a6),Noeud2(e7,a7,a8),a2) when x >= e2 && x < e3 -> Noeud4(e1,e6,e3,a1,Noeud3(e4,e5,a3,a4,a5),Noeud3(e2,e7,a6,a7,a8),a2) 
		
		
		(* 4 - x|x|2|3	 ->	 4 - x|x|3|2 *)
		| Noeud4(e1,e2,e3,a1,a2,Noeud2(e4,a3,a4),Noeud3(e5,e6,a5,a6,a7)) when x > e2 && x <= e3 ->  Noeud4(e1,e2,e5,a1,a2,Noeud3(e4,e3,a3,a4,a5),Noeud2(e6,a6,a7))
		(* 4 - x|x|2|4	 ->	 4 - x|x|3|3 *)
		| Noeud4(e1,e2,e3,a1,a2,Noeud2(e4,a3,a4),Noeud4(e5,e6,e7,a5,a6,a7,a8)) when x > e2 && x <= e3 -> Noeud4(e1,e2,e5,a1,a2,Noeud3(e4,e3,a3,a4,a5),Noeud3(e6,e7,a6,a7,a8))
		(* 4 - x|x|3|2	 ->	 4 - x|x|2|3 *)
		| Noeud4(e1,e2,e3,a1,a2,Noeud3(e4,e5,a3,a4,a5),Noeud2(e6,a6,a7)) when x >= e3 -> Noeud4(e1,e2,e5,a1,a2,Noeud2(e4,a3,a4),Noeud3(e3,e6,a5,a6,a7))
		(* 4 - x|x|4|2	 ->	 4 - x|x|3|3 *)
		| Noeud4(e1,e2,e3,a1,a2,Noeud4(e4,e5,e6,a3,a4,a5,a6),Noeud2(e7,a7,a8)) when x >= e3 -> Noeud4(e1,e2,e6,a1,a2,Noeud3(e4,e5,a3,a4,a5),Noeud3(e3,e7,a6,a7,a8)) 


	


(** ////// FUSION ////// **)	 

(** cas racine Noeud2 **)

		(* [2 - 2|2]	-> 4 - x|x|x|x *)
		| Noeud2(e1, Noeud2(e2, a1, a2), Noeud2(e3, a3, a4)) -> Noeud4(e2, e1, e3, a1, a2, a3, a4)
		 	
			
(** cas racine Noeud3 **)

		(* 3 - [2|2]|x 	-> 2 - 4|x *)
		| Noeud3(e1,e2,Noeud2(e3,a2,a3),Noeud2(e4,a4,a5),a1) when x <= e1 -> Noeud2(e2,Noeud4(e3,e1,e4,a2,a3,a4,a5),a1)
    (* 3 - x|[2|2]	-> 2 - x|4 *)      
  	| Noeud3(e1,e2,a1,Noeud2(e3,a2,a3),Noeud2(e4,a4,a5)) when x > e1-> Noeud2(e1,a1,Noeud4(e3,e2,e4,a2,a3,a4,a5))

		

(** cas racine Noeud4 **)
	 
		(* 4 - [2|2]|2|x	-> 	3 - 4|2|x *)
		| Noeud4(e1,e2,e3,Noeud2(e4,a2,a3),Noeud2(e5,a4,a5),Noeud2(e6,a6,a7),a1) when x <= e1 -> Noeud3(e2,e3,Noeud4(e4,e1,e5,a2,a3,a4,a5),Noeud2(e6,a6,a7),a1)
    (* 4 - x|2|[2|2] 	-> 	3 - x|2|4 *)    
  	| Noeud4(e1,e2,e3,a1,Noeud2(e4,a2,a3),Noeud2(e5,a4,a5),Noeud2(e6,a6,a7)) when x >= e3 -> Noeud3(e1,e2,a1,Noeud2(e4,a2,a3),Noeud4(e5,e3,e6,a4,a5,a6,a7))
		(* 4 - 2|[2|2]|x  -> 	3 - 2|4|x *)
		| Noeud4(e1,e2,e3,Noeud2(e4,a2,a3),Noeud2(e5,a4,a5),Noeud2(e6,a6,a7),a1) when x = e2 || (x < e3 && x > e1) -> Noeud3(e1,e3,Noeud2(e4,a2,a3),Noeud4(e5,e2,e6,a4,a5,a6,a7),a1)
    (* 4 - x|[2|2]|2 	-> 	3 - x|4|2 *)      
  	| Noeud4(e1,e2,e3,a1,Noeud2(e4,a2,a3),Noeud2(e5,a4,a5),Noeud2(e6,a6,a7)) when x = e2 || (x < e3 && x > e1) -> Noeud3(e1,e3,a1,Noeud4(e4,e2,e5,a2,a3,a4,a5),Noeud2(e6,a6,a7))
	
			
		| a -> a;;



(* supprime x dans l'arbre arbre234 *)
let delete x arbre234 = suppr x ( equi_suppr x arbre234 );;


		
		
		
(** ====================================================  5  ====================================================================== **)

(* renvoie la valeur maximum de l'arbre	*)
let max_234 arbre234 = 
  let rec max = function
		| Vide234 -> raise (Failure "pas de max")
    | Noeud2(e1,Vide234,Vide234) -> e1
    | Noeud3(e1,e2,Vide234,Vide234,Vide234) -> e2
    | Noeud4(e1,e2,e3,Vide234,Vide234,Vide234,Vide234) -> e3
    | Noeud2(e1,a1,a2) -> max a2 
    | Noeud3(e1,e2,a1,a2,a3) -> max a3
    | Noeud4(e1,e2,e3,a1,a2,a3,a4) -> max a4
  in max arbre234;;



(* renvoie la valeur minimum de l'arbre	*)
let min_234 arbre234 = 
  let rec min = function
	  | Vide234 -> raise (Failure "pas de min")
    | Noeud2(e1,Vide234,Vide234) -> e1
    | Noeud3(e1,e2,Vide234,Vide234,Vide234) -> e1
    | Noeud4(e1,e2,e3,Vide234,Vide234,Vide234,Vide234) -> e1
    | Noeud2(e1,a1,a2) -> min a1 
    | Noeud3(e1,e2,a1,a2,a3) -> min a1
    | Noeud4(e1,e2,e3,a1,a2,a3,a4) -> min a1
  in min arbre234;;



(* renvoie true si l'arbre est vide *)
let est_vide = function
  | Vide234 -> true
  | _ -> false;;
				
				
				
				
(** 5.1 **)
(* UNION de deux ensembles *)
let rec union a b =
  if est_vide b then a 
  else 
    let min = min_234 b in 
    if not (research234 min a) then union (insert a min) (delete min b)
    else union a (delete min b);;


(** 5.2 **)
(* IINTERSECTION de deux ensembles *)
let intersection a b = 
  let rec aux acc ab2 = 
    if est_vide ab2 then acc
    else 
      let min = min_234 ab2 in
      if  research234 min a then aux (insert acc min) (delete min ab2)
      else aux acc (delete min ab2)
  in aux Vide234 b;;


(** 5.3 **)
(* DIFFERENCE de deux ensembles *)
let difference a b = 
  let rec aux acc ab1 ab2 = 
    if est_vide ab1 then acc
    else 
      let min = min_234 ab1 in
      if  not (research234 min ab2) then aux (insert acc min)  (delete min ab1) ab2
      else aux acc  (delete min ab1) (delete min ab2)
  in aux Vide234 a b;;


(** 5.4 **)
(* DIFFERENCE SYMETRIQUE de deux ensembles *)
let difference_symetrique a b = union (difference a b) (difference b a);;


(** 5.5 **)
(* EGALITE de deux ensembles *)
let est_egal a b = 
  (est_inclus a b) && (est_inclus b a);;



(** 5.6 **)
(* INCLUSION d'un ensemble dans un autre *)
let est_inclus a b = 
  let rec aux ab1=
    if est_vide ab1 then true
    else 
      let min = min_234 ab1 in 
      if research234 min b then aux (delete min ab1)
      else false
  in aux a;;



(** ======================================================  6  ==================================================================== **)

(** 6.1 **)

(* équivalent de List.fold_left pour les arbre 2-3-4 *)
let rec fold_left234 f acc arbre234 =
  match arbre234 with
  | Vide234 -> acc 
  | Noeud2(e1,a1,a2) ->  fold_left234 f (fold_left234 f (f acc e1) a1) a2
  | Noeud3(e1,e2,a1,a2,a3) -> fold_left234 f (fold_left234 f (f (fold_left234 f (f acc e1) a1) e2) a2) a3
  | Noeud4(e1,e2,e3,a1,a2,a3,a4) -> fold_left234 f (fold_left234 f (f (fold_left234 f ((f (fold_left234 f (f acc e1) a1)) e2) a2) e3) a3) a4 ;;
              

(* équivalent de List.fold_right pour les arbre 2-3-4 *)
let rec fold_right234 f arbre234 acc = 
  match arbre234 with 
  |Vide234 -> acc 
  |Noeud2(e1,a1,a2) -> fold_right234 f a2 (f e1 (fold_right234 f a1 acc))
  |Noeud3(e1,e2,a1,a2,a3) ->  fold_right234 f a3 (f e2 (fold_right234 f a2 (f e1 (fold_right234 f a1 acc))))
  |Noeud4(e1,e2,e3,a1,a2,a3,a4) ->  fold_right234 f a4 (f e3 (fold_right234 f a3 (f e2 (fold_right234 f a2 (f e1 (fold_right234 f a1 acc))))) );;
       





(** 6.2 **)		
(* calcul le nombre d'élément dans un arbre *)		
let cardinal arbre234 =
  fold_left234 (fun a b -> 1 + a ) 0  arbre234;;
	
	
(* autre version du calcul du cardinal *)
let cardinal_vsup arbre234 =
  let rec aux acc a =
    if est_vide a then 
      acc
    else
      let max = max_234 a in
      aux (acc+1) (delete max a)
  in aux 0 arbre234;;





(* fonction de séparation *)
let separer f arbre234 = 
  let rec aux a acc1 acc2 =
    if est_vide a then
      acc1,acc2
    else
      let max = max_234 a in
      if f max then
        aux (delete max a) (insert acc1 max) acc2
      else
        aux (delete max a) acc1 (insert acc2 max)
  in aux arbre234 Vide234 Vide234;;


(* fonction de filtre *)
let filtrer p arbre234 =
  let rec aux a acc =
    if est_vide a then acc
    else
      let max = max_234 a in
      if p max then 
        aux (delete max a) (insert acc max)
      else aux (delete max a) acc
  in aux arbre234 Vide234;;







