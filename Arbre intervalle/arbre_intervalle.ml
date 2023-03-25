open Intervalle
open ArbreRN
module K = MakeIntervalle (Entier)

module MakeInter : TypeOrdonne with type t = K.inter = struct
  type t = K.inter

  let comp = K.comp
end

module I = MakeEnsemble (MakeInter)
open I
open K

(* affichage prefixe des intervalles d'un arbre RougeNoir  *)

let rec affichage a =
  if not (estVide a) then
    if left_b (el a) == true && right_b (el a) == true then
      Printf.printf "[%d;%d]\n" (left_e (el a)) (right_e (el a))
    else if left_b (el a) == false && right_b (el a) == true then
      Printf.printf "]%d;%d]\n" (left_e (el a)) (right_e (el a))
    else if left_b (el a) == true && right_b (el a) == false then
      Printf.printf "[%d;%d[\n" (left_e (el a)) (right_e (el a))
    else Printf.printf "]%d;%d[\n" (left_e (el a)) (right_e (el a));
  if not (estVide (sag a)) then affichage (sag a);
  if not (estVide (sad a)) then affichage (sad a)

(* est_dans_arbre_intervalle : Indique si une valeur apparait dans un des intervalles de l'arbre RougeNoir *)
(*let rec est_dans_arbre_intervalle x = function
  (* version moins opti que celle du dessous *)
  | a when not (estVide a) ->
      if est_dans_intervalle x (el a) == true then true
      else
        est_dans_arbre_intervalle x (sag a)
        || est_dans_arbre_intervalle x (sad a)
  | _ -> false*)

(* old version (plus performante)*)
let rec est_dans_arbre_intervalle x = function
  | a when not (estVide a) ->
      if est_dans_intervalle x (el a) then true
      else if left_e (el a) >= x then est_dans_arbre_intervalle x (sag a)
      else est_dans_arbre_intervalle x (sad a)
  | _ -> false

(* ========================= QUESTION 7 =========================== *)

(* list_ndisj : crée une liste d'intervalles non disjoints par rapport à l'intervalle i dans l'arbre RougeNoir *)
let list_ndisj i a =
  let rec aux acc = function
    | [] -> List.rev acc
    | e :: l when estVide e -> aux acc l
    | e :: l ->
        if sont_disjoins i (el e) then aux acc (sag e :: sad e :: l)
        else aux (el e :: acc) (sag e :: sad e :: l)
  in
  aux [] [ a ]

(* retrait_a_list : Retire les intervalles d'une liste dans l'arbre RougeNoir *)
let rec retrait_a_list a = function
  | [] -> a
  | e :: l -> retrait_a_list (supprimer e a) l

(* transform_either_inter  : Transforme un type Either Right en intervalle *)
let transform_either_inter e =
  match e with Either.Right u -> u | _ -> failwith "err trans"

(* union_inter_list : Effectue l'union entre les intervalles de l et de i lorsque i et les intervalles de l sont non disjoints *)
let union_inter_list i l =
  if l == [] then i
  else
    let rec aux acc l =
      match l with
      | [] -> acc
      | e :: tl -> aux (transform_either_inter (union acc e)) tl
    in
    aux i l

(* left_u : Permet de récupérer le couple d'intervalles d'un Either.left *)
let left_u = function Either.Left u -> u | _ -> failwith "err"

(* inserer_d : Insère dans l'arbre RougeNoir chaque différente entre l'intervalle e et i, [e....ei] *)
let inserer_d l i a =
  let rec aux nd ab =
    match nd with
    | [] -> ab
    | e :: tl ->
        let x = difference e i in
        if Either.is_left x then
          let a' = inserer (fst (left_u x)) ab in
          aux tl (inserer (snd (left_u x)) a')
        else aux tl (inserer (transform_either_inter x) ab)
  in
  aux l a

(* ajout : Ajoute un intervalle i dans un arbre RougeNoir; à recours à [list_ndisj; retrait_a_list; union_inter_list] *)
let ajout i a =
  if estVide a then inserer i a
  else
    let x = list_ndisj i a in
    let r = retrait_a_list a x in
    let e = union_inter_list i x in
    inserer e r

(* retrait : supprime un intervalle i dans un arbre RougeNoir; à recours à [list_ndisj; retrait_a_list; inserer_d] *)
let retrait i a =
  let x = list_ndisj i a in
  let r = retrait_a_list a x in
  if x == [] then a else inserer_d x i r

(* ------ EXEMPLES SUJET ------ *)

(* Figure 1 du sujet *)
let fig1 =
  ajout
    (inter true 126 28954 true)
    (ajout (inter true 50 50 true)
       (ajout (inter true 0 4 true)
          (ajout (inter true 100 125 true)
             (ajout (inter false 5 9 false)
                (ajout (inter true 15 17 true) vide)))))

(* affiche la liste non disjoints de la figure 1 par rapport à [6;20] *)
let non_disjoints1 = list_ndisj (inter true 6 20 true) fig1

(* Figure 5  du sujet -> ajout de [6,20] dans figure 1 *)
let fig5 = ajout (inter true 6 20 true) fig1

(* affiche la liste non disjoints de la figure 5 par rapport à [2;17] *)
let non_disjoints2 = list_ndisj (inter true 2 17 true) fig5
let fig7 = retrait (inter true 2 17 true) fig5

(* =================== QUESTION 8 ===================== *)

(* randomList : crée une liste aléatoire d'intervalles avec x taille approximatif de la liste et une borne max *)
let randomList x borne =
  let rec aux acc x b =
    match x with
    | 0 -> acc
    | _ ->
        let bo = b / 10 in
        let e2 = Random.int b in
        let e1 = max 0 (e2 - Random.int bo) in
        let b1 = if e1 = e2 then true else Random.bool () in
        let b2 = if e1 = e2 then true else Random.bool () in
        let i = inter b1 e1 e2 b2 in
        if left_e i == right_e i && left_b i == false && right_b i == false then
          aux acc (x - 1) b
        else aux (i :: acc) (x - 1) b
  in
  aux [] x borne

(* insert_list_abi_empty : Permet créer un arbre RougeNoir à partir d'une liste d'intervalles *)
let insert_list_abi_empty l =
  let rec aux acc = function
    | [] -> acc
    | e :: tl when not (inter_vide e) -> aux (ajout e acc) tl
    | _ :: tl -> aux acc tl
  in
  aux vide l

(* let test () = affichage (insert_list_abi_empty (randomList 10 1000)) *)

(* aff_list_inter : Affiche une liste d'intervalles *)
let rec aff_list_inter = function
  | [] -> ""
  | e :: tl -> inter_to_string1 e ^ " ; " ^ aff_list_inter tl

(* transform_interl_intl : Transforme une liste d'intervalles en liste d'entier présent dans l'arbre *)
let transform_interl__intl li =
  let rec aux acc = function
    | [] -> acc
    | e :: rl -> aux (inter_to_list_aux1 e @ acc) rl
  in
  aux [] li

(* valList_in_ab : Vérifie si les entiers d'une liste sont présent dans un arbre RougeNoir *)
let valList_in_ab l a =
  let rec aux = function
    | [] -> true
    | e :: rl when est_dans_arbre_intervalle e a -> aux rl
    | _ -> false
  in
  aux l

(* arbre_2a2_disjoints : Verifie si les intervalles d' un arbre sont  disjoints 2 à 2 *)
let inter_list_disjoints i l =
  let rec aux = function
    | [] -> true
    | h :: t -> if comp i h = 0 || sont_disjoins i h then aux t else false
  in
  aux l

let arbre_2a2_disjoints a =
  let l = ensemble_vers_liste a in
  List.fold_left (fun b i -> b && inter_list_disjoints i l) true l

(* randomList_entier : Permet de générer une liste d'entier de taille n, avec une borne *)
let randomList_entier x borne =
  let rec aux acc x =
    match x with 0 -> acc | _ -> aux (Random.int borne :: acc) (x - 1)
  in
  aux [] x

(* liste_dans_inter : vérifie que les entiers d'une liste sont dans un intervalle *)
let liste_dans_inter l i =
  List.fold_left
    (fun y x -> if est_dans_intervalle x i then y else y = false)
    true l

(* vérifier que chaque valeur de l’ est dans a’ si et seulement si elle est dans a, mais pas dans i’*)
let test_8 a a' i l =
  let rec aux acc = function
    | [] -> acc
    | h :: t ->
        if est_dans_arbre_intervalle h a' then
          aux (est_dans_arbre_intervalle h a && not (est_dans_intervalle h i)) t
        else true && aux acc t
  in
  aux true l

(* valide_arbres_inter : Effectue un test de validité *)
let valide_arbres_inter () =
  let borne = 1000 in
  let l = randomList (Random.int 10) borne in
  let a = insert_list_abi_empty l in
  let b = valList_in_ab (transform_interl__intl l) a in
  let ad = arbre_2a2_disjoints a in
  let e2 = Random.int borne in
  let e1 = Random.int e2 - Random.int 100 in
  let i1 = inter (Random.bool ()) e1 e2 (Random.bool ()) in
  let a' = retrait i1 a in
  let l' = randomList_entier 10 borne in
  let b2 = test_8 a a' i1 l' in
  Printf.printf "\n > la liste aléatoire l :\n\n %s\n\n" (aff_list_inter l);
  affichage a;
  Printf.printf "\n > Tout les valeurs de l sont dans l'arbre : %b\n" b;
  Printf.printf " > les intervalles de l'arbre sont disjoints :  %b\n" ad;
  Printf.printf " > intervalles présent après retrait de %s\n"
    (inter_to_string1 i1);
  Printf.printf
    " > Les valeurs de l' sont dans a' ssi elles sont dans a mais pas dans i1: \
     %b\n\n"
    b2;
  affichage a'
