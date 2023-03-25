module type TypeOrdonne = sig
  type t

  val comp : t -> t -> int
end

(*Type int *)
module Entier : TypeOrdonne with type t = int = struct
  type t = int

  let comp = compare
end

(*Type string *)
module String : TypeOrdonne with type t = string = struct
  type t = string

  let comp = compare
end

module type Intervalle = sig
  type element
  type inter

  (* inter x y b_x b_y crée un intervalle de borne gauche b_x, d'élément gauche x,
     d'élément droit y et de borne droite b_y. Renvoie None si x est supérieur strictement à y. *)
  val inter : bool -> element -> element -> bool -> inter

  (* Renvoie true si x est dans l'intervalle i, false sinon. *)
  val est_dans_intervalle : element -> inter -> bool

  (* Renvoie la borne gauche de l'intervalle. *)
  val left_b : inter -> bool

  (* Renvoie la borne droite de l'intervalle. *)
  val right_b : inter -> bool

  (* Renvoie l'élément gauche de l'intervalle. *)
  val left_e : inter -> element

  (* Renvoie l'élément droit de l'intervalle. *)
  val right_e : inter -> element

  (* comp i j: Renvoie -1 si i est strictement inférieur à j, 1 si i est strictement supérieur à j
     et 0 si i et j sont égaux. *)
  val comp : inter -> inter -> int

  (* Renvoie true si i est inclus dans j, false sinon *)
  val est_inclus : inter -> inter -> bool

  (* Renvoie true is i et j sont disjoins, false si i et j sont joins *)
  val sont_disjoins : inter -> inter -> bool

  (* Renvoie true si l'intervalle est vide, false sinon.*)
  val inter_vide : inter -> bool

  (* Renvoie l'union de i et j:
      - si i et j sont joins, l'intervalle obtenu est l'union des deux intervalles
      - si i et j sont disjoins, on obtient un couple d'intervalle *)
  val union : inter -> inter -> (inter * inter, inter) Either.t

  (* Renvoie true si la borne gauche et l'élément gauche de i et j sont identiques, false sinon. *)
  val meme_borne_g : inter -> inter -> bool

  (* Renvoie true si la borne droite et l'élément droit de i et j sont identiques, false sinon. *)
  val meme_borne_d : inter -> inter -> bool

  (* Renvoie l'intervalle i privé de l'intervalle j:
      - si i et j sont disjoins, renvoie i
      - si i et j sont joins, renvoie i privé de j si j n'est pas inclus dans i
         ou un couple d'intervalle de i scindé en deux par rapport à j *)
  val difference : inter -> inter -> (inter * inter, inter) Either.t

  (* Si e est de la forme Either.right u, renvoie u. *)
  val transform_r : ('a, 'b) Either.t -> 'b

  (* Si e est de la forme Either.left u, renvoie u. *)
  val transform_l : ('a, 'b) Either.t -> 'a

  (* Renvoie true si x est un intervalle seul, false sinon (par exemple un couple d'intervalle). *)
  val inter_solo : ('a, 'b) Either.t -> bool

  (* Renvoie true si x est dans l'intervalle / le couple d'intervalles e, false sinon *)
  val est_dans : element -> (inter * inter, inter) Either.t -> bool

  (* Renvoie l'intervalle en argument *)
  val renv_inter : inter -> inter
end

module MakeIntervalle (O : TypeOrdonne) : Intervalle with type element = O.t =
struct
  let ( $=$ ) x y = compare x y = 0
  let ( $>$ ) x y = compare x y > 0
  let ( $<$ ) x y = compare x y < 0

  (* ======================================== Q1.1 ======================================== *)

  type element = O.t
  type inter = None | Inter of (bool * element * element * bool)

  (* ======================================== Q1.2 ======================================== *)

  let inter b_x x y b_y = if x $>$ y then None else Inter (b_x, x, y, b_y)

  (* ======================================== Q1.3 ======================================== *)

  let est_dans_intervalle x i =
    match i with
    | Inter (b1, e1, _, _) when x $=$ e1 -> b1
    | Inter (_, _, e2, b2) when x $=$ e2 -> b2
    | Inter (_, e1, e2, _) when x $>$ e1 && x $<$ e2 -> true
    | _ -> false

  (* ======================================== Q2 ======================================== *)

  let left_b = function
    | Inter (b1, _, _, _) -> b1
    | _ -> failwith "left_b: err"

  let right_b = function
    | Inter (_, _, _, b2) -> b2
    | _ -> failwith "right_b: err"

  let left_e = function
    | Inter (_, e1, _, _) -> e1
    | _ -> failwith "left_e: err"

  let right_e = function
    | Inter (_, _, e2, _) -> e2
    | _ -> failwith "right_e: err"

  let comp i j =
    match (i, j) with
    | i, j
      when left_e i < left_e j
           || (left_e i = left_e j && left_b j < left_b i)
           || left_e i = left_e j
              && left_b i = left_b j
              && right_e i < right_e j
           || left_e i = left_e j
              && left_b i = left_b j
              && right_e i = right_e j
              && right_b i < right_b j ->
        -1
    | i, j
      when left_e i = left_e j
           && left_b i = left_b j
           && right_e i = right_e j
           && right_b i = right_b j ->
        0
    | _ -> 1

  (* ======================================== Q3 ======================================== *)

  let est_inclus i j =
    if left_b i && right_b i then
      est_dans_intervalle (left_e i) j && est_dans_intervalle (right_e i) j
    else if left_b i then
      est_dans_intervalle (left_e i) j && right_e i <= right_e j
    else if right_b i then
      est_dans_intervalle (right_e i) j && left_e i >= left_e j
    else left_e i >= left_e j && right_e i <= right_e j

  let sont_disjoins i j =
    if est_inclus i j || est_inclus j i then false
    else if left_b i && right_b i then
      (not (est_dans_intervalle (left_e i) j))
      && not (est_dans_intervalle (right_e i) j)
    else if left_b i then
      ((not (est_dans_intervalle (left_e i) j)) && right_e i <= left_e j)
      || (not (est_dans_intervalle (left_e i) j))
         && if right_b j then left_e i > right_e j else left_e i >= right_e j
    else if right_b i then
      ((not (est_dans_intervalle (right_e i) j)) && left_e i >= right_e j)
      || (not (est_dans_intervalle (right_e i) j))
         && if left_b j then right_e i < left_e j else right_e i <= right_e j
    else right_e i <= left_e j || left_e i >= right_e j

  (*
let sont_disjoins i j =
  if est_inclus i j || est_inclus j i then false
  else if left_b i && right_b i then
    (not (est_dans_intervalle (left_e i) j))
    && not (est_dans_intervalle (right_e i) j)
  else if left_b i then
    if right_b j then
      ((not (est_dans_intervalle (left_e i) j)) && right_e i <= left_e j)
      || ((not (est_dans_intervalle (left_e i) j)) && left_e i > right_e j)
    else
      ((not (est_dans_intervalle (left_e i) j)) && right_e i <= left_e j)
      || ((not (est_dans_intervalle (left_e i) j)) && left_e i >= right_e j)
  else if right_b i then
    if left_b j then
      ((not (est_dans_intervalle (right_e i) j)) && left_e i >= right_e j)
      || ((not (est_dans_intervalle (right_e i) j)) && right_e i > left_e j)
    else
      ((not (est_dans_intervalle (right_e i) j)) && left_e i >= right_e j)
      || ((not (est_dans_intervalle (right_e i) j)) && right_e i >= right_e j)
  else right_e i <= left_e j || left_e i >= right_e j
*)

  (* ======================================== Q4 ========================================  *)

  let inter_vide = function None -> true | _ -> false

  let union_aux i j =
    let e1 = if left_e i <= left_e j then left_e i else left_e j in
    let e2 = if right_e i <= right_e j then right_e j else right_e i in
    let b1 =
      if left_e i < left_e j then left_b i
      else if left_e i > left_e j then left_b j
      else if left_b i || left_b j then true
      else false
    in
    let b2 =
      if right_e i < right_e j then right_b j
      else if right_e i > right_e j then right_b i
      else if right_b i || right_b j then true
      else false
    in
    let u = Inter (b1, e1, e2, b2) in
    if not (sont_disjoins i j) then Either.right u else Either.left (i, j)

  let union i j =
    if inter_vide i && inter_vide j then Either.right None
    else if inter_vide i then Either.right j
    else if inter_vide j then Either.right i
    else union_aux i j

  let meme_borne_g i j = left_e i = left_e j && left_b i = left_b j
  let meme_borne_d i j = right_e i = right_e j && right_b i = right_b j

  let difference i j =
    let d =
      if sont_disjoins i j then i (*1*)
      else if comp i j = 0 || est_inclus i j then None (*5*)
      else if meme_borne_g i j (*6*) then
        if right_b j then Inter (false, right_e j, right_e i, right_b i)
        else Inter (true, right_e j, right_e i, right_b i)
      else if meme_borne_d i j (*7*) then
        if left_b j then Inter (left_b i, left_e i, left_e j, false)
        else Inter (left_b i, left_e i, left_e j, true)
      else if comp i j < 0 (*2*) then
        if left_b j then Inter (left_b i, left_e i, left_e j, false)
        else Inter (left_b i, left_e i, left_e j, true)
      else if comp i j > 0 (*4*) then
        if right_b j then Inter (false, right_e j, right_e i, right_b i)
        else Inter (true, right_e j, right_e i, right_b i)
      else None
    in
    let d2 =
      (*3*)
      if left_e i = left_e j && not (left_b j) then
        if right_b j then
          ( Inter (true, left_e i, left_e i, true),
            Inter (false, right_e j, right_e i, right_b i) )
        else
          ( Inter (true, left_e i, left_e i, true),
            Inter (true, right_e j, right_e i, right_b i) )
      else if right_e i = right_e j && not (right_b j) then
        if left_b j then
          ( Inter (left_b i, left_e i, left_e j, false),
            Inter (true, right_e i, right_e i, true) )
        else
          ( Inter (left_b i, left_e i, left_e j, true),
            Inter (true, right_e i, right_e i, true) )
      else if left_b j && right_b j then
        ( Inter (left_b i, left_e i, left_e j, false),
          Inter (false, right_e j, right_e i, right_b i) )
      else if left_b j then
        ( Inter (left_b i, left_e i, left_e j, false),
          Inter (true, right_e j, right_e i, right_b i) )
      else if right_b j then
        ( Inter (left_b i, left_e i, left_e j, true),
          Inter (false, right_e j, right_e i, right_b i) )
      else
        ( Inter (left_b i, left_e i, left_e j, true),
          Inter (true, right_e j, right_e i, right_b i) )
    in
    if est_inclus j i && (not (meme_borne_d i j)) && not (meme_borne_g i j) then
      Either.left d2
    else Either.right d

  (* ======================================== Q5 ========================================  *)

  let transform_r e =
    match e with Either.Right u -> u | _ -> failwith "err transform right"

  let transform_l e =
    match e with Either.Left u -> u | _ -> failwith "err transform left"

  let inter_solo x = Either.is_right x

  let est_dans x e =
    if inter_solo e then est_dans_intervalle x (transform_r e)
    else
      est_dans_intervalle x (fst (transform_l e))
      || est_dans_intervalle x (snd (transform_l e))

  let renv_inter = function
    | Inter (b1, e1, e2, b2) -> Inter (b1, e1, e2, b2)
    | None -> None
end

(* ========================================== MODULE ENTIER ========================================== *)
module M = MakeIntervalle (Entier)
open M

(* Transforme l'intervalle i en chaîne de caractères et le renvoie. *)
let inter_to_string1 i =
  if inter_vide i then Printf.sprintf "None"
  else
    Printf.sprintf "Inter (%b, %d, %d, %b)" (left_b i) (left_e i) (right_e i)
      (right_b i)

(* Transforme le couple d'intervalle (i,j) en chaîne de caractères et le renvoie. *)
let inter_to_string2 (i, j) =
  Printf.sprintf "(Inter (%b, %d, %d, %b), Inter (%b, %d, %d, %b))" (left_b i)
    (left_e i) (right_e i) (right_b i) (left_b j) (left_e j) (right_e j)
    (right_b j)

(* Transforme e en chaîne de caractères *)
let print_inter e =
  if inter_solo e then inter_to_string1 (transform_r e)
  else inter_to_string2 (transform_l e)

let inter_to_list_aux1 i =
  let rec aux acc = function
    | x when (x = right_e i && not (right_b i)) || x > right_e i -> List.rev acc
    | x when x = left_e i && not (left_b i) -> aux acc (x + 1)
    | x -> aux (x :: acc) (x + 1)
  in
  aux [] (left_e i)

let inter_to_list_aux2 (i, j) = inter_to_list_aux1 i @ inter_to_list_aux1 j

(* Renvoie la liste des nombres dans l'intervalle / le couple d'intervalles e*)
let inter_to_list e =
  if inter_solo e then inter_to_list_aux1 (transform_r e)
  else inter_to_list_aux2 (transform_l e)

(* Génère deux intervalle i1 et i2 puis aléatoirement entre 0 et 100:
    - affiche i1 et i2
    - affiche i3 l'union de i1 et de i2
    - affiche i4 la difference de i1 et de i2
    - affiche true ou false si toutes les valeurs de i1 & i2 sont dans i3
    - affiche true ou false si toutes les valeurs de i1 sont soit dans i2, soit dans i4 *)
let valide_inter () =
  let e2 = Random.int 100 in
  let e1 = Random.int e2 in
  let e4 = Random.int 100 in
  let e3 = Random.int e4 in
  let i1 = inter (Random.bool ()) e1 e2 (Random.bool ()) in
  let i2 = inter (Random.bool ()) e3 e4 (Random.bool ()) in
  let i3 = union i1 i2 in
  let i4 = difference i1 i2 in
  (*1.1*)
  Printf.printf " > i1 : %s\n" (inter_to_string1 i1);
  (*1.2*)
  Printf.printf " > i2 : %s\n" (inter_to_string1 i2);
  (*2*)
  Printf.printf " > i3 : %s\n" (print_inter i3);
  (*3*)
  Printf.printf " > i4 : %s\n" (print_inter i4);
  Printf.printf " > toutes les valeurs de i1 & i2 sont dans i3  : %b\n"
    (List.fold_left (fun b x -> b && est_dans x i3) true (inter_to_list_aux1 i1)
    && List.fold_left
         (fun b x -> b && est_dans x i3)
         true (inter_to_list_aux1 i2));
  Printf.printf
    " > toutes les valeurs de i1 sont soit dans i2, soit dans i4  : %b\n"
    (List.fold_left
       (fun b x -> b && (est_dans_intervalle x i2 || est_dans x i3))
       true (inter_to_list_aux1 i1))

(* ======================================== tests ======================================== *)
(*
   let i10 = Inter (true, 5, 14, true)
   let i11 = Inter (true, 13, 15, true)
   let i12 = Inter (true, 1, 8, true)
   let i13 = Inter (true, 9, 14, true)
   let i51 = Inter (false, 1, 50, false)
   let i52 = Inter (false, 1, 15, true)
   let i53 = Inter (true, 1, 15, false)
   let i54 = Inter (true, 1, 15, true)
   let i14 = Inter (false, 25, 30, false)
   let i20 = Inter (true, 6, 20, true)
   let i21 = Inter (false, 5, 9, true)
   let i22 = Inter (true, 15, 17, true)
   let i2021 = union i20 i21
   let t1 = transform_r i2021
   let i2122 = union i21 i22
   let t2 = transform_l i2122
   let t3 = Inter (false, 38, 43, false)
   let t4 = Inter (false, 18, 95, true)
   let d1 = difference t3 t4
*)

(* ========================================== MODULE STRING ========================================== *)
(*
module M1 = MakeIntervalle (String)
open M1

let a = inter (Random.bool ()) "aa" "bb" (Random.bool ())
let b = inter (Random.bool ()) "bb" "cc" (Random.bool ())
let res = comp a b*)
