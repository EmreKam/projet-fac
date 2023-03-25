module type TypeOrdonne = sig
  type t

  val comp : t -> t -> int
end

module type Ensemble = sig
  type element
  type abc

  (* vide : Génère un arbre vide *)
  val vide : abc

  (* estVide : vérifie si l'arbre est vide *)
  val estVide : abc -> bool
  (* estDans : vérifie si un élément est dans l'arbre *)

  (* 	si n == VIDE alors *)
  (* 		renvoyer faux *)
  (* 	sinon *)
  (* 		si n == e alors *)
  (* 			renvoyer vrai *)
  (*		sinon *)
  (* 			si n < e alors *)
  (*  			renvoyer estDans (e, abcg) *)
  (*			renvoyer estDans (e, abcd) *)
  val estDans : element -> abc -> bool

  (* inserer : insère un élément dans un arbre *)
  val inserer : element -> abc -> abc

  (* renvoie la racine de l'arbre *)
  val el : abc -> element

  (* renvoie le sous arbre gauche *)
  val sag : abc -> abc

  (* renvoie le sous arbre droit *)
  val sad : abc -> abc

  (* supprimer : supprime un element de l'arbre *)
  val supprimer : element -> abc -> abc

  (* supprimer_ max : supprime le maximum de l'arbre et renvoie un couple de l'element supprimé et du nouvel arbre *)
  val supprimer_max : abc -> element * abc

  (* ensemble_vers_liste : Renvoie la liste des tous les éléments d'un arbre *)
  val ensemble_vers_liste : abc -> element list
end

module MakeEnsemble (O : TypeOrdonne) : Ensemble with type element = O.t =
struct
  let comp = O.comp
  let ( $=$ ) x y = comp x y = 0
  let ( $>$ ) x y = comp x y > 0
  let ( $<$ ) x y = comp x y < 0

  type element = O.t
  type couleur = Rouge | Noir | DoubleNoir
  type abc = Vide | VideNoir | Noeud of (element * couleur * abc * abc)

  let vide = Vide
  let estVide = function Vide -> true | VideNoir -> true | _ -> false
  let el = function Noeud (e, _, _, _) -> e | _ -> failwith "err el"
  let sag = function Noeud (_, _, ag, _) -> ag | _ -> failwith "err sag"
  let sad = function Noeud (_, _, _, ad) -> ad | _ -> failwith "err sad"

  let rec estDans x = function
    | Vide -> false
    | Noeud (a, _, _, _) when a $=$ x -> true
    | Noeud (a, _, ag, _) when a $>$ x -> estDans x ag
    | Noeud (a, _, _, ad) when a $<$ x -> estDans x ad
    | _ -> failwith "err estDans"

  (* racineRouge : Renvoie si le noeud est Rouge *)
  let racineRouge = function Noeud (_, Rouge, _, _) -> true | _ -> false

  (* colorerRacine : colore la racine du noeud *)
  let colorerRacine c = function
    | Noeud (r, _, ag, ad) -> Noeud (r, c, ag, ad)
    | a -> a

  (* filsRouge : renvoie si le noeud comporte un fils rouge *)
  let filsRouge = function
    | Noeud (_, _, ag, ad) -> racineRouge ad || racineRouge ag
    | _ -> false

  (* equilibrer : équilibre l'arbre pour qu'il reste un ab rn *)
  let rec equilibrer = function
    (* definition 1 *)
    | Noeud (g, Noir, ag, ad)
      when racineRouge ad && racineRouge ag && (filsRouge ag || filsRouge ad) ->
        Noeud (g, Rouge, colorerRacine Noir ag, colorerRacine Noir ad)
    (* definition 3.1 voir les dessins que j'ai fais sur la feuille raccourci *)
    | Noeud (g, Noir, Noeud (p, Rouge, (Noeud (_, Rouge, _, _) as x), y), f) ->
        Noeud (p, Noir, x, Noeud (g, Rouge, y, f)) (*definition 3.2 *)
    | Noeud (g, Noir, Noeud (p, Rouge, a1, Noeud (x, Rouge, a2, a3)), f) ->
        equilibrer
          (Noeud (g, Noir, Noeud (x, Rouge, Noeud (p, Rouge, a1, a2), a3), f))
        (* definition 3.3*)
    | Noeud (g, Noir, f, Noeud (p, Rouge, Noeud (x, Rouge, a1, a2), a3)) ->
        equilibrer
          (Noeud (g, Noir, f, Noeud (x, Rouge, a1, Noeud (p, Rouge, a2, a3))))
        (* definition 3.4 *)
    | Noeud (g, Noir, a1, Noeud (p, Rouge, a2, (Noeud (_, Rouge, _, _) as x)))
      ->
        Noeud (p, Noir, Noeud (g, Rouge, a1, a2), x)
    | a -> a

  let inserer x t =
    let rec aux = function
      | Vide -> Noeud (x, Rouge, Vide, Vide)
      | Noeud (r, c, ag, ad) when r $<$ x ->
          equilibrer (Noeud (r, c, ag, aux ad))
      | Noeud (r, c, ag, ad) when x $<$ r ->
          equilibrer (Noeud (r, c, aux ag, ad))
      | a -> a
    in
    colorerRacine Noir (aux t)

  (* noirSansFilsRouge : vérife si un noeud noir est sans fils rouge *)
  let noirSansFilsRouge = function
    | Noeud (_, Noir, Noeud (_, c1, _, _), Noeud (_, c2, _, _))
      when c1 <> Rouge && c2 <> Rouge ->
        true
    | Noeud (_, Noir, Noeud (_, c1, _, _), Vide) when c1 <> Rouge -> true
    | Noeud (_, Noir, Vide, Noeud (_, c2, _, _)) when c2 <> Rouge -> true
    | _ -> false

  (* estProblematique : renvoie le cas le noeud est de couleur DoubleNOir ou si il est VideNoir *)
  let estProblematique = function
    | Noeud (_, DoubleNoir, _, _) | VideNoir -> true
    | _ -> false

  (* augmenterCouleur : augmente la couleur du noeud (rouge -> noir, noir -> DoubleNoir *)
  let augmenterCouleur = function
    | Noeud (r, Rouge, ag, ad) -> Noeud (r, Noir, ag, ad)
    | Noeud (r, Noir, ag, ad) -> Noeud (r, DoubleNoir, ag, ad)
    | _ -> failwith "err"

  (* diminuerCouleur : diminue la couleur du noeud (DoubleNoir -> noir, Noir -> Rouge) *)
  let diminuerCouleur = function
    | Noeud (r, DoubleNoir, ag, ad) -> Noeud (r, Noir, ag, ad)
    | Noeud (r, Noir, ag, ad) -> Noeud (r, Rouge, ag, ad)
    | _ -> failwith "err"

  (* diminuerPoidsNoir : diminue le poids du Noeud (VideNoir -> Vide) sinon diminue la couleur du noeud *)
  let diminuerPoidsNoir = function
    | VideNoir -> Vide
    | Noeud (_, (DoubleNoir | Noir), _, _) as x -> diminuerCouleur x
    | a -> a

  (* augmenterPoidsNoir : augmente le poids du Noeud (Vide -> VideNOir) sinon augmener la couleur du noeud *)
  let augmenterPoidsNoir = function
    | Vide -> VideNoir
    | Noeud (_, (Noir | DoubleNoir), _, _) as x -> augmenterCouleur x
    | a -> a

  (* eq_supp : réequilibre l'arbre après une suppression *)
  let rec eq_supp = function
    | Noeud (p, c, x, f)
      when (estProblematique x && noirSansFilsRouge f)
           || (noirSansFilsRouge x && estProblematique f) ->
        augmenterCouleur
          (Noeud (p, c, diminuerPoidsNoir x, diminuerPoidsNoir f))
    (* def 1 *)
    | Noeud
        ( p,
          c,
          ((VideNoir | Noeud (_, DoubleNoir, _, _)) as x2),
          (Noeud (f, Noir, a3, a4) as f2) )
      when not (filsRouge f2) ->
        augmenterPoidsNoir
          (Noeud (p, c, diminuerPoidsNoir x2, Noeud (f, Rouge, a3, a4)))
    (* 2.1.1 *)
    | Noeud (p, c, x, Noeud (f, Noir, a3, Noeud (d, Rouge, a4, a5))) ->
        Noeud
          ( f,
            c,
            Noeud (p, Noir, diminuerPoidsNoir x, a3),
            Noeud (d, Noir, a4, a5) )
    (*2.1.2 *)
    | Noeud (p, c, x, Noeud (f, Noir, (Noeud (g, _, a3, a4) as g2), d))
      when (not (racineRouge d)) && racineRouge g2 ->
        eq_supp (Noeud (p, c, x, Noeud (g, Noir, a3, Noeud (f, Rouge, a4, d))))
    (* 2.1.3 *)
    | Noeud (p, Noir, x, Noeud (f, Rouge, a3, a4)) ->
        Noeud (f, Noir, eq_supp (Noeud (p, Rouge, x, a3)), a4)
    (* 2.2.1 *)
    | Noeud (p, c, Noeud (f, Noir, Noeud (g, Rouge, a1, a2), a3), x) ->
        Noeud
          ( f,
            c,
            Noeud (g, Noir, a1, a2),
            Noeud (p, Noir, a3, diminuerPoidsNoir x) )
    (* 2.2.2 *)
    | Noeud (p, c, Noeud (f, Noir, g, (Noeud (d, _, a1, a2) as d2)), x)
      when (not (racineRouge g)) && racineRouge d2 ->
        eq_supp (Noeud (p, c, Noeud (d, Noir, Noeud (f, Rouge, g, a1), a2), x))
    (* 2.2.3 *)
    | Noeud (p, Noir, Noeud (f, Rouge, ag, ad), x) ->
        Noeud (f, Noir, ag, eq_supp (Noeud (p, Rouge, ad, x)))
    | a -> a
  (*
  let rec supprimer_max = function
    | Noeud (r, Noir, Vide, Vide) -> (r, VideNoir)
    | Noeud (r, Noir, Noeud (r', Rouge, Vide, Vide), Vide) ->
        (r, Noeud (r', Noir, Vide, Vide))
    | Noeud (r, Rouge, Vide, Vide) -> (r, Vide)
    | Noeud (r, c, ag, ad) ->
        let m, ad' = supprimer_max ad in
        (m, eq_supp (Noeud (r, c, ag, ad')))
    | _ -> failwith "err sup max"
*)

  let rec supprimer_max = function
    | Noeud (r, Noir, Vide, Vide) -> (r, VideNoir)
    | Noeud (r, Noir, Noeud (r', Rouge, Vide, Vide), Vide) ->
        (r, Noeud (r', Noir, Vide, Vide))
    | Noeud (r, Rouge, Vide, Vide) -> (r, Vide)
    | Noeud (_, c, ag, ad) ->
        let m, ad' = supprimer_max ad in
        (m, eq_supp (Noeud (m, c, ag, ad')))
    | _ -> failwith "err smax"

  (*
  let rec supprimer_max = function
    | Noeud (r, Noir, Vide, Vide) -> (r, VideNoir)
    | Noeud (r, Noir, Noeud (r2, Rouge, Vide, Vide), Vide) ->
        (r, Noeud (r2, Noir, Vide, Vide))
    | Noeud (r, Rouge, Vide, Vide) -> (r, Vide)
    | Noeud (r, c, ag, ad) ->
        let m, ad' = supprimer_max ad in
        (m, eq_supp (Noeud (r, c, ag, ad')))
    | _ -> failwith "supprimer_max:err "
*)
  (* supprimer_racine : supprime la racine d'un arbre *)
  let supprimer_racine = function
    | Noeud (_, Noir, Vide, Vide) -> VideNoir
    | Noeud (_, Noir, Vide, Noeud (r', Rouge, Vide, Vide)) ->
        Noeud (r', Noir, Vide, Vide)
    | Noeud (_, Rouge, Vide, Vide) -> Vide
    | Noeud (_, c, ag, ad) ->
        let m, ag' = supprimer_max ag in
        eq_supp (Noeud (m, c, ag', ad))
    | _ -> failwith "err sup rac"

  (* stabiliser : stabilise les couleurs de l'arbre *)
  let stabiliser = function
    | VideNoir -> Vide
    | Noeud (r, DoubleNoir, ag, ad) -> Noeud (r, Noir, ag, ad)
    | a -> a

  let supprimer x a =
    let rec aux = function
      | Vide -> Vide
      | Noeud (r, c, ag, ad) when comp x r = -1 -> Noeud (r, c, aux ag, ad)
      | Noeud (r, c, ag, ad) when comp x r = 1 -> Noeud (r, c, ag, aux ad)
      | a -> supprimer_racine a
    in
    stabiliser (aux a)

  let ensemble_vers_liste a =
    let rec aux acc = function
      | [] -> acc
      | h :: t ->
          if estVide h then aux acc t
          else aux (el h :: acc) (sag h :: sad h :: t)
    in
    aux [] [ a ]
end
