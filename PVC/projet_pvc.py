import tkinter
from tkinter import *
import matplotlib.pyplot as plt
import numpy.random as nr
import numpy as np
import itertools
import threading
import time
import sys
from math import *
import os

NOMBRE_SOMMET = 0
mainframe = Tk()
mainframe.title("PVC")
TAILLE_ZT = tkinter.Entry()
titre = tkinter.Label (text = "Veuillez entrer un nombre de sommets du graphe")


# Renvoie un graphe avec n points aléatoire
def create_graph_alea(n):
  graph = []
  for k in range(n):
    x = round(nr.rand(), 3)
    y = round(nr.rand(), 3)
    while ([x, y] in graph):
      x = round(nr.rand(), 3)
      y = round(nr.rand(), 3)
    graph.append([x, y])
  return graph



#  2.1 =========================================================================

# Renvoie une matrice des distances par rapport au graphe graph
def matrice_poids(graph) :
  matrice = []
  for k in range(len(graph)) :
    matrice.append([])
    for p in range(len(graph)) :
      if k == p :
        matrice[k].append(0.0)
      else :
        matrice[k].append(poids_arrete(graph[k], graph[p]))
  return matrice


# Renvoie la distance entre les points p1 et p2
def poids_arrete(p1, p2) :
  res = round(sqrt((p2[0] - p1[0])**2 + (p2[1] - p1[1])**2), 3)
  return float(format(res, '.3f'))


# Affiche une matrice sur la sortie standard
def affiche_matrice(matrice) :
  for k in range(len(matrice)) :
    for p in range(len(matrice)) :
      print(format(matrice[k][p], '.3f'), "\t", end='')
    print("\n", end='')

# Renvoi l'indice du minimum strictement positif de liste,
# à l'exception des indices déjà parcourus
def trouve_min(liste, indices_parcourus) :
  mini = 2.0
  ind = None
  for p in range(len(liste)) :
    if (p not in indices_parcourus) :
      if (liste[p] > 0 and liste[p] <= mini) :
        mini = liste[p]
        ind = p
  return ind



# L'algorithme du plus proche voisin
def Ppvoisin(matrice, graph, sommet) :
  try:
    p = graph.index(sommet)
  except ValueError:
    return None
  voisin = [sommet]
  indices_parcourus = [p]
  while (len(indices_parcourus) < len(graph)) :
    p = trouve_min(matrice[p], indices_parcourus)
    indices_parcourus.append(p)
    voisin.append(graph[p])
  return voisin



# Renvoie le poids du cycle hamiltonien
def poids_cycle_hamiltonien(cycle) :
    res = 0
    for k in range(len(cycle)-1) :
        res += poids_arrete(cycle[k],cycle[k+1])
    res += poids_arrete(cycle[len(cycle)-1],cycle[0])
    return res

# Calcule les poids des cycles hamiltonien de tous les sommets et renvoie le plus petit
def Ppvoisin_all(G) :
    l = len(G)
    voisin = np.zeros(l)
    mat = matrice_poids(G)
    for k in range(l):
        res = round(poids_cycle_hamiltonien(Ppvoisin(mat,G,G[k])),3)
        voisin[k] = res
    return min(voisin)




#  2.2 =========================================================================

# Renvoie vrai si il y a une intersection entre [AB] et [CD]
# et qu'un décroisement est avantageux
def intersection(a ,b, c, d) :
  r = False
  if (a[0] == b[0] and a[1] == b[1]
      or a[0] == c[0] and a[1] == c[1]
      or a[0] == d[0] and a[1] == d[1]
      or b[0] == c[0] and b[1] == c[1]
      or b[0] == d[0] and b[1] == d[1]
      or c[0] == d[0] and c[1] == d[1]) :
    return r
  v_ab = [b[0] - a[0], b[1] - a[1]]
  v_cd = [d[0] - c[0], d[1] - c[1]]

  #test droites parallèles
  if ((v_ab[0] * v_cd[1] - v_ab[1] * v_cd[0]) == 0) :
    return r
  paramAB = -(-v_ab[0] * a[1] + v_ab[0] * c[1] + v_ab[1] * a[0] - v_ab[1] * c[0]) / (v_ab[0] * v_cd[1] - v_ab[1] * v_cd[0])
  paramCD = -(a[0] * v_cd[1] - c[0] * v_cd[1] - v_cd[0] * a[1] + v_cd[0] * c[1]) / (v_ab[0] * v_cd[1] - v_ab[1] * v_cd[0])
  if ((paramAB > 0 and paramAB < 1) and (paramCD > 0 and paramCD < 1)) :
    # test avantage
    if ((poids_arrete(a, b) + poids_arrete(c, d)) > (poids_arrete(a, c) + poids_arrete(d, b))) :
      r = True
  return r


# Fonction OptimisePpvoisin
def OptimisePpvoisin(cycle) :
  sommets = list(cycle)
  if (len(sommets) < 4) :
    return None
  inter = True
  while(inter) :
    inter = False
    for i in range(len(sommets) - 1) :
      for j in range(i + 1, len(sommets) - 1) :
        if (intersection(sommets[i], sommets[i + 1], sommets[j], sommets[j + 1])) :
          inter = True
          a = sommets[i + 1]
          sommets[i + 1] = sommets[j]
          sommets[j] = a
          i = 0
          break
      if (intersection(sommets[i], sommets[i + 1], sommets[len(sommets) - 1], sommets[0])) :
        inter = True
        a = sommets[i + 1]
        sommets[i + 1] = sommets[len(sommets) - 1]
        sommets[len(sommets) - 1] = a
        i = 0
  return sommets



#  2.3 =========================================================================

# Prend un graphe ainsi qu'un sommet de ce graphe en paramètres,
# renvoie le cycle qui choisit les arêtes de poids minimum à partir
# du sommet jusqu'à parcourir tout le graphe
def Apminimum(graph,sommet) :
  g = graph.copy()
  cur = sommet
  cycle = []
  g.remove(cur)
  cycle.append(cur)
  voisins = []
  while len(g) != 0 :
    for i in range(len(g)) :
      voisins.append(poids_arrete(cur,g[i]))
    cur = g[np.argmin(voisins)]
    g.remove(cur)
    cycle.append(cur)
    voisins.clear()
  return cycle



#  2.4 =========================================================================



# Prend un graphe ainsi qu'un sommet de ce graphe en paramètres,
# un cycle hamiltonien du graphe qui visite les sommets de l’arbre
# couvrant de poids minimum construit par l’algorithme de Prim,
# dans l’ordre préfixe
# cycleh -> cycle hamiltonien
def Pvcprim(graph, sommet) :
  g = graph.copy()
  cycleh = []
  cur = sommet
  cycleh.append(cur)
  g.remove(cur)
  while len(cycleh) != len(graph) :
    min = 1.5 #une arête peut valoir au maximum 1.414
    for i in range(len(cycleh)) :
      for i2 in range(len(g)) :
        if min > (min2 := poids_arrete(cycleh[i],g[i2])) :
          min = min2
          cur = g[i2]
    g.remove(cur)
    cycleh.append(cur)
  return cycleh





#  2.5 =========================================================================

# calcule toutes les arêtes du sommet vers tous les autres points du graphe
def calc_aretes(graph, sommet) :
  l = []
  for i in range(len(graph)) :
    l.append(poids_arrete(sommet,graph[i]))
  l.remove(0.0)
  return l

# calcule la borne minorante
def calcul_borne(graph, cycle) :
  res = 0.0
  min2 = 0.0
  for i in range(len(graph)) :
    l_arete = calc_aretes(graph, graph[i])
    if graph[i] in cycle and len(cycle) > 1:
      ind = cycle.index(graph[i])
      if ind == len(cycle)-1:
        min1 = poids_arrete(cycle[ind], cycle[ind-1])
      elif ind == 0:
        min1 = poids_arrete(cycle[0], cycle[1])
      else:
        min1 = poids_arrete(cycle[ind], cycle[ind+1])
        min2 = poids_arrete(cycle[ind], cycle[ind-1])
    else :
      min1 = min(l_arete)
    l_arete.remove(min1)
    if min2 == 0:
      min2 = min(l_arete)
    res += (min1 + min2)
  return res/2.0

# calcule une liste de tuples (borne, nouveau_point) à partir d'un cycle
# existant
def compute_succ_borne(graph, cycle, point):
  res_l = []
  for p in point:
    res = calcul_borne(graph, cycle)
    res_l.append((res, p))
  return res_l



def Esdemisomme(graph) :
  len_g = len(graph)
  cycle = []
  point = graph.copy()
  sommet = point[0]
  point.remove(sommet)
  cycle.append(sommet)
  borne = calcul_borne(graph, cycle)
  backtrack = 0
  while (len(cycle) != len_g):
    l_succ_borne = compute_succ_borne(graph, cycle, point)
    l_succ_borne.sort(key=lambda tup: tup[0])
    if ((l_succ_borne[backtrack])[0] <= borne or backtrack == len(point)-1 ):
      sommet = (l_succ_borne[0])[1]
      cycle.append(sommet)
      point.remove(sommet)
      borne = calcul_borne(graph, cycle)
      backtrack = 0
    else :
      cycle.remove(sommet)
      point.append(sommet)
      sommet = cycle[len(cycle)-1]
      backtrack += 1
  return cycle






#  affichage ===================================================================



def affichage(graphe, matrice, n) :

  plt.figure(figsize=(12, 6), dpi=80)
  start_time = time.time()
  ppv = Ppvoisin(matrice, graphe, graphe[0])
  end_time = time.time()

  list_x = []
  list_y = []
  plt.subplot(3, 3, 1)
  plt.axis("off")
  longueur_ppv = poids_cycle_hamiltonien(ppv)
  plt.title("Ppvoisin " + str(round(end_time - start_time, 10)) + "s")
  for gl in range(len(ppv)) :
    list_x.append(ppv[gl][0])
    list_y.append(ppv[gl][1])
  plt.plot(list_x, list_y, 'bo')
  for gl in range(len(ppv) - 1) :
    plt.plot([list_x[gl], list_x[gl + 1]], [list_y[gl], list_y[gl + 1]], 'r')
  plt.plot([list_x[len(ppv) - 1], list_x[0]], [list_y[len(ppv) - 1], list_y[0]], 'r')
  plt.axis([0, 1, 0, 1])


  start_time = time.time()
  oppv = OptimisePpvoisin(ppv)
  end_time = time.time()
  longueur_optimised_ppv = poids_cycle_hamiltonien(oppv)
  plt.subplot(3, 3, 2)
  plt.title("OptimisePpvoisin " + str(round(end_time - start_time,10)) +"s")
  plt.axis("off")
  list_x = []
  list_y = []
  for gl in range(len(oppv)) :
    list_x.append(oppv[gl][0])
    list_y.append(oppv[gl][1])
  plt.plot(list_x, list_y, 'bo')
  for gl in range(len(oppv) - 1) :
    plt.plot([list_x[gl], list_x[gl + 1]], [list_y[gl], list_y[gl + 1]], 'r')
  plt.plot([list_x[len(oppv) - 1], list_x[0]], [list_y[len(oppv) - 1], list_y[0]], 'r')
  plt.axis([0, 1, 0, 1])

  start_time = time.time()
  apm = Apminimum(graphe, graphe[0])
  end_time = time.time()
  longueur_apm = poids_cycle_hamiltonien(apm)
  plt.subplot(3, 3, 3)
  plt.title("Apminimum " + str(round(end_time - start_time,10)) + "s")
  plt.axis("off")
  list_x = []
  list_y = []
  for gl in range(len(apm)) :
    list_x.append(apm[gl][0])
    list_y.append(apm[gl][1])
  plt.plot(list_x, list_y, 'bo')
  for gl in range(len(apm) - 1) :
    plt.plot([list_x[gl], list_x[gl + 1]], [list_y[gl], list_y[gl + 1]], 'r')
  plt.plot([list_x[len(apm) - 1], list_x[0]], [list_y[len(apm) - 1], list_y[0]], 'r')
  plt.axis([0, 1, 0, 1])

  start_time = time.time()
  prim = Pvcprim(graphe, graphe[0])
  end_time = time.time()
  longueur_pvc = poids_cycle_hamiltonien(prim)
  plt.subplot(3, 2, 3)
  plt.title("Prim " + str(round(end_time - start_time,10)) + "s")
  plt.axis("off")
  list_x = []
  list_y = []
  for gl in range(len(prim)) :
    list_x.append(prim[gl][0])
    list_y.append(prim[gl][1])
  plt.plot(list_x, list_y, 'bo')
  for gl in range(len(prim) - 1) :
    plt.plot([list_x[gl], list_x[gl + 1]], [list_y[gl], list_y[gl + 1]], 'r')
  plt.plot([list_x[len(prim) - 1], list_x[0]], [list_y[len(prim) - 1], list_y[0]], 'r')
  plt.axis([0, 1, 0, 1])



  start_time = time.time()
  ed = Esdemisomme(graphe)
  end_time = time.time()
  longueur_ed = poids_cycle_hamiltonien(ed)
  plt.subplot(3, 2, 4)
  plt.title("Esdemisomme " + str(round(end_time - start_time,10)) + "s")
  plt.axis("off")
  list_x = []
  list_y = []
  for gl in range(len(ed)) :
    list_x.append(ed[gl][0])
    list_y.append(ed[gl][1])
  plt.plot(list_x, list_y, 'bo')
  for gl in range(len(ed) - 1) :
    plt.plot([list_x[gl], list_x[gl + 1]], [list_y[gl], list_y[gl + 1]], 'r')
  plt.plot([list_x[len(ed) - 1], list_x[0]], [list_y[len(ed) - 1], list_y[0]], 'r')
  plt.axis([0, 1, 0, 1])




  plt.subplot(3, 2, 6)
  plt.axis("off")
  txt = "Distance Plus Proche Voisin = " + str(round(longueur_ppv, 3))
  plt.text(0, 1, txt)
  txt = "Distance Optimisation Plus Proche Voisin = " + str(round(longueur_optimised_ppv, 3))
  plt.text(0, 0.8, txt)
  txt = "Distance Apm = " + str(round(longueur_apm, 3))
  plt.text(0, 0.6, txt)
  txt = "Distance PvcPrim = " + str(round(longueur_pvc, 3))
  plt.text(0, 0.4, txt)
  txt = "Distance EsDemisomme = " + str(round(longueur_ed, 3))
  plt.text(0, 0.2, txt)
  #txt = "Rapport distances\n Optimisation PPV  / PPV = " + str(round(longueur_optimised_ppv / longueur_ppv, 3))
  #plt.text(0, 0.0, txt)

  plt.subplot(3, 2, 5)
  plt.axis("off")
  plt.title("Moyenne sur 100 essais")
  plt.ylabel("moyenne distances")
  plt.xticks([], [])
  plt.yticks([], [])
  moyppv = 0
  moyoptippv = 0
  moyAp = 0
  moyPrim = 0
  moyEd = 0
  for k in range(100) :
    gra = create_graph_alea(n)
    mat = matrice_poids(gra)
    g = Ppvoisin(mat, gra, gra[0])
    moyppv += poids_cycle_hamiltonien(g)
    moyoptippv += poids_cycle_hamiltonien(OptimisePpvoisin(ppv))
    g2 = Apminimum(gra,gra[0])
    moyAp += poids_cycle_hamiltonien(g2)
    g3 = Pvcprim(gra, gra[0])
    moyPrim += poids_cycle_hamiltonien(g3)
    g4 = Esdemisomme(gra)
    moyEd += poids_cycle_hamiltonien(g4)
  moyppv /= 100
  moyoptippv /= 100
  moyAp /= 100
  moyPrim /= 100
  moyEd /= 100
  plt.bar(0, moyppv, width = 0.8, color='y')
  plt.text(0, moyppv, "Ppv "+ str(round(moyppv,3)) + "%", rotation=90, va="top", color = 'white')
  plt.bar(1.5, moyoptippv, width = 0.8, color='g')
  plt.text(1.5, moyoptippv, "OptPpv " + str(round(moyoptippv,3)) + "%", rotation=90,  va="top", color = 'white')
  plt.bar(3, moyAp, width = 0.8, color='blue')
  plt.text(3, moyAp, "Apm " + str(round(moyAp,3)) + "%", rotation=90,  va="top", color = 'white')
  plt.bar(4.5, moyPrim, width = 0.8, color='red')
  plt.text(4.5, moyPrim, "Prim " + str(round(moyPrim,3)) +"%", rotation=90,  va="top", color = 'white')
  plt.bar(6, moyEd, width = 0.8, color='purple')
  plt.text(6, moyEd, "EsDem " + str(round(moyEd,3)) +"%", rotation=90,  va="top", color = 'white')

  plt.savefig("res.png")
  plt.clf()





#  tests =======================================================================



def execPVC():
  IS_DONE = False

  def chargement():
    for c in itertools.cycle(['|', '/', '-', '\\']):
      if IS_DONE:
        sys.stdout.write('\r')
        break
      sys.stdout.write("\rchargement " + c)
      sys.stdout.write("\r")
      time.sleep(0.1)

  n2 = int(TAILLE_ZT.get())
  if n2 < 3 or n2 > 1000 :
    titre.config(text = "Le nombre de sommet doit être supérieur à 2 et inférieur à 1000")
  else :
    NOMBRE_SOMMET = n2
    gr = create_graph_alea(NOMBRE_SOMMET)
    mat = matrice_poids(gr)
    t = threading.Thread(target=chargement)
    t.start()
    titre.config(text = "Voici les algorithmes d’approximation pour le problème du voyageur de commerce !")
    affichage(gr, mat, NOMBRE_SOMMET)

    photo = PhotoImage(file=os.getcwd() + "/res.png")
    label = Label(mainframe, image=photo)
    label.photo = photo
    label.grid(row=3,column=2)

    IS_DONE = True
    time.sleep(0.15)




def main():


  taille = tkinter.Label (text = "Taille :")
  bouton = tkinter.Button(text = "Valider", command=execPVC)
  titre.grid(row=1,column=2)
  taille.grid(row=2,column=1)
  TAILLE_ZT.grid(row=2,column=2)
  bouton.grid(row=2,column=3)
  mainframe.mainloop()


if __name__ == "__main__":
  main()



