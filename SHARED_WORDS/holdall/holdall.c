//  Partie implantation du module holdall.

#include "holdall.h"
#include "stdio.h"

//  struct holdall, holdall : implantation par liste dynamique simplement
//    chainée. L'insertion a lieu en queue si la macroconstante
//    HOLDALL_INSERT_TAIL est définie, en tête sinon.

typedef struct choldall choldall;

struct choldall {
  void *value;
  choldall *next;
};

struct holdall {
  choldall *head;
#ifdef HOLDALL_INSERT_TAIL
  choldall *tail;
#endif
  size_t count;
};

//  count_x : compte le nombre de x dans la chaine de caractère associé à s.
//    Renvoie cette somme.
static size_t count_x(char *s);

//  holdall_move_all_head : déplace en tête de la liste associée à has2 la
//    liste associée à has1, de telle sorte que, à la terminaison, la liste
//    associée à has1 est vide et la liste associée à has2 est la concaténation
//    des deux listes. En cas de succès, la fonction renvoie zéro.
static int holdall_move_all_head(holdall *has1, holdall *has2);

//  holdall_move_head_head : déplace l'élément de tête de la
//    liste associée à src vers la tête de la liste associée à dest.
//    Renvoie zéro en cas de succès, une valeur non nulle en cas d'échec pour
//    cause de liste associée à src vide.
static int holdall_move_head_head(holdall *src, holdall *dest);

//  holdall_quicksort : trie de manière stable la liste associée à s au sens
//    de compar selon la méthode du tri rapide. Renvoie zéro en cas de succès,
//    une valeur non nulle en cas d'échec.
static int holdall_quicksort(holdall *s, int (*compar)(const void *,
    const void *));

//  holdall_partition_pivot : déplace en tête des listes associées à slth, seq
//    et sgth, les éléments de la liste associée à s qui sont strictement
//    inférieurs, égaux et strictement supérieurs au premier élément de la liste
//    associée à s au sens de compar. Si la liste associée à s est vide, les
//    quatre listes demeurent inchangées. À la terminaison, la liste associée
//    à s est vide.
static void holdall_partition_pivot(holdall *s, holdall *slth, holdall *seq,
    holdall *sgth, int (*compar)(const void *,
    const void *));

//  holdall_quicksort_occ : trie de manière stable la liste associée à s dans
//    l'ordre décroissant des nombres totaux d'occurrences cptr de chaque objet
//    de s, puis au sens de compar présent dans ht selon la méthode du tri
//    rapide. Renvoie zéro en cas de succès, une valeur non nulle en cas d'échec
static int holdall_quicksort_occ(holdall *s,
    void *ht,
    void *(*hsearch)(void *ht, void *ptr),
    char * (*cptr)(
    const void *), int (*compar)(const void *, const void *));

//  holdall_partition_pivot_occ : déplace en tête des listes associées à slth,
//    seq et sgth, les éléments de la liste associée à s qui sont strictement
//    inférieurs, égaux et strictement supérieurs au premier élément de la liste
//    associée à s par rapport à cptr. Si la liste associée à s est vide, les
//    quatre listes demeurent inchangées. À la terminaison, la liste associée
//    à s est vide.
static void holdall_partition_pivot_occ(holdall *s, holdall *slth, holdall *seq,
    holdall *sgth,
    void *ht,
    void *(*hsearch)(void *ht, void *ptr),
    char * (*cptr)(
    const void *));

holdall *holdall_empty(void) {
  holdall *ha = malloc(sizeof *ha);
  if (ha == NULL) {
    return NULL;
  }
  ha->head = NULL;
#ifdef HOLDALL_INSERT_TAIL
  ha->tail = NULL;
#endif
  ha->count = 0;
  return ha;
}

int holdall_put(holdall *ha, void *ptr) {
  choldall *p = malloc(sizeof *p);
  if (p == NULL) {
    return -1;
  }
  p->value = ptr;
#ifdef HOLDALL_INSERT_TAIL
  p->next = NULL;
  if (ha->tail == NULL) {
    ha->head = p;
  } else {
    ha->tail->next = p;
  }
  ha->tail = p;
#else
  p->next = ha->head;
  ha->head = p;
#endif
  ha->count += 1;
  return 0;
}

size_t holdall_count(holdall *ha) {
  return ha->count;
}

int holdall_apply(holdall *ha, int (*fun)(void *)) {
  for (const choldall *p = ha->head; p != NULL; p = p->next) {
    int r = fun(p->value);
    if (r != 0) {
      return r;
    }
  }
  return 0;
}

int holdall_apply_context(holdall *ha,
    void *context, void *(*fun1)(void *context, void *ptr),
    int (*fun2)(void *ptr, void *resultfun1)) {
  for (const choldall *p = ha->head; p != NULL; p = p->next) {
    int r = fun2(p->value, fun1(context, p->value));
    if (r != 0) {
      return r;
    }
  }
  return 0;
}

int holdall_apply_context2(holdall *ha,
    void *context1, void *(*fun1)(void *context1, void *ptr),
    void *context2, int (*fun2)(void *context2, void *ptr, void *resultfun1)) {
  for (const choldall *p = ha->head; p != NULL; p = p->next) {
    int r = fun2(context2, p->value, fun1(context1, p->value));
    if (r != 0) {
      return r;
    }
  }
  return 0;
}

void holdall_dispose(holdall **haptr) {
  if (*haptr == NULL) {
    return;
  }
  choldall *p = (*haptr)->head;
  while (p != NULL) {
    choldall *t = p;
    p = p->next;
    free(t);
  }
  free(*haptr);
  *haptr = NULL;
}

// HOLDALL_SORT ----------------------------------------------------------------

int holdall_sort(holdall *ha, void *ht,
    void *(*hsearch)(void *ht, void *ptr),
    void *(*hremove)(void *ht, void *ptr),
    size_t nb, int nbf, int samenb,
    int (*compar)(const void *, const void *),
    char * (*fword)(const void *), char * (*cptr)(const void *)) {
  if (ha->count < 2) {
    const void *xptr = hsearch(ht, ha->head->value);
    char *x = fword(xptr);
    int n = (int) count_x(x);
    if (n < 2) {
      free(x);
      free((void *) xptr);
      free(ha->head);
      ha->head = NULL;
    }
    return 0;
  }
  holdall *tab[nbf - 1];
  int k = 0;
  while (k < nbf - 1) {
    holdall *h = holdall_empty();
    if (h == NULL) {
      return -1;
    }
    tab[k] = h;
    k++;
  }
  choldall *p = ha->head;
  while (p != NULL) {
    const void *xptr = hsearch(ht, p->value);
    char *x = fword(xptr);
    int n = (int) count_x(x);
    if (n > 1) {
      holdall *hb = tab[nbf - n];
      holdall_put(hb, p->value);
      hb->count += 1;
    } else {
      free(x);
      free((void *) xptr);
      hremove(ht, p->value);
      free(p->value);
    }
    p = p->next;
  }
  // tri chaque holdall du tableau tab par occurence et au sens de compar
  for (int k = 0; k < nbf - 1; k++) {
    holdall_quicksort_occ(tab[k], ht, hsearch, cptr, compar);
  }
  // concatene tous les holdall
  size_t words = 0;
  holdall *ltab = tab[0];
  for (int k = 0; k < nbf - 2; k++) {
    if (tab[k]->head != NULL) {
      holdall_move_all_head(tab[k], tab[k + 1]);
      words += tab[k]->count;
    }
    ltab = tab[k + 1];
  }
  choldall *ptr = ltab->head;
  if (nb != 1) {
    words = 1;
    while (ptr != NULL && nb != 1) {
      ptr = ptr->next;
      words += 1;
      nb -= 1;
    }
    if (samenb == 1) {
      int r = 0;
      char *x1 = fword(hsearch(ht, ptr->value));
      size_t n1 = count_x(x1);
      size_t c1 = (size_t) cptr(hsearch(ht, ptr->value));
      choldall *ptr2 = ptr->next;
      while (r == 0 && ptr2 != NULL) {
        char *x2 = fword(hsearch(ht, ptr2->value));
        size_t n2 = count_x(x2);
        size_t c2 = (size_t) cptr(hsearch(ht, ptr2->value));
        if (n1 != n2 || c1 != c2) {
          r += 1;
        } else {
          ptr2 = ptr2->next;
          ptr = ptr->next;
          words += 1;
        }
      }
    }
    if (ptr != NULL) {
      choldall *ptr3 = ptr->next;
      ptr->next = NULL;
      holdall *hc = malloc(sizeof *hc);
      if (hc == NULL) {
        return -1;
      }
      hc->head = ptr3;
      while (ptr3 != NULL) {
        const void *xptr = hsearch(ht, ptr3->value);
        char *s = fword(xptr);
        free(s);
        free((void *) xptr);
        hremove(ht, ptr3->value);
        free(ptr3->value);
        ptr3 = ptr3->next;
      }
      holdall_dispose(&hc);
    }
  }
  choldall *psupr2 = ha->head;
  while (psupr2 != NULL) {
    choldall *psupr3 = psupr2;
    psupr2 = psupr2->next;
    free(psupr3);
  }
  if (ltab->count == 0) {
    ltab->head = NULL;
  }
  ha->head = ltab->head;
  ltab->head = NULL;
  for (int k = 0; k < nbf - 1; k++) {
    free(tab[k]);
  }
  ha->count = words;
  return 0;
}

// HOLDALL_SORT_AUX ------------------------------------------------------------

void holdall_partition_pivot(holdall *s, holdall *slth, holdall *seq,
    holdall *sgth, int (*compar)(const void *,
    const void *)) {
  void *pivot = s->head->value;
  while (s->head != NULL) {
    int c = compar(s->head->value, pivot);
    if (c == 0) {
      holdall_move_head_head(s, seq);
    }
    if (c < 0) {
      holdall_move_head_head(s, slth);
    }
    if (c > 0) {
      holdall_move_head_head(s, sgth);
    }
  }
}

void holdall_partition_pivot_occ(holdall *s, holdall *slth, holdall *seq,
    holdall *sgth,
    void *ht,
    void *(*hsearch)(void *ht, void *ptr),
    char * (*cptr)(
    const void *)) {
  size_t n = (size_t) cptr(hsearch(ht, s->head->value));
  while (s->head != NULL) {
    size_t m = (size_t) cptr(hsearch(ht, s->head->value));
    if (n == m) {
      holdall_move_head_head(s, seq);
    }
    if (n < m) {
      holdall_move_head_head(s, slth);
    }
    if (m < n) {
      holdall_move_head_head(s, sgth);
    }
  }
}

// tri rapide

int holdall_quicksort(holdall *s, int (*compar)(const void *, const void *)) {
  if (s->head == NULL) {
    return 0;
  }
  int r = 0;
  holdall *slth = holdall_empty();
  holdall *seq = holdall_empty();
  holdall *sgth = holdall_empty();
  if (slth == NULL || seq == NULL || sgth == NULL) {
    goto error;
  }
  holdall_partition_pivot(s, slth, seq, sgth, compar);
  if (slth->head != NULL) {
    holdall_quicksort(slth, compar);
  }
  if (sgth->head != NULL) {
    holdall_quicksort(sgth, compar);
  }
  holdall_move_all_head(slth, seq);
  holdall_move_all_head(seq, sgth);
  holdall_move_all_head(sgth, s);
  goto dispose;
error:
  r = -1;
  goto dispose;
dispose:
  free(slth);
  free(seq);
  free(sgth);
  return r;
}

int holdall_quicksort_occ(holdall *s,
    void *ht,
    void *(*hsearch)(void *ht, void *ptr),
    char * (*cptr)(
    const void *),
    int (*compar)(const void *, const void *)) {
  if (s->head == NULL) {
    return 0;
  }
  int r = 0;
  holdall *slth = holdall_empty();
  holdall *seq = holdall_empty();
  holdall *sgth = holdall_empty();
  if (slth == NULL || seq == NULL || sgth == NULL) {
    goto error;
  }
  holdall_partition_pivot_occ(s, slth, seq, sgth, ht, hsearch, cptr);
  if (slth->head != NULL) {
    holdall_quicksort_occ(slth, ht, hsearch, cptr, compar);
  }
  if (sgth->head != NULL) {
    holdall_quicksort_occ(sgth, ht, hsearch, cptr, compar);
  }
  holdall_quicksort(seq, compar);
  holdall_move_all_head(slth, seq);
  holdall_move_all_head(seq, sgth);
  holdall_move_all_head(sgth, s);
  goto dispose;
error:
  r = -1;
  goto dispose;
dispose:
  free(slth);
  free(seq);
  free(sgth);
  return r;
}

int holdall_move_head_head(holdall *src, holdall *dest) {
  if (src->head == NULL) {
    return -1;
  }
  choldall *p = src->head;
  src->head = p->next;
  p->next = dest->head;
  dest->head = p;
  return 0;
}

int holdall_move_all_head(holdall *has1, holdall *has2) {
  if (has1->head == NULL) {
    return 0;
  }
  if (has2->head == NULL) {
    has2->head = has1->head;
    return 0;
  }
  choldall *p = has1->head;
  while (p->next != NULL) {
    p = p->next;
  }
  p->next = has2->head;
  has2->head = has1->head;
  has1->head = NULL;
  return 0;
}

size_t count_x(char *s) {
  size_t k = 0;
  while (*s != '\0') {
    if (*s == 'x') {
      k += 1;
    }
    s += 1;
  }
  return k;
}
