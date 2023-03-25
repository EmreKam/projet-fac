#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "hashtable.h"
#include "holdall.h"
#include <limits.h>

#define MAX_OCC CHAR_MAX

//  lecture : lit les caractères du fichier pointé par f et crée une chaine de
//    caractères avec ces caractères jusqu'à ce que la fonction trouve un
//    caractère qui valide la fonction isspace, si ponct est différent de 0,
//    la fonction ispunct, ou sinon si le mot dépasse la taille maximale de n,
//    dans ce cas un message d'erreur est écrit sur la sortie erreur avec des
//    détails supplémentaires, et le reste du mot en cours de lecture est ignoré
//    Renvoie ensuite le mot créé. Si le caractère obtenu est égal à EOF,
//    elle renvoie NULL.
static char *lecture(FILE *f, unsigned int n, int ponct,
    char *files[], int file, size_t lignes);

//  str_hashfun : l'une des fonctions de pré-hachage conseillées par Kernighan
//    et Pike pour les chaines de caractères.
static size_t str_hashfun(const char *s);

//  strtoupper : prend la chaine de caractères pointée par w et la renvoie
//    avec toutes ses lettres minuscules transformés en majuscules. Si w vaut
//    NULL, elle renvoie NULL.
static char *strtoupper(char *w);

//  rfree : libère la zone mémoire pointée par ptr et renvoie zéro.
static int rfree(void *ptr);

typedef struct val val;

//  fword : correspond au 'file word', c'est à dire à une chaine de caractère
//    déterminant à quelle(s) fichier(s) appartien(nen)t l(es) objet(s).
//    C'est une chaine de caractère composé de 'x' et de '-' où 'x' signifie
//    qu'il appartient au fichier à cet indice et '-' l'inverse
//  cptr : correspond à un compteur qui permet de compter le nombre d'occurence
//    total d'un objet dans tous les fichiers. Il s'agit d'un pointeur.
struct val {
  char *fword;
  char *cptr;
};

//  add_value : Crée une nouvelle structure de type val, initialise le fword et
//    le cptr par rapport à curf, nbf et count puis renvoie un pointeur vers
//    cette nouvelle structure. Renvoie NULL en cas de dépassement de capacité.
static val *add_value(int curf, int nbf, char *count);

//  update_fword : Met à jour le fword associé à v  par rapport au fichier
//    courant définit par curf. Ajoute le nombre de '-' et de 'x' nécessaire
//    puis renvoie v.
static val *update_fword(val *v, int curf);

// affiche les mots
static int valptr_display(const char *count, const char *s, const val *v);

//  hehe : Rien à voir ici.
static void hehe();

//  fword_value : retourne le fword associé à v.
static char *fword_value(const val *v);

//  cptr_value : retourne le cptr associé à v.
static char *cptr_value(const val *v);

int main(int argc, char *argv[]) {
  --argc;
  ++argv;
  char count[MAX_OCC];
  char *files[argc];
  int nbfil = 0;
  int r = EXIT_SUCCESS;
  unsigned int prefix = 63;
  int punct = 0;
  int upper = 0;
  int samenb = 0;
  int verbose = 0;
  size_t nbword = 10;
  int i = 0;
  for (i = 0; i < argc; i++) {
    if (strcmp(argv[i], "-") == 0) {
      char *si = malloc(15);
      strcpy(si, "Standard input");
      files[nbfil] = si;
      nbfil += 1;
    } else if (strncmp(argv[i], "--help", strlen(argv[i])) == 0) {
      printf("\nUsage: ws [OPTION] [FILES]\n\nPrint a list of words "
          "shared by text files.\n\nYou can find below a list of the "
          "available options:\n\n\t"
          "-i VALUE,\tSet the maximal number of significant initial letters"
          "\n\t\t\tfor words to VALUE, 0 means without limitation."
          "\n\t\t\tDefault is 63."
          "\n\n\t-p,\t\tMake the punctuation characters play the same"
          "\n\t\t\trole as space characters in the meaning of words.\n"
          "\n\t-s,\t\tPrint more words than the limit in case "
          "of same numbers.\n\n\t-t VALUE,"
          "\tSet the maximal number of words to printf to VALUE\n"
          "\t\t\t0 means all the words, default is 10.\n\n"
          "\t-u,\t\tConvert each lowercase letter of words to the\n"
          "\t\t\tcorresponding uppercase letter.\n\n"
          "\t-verbose,\tPrint a detailed list of the option used during the"
          "\n\t\t\texecution of the program and other details.\n\n"
          "\t--help,\t\tPrint this help message and exit.\n\n\n"
          "The number of FILES that can be taken into account is between"
          " 2 and 32.\nThe counters of the total number of occurences of "
          "the words have a\nmaximum value of %lld.\n\n", LLONG_MAX);
      return r;
    } else if (strcmp(argv[i], "-i") == 0) {
      if (argc - 1 == i) {
        goto error_read;
      } else {
        char *prefix2 = (char *) argv[i + 1];
        if (strcmp(prefix2, "0") == 0) {
          prefix = INT_MAX;
        } else {
          prefix = (unsigned int) atoi(argv[i + 1]);
          if (prefix == 0) {
            goto error_option;
          }
        }
      }
      i++;
    } else if (strcmp(argv[i], "-t") == 0) {
      if (argc - 1 == i) {
        goto error_read;
      } else {
        char *nbword2 = (char *) argv[i + 1];
        if (strcmp(nbword2, "0") == 0) {
          nbword = 0;
        } else {
          nbword = (unsigned int) atoi(argv[i + 1]);
          if (nbword == 0) {
            goto error_option;
          }
        }
      }
      i++;
    } else if (strcmp(argv[i], "-p") == 0) {
      punct = 1;
    } else if (strcmp(argv[i], "-u") == 0) {
      upper = 1;
    } else if (strcmp(argv[i], "-s") == 0) {
      samenb = 1;
    } else if (strcmp(argv[i], "-hancart") == 0) {
      hehe();
      return r;
    } else if (strncmp(argv[i], "-verbose", strlen(argv[i])) == 0) {
      verbose = 1;
    } else {
      if (strncmp(argv[i], "-", 1) == 0) {
        fprintf(stderr, "--- Info : The option '%s' does not exists, the "
            "program will continue ignoring it.\n", argv[i]);
      } else {
        FILE *f2 = fopen(argv[i], "r");
        files[nbfil] = argv[i];
        if (f2 != 0) {
          if (fclose(f2) == EOF) {
            fprintf(stderr, "*** Error: Error while closing the file\n");
            goto error;
          }
          nbfil += 1;
        } else {
          goto error_files2;
        }
      }
    }
  }
  if (nbfil < 2 || nbfil > 32) {
    goto error_files;
  }
  if (verbose == 1) {
    printf("--- Info : Detailed list of the options.\n\n"
        "[OPTION]\t[VALUE]\n\n"
        "prefix  (-i)\t%u\n"
        "punct   (-p)\t%d\n"
        "upper   (-u)\t%d\n"
        "samenb  (-s)\t%d\n"
        "nbword  (-t)\t%zu\n"
        "verbose (-v)\t%d\n\n", prefix, punct, upper, samenb, nbword, verbose);
  }
  hashtable *ht = hashtable_empty((int (*)(const void *, const void *))strcmp,
      (size_t (*)(const void *))str_hashfun);
  holdall *has = holdall_empty();
  if (ht == NULL || has == NULL) {
    goto error_capacity;
  }
  FILE *f;
  int k = 0;
  for (k = 0; k < nbfil; ++k) {
    char *w;
    size_t ligne = 1;
    if (strcmp(files[k], "Standard input") == 0) {
      f = stdin;
    } else {
      f = fopen(files[k], "r");
      if (f == NULL) {
        fprintf(stderr, "*** Error: Error while the opening of the file\n");
        goto error;
      }
    }
    while ((w = lecture(f, prefix, punct, files, k, ligne)) != NULL) {
      if ((int) *w == 10) {
        ligne += 1;
      } else {
        if (upper == 1) {
          if (strtoupper(w) == NULL) {
            goto error_read;
          }
        }
        val *vptr = (val *) hashtable_search(ht, w);
        if (vptr != NULL) {
          if (hashtable_add(ht, w, update_fword(vptr, k)) == NULL) {
            goto error_capacity;
          }
        } else {
          char *s = malloc(strlen(w) + 1);
          if (s == NULL) {
            goto error_capacity;
          }
          strcpy(s, w);
          if (holdall_put(has, s) != 0) {
            free(s);
            goto error_capacity;
          }
          if (hashtable_add(ht, s, add_value(k, nbfil, count)) == NULL) {
            goto error_capacity;
          }
        }
      }
      rfree(w);
    }
    if (strcmp(files[k], "-") != 0) {
      if (!feof(f)) {
        goto error_read;
      }
      if (fclose(f) == EOF) {
        fprintf(stderr, "*** Error: Error while closing the file\n");
        goto error;
      }
    }
  }
  for (int k = 0; k < nbfil; k++) {
    if (strcmp(files[k], "Standard input") == 0) {
      free(files[k]);
    }
  }
  if (holdall_sort(has, ht,
      (void *(*)(void *, void *))hashtable_search,
      (void *(*)(void *, void *))hashtable_remove,
      nbword, nbfil, samenb,
      ((int (*)(const void *, const void *))strcmp),
      (char *(*)(const void *))fword_value,
      (char *(*)(const void *))cptr_value) != 0) {
    goto error_capacity;
  }
  if (verbose == 1) {
    printf("\n--- Info: Number of words: %zu.\n\n", holdall_count(has));
  } else {
    printf("\n");
  }
  if (holdall_apply_context2(
      has,
      ht,
      (void *(*)(void *, void *))hashtable_search,
      count,
      (int (*)(void *, void *, void *))valptr_display) != 0) {
    goto error_write;
  }
  goto dispose;
error_capacity:
  fprintf(stderr, "*** Error: Not enough memory.\n");
  goto error;
error_read:
  fprintf(stderr, "*** Error: A read error occurs.\n");
  goto error;
error_write:
  fprintf(stderr, "*** Error: A write error occurs.\n");
  goto error;
error_files:
  if (nbfil < 2) {
    fprintf(stderr, "*** Error: At least two files are expected.\n");
  } else {
    fprintf(stderr, "*** Error: There is a limit of a maximum of 32 files.\n");
  }
  return EXIT_FAILURE;
error_files2:
  fprintf(stderr, "*** Error: The following argument '%s' cannot be read"
      " as a file.\n", files[nbfil]);
  return EXIT_FAILURE;
error_option:
  fprintf(stderr, "*** Error: Invalid value '%s' for the "
      "option '%s'.\n", argv[i + 1], argv[i]);
  return EXIT_FAILURE;
error:
  r = EXIT_FAILURE;
  goto dispose;
dispose:
  hashtable_dispose(&ht);
  if (has != NULL) {
    holdall_apply(has, rfree);
  }
  holdall_dispose(&has);
  return r;
}

//----- FONCTIONS AUX ----------------------------------------------------------

char *lecture(FILE *f, unsigned int n, int ponct,
    char *files[], int file, size_t ligne) {
  int c = fgetc(f);
  char *w = malloc(n + 1);
  char *p = w;
  if (ponct == 0) {
    while (isspace(c)) {
      char *p = w;
      *p = (char) c;
      if (*p == '\n') {
        return w;
      }
      c = fgetc(f);
    }
  } else {
    while (isspace(c) || ispunct(c)) {
      char *p = w;
      *p = (char) c;
      if (*p == '\n') {
        return w;
      }
      c = fgetc(f);
    }
  }
  if (c == EOF) {
    rfree(w);
    return NULL;
  }
  int r = 0;
  if (ponct == 0) {
    while (c != EOF && !isspace(c)) {
      if (p - w < n) {
        *p = (char) c;
        p = p + 1;
      } else if (r == 0) {
        *p = '\0';
        if (strcmp(files[file], "Standard input") == 0) {
          fprintf(stderr, "Word from %s cut '%s...'\n",
              files[file], w);
        } else {
          fprintf(stderr, "Word from file '%s' at line %zu cut '%s...'\n",
              files[file], ligne, w);
        }
        r += 1;
      }
      c = fgetc(f);
    }
  } else {
    while (c != EOF && !isspace(c) && !ispunct(c)) {
      if (p - w < n) {
        *p = (char) c;
        p = p + 1;
      } else if (r == 0) {
        *p = '\0';
        if (strcmp(files[file], "Standard input") == 0) {
          fprintf(stderr, "Word from %s cut '%s...'\n",
              files[file], w);
        } else {
          fprintf(stderr, "Word from file '%s' at line %zu cut '%s...'\n",
              files[file], ligne, w);
        }
        r += 1;
      }
      c = fgetc(f);
    }
  }
  if (c == '\n' && strcmp(files[file], "Standard input") != 0) {
    fseek(f, -1, SEEK_CUR);
  }
  *p = '\0';
  return w;
}

size_t str_hashfun(const char *s) {
  size_t h = 0;
  for (const unsigned char *p = (const unsigned char *) s; *p != '\0'; ++p) {
    h = 37 * h + *p;
  }
  return h;
}

int valptr_display(const char *count, const char *s, const val *v) {
  if (v->cptr >= count + INT_MAX) {
    fprintf(stderr, "*** Count overflow for word '%s'. \n", s);
  }
  int r = printf("%s\t%ld\t%s\n", v->fword, v->cptr - count, s);
  free(v->fword);
  val *v2 = (val *) v;
  free(v2);
  return r < 0;
}

int rfree(void *ptr) {
  free(ptr);
  return 0;
}

char *strtoupper(char *w) {
  if (w == NULL) {
    return NULL;
  }
  char *p = w;
  while (*p != '\0') {
    char c = (char) toupper(*p);
    *p = c;
    p = p + 1;
  }
  return w;
}

void hehe() {
  int aa = 0;
  printf("[ ]");
  printf("\b\b");
  while (aa != 20000000) {
    printf("\\\b");
    printf("-\b");
    printf("/\b");
    aa++;
  }
  printf("\b\b\b   He is the best teacher.\n");
}

val *add_value(int curf, int nbf, char *count) {
  val *v = malloc(sizeof *v);
  if (v == NULL) {
    return NULL;
  }
  v->cptr = count + 1;
  char *x = malloc((size_t) (nbf + 1));
  if (x == NULL) {
    return NULL;
  }
  char *p = x;
  for (int k = 0; k < nbf; k++) {
    if (curf == 0) {
      *p = 'x';
    } else {
      *p = '-';
    }
    --curf;
    p = p + 1;
  }
  *p = '\0';
  v->fword = x;
  return v;
}

val *update_fword(val *v, int curf) {
  v->cptr += 1;
  char *x = v->fword;
  char *p = x;
  p = p + curf;
  *p = 'x';
  v->fword = x;
  return v;
}

char *fword_value(const val *v) {
  return v->fword;
}

char *cptr_value(const val *v) {
  return v->cptr;
}
