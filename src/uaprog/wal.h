typedef struct funct {
  int arity;
  int * values;        /* lexicographical list of values */
  struct funct *next;  /* next function of algebra */
  }  FUNCT;


typedef struct algebra {
  int size;     /* number of elements in underlying set */
  FUNCT *func;  /* pointer to first operation */
  }  ALG;

typedef int * VEC;  /* vector containing integers */

typedef struct vector {
  int lvec;    /* number of elements */
  VEC list;    /* pointer to list of elements */
  }  VECTOR;
typedef struct vectors {
  int nvecs;     /* number of vectors listed */
  int lvecs;     /* common length of vectors */
  VEC * list;    /* pointer to array of pointers to vectors */
  } VECTORS;

typedef struct pair {
  int first;
  int second;
  }  PAIR;
typedef struct pairs {
  int npairs;    /* number of pairs listed */
  PAIR * list;   /* list of pairs */
  }  PAIRS;

/* Pairs of vectors are stored as vectors of double length */


typedef struct part {
  int size;  /* size of underlying set */
  VEC func;  /* func[i]=smallest element of the class if i */
  }  PART;

/* IMPORTANT! In Matt's program, different functions are used
   to represent partitions. The partition is also their kernel,
   but their range is [0, #classes-1]. The classes are numbered
   starting with zero, in the order (I think) of their smallest
   elements. We use the above representation internally,
   but on input and output, to be compatible, we use Matt's representation.
   Many routines that USE a congruence use only the fact that
   its kernel is the congruence, so in those routines no conversion
   is necessary. The converter arb2ewk_con() and arb2matt_con()
   convert an arbitrary function to the corresponding format
   (so that the kernel remains the same).
*/


typedef struct parts {
  int nparts;    /* number of vectors listed */
  int lparts;    /* common length of vectors */
  VEC * list;    /* pointer to array of pointers to vectors */
  } PARTS;

/* The structures `part' and `vector' and also the structures
   `parts' and `vectors' are physically the same, and therefore can
   be freely casted among each other. They have different names only
   to make the program more readable. We use the vector utility
   functions to deal with partitions. */

/*   An arguments structure corresponts to an expression
   r= f_i( a_0, ..., a_{k-1} ). */

typedef struct arguments {
  int opno;       /* number of the operation i */
  int arity;      /* the arity k of this operation */
  VEC args;       /* a_0, ..., a_{k-1} or NULL */
  int result;     /* The result r of the operation */
  }  ARGUMENTS;

typedef struct argslist {
  int nargs_st;        /* number of structures */
  ARGUMENTS *list;      /* pointer to array of structures */
  } ARGSLIST;

/* NEW_TUPLE is typically filled by spf(), and read by check_???(). */

typedef struct new_tuple {
  ALG *alg;          /* the algebra in question */
  VECTORS *gen;      /* pointer to the list of vectors being generated */
  ARGSLIST *argsl;   /* pointer to the list of corresponding arguments */
  int offset;        /* offset of new tuple in these lists */
  }  NEW_TUPLE;

