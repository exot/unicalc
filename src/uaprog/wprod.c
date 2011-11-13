#define EXTERN extern
#include "includes.h"

static int prod_func(int *, FUNCT **, VECTORS *, int **);
static int factor_func(int , int ,FUNCT *, VEC , VEC , int **);

int
sub_prod_alg( ALG *algs, VECTORS *vecs, ALG *P )
/* The length of algs is ASSUMED to be the same as the length of
   vectors in vecs.
   The elements of vecs are considered as elements of the direct
   product of the entries in algs. The routine returns
   NOT_SUBUNIVERSE if these vectors do not form a subalgebra.
   If they do, the corresponding algebra is computed, and put into P.
   If vecs->list==NULL, then the full direct product is computed,
   and vecs is filled with the corresponding universe.

   WARNING: on error, vecs is not freed, not even if we have filled it up!
   Returns NO_ERROR if succesfull, ERROR otherwise.
*/
{
  int i;
  int err;
  int *maxlen;
  FUNCT **fun, *m, *new;

  maxlen=(int *)(calloc(vecs->lvecs,sizeof(int)));
  if(maxlen==NULL) {
    print_derr("Not enough memory to store product sizes.");
    return(ERROR);
  }
  for(i=0; i<vecs->lvecs; i++) {
    maxlen[i]=(algs[i]).size;
  }
  if(vecs->list==NULL) {
    err=cr_vectors(vecs, maxlen);
    if(err!=NO_ERROR) return(ERROR);
  }
/* now vecs is correctly filled */
  P->func=NULL; /* so that free_alg works */
  fun = (FUNCT **) (calloc(vecs->lvecs, sizeof(FUNCT *)));
  if( fun == NULL) {
    print_derr("Not enough memory to store operation pointers.");
    return(ERROR);
  }
  for(i=0; i<vecs->lvecs; i++) {
    fun[i]=(algs[i]).func;
  }
  m=NULL;
  P->size=vecs->nvecs;
  if(P->size>=ALG_SIZE_LIMIT) {
    print_derr("Illegal size %d of product (should be in [%d, %d]).",
                                       P->size, 0, ALG_SIZE_LIMIT-1);
    return(ERROR);
  }
  for(;;) {
    if(fun[0]==NULL) {
      for(i=1; i<vecs->lvecs; i++) {
        if(fun[i]!=NULL) {
          print_derr("Algebras must be similar to form product.");
          free_alg(P);
          return(ERROR);
        }
      }
      /* so all pointers are zero, we are done */
      return(NO_ERROR);
    }
/* so fun[0]!=NULL */
    for(i=1; i<vecs->lvecs; i++) {
      if(fun[i]==NULL) {
        print_derr("Algebras must be similar to form product.");
        free_alg(P);
        return(ERROR);
      }
    }
/* so none of the function pointers is zero */
    for(i=1; i<vecs->lvecs; i++) {
      if((fun[0])->arity!=(fun[i])->arity) {
        print_derr("Algebras must be similar to form product.");
        free_alg(P);
        return(ERROR);
      }
    }
    if( (new= (FUNCT *) (malloc(sizeof(FUNCT))))== NULL) {
      print_derr("Not enough memory to store function in product.");
      free_alg(P);
      return(ERROR);
    }
    new->arity=(fun[0])->arity;
    new->next= (FUNCT *)NULL;
    err=prod_func(maxlen, fun, vecs, &(new->values));
    if(err != NO_ERROR) {
      free((void *)new);
      free_alg(P);
      return(err);
    }
/* everything is OK, chain in new operation */
    if(m==NULL) P->func=new;
    else m->next=new;
/*  update fun, m */
    m=new;
    for(i=0; i<vecs->lvecs; i++) {
      fun[i]=(fun[i])->next;
    }
  }
}

static int
prod_func(int *sizes, FUNCT **fun, VECTORS *vecs, int **newp)
/* Prepare the value of these operations on vecs,
   and put the values into *newp. 
   sizes contains the sizes of the corresponding sets.
   It is assumed that the functions are not NULL, and have the same arity.
   Returns ERROR on error, NO_ERROR otherwise.
   Speed is not a concern here.
*/
{
  int i, j;
  int args[ARITY_LIMIT];
  int len, arity, offset, err;
  VEC generated;

  arity=(fun[0])->arity;
  len = intpow(vecs->nvecs, arity);
  if(len == -1)
  {
    print_derr("(Algebra size)^(arity) too big.");
    return(ERROR);
  }
  (*newp)=(int *)(calloc(len,sizeof(int)));
  if(*newp==NULL) {
    print_derr("Not enough memory to store function in product.");
    return(ERROR);
  }
  generated=(int *)(calloc(vecs->lvecs,sizeof(int)));
  if(generated==NULL) {
    print_derr("Not enough memory to compute function in product.");
    free((void *)(*newp));
    return(ERROR);
  }
/* fill values of newp */
  for(i=0; i<arity; i++) {
    args[i]=0;
  }
  for(;;) {/* this cycles through the arguments */
/* apply the operations with these arguments to fill `generated' */
    for(i=0; i<vecs->lvecs; i++) { /* cyle the components */
      offset=0;
      for(j=arity-1; j>=0; j--) { /* Horner */
        offset = (sizes[i]*offset) + (((vecs->list)[args[j]])[i]);
      }
      generated[i]=((fun[i])->values)[offset];
    }
/* see if `generated' is new */
    err=find_vec(generated, vecs);
    if(err==NOT_FOUND) {
      print_derr("The given set of vectors is not a subuniverse.");
      free((void *)(*newp));
      free((void *)generated);
      return(ERROR);
    }
/* so now err is the desired value of the operation */
    set_func_value(vecs->nvecs, arity, *newp, args, err);
/* get new arguments */
    for(i= 0;i<arity;i++) {
      args[i]++;
      if (args[i]>= vecs->nvecs) {
        args[i]= 0;
        continue;  /* get next coordinate */
      }
      break;      /* args[i] has been increased */
    }
    if (i== arity) {   /* all arguments done */
      free((void *)generated);
      return(NO_ERROR);
    }
/* new arguments computed, next iteration  */
  }
/* UNREACHED */
}

int
factor_alg( ALG *A, VEC cong, ALG *F )
/* F = A/cong is created. `cong' can be in any format whose kernel
   we want. It is assumed that this is a congrence, otherwise
   the resulting algebra is unpredictable.
   On error, A is not freed, but F is.
   Returns NO_ERROR if succesfull, ERROR otherwise.
   cong is not altered in any way.
*/
{
  int i, j;
  int err;
  FUNCT *fun, *m, *new;
  VEC mattcong;
  VEC repr; /* representatives of the classes */

  F->func=NULL;
  if ((mattcong= (VEC)(calloc(A->size,sizeof(int))))== NULL)
  {
    print_derr("Not enough memory for converting congruences.");
    return(ERROR);
  }
  for(i=0; i<A->size; i++) {
    mattcong[i]=cong[i];
  }
  arb2matt_con(A->size, mattcong);
/* compute size of factor, as the maximal number in mattcong+1 */
  F->size=0;
  for(i=0; i<A->size; i++) {
    if(F->size < mattcong[i]) F->size=mattcong[i];
  }
  (F->size)++;
  if ((repr= (VEC)(calloc(F->size,sizeof(int))))== NULL)
  {
    print_derr("Not enough memory to store representatives.");
    free((void *)mattcong);
    return(ERROR);
  }
/* find smallest representative for each element in F */
  for(i=0; i<F->size; i++) {
    repr[i]=0; /* unnecessary */
    for(j=0; j<A->size; j++) {
      if(mattcong[j]==i) {
        repr[i]=j;
        break;
      }
    /* never reached */
    }
  }
  m=NULL;
  fun=A->func;
  while(fun != NULL) {
    if( (new= (FUNCT *) (malloc(sizeof(FUNCT))))== NULL) {
      print_derr("Not enough memory to store function in factor.");
      free((void *)mattcong);
      free((void *)repr);
      free_alg(F);
      return(ERROR);
    }
    new->arity=fun->arity;
    new->next= (FUNCT *)NULL;
    err=factor_func( A->size, F->size, fun, mattcong, repr, &(new->values));
    if(err != NO_ERROR) {
      free((void *)new);
      free((void *)mattcong);
      free((void *)repr);
      free_alg(F);
      return(ERROR);
    }
/* everything is OK, chain in new operation */
    if(m==NULL) F->func=new;
    else m->next=new;
/*  update fun, m */
    m=new;
    fun=fun->next;
  }
  free((void *)mattcong);
  free((void *)repr);
  return(NO_ERROR);
}

static int
factor_func(int size,int fsize,FUNCT *fun,VEC mattcong,VEC repr,int **newp)
/* Prepare the values of the factor operation,
   and put the values into *newp. 
   size contains the size of the algebra,
   fsize contains the size of the factor.
   It is assumed that fun is not NULL.
   Returns ERROR on error, NO_ERROR otherwise.
   Speed is not a concern here.
*/
{
  int i;
  int args[ARITY_LIMIT];
  int repr_args[ARITY_LIMIT];
  int flen;
  int arity, value;

  arity=fun->arity;
  flen = intpow(fsize, arity);
  if(flen == -1)
  {
    print_derr("(Factor size)^(arity) too big.");
    return(ERROR);
  }
  (*newp)=(int *)(calloc(flen,sizeof(int)));
  if(*newp==NULL) {
    print_derr("Not enough memory to store function in factor.");
    return(ERROR);
  }
/* fill values of newp */
  for(i=0; i<arity; i++) {
    args[i]=0;
  }
  for(;;) {/* this cycles through the arguments */
/* apply fun with these arguments to fill `value' */
    for(i=0; i<arity; i++) {
      repr_args[i]=repr[args[i]]; /* fill with the representatives of args */
    }
    value=get_func_value(size, arity, fun->values, repr_args);
    set_func_value(fsize, arity, *newp, args, mattcong[value]);
/* get new arguments */
    for(i= 0;i<arity;i++) {
      args[i]++;
      if (args[i]>= fsize) {
        args[i]= 0;
        continue;  /* get next coordinate */
      }
      break;      /* args[i] has been increased */
    }
    if (i== arity) {   /* all arguments done */
      return(NO_ERROR);
    }
/* new arguments computed, next iteration  */
  }
/* UNREACHED */
}

int
prod_cong( VECTORS *vecs, PART *congs, VEC *res )
/* The elements of vecs are considered as elements
   of a direct product. congs is an array of pointers to
   partitions, with congs[i] containing a partition
   on the set of i-th components of the vectors in vecs. Thus, the length
   of congs[] is ASSUMED to be the same as vecs->lvevcs.
   This module computes the product of these partitions, and
   returns it in res. Allocates storage for res.
   Returns NO_ERROR if succesfull, ERROR otherwise.
*/
{
  int i, j, k;
  int a, b;
  VEC c;

  print_spopen("Computing a product congruence.");
  (*res)= (VEC)(calloc(vecs->nvecs,sizeof(int)));
  if( (*res) == NULL) {
    print_derr("Not enough memory to store product congruence.");
    return(ERROR);
  }
  for(i=0; i<vecs->nvecs; i++) {
    (*res)[i]=i;
  }
  for(i=0; i<vecs->nvecs; i++) {
    for(j=0; j<vecs->nvecs; j++) {
      for(k=0; k<vecs->lvecs; k++) {
        a=((vecs->list)[i])[k];
        b=((vecs->list)[j])[k];
        c=(congs[k]).func;
        if( c[a] != c[b] ) goto WLnext_pair;
      }
      /* this is a good pair */
      ins_pair( i, j, vecs->nvecs, *res);
WLnext_pair:;
    }
  }
  print_spclose("End of computing a product congruence.");
  print_spres("Product=");
  print_r_cong(vecs->nvecs, *res);
  return(NO_ERROR);
}

