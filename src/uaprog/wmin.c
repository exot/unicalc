#define EXTERN extern
#include "includes.h"

static int check_min( NEW_TUPLE *, void * );
static int fill_min(VECTORS *, VEC, VEC, VEC, VECTORS *);

typedef struct min_check {
  VEC alpha;
  VEC beta;
  VEC polyn;
  int is_comp_size; /* of is_comp */
  VEC is_comp; /* see min_sets_np1  */
  } MIN_CHECK;

int
min_sets_np1(ALG *A, VEC alpha, VEC beta, VECTORS *msets)
/*  This routine computes the <alpha,beta> minimal sets,
    and returns them in msets. The sets in msets are vectors,
    whose components are in increasing order. 
    The unary polynomials are not computed, because it
    is faster to compute the values of the unary polynomials
    only on a subset. The program starts generating all
    unary polynomials on A. Each of them is checked,
    and if it satisfies f(alpha)\not\subseteq beta, and the range of f
    is smaller than A, then the process is restarted on the range
    R of f. This is repeated until all polynomials are
    generated on some set.
    WARNING. We must always check if the composition of these unary
    plynomials satisfies f(beta) \not\subseteq(alpha). It is NOT sufficient
    to see that the last step satisfies this. For this reason,
    and for speed, we use the following data:
    is_comp[] as an array of size A->size. It is maintained so that
    (set\circ is_comp) is a unary polynomial, whose range is the current
    minimal set. is_ refers to the fact that the composition has been
    composed with inv_set (which is the inverse of set).
    We use is_comp rather than comp to avoid extra indexing
    in the speed-critical check_-routine.
    Returns ERROR or NO_ERROR.
*/
{
  int i;
  VECTORS pols;
  VEC set, inv_set, tmp, is_comp;
  int set_size;
  int err, errf;
  MIN_CHECK s;

  print_spopen("Computing <alpha,beta>-minimal sets.");
  set= (VEC)(calloc(A->size,sizeof(int)));
  if (set== NULL) {
    print_derr("Not enough memory to compute minimal sets.");
    return(ERROR);
  }
  is_comp= (VEC)(calloc(A->size,sizeof(int)));
  if (is_comp== NULL) {
    print_derr("Not enough memory to compute minimal sets.");
    free((void *)set);
    return(ERROR);
  }
  inv_set= (VEC)(calloc(A->size,sizeof(int)));
  if (inv_set== NULL) {
    print_derr("Not enough memory to compute minimal sets.");
    free((void *)set);
    free((void *)is_comp);
    return(ERROR);
  }
  set_size=A->size;
  for(i=0; i<A->size; i++) {
    set[i]=i;
    inv_set[i]=i;
    is_comp[i]=i;
  }
  pols.nvecs=0;
  pols.lvecs=set_size;
  pols.list=NULL;
  s.alpha=alpha;
  s.beta=beta;
  s.polyn=NULL;
  s.is_comp=is_comp;
  s.is_comp_size=A->size;
  for(;;) { /* this cycles through smaller and smaller sets. */
    tmp= (VEC)(calloc(set_size,sizeof(int)));
    if (tmp== NULL) {
      print_derr("Not enough memory to compute minimal sets.");
      free((void *)set);
      free((void *)inv_set);
      free((void *)is_comp);
      return(ERROR);
    }
    for(i=0; i<set_size; i++) {
      tmp[i]=set[i];
    }
    err=add_consts(&pols, A->size); /* add diagonal */
    if(err!=NO_ERROR) {
      free_vectors(&pols);
      free((void *)set);
      free((void *)inv_set);
      free((void *)is_comp);
      free((void *)tmp);
      return(ERROR);
    }
    err=add_vec(&tmp, &pols);
    if(err==ERROR) {
      free_vectors(&pols);
      free((void *)set);
      free((void *)inv_set);
      free((void *)is_comp);
      free((void *)tmp);
      return(ERROR);
    }
    /* from now on, we must not free tmp after having freed pols. */
    err=spf(A, &pols, NULL, check_min, (void *)(&s));
    if(err==ABORT) { /* a smaller set has been found */
      /* update set */
      set_size=find_range(set_size, s.polyn, set);
      /* inv_set must satisfy set[inv_set[i]]=i for all i\in set */
      for(i=0; i<set_size; i++) {
        inv_set[set[i]]=i; /* inv_set is garbage outside the range of set */
      }
      /* update is_comp */
      for(i=0; i< A->size; i++) {
        is_comp[i]=inv_set[(s.polyn)[is_comp[i]]];
      }
      print_spres("Smaller set found: ");
      print_r_set(set_size, set);
      free_vectors(&pols);
      pols.nvecs=0;
      pols.lvecs=set_size;
      pols.list=NULL;
      continue; /* start cycle with smaller set. */
    }
    if(err==NO_ERROR) { /* all minimal sets have been computed */
	errf = fill_min(&pols, set, alpha, beta, msets);
	if ( errf != NO_ERROR ) {
	    free_vectors(&pols);
	    free((void *)set);
	    free((void *)inv_set);
	    free((void *)is_comp);
	    if( errf  != NO_ERROR ) return(errf);
	}
	free_vectors(&pols);
	free((void *)set);
	free((void *)inv_set);
	free((void *)is_comp);
	print_spclose("End of computing <alpha,beta>-minimal sets.");
	print_spres("This quotient has %d minimal set(s), they are of size %d.", 
		    msets->nvecs, msets->lvecs);
	return(NO_ERROR);
    }
/* so err is ERROR or maybe INTERRUPT */
    free_vectors(&pols);
    free((void *)set);
    free((void *)inv_set);
    free((void *)is_comp);
    return(err);
  }
/* NOTREACHED */
  return(ERROR);
}

static int
check_min( NEW_TUPLE *ntp, void *sp)
/* If new satisfies (new\circ is_comp)(beta)\not\subseteq alpha,
   and it is not bijective, then we put it into polyn,
   and return ABORT. Otherwise, we return NO_ERROR.
   ABORT is a rare event. Thus we minimize running time
   for the other vectors.
*/
{
  int i, j, cont, bij;
  MIN_CHECK *s;
  int size;
  VEC new;

  if( (((ntp->argsl)->list)[ntp->offset]).args==NULL) {
    return(NO_ERROR); /* skip generators */
  }
  new=((ntp->gen)->list)[ntp->offset];
  size=((ntp->gen)->lvecs); /* size of new */
  s=((MIN_CHECK *)sp);
  cont=YES;
  bij=YES;
  for(i=0; i<size; i++) {
    for(j=i+1; j<size; j++) {
      if( new[i]==new[j]) { 
        bij=NO;
      }
    }
  }
  for(i=0; i<s->is_comp_size; i++) {
    for(j=i+1; j<s->is_comp_size; j++) {
      if( ( (s->beta)[i] == (s->beta)[j] ) &&
                    ( s->alpha[new[(s->is_comp)[i]]] !=
                      s->alpha[new[(s->is_comp)[j]]] ) ) {
        cont=NO; 
           /* new satisfies new \circ is_comp(beta)\not\subseteq (alpha) */
      }
    }
  }
  if(bij==NO && cont==NO) {
    s->polyn=new;
    return(ABORT);
  }
  return(NO_ERROR);
}

static int
fill_min(VECTORS *pols, VEC set, VEC alpha, VEC beta, VECTORS *msets)
/* put all vectors satisfying f(beta)\not\subseteq alpha into msets
   They are all bijections.
   By TCT it is sufficient to check f(beta)\not\subseteq alpha 
   on the range of the original minimal set, which is `set'.
*/
{
  int i, j, k;
  int size, isbij, cont;
  VEC new_set, pol;
  int err;

  msets->list=NULL;
  msets->nvecs=0;
  size=pols->lvecs;
  msets->lvecs=size;
  print_spres("Minimal sets (size=%d):", size);
  for(k=0; k<pols->nvecs; k++) {
    pol=(pols->list)[k];
    new_set= (VEC)(calloc(size,sizeof(int)));
    if (new_set== NULL) {
      print_derr("Not enough memory to store minimal sets.");
      free_vectors(msets);
      return(ERROR);
    }
    isbij=find_range(size, pol, new_set); /* we do this to copy and sort the
                                         entries, no need for speed here. */
    if(isbij!=size) { /* not a bijection */
      free((void *)new_set);
      goto WL_next_pol;
    }
    cont=YES;
    for(i=0; i<size; i++) {
      for(j=i+1; j<size; j++) {
        if( (beta[set[i]]==beta[set[j]]) &&
                   (alpha[new_set[i]]!=alpha[new_set[j]]) ) {
          cont=NO; /* new satisfies f(beta)\not\subseteq (alpha) */
          break;
        }
      }
    }
    if(cont==YES) { /* new_set(beta) \subseteq alpha */
      free((void *)new_set);
      goto WL_next_pol;
    }
    err= add_vec(&new_set, msets);
    if( err==ERROR ) {
      free((void *)new_set);
      free_vectors(msets);
      return(ERROR);
    }
    if(err==NOT_FOUND) {
      print_npres("M%3d: ", (msets->nvecs)-1);
      print_r_set(msets->lvecs, (msets->list)[(msets->nvecs)-1]);
    }
WL_next_pol:
    continue;  /* some compilers cannot have an empty label */
  }
  return(NO_ERROR);
}

int
comp_un_pol(ALG *A, VECTORS *un, VECTORS *id)
/*  This routine computes all unary polynomials of A.
    They are put into un, and the idempotent ones into id.
    Returns ERROR or NO_ERROR.
*/
{
  VEC idmap;
  int i, err;

  print_spopen("Generating unary polynomials.");
  un->nvecs=0;
  un->lvecs=A->size;
  un->list=NULL;
  idmap= (VEC)(calloc(A->size,sizeof(int)));
  if (idmap== NULL) {
    print_derr("Not enough memory to compute unary polynomials.");
    return(ERROR);
  }
  for(i=0; i<A->size; i++) {
    idmap[i]=i;
  }
  err=add_vec(&idmap, un);
  if(err==ERROR) {
    free((void *)idmap);
    return(ERROR);
  }
/* from now on, we should not free idmap */
  err=add_consts(un, A->size); /* add diagonal */
  if(err!=NO_ERROR) {
    return(ERROR);
  }
  err=spf(A, un, NULL, NULL, NULL);
  if(err == INTERRUPT) return(err);
  if(err!=NO_ERROR) {
    return(ERROR);
  }
  err=comp_id_pol( un, id);
  if(err == INTERRUPT) return(err);
  if(err!=NO_ERROR) {
    return(ERROR);
  }
  print_spclose("End of generating unary polynomials.");
  print_spres("The algebra has %d unary polynomial(s), %d are idempotent.",
                                                 un->nvecs, id->nvecs);
  return(NO_ERROR);
}

int
comp_id_pol(VECTORS *un, VECTORS *id)
/*  This puts all idempotent unary polynomials in `un' into `id'.
    They are copied.
    Returns ERROR or NO_ERROR.
*/
{
  int i, j;
  VEC new;
  int err;

  id->nvecs=0;
  id->lvecs=un->lvecs;
  id->list=NULL;
  for(i=0; i< un->nvecs; i++) {
    if( is_idempotent(un->lvecs, (un->list)[i])==YES ) {
      new= (VEC)(calloc(un->lvecs,sizeof(int)));
      if (new== NULL) {
        print_derr("Not enough memory to compute unary polynomials.");
        return(ERROR);
      }
      for(j=0; j< un->lvecs; j++) {
        new[j] = ((un->list)[i])[j];
      }
      err=add_vec(&new, id);
      if(err==ERROR) {
        free((void *)new);
        return(ERROR);
      }
      /* from now on, we should not free new */
    }
  }
  return(NO_ERROR);
}

int
is_idempotent(int size, VEC v)
/*  Returns YES if v is idempotent, no otherwise.
    WARNING: 0<=v[i]<size must hold, it is not checked.
*/
{
  int i;

  for(i=0; i< size; i++) {
    if( v[v[i]]!=v[i] ) return(NO);
  }
  return(YES);
}

