#define EXTERN extern
#include "includes.h"

int
comp_cong( ALG *A, VECTORS *vecps, VEC *cong )
/* This routine generates a congruence in A.
   Given a list of pairs `vecps' in A^2, the function
   prepares the congruence on A generated by these pairs,
   and returns it in `cong' (which is in ewk format).
   Returns ERROR on error. vecps is not freed on error.
   It is the caller's responsibility to ensure that
   vecps->lvecs= 2.

   `vecps' is changed in the process, actually it becomes the
   subalgebra generated by `vecps' and the diagonal of A. */
{
  int i;
  int err;

  print_spopen("Generating the congruence.");
  err = add_consts(vecps, A->size);
  if( err!=NO_ERROR) return(err);

/* diagonal added */
  err= spf(A, vecps, NULL, NULL, NULL);
  if(err == INTERRUPT) return(err);
  if(err!= NO_ERROR) return(ERROR);
  (*cong)= (VEC)(calloc(A->size,sizeof(int)));
  if( (*cong) == NULL) {
    print_derr("Not enough memory to store congruence.");
    return(ERROR);
  }
  for(i= 0; i<A->size; i++)
  {
    (*cong)[i]= i;
  }
  for(i=0; i<vecps->nvecs; i++) {
    ins_pair( ((vecps->list)[i])[0], ((vecps->list)[i])[1], A->size, *cong);
  }
  print_spclose("End of generating the congruence.");
  print_spres("RESULT: ");
  print_r_cong(A->size, *cong);
  return(NO_ERROR);
}

int
comp_prin_cong( ALG *A, int a, int b, VEC *cong )
/* This routine generates a Cg(a,b) in A, and
   returns it in `cong' (which is in ewk format).
   ERROR if there is not enough memory.
*/
{
  int err;
  VECTORS univ;

  univ.lvecs=2;
  univ.nvecs=1;
  univ.list=NULL;
/* Put (a,b) into univ */
  if ((univ.list= (VEC *)(calloc(univ.nvecs,sizeof(VEC))))== NULL)
  {
    print_derr("Not enough memory to compute principal congruence.");
    return(ERROR);
  }
  (univ.list)[0]= (VEC)NULL;  /* to ensure that free_vectors works OK */
  if(((univ.list)[0]= (VEC)(calloc(univ.lvecs,sizeof(int))))== NULL) {
    print_derr("Not enough memory to compute principal congruence.");
    free_vectors(&univ);
    return(ERROR);
  }
  ((univ.list)[0])[0]=a;
  ((univ.list)[0])[1]=b;
/* now compute congruence with these pairs */
  err=comp_cong(A, &univ, cong);
  free_vectors(&univ);
  return(err);
}

void
ins_pair( int a, int b, int size, VEC cong)
/* Puts into cong the partition generated by cong and (a,b).
   It is assumed that cong is a valid partition in ewk format
   of length `size',(hence, func[i]<= i), and it remains so on return.
   IT IS THE RESPONSIBILITY OF THE CALLER to ensure that
   a and b are non--negative, and smaller than cong->size). 
*/
{
  VEC i, ei;
  int mi, ma, m;

  mi= cong[a];
  ma= cong[b];
  if(mi == ma) return;
  if(mi>ma)
  {
    m= ma;
    ma= mi;
    mi= m;
  }
  /* now change all values of ma to mi */
  ei= cong+size;
  for(i= cong+ma; i<ei; i++)
  {
    if(*i== ma) *i= mi;
  }
}

void
arb2matt_con( int size, VEC f)
/* Congruence format converter, see WAL.H.
   The result is put back into cong, so beware when calling!
   Speed is not a concern here, since this converter is
   used only on I/O. We do not assume anything on the values
   of in->func, except that they are not so big or small
   as to cause overflow.

   ALGORITHM (find a better one without the need to allocate storage):
   1. Find the smallest entry, say `min' (can be negative!).
   2. Add size - min to every entry. Thus they are all
      at least size.
   3. For every i such that v=func[i]>=size, change all entries
      that are equal to m to its proper value.
*/

{
  int min, c, v, i, j;

  min=f[0];
  for(i=0; i<size; i++) {
    if(f[i]<min) min=f[i];
  }
  min=(size-min); /* for speed: subtraction done only once */
  for(i=0; i<size; i++) {
    f[i] += min;
  }
  c=0; /* number of congruence class */
  for(i=0; i<size; i++) {
    if(f[i]<size) continue; /* this value is already correctly set */
    v=f[i];
    for(j=i; j<size; j++) {
      if(f[j]==v) f[j]=c;
    }
    c++; /* a new class has been found */
  }
}

void
arb2ewk_con( int size, VEC f)
/* Congruence format converter, see WAL.H.
   See the remarks in arb2matt_con().
*/

{
  int min, v, i, j;

  min=f[0];
  for(i=0; i<size; i++) {
    if(f[i]<min) min=f[i];
  }
  min=(size-min); /* for speed: subtraction done only once */
  for(i=0; i<size; i++) {
    f[i] += min;
  }
  for(i=0; i<size; i++) {
    if(f[i]<size) continue; /* this value is already correctly set */
    v=f[i];
    for(j=i; j<size; j++) {
      if(f[j]==v) f[j]=i; /* the only difference with arb2matt */
    }
  }
}

int
is_smaller_part( int size, VEC one, VEC two )
/* It is assumed that one and two are valid partitions on the same set
   of size `size', in any format.
   Returns YES if one is smaller (or equal) than two.
   Returns NO otherwise.
*/
{
  int i, j;

  for(i=0; i<size; i++) {
    for(j=i+1; j<size; j++) {
      if( (one[i]==one[j]) && (two[i]!=two[j]) ) {
        return(NO);
      }
    }
  }
  return(YES);
}

int
join_part( int size, VEC one, VEC two, VEC *result)
/* Puts into `result' the join of partitions `one' and `two'.
   It is assumed that these are valid partitions on the same set
   of size `size', in ewk format.
   Allocates space for the new partition, returns ERROR on error.
   Probably there are much faster algorithms. This one seems to work.
*/
{
  int i, j;

  if ( ((*result)= (VEC)(calloc(size,sizeof(int))))== NULL ) {
    print_derr("Not enough memory to store join congruence.");
    return(ERROR);
  }
  for(i=0; i<size; i++) (*result)[i]=one[i];  /* copy `one' to `result' */
  for(i=0; i<size; i++) {
    for(j=i+1; j<size; j++) {
      if(two[i]==two[j]) {
        ins_pair(i, j, size, *result);
      }  /* function call could be avoided for speed */
    }
  }
  return(NO_ERROR);
}

int
meet_part( int size, VEC one, VEC two, VEC *result)
/* Puts into `result' the meet of partitions `one' and `two'.
   It is assumed that these are valid partitions on the same set
   of size `size', in ewk format.
   Allocates space for the new partition, returns ERROR on error.
   Probably there are much faster algorithms. This one seems to work.
*/
{
  int i, j;

  if ( ((*result)= (VEC)(calloc(size,sizeof(int))))== NULL ) {
    print_derr("Not enough memory to store meet congruence.");
    return(ERROR);
  }
  for(i=0; i<size; i++) (*result)[i]=i;  /* make *result 0_A */
  for(i=0; i<size; i++) {
    for(j=i+1; j<size; j++) {
      if(one[i]==one[j] && two[i]==two[j]) {
        ins_pair(i, j, size, *result);
      }  /* function call could be avoided for speed */
    }
  }
  return(NO_ERROR);
}

int
comp_all_prin_cong( ALG *A, PARTS *conlist )
/* This routine generates all principal congruences of A, and
   returns the list in `conlist' (which is in ewk format).
   ERROR if there is not enough memory.
   On error, conlist is freed.
*/
{
  int a, b, err;
  VEC cong;

#ifdef RUN_PROGRESS
  double elapsed_ratio; /* for a progress report */
  double tick, all;     /* for a progress report */
#ifdef TIME_DIAG
  time_t start_time; /* time when routine started */
  time_t last_time; /* last_time when a diagnostic message was printed */
#endif /* TIME_DIAG */
#endif /* RUN_PROGRESS */

#ifdef RUN_PROGRESS
  tick=(double)0;
  all= (double)1 + (((double)(A->size)) * ((double)((A->size)-1))/(double)2);
#ifdef TIME_DIAG
/* record the time */
  start_time=time( NULL );
  last_time=start_time;
#endif /* TIME_DIAG */
#endif /* RUN_PROGRESS */
  conlist->nparts=0;
  conlist->lparts=A->size;
  print_spopen("Generating all principal congruences.");
  for(a=0; a<A->size; a++) {
    for(b=((a>0)?a+1:a); b<A->size; b++) {
                             /* the zero congruence added only once! */
	if(StopIt == YES) return(INTERRUPT);
#ifdef RUN_PROGRESS
      tick += (double)1;
      if (all <= (double)0) elapsed_ratio = (double)0;
      else elapsed_ratio= tick / all;
      print_time_progr( 
#ifdef TIME_DIAG
                            start_time, &last_time,
#endif /* TIME_DIAG */
                            elapsed_ratio, "All princ. congr.");
#else
      print_rshort();
#endif /* RUN_PROGRESS */
      err = comp_prin_cong(A, a, b, &cong);
      if( err!=NO_ERROR) {
        return(err); /* *cong has been freed */
      }
      err=add_con( &cong, conlist);
      if( err == ERROR ) {
        free((void *)cong);
        free_conlist(conlist);
        return(ERROR);
      }
      if(err==NOT_FOUND) {
        print_spres("Cg(%2d ,%2d )=C%d: ", a, b, conlist->nparts-1);
        print_r_cong(A->size, cong);
      } else {
        print_spres("Cg(%2d ,%2d )=C%d.", a, b, err);
      }
    }
  }
  print_spclose("End of generating principal congruences.");
  print_spres("The algebra has %d principal congruence(s).", conlist->nparts);
  return(NO_ERROR);
}

int
comp_conlat( ALG *A, ALG *L, PARTS *conlist )
/* This routine generates all congruences of A, and
   returns the list in `conlist' (which is in ewk format).
   If conlist->list==NULL, then it calculates the principal congruences,
   OTHERWISE IT IS ASSUMED THAT CONLIST CONTAINS THE PRINCIPAL CONGRUENCES.
   If L!=NULL, then the congruence lattice is computed, and put into L.
   (f0 is join, f1 is meet). 
   Returns ERROR if there is not enough memory to compute congruences.
   If `conlist' is succesfully computed, then returns NO_ERROR.
   However, if L is not computed succesfully, then L->func = NULL.

   The algorithm is a simplified form of Berman's routine,
   no hashing, only one binary operation, the join,
   starting with the list of principal congruences.

   A `tick' now is a computing of a join (table lookups don't count).

*/
{
  int i, j, a, b, err;
  int *join_table, *join_table_new;
  int level_limit, args_limit;
  VEC join;
  FUNCT *join_op, *meet_op;
#ifdef RUN_PROGRESS
  double all;           /* all ticks needed (as it seems at the moment) */
  double tick;          /* # of ticks elapsed so far */
  double elapsed_ratio; /* basically tick/all */
  int tick_mod;         /* # of ticks elapsed since the last report */
  int old_gennum;       /* # of generated vectors at the last report */
#ifdef TIME_DIAG
  time_t start_time; /* time when routine started */
  time_t last_time; /* last_time when a diagnostic message was printed */
#endif /* TIME_DIAG */
#endif /* RUN_PROGRESS */


  if(conlist->list == NULL) {
    err=comp_all_prin_cong( A, conlist );
    if(err!= NO_ERROR) {
      return(err);
    }
  }
  print_spopen("Generating all joins of principal congruences.");
/* Check number of generators */
  if(conlist->nparts>= CONLAT_SIZE_LIMIT) {
    print_derr("Illegal conlat size %d (should be at most %d).",
                        conlist->nparts, CONLAT_SIZE_LIMIT-1 );
    free_conlist(conlist);
    return(ERROR);
  }
/* allocate a small join_table first, and expand it with each level */
  if( (join_table= (int *) (calloc( 
         (conlist->nparts)*(conlist->nparts), sizeof(int))))== NULL)
  {
    print_derr("Not enough memory to store join_table.");
    free_conlist(conlist);
    return(ERROR);
  }
  for(a=0; a<conlist->nparts; a++) {
    for(b=0; b<conlist->nparts; b++) {
      if(a==b) {
        join_table[a+(conlist->nparts)*b]=a; /* join is idempotent */
      } else {
        join_table[a+(conlist->nparts)*b]=UNDEFINED;
      }
    }
  }
#ifdef RUN_PROGRESS
  tick=(double)0;
  tick_mod=0;
  old_gennum=conlist->nparts;
  all= ((double)(conlist->nparts)) * 
                     ((double)((conlist->nparts)-1)) /(double)2;
#ifdef TIME_DIAG
/* record the time */
  start_time=time( NULL );
  last_time=start_time;
#endif /* TIME_DIAG */
#endif /* RUN_PROGRESS */
/*  Now compute new `levels'.
  We compute join(i,j) for all possible values
  of 0<= i, j < conlist->nparts.
  The variable level_limit is such, that join
  has already been performed for all values
  0<= i, j <level_limit.
*/
  level_limit= 0; /* There is at least one principal congruence */
  args_limit= conlist->nparts;
  for(;;) {     /* this cycles through levels */
                 /* level_limit, args_limit are fixed inside */
    i = level_limit;
    j = 0; /* join is commutative, and idempotent, but it is simpler
              to compute all i join i than taking care of levels */
    for(;;) {      /* perform join */
      if(join_table[i+(args_limit)*j]!=UNDEFINED) goto WLnext_args;
/* args_limit is the current size of join_table */

      if(StopIt == YES) return(INTERRUPT);
#ifdef RUN_PROGRESS
      tick += (double)1;
      tick_mod++;
      if(tick_mod >= JM_PROGR_TICKS )
      {
        tick_mod=0;
        if( (conlist->nparts) > old_gennum ) {
          all= ((double)(conlist->nparts)) * 
                     ((double)((conlist->nparts)-1)) /(double)2;
          old_gennum=conlist->nparts;
        }
        if( all <= (double)0 ) elapsed_ratio=(double)0;
        else elapsed_ratio= tick/all;
        print_time_progr( 
#ifdef TIME_DIAG
                                start_time, &last_time, 
#endif /* TIME_DIAG */
                                elapsed_ratio, "Conlat joins.");
      }
#endif /* RUN_PROGRESS */
      err=join_part( A->size, (conlist->list)[i], (conlist->list)[j], &join);
      if(err!= NO_ERROR) {
        free((void *)join_table);
        free_conlist(conlist);
        return(err);
      }
      err= add_con( &join, conlist);
      if(err == ERROR) {
        free((void *)join_table);
        free((void *)join);
        free_conlist(conlist);
        return(ERROR);
      }
      if(err==NOT_FOUND) {
        if( conlist->nparts>= CONLAT_SIZE_LIMIT) {
          print_derr("Illegal conlat size %d (should be at most %d).",
                        conlist->nparts, CONLAT_SIZE_LIMIT-1 );
          free((void *)join_table);
          free_conlist(conlist);
          return(ERROR);
        }
        join_table[i+(args_limit)*j]=(conlist->nparts)-1;
        join_table[j+(args_limit)*i]=(conlist->nparts)-1;
      } else {
        join_table[i+(args_limit)*j]=err;
        join_table[j+(args_limit)*i]=err;
      }
      if(err==NOT_FOUND) {
        print_spres("C%3d join C%3d = C%3d: ", i, j, (conlist->nparts)-1);
        print_r_cong(A->size, join);
      } else {
        print_spresl("C%3d join C%3d = C%3d.", i, j, err);
      }
/* find next args */
WLnext_args:
      i++;
      if(i>=args_limit) {
        i=0;
        j++;
        if(j>=args_limit) {
          break; /* out of the for(;;), go to new level */
        }
        if(j<level_limit) {
          i=level_limit;
        }
      }
    }
/* compute new level */
    if(conlist->nparts == args_limit) {
      break; /* all joins have been computed */
    }
/* at this point we have to reallocate join_table, and copy the old table */
    if( (join_table_new= (int *) (calloc( 
           (conlist->nparts)*(conlist->nparts), sizeof(int))))== NULL)
    {
      print_derr("Not enough memory to reallocate join_table.");
      free((void *)join_table);
      free_conlist(conlist);
      return(ERROR);
    }
/* fill the new table */
    for(a=0; a<conlist->nparts; a++) {
      for(b=0; b<conlist->nparts; b++) {
        if(a==b) {
          join_table_new[a+(conlist->nparts)*b]=a; /* join is idempotent */
        } else {
          join_table_new[a+(conlist->nparts)*b]=UNDEFINED;
        }
      }
    }
/* copy the new table */
    for(a=0; a<args_limit; a++) {
      for(b=0; b<args_limit; b++) {
        join_table_new[a+(conlist->nparts)*b]=
                                    join_table[a+(args_limit)*b];
      }
    }
/* now forget the old table */
   free((void *)join_table);
   join_table=join_table_new;
/* finally reset the limits */
    level_limit= args_limit;
    args_limit= conlist->nparts;
  }      /* get next level */
  print_spclose("End of generating all joins.");
  print_spres("The congruence lattice has %d element(s).", conlist->nparts);

/* conlist is ready, now compute L if required */

  if(L!=NULL) {
    L->size=conlist->nparts;
    L->func=NULL;
    if( (join_op= (FUNCT *) (malloc(sizeof(FUNCT))))== NULL)
    {
      print_derr("Not enough memory to store lattice join.");
      free((void *)join_table);
      return(NO_ERROR);
    }
    L->func=join_op;
    join_op->arity=2;
    join_op->next= (FUNCT *)NULL;
    join_op->values=join_table;
/* from now on, free(L) frees join_table, too */
    if( (meet_op= (FUNCT *) (malloc(sizeof(FUNCT))))== NULL)
    {
      print_derr("Not enough memory to store lattice meet.");
      free_alg(L);
      return(NO_ERROR);
    }
    join_op->next= meet_op;
    meet_op->arity=2;
    meet_op->next= (FUNCT *)NULL;
    meet_op->values=NULL;
    (meet_op->values)=(int *)(calloc((L->size)*(L->size),sizeof(int)));
    if(meet_op->values==NULL) {
      print_derr("Not enough memory to store lattice meet.");
      free_alg(L);
      return(NO_ERROR);
    }
/* everything is succesfully allocated, fill in meet values */
    if( meet_table(meet_op->values, conlist) != NO_ERROR ) {
      free_alg(L);
      return(NO_ERROR);
    }
  }
  return(NO_ERROR);
}

int
meet_table(int * meet_values, PARTS *cons)
/*
  Prepare the meet table of the partition lattice in `cons'.
  Returns ERROR, if this is not a meet-semilattice, or if
  there is not enough memory. Returns NO_ERROR if succesful.
  It is assumed that meet_op is big enough.
*/
{
  int i, j, err;
  VEC meet;
#ifdef RUN_PROGRESS
  double elapsed_ratio; /* for a progress report */
  double tick, all;     /* for a progress report */
  int tick_mod;         /* # of ticks elapsed since the last report */
#ifdef TIME_DIAG
  time_t start_time; /* time when routine started */
  time_t last_time; /* last_time when a diagnostic message was printed */
#endif /* TIME_DIAG */
#endif /* RUN_PROGRESS */

  print_spopen("Computing all meets.");
#ifdef RUN_PROGRESS
  tick=(double)0;
  tick_mod=0;
  all= ((double)(cons->nparts)) * ((double)((cons->nparts)-1))/(double)2;
#ifdef TIME_DIAG
/* record the time */
  start_time=time( NULL );
  last_time=start_time;
#endif /* TIME_DIAG */
#endif /* RUN_PROGRESS */
  for(i=0; i<cons->nparts; i++) {
    for(j=i+1; j<cons->nparts; j++) {

	if(StopIt == YES) return(INTERRUPT);
#ifdef RUN_PROGRESS
      tick += (double)1;
      tick_mod++;
      if(tick_mod >= JM_PROGR_TICKS )
      {
        tick_mod=0;
        if (all <= (double)0) elapsed_ratio = (double)0;
        else elapsed_ratio= tick / all;
        print_time_progr( 
#ifdef TIME_DIAG
                            start_time, &last_time,
#endif /* TIME_DIAG */
                            elapsed_ratio, "Conlat meets.");
      }
#endif /* RUN_PROGRESS */
      err=meet_part(cons->lparts,(cons->list)[i],(cons->list)[j],&meet);
      if(err!= NO_ERROR) {
        return(err);
      }
      err= find_con( meet, cons);
      free((void *)meet);
      if(err == NOT_FOUND) {
        print_derr("This set of partitions is not closed under meet.");
        return(ERROR);
      }
      (meet_values)[i+j*(cons->nparts)]=err;
      (meet_values)[j+i*(cons->nparts)]=err;
      print_spresl("C%3d meet C%3d = C%3d.", i, j, err);
    }
  }
/* meet is idempotent: */
  for(i=0; i<cons->nparts; i++) {
    (meet_values)[i+i*(cons->nparts)]=i;
  }
  print_spclose("End of computing all meets.");
  return(NO_ERROR);
}

int
order_congs( PARTS *congs, int **ord )
/* Prepares 
   ord[a][b]=O_S_COVER if a-<b, O_S_N_COVER if a<b, but not a-<b; 
   ord[a][b]=O_B_COVER if a>-b, O_B_N_COVER if a>b, but not a>-b; 
   O_EQUAL if a=b, O_INCOMP if they are incomparable.
   Allocates storage, and returns the table in ord.
   Returns ERROR, or NO_ERROR.
   Here, a, b runs over congs. It is assumed that all elements
   of congs are different.
 */
{
  int i, j, k;
  int tmp;
  int flag;

#define BIN_ARR_SIZE (congs->nparts)

  (*ord)= (int *)(calloc((congs->nparts)*(congs->nparts),sizeof(int)));
  if( (*ord) == NULL) {
    print_derr("Not enough memory to order partitions.");
    return(ERROR);
  }
  for(i=0; i<congs->nparts; i++) {
    for(j=0; j<congs->nparts; j++) {
      (*ord)[BIN_ARR_INDEX(i,j)]=O_INCOMP;
    }
  }
  for(i=0; i<congs->nparts; i++) {
    (*ord)[BIN_ARR_INDEX(i,i)]=O_EQUAL;
  }
  for(i=0; i<congs->nparts; i++) {
    for(j=0; j<congs->nparts; j++) {
      flag = is_smaller_part(congs->lparts,
			       (congs->list)[i], (congs->list)[j]);
      if( (i!=j) && ( flag == YES )) {
        (*ord)[BIN_ARR_INDEX(i,j)]=O_S_COVER;
        (*ord)[BIN_ARR_INDEX(j,i)]=O_B_COVER;
      /* not yet covers, just smaller and bigger */
      }
    }
  }
/* now *ord is filled in. Next we prepare coverings. 
   We consider all triplets (a < b < c), and mark (a,c).
   Is there a faster way? */
  for(i=0; i<congs->nparts; i++) {
    for(j=0; j<congs->nparts; j++) {
      tmp= (*ord)[BIN_ARR_INDEX(i,j)];
      if( !(tmp == O_S_COVER || tmp==O_S_N_COVER) ) continue;
      /* now i < j */
      for(k=0; k<congs->nparts; k++) {
        tmp= (*ord)[BIN_ARR_INDEX(j,k)];
        if( !(tmp == O_S_COVER || tmp==O_S_N_COVER) ) continue;
        /* now j < k */
        (*ord)[BIN_ARR_INDEX(i,k)]= O_S_N_COVER; /* so (i,k) is not a cover */
        (*ord)[BIN_ARR_INDEX(k,i)]= O_B_N_COVER; /* so (k,i) is not a cover */
      }
    }
  }
  return(NO_ERROR);

#undef BIN_ARR_SIZE

}

int
find_pers_mi( int size, int *ord, int c, int d, int *m, int *u )
/* ord is an ordering of a lattice (poset?) of size `size'.
   c<d are elements of this lattice (0<=c,d<size, 
   c<d are not checked.).
   The program finds a maximal *m with *m>=c, *m not >=d.
   Then checks if m has a unique cover, returns ERROR if not.
   If it does, then it is put into *u, and NO_ERROR is returned.
   *m is always set correctly, *u is undefined on ERROR.
 */
{
  int i;
  int tmp, count;

#define BIN_ARR_SIZE (size)

  *m=c;
  for(i=0; i<size; i++) {
    tmp=ord[BIN_ARR_INDEX(*m,i)];
    if( !(tmp==O_S_COVER || tmp==O_S_N_COVER) ) continue;
    /* so  *m<i */
    tmp=ord[BIN_ARR_INDEX(d,i)];
    if( (tmp==O_S_COVER || tmp==O_S_N_COVER|| tmp==O_EQUAL) ) continue;
    /* so d<=i is false */
    *m=i;
  }
/* *m is correctly set, look for *u */
/* could call a modified is_mi() here. */
  count=0;
  for(i=0; i<size; i++) {
    tmp=ord[BIN_ARR_INDEX(*m,i)];
    if( !(tmp==O_S_COVER) ) continue;
    /* so  *m-<i */
    if(count>=1) return(ERROR);
    count++;
    *u=i;
  }
  if(count!=1) return(ERROR); /* count must be 0 */
  return(NO_ERROR);

#undef BIN_ARR_SIZE

}

int
is_mi( int size, int *ord, int m )
/* ord is an ordering of a lattice (poset?) of size `size'.
   m is an element of this lattice.
   Returns YES if m is meet-irred (has at most one covers),
   NO otherwise.
   This function is essentially a part of find_pers_mi().
 */
{
  int i;
  int tmp, count;

#define BIN_ARR_SIZE (size)

  count=0;
  for(i=0; i<size; i++) {
    tmp=ord[BIN_ARR_INDEX(m,i)];
    if( !(tmp==O_S_COVER) ) continue;
    /* so  m-<i */
    if(count>=1) return(NO);
    count++;
  }
  return(YES);

#undef BIN_ARR_SIZE

}

int
cong2pairs(VECTORS *v, PART *cong )
/* This routine puts into v all vectors of length 2
   that are related modulo cong.
   Returns ERROR if memory error.
*/
{
  int i,j;
  VEC pair;

  v->list=NULL;
  v->lvecs=2;
  v->nvecs=0;

  for(i=0; i<cong->size; i++) {
    for(j=0; j<cong->size; j++) {
      if( (cong->func)[i] == (cong->func)[j]) {
        pair= (VEC)(calloc(2,sizeof(int)));
        if (pair== (VEC)NULL) {
          print_derr("Not enough memory to store pairs.");
          free_vectors(v);
          return(ERROR);
        }
        pair[0]=i;
        pair[1]=j;
        if(add_vec(&pair, v)==ERROR) {
          free((void *)pair);
          free_vectors(v);
          return(ERROR);
        }
      }
    }
  }
  return(NO_ERROR);
}

int
one_cong( PART *cong)
/* Put the one congruence on cong->size into cong.
*/
{
  int i;

  if(cong->size <0 || cong->size >=ALG_SIZE_LIMIT) {
    print_derr("Illegal algebra size %d (should be in [%d, %d]).",
                                       cong->size, 0, ALG_SIZE_LIMIT-1);
    return(ERROR);
  }
  cong->func= (VEC)(calloc(cong->size,sizeof(int)));
  if(cong->func== NULL) {
    print_derr("Not enough memory to store the one congruence.");
    return(ERROR);
  }
  for(i=0; i<cong->size; i++) {
    (cong->func)[i]=0;
  }
  return(NO_ERROR);
}

int
zero_cong( PART *cong)
/* Put the zero congruence on cong->size into cong.
*/
{
  int i;

  if(cong->size <0 || cong->size >=ALG_SIZE_LIMIT) {
    print_derr("Illegal algebra size %d (should be in [%d, %d]).",
                                       cong->size, 0, ALG_SIZE_LIMIT-1);
    return(ERROR);
  }
  cong->func= (VEC)(calloc(cong->size,sizeof(int)));
  if(cong->func== NULL) {
    print_derr("Not enough memory to store the zero congruence.");
    return(ERROR);
  }
  for(i=0; i<cong->size; i++) {
    (cong->func)[i]=i;
  }
  return(NO_ERROR);
}

int
comp_mi_congs( PARTS *conlat, PARTS *mi )
/* This routine computes all meet-irred partitions in conlat,
   and puts them into mi. The partitions are copied.
   The maximal size of a corresponding factor is printed.
*/
{
  int i, m;
  int err;
  int *ord;
  VEC mientry;
  int max_factor, factor, max_index;

  print_spopen("Computing all meet_irreducibles.");
  max_factor=UNDEFINED;
  max_index=UNDEFINED;
  mi->nparts=0;
  mi->lparts=conlat->lparts;
  mi->list=NULL;
  err=order_congs(conlat, &ord);
  if(err!=NO_ERROR) return(err);
  for(m=0; m<conlat->nparts; m++) {
    err=is_mi( conlat->nparts, ord, m );
    if(err==YES) {
      mientry = (VEC)(calloc(mi->lparts,sizeof(int)));
      if (mientry == NULL) {
        print_derr("Not enough memory to store meet-irreducible partitions.");
        free((void *)ord);
        free_conlist(mi);
        return(ERROR);
      }
      /* first compute size of factor */
      for(i=0; i<conlat->lparts; i++) {
        mientry[i] = ((conlat->list)[m])[i];
      }
      arb2matt_con(conlat->lparts, mientry);
      /* The size of the factor is the max entry+1 in Matt's format */
      factor=0;
      for(i=0; i<conlat->lparts; i++) {
        if( factor < mientry[i] ) factor=mientry[i];
      }
      factor++; /* is the size of the factor */
      if( max_factor < factor) {
        max_factor=factor;
        max_index=m;
      }
      /* mientry has been spoiled by arb2matt, copy again */
      for(i=0; i<conlat->lparts; i++) {
        mientry[i] = ((conlat->list)[m])[i];
      }
      err=add_con(&mientry, mi);
      if(err==ERROR) {
        free((void *)mientry);
        free((void *)ord);
        free_conlist(mi);
        return(ERROR);
      }
      if(err!=NOT_FOUND) {
        print_serr("The same partition occurs twice in the conlist file.");
      }
      print_spres("C%d is meet-irreducible, %d classes", m, factor);
    }
  }
  print_spclose("End of computing meet-irreducibles.");

  print_spres("The biggest Si factor is mod C%d, and has %d element(s).", 
                                              max_index, max_factor);
  return(NO_ERROR);
}
