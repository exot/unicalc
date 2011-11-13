#define EXTERN extern
#include "includes.h"

static int comp_type_quotient(ALG *, VEC , VEC );
static int comp_type_subtr(ALG *, VEC , VEC , int , int , int );
static int find_subtrace(ALG *, VEC , VEC , int *, int *, int *);
static int is_subtrace(ALG *, VEC , VEC , int , int , int *);
static int is_image(ALG *, int , int , int , int );

static int check_type( NEW_TUPLE *, void * );
static int check_pair( NEW_TUPLE  *, void * );
static int check_subtr( NEW_TUPLE *, void * );

typedef struct type_check {
  int a;
  int b;
  VEC alpha;
  VEC beta;
  int neg;
  int join;
  int meet;
  int one_snag;
  int type;
  } TYPE_CHECK;

typedef struct pair_check {
  int a;
  int b;
  } PAIR_CHECK;

typedef struct subtr_check {
  int a;
  int b;
  VEC alpha;
  int neg;
  } SUBTR_CHECK;

int
type_quotient(ALG *A, VEC alpha, VEC beta, int *type)
/*  This routine computes the type of <alpha,beta>,
    and returns it in type. The subtrace algorithm is used.
    Returns ERROR on error, NO_ERROR if succesful.
    Prints the result to the console.
*/
{
  int err;

  err=comp_type_quotient(A, alpha, beta);
  if(err == INTERRUPT) return(err);
  if(err==ERROR) return(err);
  *type=err;
  switch(err) {
    case UTYP:
      print_dpres("RESULT: <alpha,beta> is of unary type (1).");
      break;
    case ATYP:
      print_dpres("RESULT: <alpha,beta> is of affine type (2).");
      break;
    case BTYP:
      print_dpres("RESULT: <alpha,beta> is of Boolean type (3).");
      break;
    case LTYP:
      print_dpres("RESULT: <alpha,beta> is of lattice type (4).");
      break;
    case STYP:
      print_dpres("RESULT: <alpha,beta> is of semilattice type (5).");
      break;
    default:
      print_derr("The type of <alpha,beta> invalid.");
      return(ERROR);
  }
  return(NO_ERROR);
}

static int
comp_type_quotient(ALG *A, VEC alpha, VEC beta)
/*  This routine computes the type of <alpha,beta>,
    and returns it. The subtrace algorithm is used.
    Returns ERROR on error.
*/
{
  int a, b, neg;
  int err;

  print_spopen("Computing typ<alpha,beta>.");
  err=find_subtrace(A, alpha, beta, &a, &b, &neg);
  if(err!=NO_ERROR) return(err);
  err=comp_type_subtr(A, alpha, beta, a, b, neg);
  print_spclose("End of computing typ<alpha,beta>.");
  return(err);
}

static int
comp_type_subtr(ALG *A, VEC alpha, VEC beta, int a, int b, int neg)
/* neg is YES, if there is a unary complementation on <a,b>,
   NO otherwise. <a,b> is assumed to be an <alpha,beta>-subtrace.
   Returns the type is succesful, ERROR otherwise.
*/
{
  VECTORS bins; /* 4-tuples: partial bin ops on <a,b> */
  int err;
  VEC abab;
  VEC aabb;
  TYPE_CHECK s;

  print_spopen("Computing the type of the subtrace (%d, %d).", a, b);
  bins.nvecs=0;
  bins.lvecs=4;
  bins.list=NULL;
  abab = (VEC)(calloc(4,sizeof(int)));
  if (abab == NULL) {
    print_derr("Not enough memory to compute type.");
    return(ERROR);
  }
  abab[0]=a; abab[1]=b; abab[2]=a; abab[3]=b;
  aabb = (VEC)(calloc(4,sizeof(int)));
  if (aabb == NULL) {
    free((void *)abab);
    print_derr("Not enough memory to compute type.");
    return(ERROR);
  }
  aabb[0]=a; aabb[1]=a; aabb[2]=b; aabb[3]=b;
  bins.nvecs=0;
  bins.lvecs=4;
  bins.list=NULL;
  err=add_consts(&bins, A->size); /* add diagonal */
  if(err!=NO_ERROR) {
    free((void *)aabb);
    free((void *)abab);
    return(ERROR);
  }
  err=add_vec(&aabb, &bins);
  if(err==ERROR) {
    free_vectors(&bins);
    free((void *)aabb);
    free((void *)abab);
    return(ERROR);
  }
/* from now on, we must not free aabb after having freed bins. */
  err=add_vec(&abab, &bins);
  if(err==ERROR) {
    free_vectors(&bins);
    free((void *)abab);
    return(ERROR);
  }
/* from now on, we must not free abab after having freed bins. */

  s.a=a; s.b=b;
  s.neg=neg;
  s.join=NO;
  s.meet=NO;
  s.one_snag=NO;
  s.type=NOTYP;
  s.alpha=alpha;
  s.beta=beta;
  err=spf(A, &bins, NULL, check_type, (void *)(&s) );
  free_vectors(&bins);
  if( err== NO_ERROR ) { /* all binops checked */
    if(s.join==YES || s.meet==YES) {
      s.type=STYP;
    } else {
    /* so s.joinmeet=0; */
      s.type= (s.one_snag==YES) ? ATYP : UTYP;
    }
  }
  if(err == NO_ERROR || err==ABORT) { /* succesful run */
    print_spclose("End of computing subtrace type.");
    return(s.type);
  }
  return(err);
}

static int
check_type( NEW_TUPLE *ntp, void *sp)
/* new is a table of a binary operation on <a,b>, with values in
   the algebra, not necessarily in {a,b}. The purpose of this routine
   is to decide asap (that is, without generating too many such tables)
   the type of the subtrace <a,b>. The following algorithm can be
   justified by doing some easy math.
   Returns ABORT, if the type is known, and puts it into type.
   Returns NO_ERROR otherwise.

   Algorithm:
   For a table xyuv, if
   ((x==y) && (u!=v)) || ((x==u) && (y!=v)), then this is a meet;
   ((x!=y) && (u==v)) || ((x!=u) && (y==v)), then this is a join;
   both of these are one-snags, as also are
   ((x==v) && ((x!=y) || (u!=v)) and
   ((y==u) && ((x!=y) || (u!=v)).
   (Note: if neg is NO, then the above is true. If neg is YES, then
   either of the first two above yields both meet and join.
   So we can use this terminology).

   One has to:
   set one_snag to YES, if any of these is found;
   set meet to YES if a meet is found;
   set join to YES if a join is found.
   If neg is YES, then a join implies a meet and a meet implies a join.
   If there is a join and a meet, then we are done:
   either neg is NO and the type is LTYP,
   or neg is YES, and the type is BTYP. So we can ABORT.

   The next problem is to realize this, and doing it as fast as possible.
   We do not want to do too many if-s for the average tuple.
*/
{
  TYPE_CHECK *s;
  int x,y,u,v; /* mod alpha pattern of the table */
  int err;
  VEC new;
  char *out; /* print arguments join, meet, or one_snag found */

  if( (((ntp->argsl)->list)[ntp->offset]).args==NULL) {
    return(NO_ERROR); /* skip generators */
  }
  new=((ntp->gen)->list)[ntp->offset];
  s=(TYPE_CHECK *)sp;
  x=(s->alpha)[new[0]];
  y=(s->alpha)[new[1]];
  u=(s->alpha)[new[2]];
  v=(s->alpha)[new[3]];
  if(s->meet==YES) { 
    /* neg must be NO, since we would have aborted earlier */
    /* one_snag is YES; look for a join */
    if( ((x!=y) && (u==v)) || ((x!=u) && (y==v)) ) { /* join found */
      print_spres("Join found.");
      s->join=YES;
      s->type=LTYP;
      return(ABORT);
    }
    /* not a join, nothing else can be interesting, so continue */
    return(NO_ERROR);
  }
  if(s->join==YES) { 
    /* neg must be NO, since we would have aborted earlier */
    /* one_snag is also YES; look for a meet */
    if( ((x==y) && (u!=v)) || ((x==u) && (y!=v)) ) { /* meet found */
      print_spres("Meet found.");
      s->meet=YES;
      s->type=LTYP;
      return(ABORT);
    }
    /* not a meet, nothing else can be interesting, so continue */
    return(NO_ERROR);
  }
  /* so s->meet==NO, s->join=NO. Is this a meet? */
  if( ((x==y) && (u!=v)) || ((x==u) && (y!=v)) ) { /* meet found */
    s->meet=YES;
    s->one_snag=YES;
    print_spres("Meet found.");
    if(s->neg==YES) {
      s->join=YES;
      s->type=BTYP;
      return(ABORT);
    }
    err=build_expression(ntp->gen, ntp->argsl, ntp->offset, &out);
    if(err==NO_ERROR) print_npres("%s", out);
    if(out!=NULL) free((void *)(out));
    /* this was a meet, but no join yet: continue */
    return(NO_ERROR);
  }
  /* This is not a meet. Is it a join? */
  if( ((x!=y) && (u==v)) || ((x!=u) && (y==v)) ) { /* join found */
    s->join=YES;
    s->one_snag=YES;
    print_spres("Join found.");
    if(s->neg==YES) {
      s->meet=YES;
      s->type=BTYP;
      return(ABORT);
    }
    err=build_expression(ntp->gen, ntp->argsl, ntp->offset, &out);
    if(err==NO_ERROR) print_npres("%s", out);
    if(out!=NULL) free((void *)(out));
    /* this was a join, but no meet yet: continue */
    return(NO_ERROR);
  }
/* so not a meet, not a join. Is it an other kind of one-snag? */
  if(s->one_snag==YES) return(NO_ERROR); /* we do not care */
  if(    ( (x==v) && ((x!=y) || (u!=v)) ) ||
         ( (y==u) && ((x!=y) || (u!=v)) )     ) {
    s->one_snag=YES;
    print_spres("One-snag found.");
    err=build_expression(ntp->gen, ntp->argsl, ntp->offset, &out);
    if(err==NO_ERROR) print_npres("%s", out);
    if(out!=NULL) free((void *)(out));
    return(NO_ERROR);
  }
  return(NO_ERROR); /* not a one-snag */
}

static int
find_subtrace(ALG *A, VEC alpha, VEC beta, int *a, int *b, int *neg)
/* neg is YES, if there is a unary complementation on <a,b>,
   NO otherwise. <a,b> is assumed to be an <alpha,beta>-subtrace.
   Returns ERROR on ERROR, or if not found, NO_ERROR otherwise.
   A->size > 1 is assumed.
*/
{
  int i, j;
  int err;
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
  all= ((double)(A->size)) * ((double)((A->size)-1)) /(double)2;
#ifdef TIME_DIAG
/* record the time */
  start_time=time( NULL );
  last_time=start_time;
#endif /* TIME_DIAG */
#endif /* RUN_PROGRESS */
  print_spopen("Looking for an <alpha,beta>-subtrace.");
  for(i=0; i< A->size; i++) {
    for(j=i+1; j< A->size; j++) {
      err=is_subtrace(A, alpha, beta, i, j, neg);
      if( err==YES) {
        *a=i; *b=j;
        print_spres("(%d, %d) is an <alpha,beta>-subtrace.", i, j);
        print_spclose("End of looking for an <alpha,beta>-subtrace.");
        return(NO_ERROR);
      }
      if( err!=NO) return(err);
      if(StopIt == YES) return(INTERRUPT);
#ifdef RUN_PROGRESS
      tick += (double)1;
      if (A-> size <= 1) elapsed_ratio = (double)0;
      else elapsed_ratio= tick / all;
      print_time_progr( 
#ifdef TIME_DIAG
                           start_time, &last_time,
#endif /* TIME_DIAG */
                           elapsed_ratio, "Subtrace checker.");
#else
      print_rshort();
#endif /* RUN_PROGRESS */
    }
  }
  print_derr("No subtrace found, is <alpha,beta> tame?");
  return(ERROR);
}

static int
is_subtrace(ALG *A, VEC alpha, VEC beta, int a, int b, int *neg)
/* Returns YES, if <a,b> is an <alpha,beta> subtrace,
   NO if it isn't, ERROR on error. It is checked if <a,b> is in beta.
   If this is a subtrace, then neg is set to YES, if there is a unary
   complementation on <a,b>, and to NO otherwise. (If not a subtrace,
   *neg is NO.)
*/
{
  int err;
  VECTORS pairs;
  SUBTR_CHECK s;

  if( beta[a] != beta[b] ) return(NO);
  if( alpha[a] == alpha[b] ) return(NO);
  pairs.lvecs=2;
  pairs.nvecs=1;
  pairs.list=NULL;
/* Put (a,b) into pairs */
  if ((pairs.list= (VEC *)(calloc(pairs.nvecs,sizeof(VEC))))== NULL)
  {
    print_derr("Not enough memory to check subtrace.");
    return(ERROR);
  }
  (pairs.list)[0]= (VEC)NULL;  /* to ensure that free_vectors works OK */
  if(((pairs.list)[0]= (VEC)(calloc(pairs.lvecs,sizeof(int))))== NULL) {
    print_derr("Not enough memory to check subtrace.");
    free_vectors(&pairs);
    return(ERROR);
  }
  ((pairs.list)[0])[0]=a;
  ((pairs.list)[0])[1]=b;
/* now generate with these pairs */
  if(add_consts(&pairs, A->size)!=NO_ERROR) return(ERROR);
/* diagonal added */
  s.a=a; s.b=b;
  s.neg=NO;
  s.alpha=alpha;
  err= spf(A, &pairs, NULL, check_subtr, (void *)(&s));
  free_vectors(&pairs);
  *neg=NO;
  if(err == INTERRUPT) return(err);
  if(err==ABORT) return(NO);
  if(err==NO_ERROR) {
    *neg=s.neg;
    return(YES);
  }
  return(ERROR);
}

static int
check_subtr( NEW_TUPLE *ntp, void *sp)
/* Checks if <a,b> is the image of `new' in every case, when
   `new' is not in alpha. If this fails, returns ABORT.
   Sets neg to YES, if <b,a> is found.
   Returns ERROR on error, NO_ERROR otherwise.
*/
{
  SUBTR_CHECK *s;
  int c, d;
  int err;
  VEC new;
  char *out; /* print arguments if neg is changed to YES */

  if( (((ntp->argsl)->list)[ntp->offset]).args==NULL) {
    return(NO_ERROR); /* skip generators */
  }
  new=((ntp->gen)->list)[ntp->offset];
  s=(SUBTR_CHECK *)sp;
  if( (new[0]==s->b) && (new[1]==s->a) ) {
    s->neg=YES;
    /* Print, which one is complementation */
    print_spres("Negation found.");
    err=build_expression(ntp->gen, ntp->argsl, ntp->offset, &out);
    if(err==NO_ERROR) print_npres("%s", out);
    if(out!=NULL) free((void *)(out));
  }
  c=new[0]; d=new[1];
  if( (s->alpha)[c]==(s->alpha)[d] ) {
    return(NO_ERROR);
  }
  err= is_image(ntp->alg, s->a, s->b, c, d);
  if ( err==NO ) return(ABORT);
  if ( err==YES ) return(NO_ERROR);
  return(err);
}

static int
is_image(ALG *A, int a, int b, int c, int d)
/* Returns YES, if <a,b> is the image of <c,d> under a unary pol.
   NO if it isn't, ERROR on error. */
{
  int err;
  VECTORS pairs;
  PAIR_CHECK s;

  if(a==b) return(YES);
  if( (a==c) && (b==d) ) return(YES);
  pairs.lvecs=2;
  pairs.nvecs=1;
  pairs.list=NULL;
/* Put (c,d) into pairs */
  if ((pairs.list= (VEC *)(calloc(pairs.nvecs,sizeof(VEC))))== NULL)
  {
    print_derr("Not enough memory to check polynomial images.");
    return(ERROR);
  }
  (pairs.list)[0]= (VEC)NULL;  /* to ensure that free_vectors works OK */
  if(((pairs.list)[0]= (VEC)(calloc(pairs.lvecs,sizeof(int))))== NULL) {
    print_derr("Not enough memory to check polynomial images.");
    free_vectors(&pairs);
    return(ERROR);
  }
  ((pairs.list)[0])[0]=c;
  ((pairs.list)[0])[1]=d;
/* now generate with these pairs */
  if(add_consts(&pairs, A->size)!=NO_ERROR) return(ERROR);
/* diagonal added */
  s.a=a; s.b=b;
  err= spf(A, &pairs, NULL, check_pair, (void *)(&s));
  free_vectors(&pairs);
  if(err == INTERRUPT) return(err);
  if(err==ABORT) {
    print_spres("(%d, %d) is an image of (%d, %d).", a, b, c, d);
    return(YES);
  }
    print_spres("(%d, %d) is not an image of (%d, %d).", a, b, c, d);
  return(NO);
}

static int
check_pair( NEW_TUPLE *ntp, void *sp)
/* Checks if new is <a,b>. Returns ABORT if yes, NO_ERROR otherwise.
   GENERATORS ARE NOT CHECKED, do that before calling spf().
*/
{
  PAIR_CHECK *s;
  VEC new;

  if( (((ntp->argsl)->list)[ntp->offset]).args==NULL) {
    return(NO_ERROR); /* skip generators */
  }
  new=((ntp->gen)->list)[ntp->offset];
  s=(PAIR_CHECK *)sp;
  if( (new[0]==s->a) && (new[1]==s->b) ) {
    return(ABORT);
  }
  return(NO_ERROR);
}

int
type_conlat(ALG *A, PARTS *conlat, VECTORS *types)
/*  This routine computes the type of all prime
    quotients, and returns it in types.
    These are vectors of length 3, and the entries are
    (a,b,t), where a-<b, and t is the type of this quotient.
    Returns ERROR on error, NO_ERROR if succesful.
*/
{
  int i, j, a, b;
  int m, u;
  int err;
  int tmp;
  int type;
  VEC tentry;
  int *ord;
  int typeset[6], flag; /* typeset[1-5] = number of these types */
  char typeset_str[12]; /* maximum is "{1,2,3,4,5}" or "empty" */
#ifdef RUN_PROGRESS
  double elapsed_ratio; /* for a progress report */
#ifdef TIME_DIAG
  time_t start_time; /* time when routine started */
  time_t last_time; /* last_time when a diagnostic message was printed */
#endif /* TIME_DIAG */
#endif /* RUN_PROGRESS */

#define BIN_ARR_SIZE (conlat->nparts)

#ifdef RUN_PROGRESS
#ifdef TIME_DIAG
/* record the time */
  start_time=time( NULL );
  last_time=start_time;
#endif /* TIME_DIAG */
#endif /* RUN_PROGRESS */

  print_spopen("Computing labels.");
  typeset[UTYP]=0;
  typeset[ATYP]=0;
  typeset[BTYP]=0;
  typeset[LTYP]=0;
  typeset[STYP]=0;

  types->nvecs=0;
  types->lvecs=3;
  types->list=NULL;
  err=order_congs(conlat, &ord);
  if(err!=NO_ERROR) return(err);
/* We want the covers ordered lexicographically. 
  The simplest way to achieve this is to fill types up now.
*/
  for(a=0; a<conlat->nparts; a++) {
    for(b=0; b<conlat->nparts; b++) {
      tmp=ord[BIN_ARR_INDEX(a,b)];
      if( tmp!=O_S_COVER ) continue;
      /* so a-<b */
/* add (a,b) to types */
      tentry = (VEC)(calloc(3,sizeof(int)));
      if (tentry == NULL) {
        print_derr("Not enough memory to store type.");
        free((void *)ord);
        free_vectors(types);
        return(ERROR);
      }
      tentry[0]=a; tentry[1]=b; tentry[2]=NOTYP; 
      err=add_vec(&tentry, types);
      if(err==ERROR) {
        free((void *)tentry);
        free((void *)ord);
        free_vectors(types);
        return(ERROR);
      }
    }
  }
/* Now types is filled, we start filling in the type values. */
/* the order relation is ready */
  for(i=0; i<types->nvecs; i++) {
    a=((types->list)[i])[0];
    b=((types->list)[i])[1];
    err=find_pers_mi( conlat->nparts, ord, a, b, &m, &u );
    if(err!=NO_ERROR) {
      free((void *)ord);
      free_vectors(types);
      if( err == INTERRUPT ) return(err);
      print_derr("No meet-irred for C%d-<C%d, is this a prime quotient?",
                                                            a, b);
      return(ERROR);
    }
    print_spres("A meet-irreducible covering to C%d-<C%d is C%d-<C%d.",
                                                             a, b, m, u);
/* check if this type has been computed */
    type=NOTYP; /* unnecessary */
    for(j=0; j<types->nvecs; j++) {
      if( ( ((types->list)[j])[0]==m ) && ( ((types->list)[j])[1]==u ) ) {
        type=((types->list)[j])[2];
        break;
      }
    }
/* now j is the index of (m,u) */
    if(j>=types->nvecs) {
      print_derr("Internal error: missing cover.");
      print_nerr("C%d-<C%d is missing.",m, u);
      free((void *)ord);
      free_vectors(types);
      return(ERROR);
    }
    if( type==NOTYP) {
/* not computed, do it */
      type=comp_type_quotient(A, (conlat->list)[m], (conlat->list)[u]);
      if( (type < 0) && (type != NO_ERROR) ) {/* some error message */
        free((void *)ord);
        free_vectors(types);
        return(type);
      }
/* fill in the (m,u)-type */
      ((types->list)[j])[2]=type;
      typeset[type]++;
      print_spres("typ(C%d, C%d)=%d.", m, u, type);
    }
/* and the (a,b) type, if (a,b) != (m,u) */
    if( i!=j) {
      ((types->list)[i])[2]=type;
      typeset[type]++;
      print_spres("typ(C%d, C%d)=%d.", a, b, type);
    }
    if(StopIt == YES) return(INTERRUPT);
#ifdef RUN_PROGRESS
      if (types->nvecs == 0) elapsed_ratio = (double)0;
      else elapsed_ratio= (double)(i+1) / (double)(types->nvecs);
      print_time_progr( 
#ifdef TIME_DIAG
                           start_time, &last_time,
#endif /* TIME_DIAG */
                           elapsed_ratio, "Computing labels.");
#else
      print_rshort();
#endif /* RUN_PROGRESS */
  }
  free((void *)ord);
/* everything is done, prepare typeset string */
  *typeset_str=EOS;
  strcat(typeset_str, "{");
  flag=NO;
  if(typeset[UTYP] > 0) {
    strcat(typeset_str, "1");
    flag=YES;
  }
  if(typeset[ATYP] > 0) {
    if(flag==YES) strcat(typeset_str, ",");
    strcat(typeset_str, "2");
    flag=YES;
  }
  if(typeset[BTYP] > 0) {
    if(flag==YES) strcat(typeset_str, ",");
    strcat(typeset_str, "3");
    flag=YES;
  }
  if(typeset[LTYP] > 0) {
    if(flag==YES) strcat(typeset_str, ",");
    strcat(typeset_str, "4");
    flag=YES;
  }
  if(typeset[STYP] > 0) {
    if(flag==YES) strcat(typeset_str, ",");
    strcat(typeset_str, "5");
    flag=YES;
  }
  strcat(typeset_str, "}");
  if(flag==NO) strcpy(typeset_str, "empty");
  print_spclose("End of computing labels.");
  print_spres("The type set is %s.", typeset_str);
  return(NO_ERROR);

#undef BIN_ARR_SIZE

}

