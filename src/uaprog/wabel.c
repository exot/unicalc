#define EXTERN extern
#include "includes.h"

typedef struct centr_check {
    VEC congruence;
    PARTS *cons;
    int failure_congnum;
    char failure_string[10];
  } CENTR_CHECK;

typedef struct rlr_check {
    VEC congruence;
    VECTORS *rlr_result;
  } RLR_CHECK;

typedef struct kkvm_check {
    int v;
  } KKVM_CHECK;

static int abel_gen( VECTORS *,  VECTORS *,  VECTORS *);
static int check_centr( NEW_TUPLE *, void * );
static int check_wcentr( NEW_TUPLE *, void *  );
static int check_scentr( NEW_TUPLE *, void *  );
static int check_rcentr( NEW_TUPLE *, void *  );
static int check_centr_l( NEW_TUPLE *, void *  );
static int check_wcentr_l( NEW_TUPLE *, void *  );
static int check_scentr_l( NEW_TUPLE *, void *  );
static int check_rcentr_l( NEW_TUPLE *, void *  );
static int is_tuple_abel( VEC , VEC, char * );
static int is_tuple_wabel( VEC , VEC, char * );
static int is_tuple_sabel( VEC , VEC, char * );
static int is_tuple_rabel( VEC , VEC, char * );
static int check_rlr( NEW_TUPLE *, void *  );
static int is_kkvm_germ2(ALG *, VECTORS *, int, int, int, int);
static int check_kkvm(NEW_TUPLE *, void *);

int
is_centrf( ALG *A,
       VECTORS *left, VECTORS *right, VEC cong, PARTS *conlist, int centr)
/*  This routine checks various versions of centrality according to centr.
    If cong!=NULL, then one congruence is checked.
    Otherwise, we run over conlist, and stop at the first failure.
    (see hlp_is_centr()).
    We generate a subalgebra with 
    {(a,b,a,b)|(a,b)\in right}\cup{(a,a,b,b)|(a,b)\in left},
    in all cases, namely the set of all left-right rectangles.
*/
{

  VECTORS abvec;
  int flag, err;
  CENTR_CHECK s;
  char *out;
  char cs[5]; /* C, CW, C*, or CR */
  ARGSLIST argsl;

  abvec.list=NULL;
  argsl.list=NULL;
  flag=ERROR;
  err=abel_gen( &abvec, left, right);
  if(err!=NO_ERROR) return(err);

  if(cong!=NULL) { /* check cong only */
    s.congruence=cong;
    switch(centr) {
      case CENTR_NORMAL :
        flag=spf(A, &abvec, &argsl, check_centr, (void *)(&s));
        break;
      case CENTR_WEAK :
        flag=spf(A, &abvec, &argsl, check_wcentr, (void *)(&s));
        break;
      case CENTR_STRONG :
        flag=spf(A, &abvec, &argsl, check_scentr, (void *)(&s));
        break;
      case CENTR_RECT :
        flag=spf(A, &abvec, &argsl, check_rcentr, (void *)(&s));
        break;
      default:
        print_derr("Internal error: invalid centr value.");
        exit(1);
    }
  } else { /* check conlist */
    s.cons=conlist;
    switch(centr) {
      case CENTR_NORMAL :
        flag=spf(A, &abvec, &argsl, check_centr_l, (void *)(&s));
        break;
      case CENTR_WEAK :
        flag=spf(A, &abvec, &argsl, check_wcentr_l, (void *)(&s));
        break;
      case CENTR_STRONG :
        flag=spf(A, &abvec, &argsl, check_scentr_l, (void *)(&s));
        break;
      case CENTR_RECT :
        flag=spf(A, &abvec, &argsl, check_rcentr_l, (void *)(&s));
        break;
      default:
        print_derr("Internal error: invalid centr value.");
        exit(1);
    }
  }
/* Now we print the result */
  if(flag!= ABORT && flag != NO_ERROR) {
    free_vectors(&abvec);
    free_argslist(&argsl);
    return(flag);
  }
  switch(centr) {
    case CENTR_NORMAL :
      sprintf(cs, "C");
      break;
    case CENTR_WEAK :
      sprintf(cs, "CW");
      break;
    case CENTR_STRONG :
      sprintf(cs, "C*");
      break;
    case CENTR_RECT :
      sprintf(cs, "CR");
      break;
    default:
      print_derr("Internal error: invalid centr value.");
      exit(1);
  }
  if(flag==NO_ERROR) { /* centrality holds */
    if(cong!=NULL) { /* check cong only */
      print_dpres("RESULT: %s(left, right; cong) holds.", cs);
    } else { /* check conlist */
      print_dpres("RESULT: %s(left, right; cong) holds for every cong.", cs);
    }
    free_vectors(&abvec);
    free_argslist(&argsl);
    return(NO_ERROR);
  }
  if(flag==ABORT) { /* centrality does not hold */
    err=build_expression(&abvec, &argsl, (argsl.nargs_st)-1, &out);
    if(err==ERROR) {
      if(out!=NULL) free((void *)(out));
    }
    if(cong!=NULL) { /* check cong only */
      print_dpres("RESULT: %s(left, right; cong) %s-fails. Details: ", 
                                                 cs, s.failure_string);
      print_npres("cong=");
      print_r_cong(A->size, cong);
      if(err==NO_ERROR) print_npres("%s", out);
    } else { /* check conlist */
      print_dpres("RESULT: %s(left, right; C%d) %s-fails. Details: ", 
                                  cs, s.failure_congnum, s.failure_string);
      print_npres("C%d=", s.failure_congnum);
      print_r_cong(A->size, (conlist->list)[s.failure_congnum]);
      if(err==NO_ERROR) print_npres("%s", out);
    }
    free_vectors(&abvec);
    if(out!=NULL) free((void *)(out));
    free_argslist(&argsl);
    return(NO_ERROR);
  }
/* NOTREACHED */
  return(ERROR);
}

static int
abel_gen( VECTORS *abvec, VECTORS *left, VECTORS *right)
/*  This routine generates all the 4-tuples
  {(a,b,a,b)|(a,b)\in right}\cup{(a,a,b,b)|(a,b)\in left},
  (all left-right rectangles) and puts the result in abvec.
  left and right must be binary relations, this is NOT checked.
  Returns ERROR or NO_ERROR.
*/
{
  int a,b,i;
  VEC vec;

  abvec->list=NULL;
  abvec->nvecs=0;
  abvec->lvecs=4;
  for(i=0; i<left->nvecs; i++) {
    vec= (VEC)(calloc(4,sizeof(int)));
    if (vec== NULL) {
      print_derr("Not enough memory to generate left,right matrices.");
      return(ERROR);
    }
    a=((left->list)[i])[0];
    b=((left->list)[i])[1];
    vec[0]=a;
    vec[1]=a;
    vec[2]=b;
    vec[3]=b;
    if(add_vec(&vec, abvec)==ERROR) {
      free((void *)vec);
      free_vectors(abvec);
      return(ERROR);
    }
  }
  for(i=0; i<right->nvecs; i++) {
    vec= (VEC)(calloc(4,sizeof(int)));
    if (vec== NULL) {
      print_derr("Not enough memory to generate left,right matrices.");
      return(ERROR);
    }
    a=((right->list)[i])[0];
    b=((right->list)[i])[1];
    vec[0]=a;
    vec[1]=b;
    vec[2]=a;
    vec[3]=b;
    if(add_vec(&vec, abvec)==ERROR) {
      free((void *)vec);
      free_vectors(abvec);
      return(ERROR);
    }
  }
  return(NO_ERROR);
}

static int
check_centr(NEW_TUPLE *ntp, void *s)
/* size must be 4, we do not check 
   Returns ABORT, if vec fails mod s->congruence.
   Returns NO_ERROR otherwise.
*/
{
  if( (((ntp->argsl)->list)[ntp->offset]).args==NULL) {
    return(NO_ERROR); /* skip generators */
  }
  if(is_tuple_abel(
      ((ntp->gen)->list)[ntp->offset], 
      ((CENTR_CHECK *)s)->congruence,
      ((CENTR_CHECK *)s)->failure_string                      )==NO) {
    return(ABORT);
  }
  return(NO_ERROR);
}

static int
check_wcentr(NEW_TUPLE *ntp, void *s)
/* size must be 4, we do not check 
   Returns ABORT, if vec fails mod s->congruence.
   Returns NO_ERROR otherwise.
*/
{
  if( (((ntp->argsl)->list)[ntp->offset]).args==NULL) {
    return(NO_ERROR); /* skip generators */
  }
  if(is_tuple_wabel(
      ((ntp->gen)->list)[ntp->offset], 
      ((CENTR_CHECK *)s)->congruence,
      ((CENTR_CHECK *)s)->failure_string                      )==NO) {
    return(ABORT);
  }
  return(NO_ERROR);
}

static int
check_scentr(NEW_TUPLE *ntp, void *s)
/* size must be 4, we do not check 
   Returns ABORT, if vec fails mod s->congruence.
   Returns NO_ERROR otherwise.
*/
{
  if( (((ntp->argsl)->list)[ntp->offset]).args==NULL) {
    return(NO_ERROR); /* skip generators */
  }
  if(is_tuple_sabel(
      ((ntp->gen)->list)[ntp->offset], 
      ((CENTR_CHECK *)s)->congruence,
      ((CENTR_CHECK *)s)->failure_string                      )==NO) {
    return(ABORT);
  }
  return(NO_ERROR);
}

static int
check_rcentr(NEW_TUPLE *ntp, void *s)
/* size must be 4, we do not check 
   Returns ABORT, if vec fails mod s->congruence.
   Returns NO_ERROR otherwise.
*/
{
  if( (((ntp->argsl)->list)[ntp->offset]).args==NULL) {
    return(NO_ERROR); /* skip generators */
  }
  if(is_tuple_rabel(
      ((ntp->gen)->list)[ntp->offset], 
      ((CENTR_CHECK *)s)->congruence,
      ((CENTR_CHECK *)s)->failure_string                      )==NO) {
    return(ABORT);
  }
  return(NO_ERROR);
}

static int
check_centr_l(NEW_TUPLE *ntp, void *s)
/* size must be 4, we do not check 
   Returns ABORT, if vec fails mod some element of s->cons.
   Returns NO_ERROR otherwise.
*/
{
  int i;
  VEC cong;
  VEC new;
  char *failure;

  if( (((ntp->argsl)->list)[ntp->offset]).args==NULL) {
    return(NO_ERROR); /* skip generators */
  }
  new=((ntp->gen)->list)[ntp->offset];
  failure=((CENTR_CHECK *)s)->failure_string;
  for(i=0; i< (((CENTR_CHECK *)s)->cons)->nparts; i++) {
    cong=( (((CENTR_CHECK *)s)->cons)->list)[i];
    if(is_tuple_abel(new, cong, failure)==NO) {
      ((CENTR_CHECK *)s)->failure_congnum=i;
      return(ABORT);
    }
  }
  return(NO_ERROR);
}

static int
check_wcentr_l(NEW_TUPLE *ntp, void *s)
/* size must be 4, we do not check 
   Returns ABORT, if vec fails mod some element of s->cons.
   Returns NO_ERROR otherwise.
*/
{
  int i;
  VEC cong;
  VEC new;
  char *failure;

  if( (((ntp->argsl)->list)[ntp->offset]).args==NULL) {
    return(NO_ERROR); /* skip generators */
  }
  new=((ntp->gen)->list)[ntp->offset];
  failure=((CENTR_CHECK *)s)->failure_string;
  for(i=0; i< (((CENTR_CHECK *)s)->cons)->nparts; i++) {
    cong=( (((CENTR_CHECK *)s)->cons)->list)[i];
    if(is_tuple_wabel(new, cong, failure)==NO) {
      ((CENTR_CHECK *)s)->failure_congnum=i;
      return(ABORT);
    }
  }
  return(NO_ERROR);
}

static int
check_scentr_l(NEW_TUPLE *ntp, void *s)
/* size must be 4, we do not check 
   Returns ABORT, if vec fails mod some element of s->cons.
   Returns NO_ERROR otherwise.
*/
{
  int i;
  VEC cong;
  VEC new;
  char *failure;

  if( (((ntp->argsl)->list)[ntp->offset]).args==NULL) {
    return(NO_ERROR); /* skip generators */
  }
  new=((ntp->gen)->list)[ntp->offset];
  failure=((CENTR_CHECK *)s)->failure_string;
  for(i=0; i< (((CENTR_CHECK *)s)->cons)->nparts; i++) {
    cong=( (((CENTR_CHECK *)s)->cons)->list)[i];
    if(is_tuple_sabel(new, cong, failure)==NO) {
      ((CENTR_CHECK *)s)->failure_congnum=i;
      return(ABORT);
    }
  }
  return(NO_ERROR);
}

static int
check_rcentr_l(NEW_TUPLE *ntp, void *s)
/* size must be 4, we do not check 
   Returns ABORT, if vec fails mod some element of s->cons.
   Returns NO_ERROR otherwise.
*/
{
  int i;
  VEC cong;
  VEC new;
  char *failure;

  if( (((ntp->argsl)->list)[ntp->offset]).args==NULL) {
    return(NO_ERROR); /* skip generators */
  }
  new=((ntp->gen)->list)[ntp->offset];
  failure=((CENTR_CHECK *)s)->failure_string;
  for(i=0; i< (((CENTR_CHECK *)s)->cons)->nparts; i++) {
    cong=( (((CENTR_CHECK *)s)->cons)->list)[i];
    if(is_tuple_rabel(new, cong, failure)==NO) {
      ((CENTR_CHECK *)s)->failure_congnum=i;
      return(ABORT);
    }
  }
  return(NO_ERROR);
}

static int
is_tuple_abel(VEC vec, VEC cong, char *failure_s)
/* Returns NO, if vec is one of the forms
   (c,c,d,e) or (d,e,c,c) with d!= e, mod cong.
   Returns YES otherwise.
   ENHANCEMENT. This is the only noncommutative centrality relation.
   It would make the program run considerably faster for the
   case when left=right, if we checked (c,d,c,e) and (d,c,e,c).
*/
{
  int a, b, c, d;

  a=cong[vec[0]];
  b=cong[vec[1]];
  c=cong[vec[2]];
  d=cong[vec[3]];
  if(a==b && c!=d) {
    sprintf(failure_s, "(cc,de)");
    return(NO);
  }
  if(c==d && a!=b) {
    sprintf(failure_s, "(de,cc)");
    return(NO);
  }
  return(YES);
}

static int
is_tuple_wabel( VEC vec, VEC cong, char *failure_s)
/* Returns NO, if vec is one of the forms
   (c,c,c,d) or (c,c,d,c) or (c,d,c,c) or (d,c,c,c)
   with c!= d, mod cong.
   Returns YES otherwise.
*/
{
  int a, b, c, d;

  a=cong[vec[0]];
  b=cong[vec[1]];
  c=cong[vec[2]];
  d=cong[vec[3]];
  if(a==b && b==c && c!=d) {
    sprintf(failure_s, "(cccd)");
    return(NO);
  }
  if(a==b && b==d && c!=d) {
    sprintf(failure_s, "(ccdc)");
    return(NO);
  }
  if(a==c && c==d && a!=b) {
    sprintf(failure_s, "(cdcc)");
    return(NO);
  }
  if(b==c && c==d && a!=b) {
    sprintf(failure_s, "(dccc)");
    return(NO);
  }
  return(YES);
}

static int
is_tuple_sabel( VEC vec, VEC cong, char *failure_s)
/* Returns NO, if either of CR or C fails.
   Returns YES otherwise.
*/
{
  if(is_tuple_abel( vec, cong, failure_s)==NO) return(NO);
  return(is_tuple_rabel( vec, cong, failure_s));
}

static int
is_tuple_rabel( VEC vec, VEC cong, char *failure_s)
/* Returns NO, if vec is one of the forms
   (c,d,e,c) or (d,c,c,e)
   with c!= d or c!= e, mod cong.
   Returns YES otherwise.
*/
{
  int a, b, c, d;

  a=cong[vec[0]];
  b=cong[vec[1]];
  c=cong[vec[2]];
  d=cong[vec[3]];
  if(a==d && a!=b) {
    sprintf(failure_s, "(cd?c)");
    return(NO);
  }
  if(a==d && c!=c) {
    sprintf(failure_s, "(c?dc)");
    return(NO);
  }
  if(b==c && a!=b) {
    sprintf(failure_s, "(dcc?)");
    return(NO);
  }
  if(b==c && c!=d) {
    sprintf(failure_s, "(?ccd)");
    return(NO);
  }
  return(YES);
}

int
build_expression( VECTORS *gen, ARGSLIST *argsl, int result, char **out)
/* We assume ((argsl->list)[i]).result=i.
   The routine parses result, and puts the result to out.
*/
{
  int i, err;
  char buf[NUM_LEN_LIMIT+10]; /* Must be able to accomodate "%d=(" */

  *out=NULL; /* realloc() deals with this */
  sprintf(buf, "V%d=(", result);
  err=append_alloc_str(out, buf);
  if(err!=NO_ERROR) {
    print_derr("Not enough memory to write expression.");
    return(ERROR);
  }
  for(i=0; i<(gen->lvecs)-1; i++) {
    sprintf(buf, "%d,", ((gen->list)[result])[i] );
    err=append_alloc_str(out, buf);
    if(err!=NO_ERROR) {
      print_derr("Not enough memory to write expression.");
      return(ERROR);
    }
  }
  sprintf(buf, "%d)=", ((gen->list)[result])[(gen->lvecs)-1] );
  err=append_alloc_str(out, buf);
  if(err!=NO_ERROR) {
    print_derr("Not enough memory to write expression.");
    return(ERROR);
  }
  err=next_expression(gen, argsl, result, out);
  if(err!=NO_ERROR) {
    print_derr("Not enough memory to write expression.");
    return(ERROR);
  }
  return(NO_ERROR);
}

int
next_expression( VECTORS *gen, ARGSLIST *argsl, int result, char **out)
/* We assume ((argsl->list)[i]).result=i,
   that the length of gen and argsl are equal, and the entries
   correspond to each other.
   This routine recursively calls itself until the arguments
   are resolved. Reallocates out, and writes there the result.
   Returns ERROR on error, NO_ERROR otherwise.
*/
{
  int i, err;
  ARGUMENTS *ap;
  char buf[NUM_LEN_LIMIT+10];

  ap= &((argsl->list)[result]);
  if (ap->args==NULL) { /* This is a generator */
    /* put it into out */
    sprintf(buf, "G(");
    err=append_alloc_str(out, buf);
    if(err!=NO_ERROR) {
      print_derr("Not enough memory to write expression.");
      return(ERROR);
    }
    for(i=0; i<(gen->lvecs)-1; i++) {
      sprintf(buf, "%d,", ((gen->list)[result])[i] );
      err=append_alloc_str(out, buf);
      if(err!=NO_ERROR) {
        print_derr("Not enough memory to write expression.");
        return(ERROR);
      }
    }
    sprintf(buf, "%d)", ((gen->list)[result])[(gen->lvecs)-1] );
    err=append_alloc_str(out, buf);
    if(err!=NO_ERROR) {
      print_derr("Not enough memory to write expression.");
      return(ERROR);
    }
    return(NO_ERROR);
  }
  /* so this is not a generator */
  sprintf(buf, "f%d(", ap->opno );
  err=append_alloc_str(out, buf);
  if(err!=NO_ERROR) {
    print_derr("Not enough memory to write expression.");
    return(ERROR);
  }
  for(i=0; i< (ap->arity)-1; i++) {
    err=next_expression(gen, argsl, (ap->args)[i], out);
    if(err!=NO_ERROR) {
      print_derr("Not enough memory to write expression.");
      return(ERROR);
    }
    sprintf(buf, ",");
    err=append_alloc_str(out, buf);
    if(err!=NO_ERROR) {
      print_derr("Not enough memory to write expression.");
      return(ERROR);
    }
  }
  err=next_expression(gen, argsl, (ap->args)[(ap->arity)-1], out);
  if(err!=NO_ERROR) {
    print_derr("Not enough memory to write expression.");
    return(ERROR);
  }
  sprintf(buf, ")");
  err=append_alloc_str(out, buf);
  if(err!=NO_ERROR) {
    print_derr("Not enough memory to write expression.");
    return(ERROR);
  }
  return(NO_ERROR);
}

/***********************************************************
   Building R(L,R)
************************************************************/

int
wr_rlr( ALG *A, 
       VECTORS *left, VECTORS *right, VEC cong, VECTORS *rlr)
/*  This routine computes the relation R(left, right, cong)
    defined as follows. Consider all left-right matrices 
    (a,b,c,d) whose main diagonal ad is in `cong',
    and put the top row ab into the list rlr (see hlp_wr_rlr()).
    For each pair ab found, we write the corresponding
    expression to the log file.
*/
{

  VECTORS abvec;
  RLR_CHECK s;
  int err;

  print_spopen("Begin computing R(left, right).");
  abvec.list=NULL;
  err=abel_gen( &abvec, left, right);
  if(err!=NO_ERROR) return(err);
  rlr->nvecs=0;
  rlr->lvecs=2;
  s.congruence=cong;
  s.rlr_result=rlr;
  err=spf(A, &abvec, NULL, check_rlr, (void *)(&s));
  print_spclose("Finished computing R(left, right).");
  if(err == NO_ERROR ) {
    print_dpres("R(left, right) mod cong contains %d pair(s).",
                                                          rlr->nvecs);
  }
  free_vectors(&abvec);
  return(err);
}

static int
check_rlr(NEW_TUPLE *ntp, void *s)
/* size must be 4, we do not check 
   If this is a new instance of R(L,R;cong), then
   appends it to res, and writes to the log file.
   Always returns NO_ERROR or ERROR, never ABORT.
*/
{
  VEC vec;
  VEC cong;
  VECTORS *res;
  VEC newvec;
  int err;
  char *out;

  vec=((ntp->gen)->list)[ntp->offset];
  cong=((RLR_CHECK *)s)->congruence;
  if(cong[vec[0]] != cong[vec[3]]) return(NO_ERROR);
/* add new pair ab to R(L,R) */
  res=((RLR_CHECK *)s)->rlr_result;
  newvec= (VEC)(calloc(2,sizeof(int)));
  if (newvec== NULL) {
    print_derr("Not enough memory to store R(left, right).");
    return(ERROR);
  }
  newvec[0]=vec[0];
  newvec[1]=vec[1];
  err=add_vec(&newvec, res);
  if( err==ERROR) {
    print_derr("Not enough memory to store R(left, right).");
    free((void *)newvec);
    free_vectors(res);
    return(ERROR);
  }
  if(err != NOT_FOUND) return(NO_ERROR);
/* this is a new pair in R(L,R). Write to log file. */

  err=build_expression(ntp->gen, ntp->argsl,
		       ((ntp->argsl)->nargs_st)-1, &out);
  if(err==ERROR) {
    if(out!=NULL) free((void *)(out));
  }
  print_dpresh("(%d,%d) in R(left, right) mod cong.",
	      vec[0], vec[1]);
  if(err==NO_ERROR) print_npresh("Details: %s", out);
  if(out!=NULL) free((void *)(out));
  return(NO_ERROR);
}

/*****************************************************
  KKVM germ.
******************************************************/

int
find_kkvm_germ(ALG *A, VECTORS *t, VECTORS *rtt)
/* We scan all possible kkvm-germs
   coming from t. The answer is written to the terminal.
*/
{
  int i, j, err, num;
  int u1, u2, v1, v2;
  
  print_spopen("Scanning all kkvm-germ candidates.");
  num=0;
  for(i=0; i<rtt->nvecs; i++) {
    for(j=0; j<rtt->nvecs; j++) {
      v1=(rtt->list[i])[0];
      v2=(rtt->list[j])[0];
      if(v1 == v2) continue;
      u1=(rtt->list[i])[1];
      u2=(rtt->list[j])[1];
      err=is_kkvm_germ2(A, t, u1, v1, u2, v2);
      if( err==INTERRUPT) return(err);
      if(err==ERROR) {
	free_vectors(rtt);
	return(ERROR);
      }
      if(err==YES) {
	num++;
      }
    }
  }
  print_spclose("End of scanning germs.");
  print_dpres("The number of kkvm-germs found is %d.", num);
  free_vectors(rtt);
  return(NO_ERROR);
}


static int
is_kkvm_germ2(ALG *A, VECTORS *t,
	     int u1, int v1, int u2, int v2)
/* It is assumed that (v1,u1) and (v2,u2) are in R(t,t)
   modulo 0_A, and also that v1\ne v2.
   The program checks condition (2) for a kkvm germ.
   That is, it generates a subalgebra B in A^4 by
   - the diagonal (in case t is not reflexive);
   - by all tuples abab with a t b;
   - the tuple (u1,v1,u2,v2).
   If B satisfies that vvxy\in B => x=y=v
   and also that xyvv\in B => x=y=v,
   then this is a kkvm-germ, and YES is returned.
   Otherwise, NO is returned, unless there is an ERROR.
*/
{
  VECTORS B;
  int err;
  int a,b,i;
  KKVM_CHECK s;
  VEC vec;


  print_spopen("Checking a germ.");
  B.list=NULL;
  B.nvecs=0;
  B.lvecs=4;
  for(i=0; i<A->size; i++) {
    vec= (VEC)(calloc(4,sizeof(int)));
    if (vec== NULL) {
      print_derr("Not enough memory to generate germ-congruence.");
      return(ERROR);
    }
    vec[0]=i;
    vec[1]=i;
    vec[2]=i;
    vec[3]=i;
    if(add_vec(&vec, &B)==ERROR) {
      free((void *)vec);
      free_vectors(&B);
      return(ERROR);
    }
  }
  for(i=0; i<t->nvecs; i++) {
    vec= (VEC)(calloc(4,sizeof(int)));
    if (vec== NULL) {
      print_derr("Not enough memory to generate germ-congruence.");
      return(ERROR);
    }
    a=((t->list)[i])[0];
    b=((t->list)[i])[1];
    vec[0]=a;
    vec[1]=b;
    vec[2]=a;
    vec[3]=b;
    if(add_vec(&vec, &B)==ERROR) {
      free((void *)vec);
      free_vectors(&B);
      print_derr("Not enough memory to generate germ-congruence.");
      return(ERROR);
    }
  }
  vec= (VEC)(calloc(4,sizeof(int)));
  if (vec== NULL) {
    print_derr("Not enough memory to generate germ-congruence.");
    return(ERROR);
  }
  vec[0]=u1;
  vec[1]=v1;
  vec[2]=u2;
  vec[3]=v2;
  if(add_vec(&vec, &B)==ERROR) {
    free((void *)vec);
    free_vectors(&B);
    print_derr("Not enough memory to generate germ-congruence.");
    return(ERROR);
  }
/* all generators for B have been added */
  s.v=v1;
  err=spf(A, &B, NULL, check_kkvm, (void *)(&s));
  free_vectors(&B);
  if(err == INTERRUPT) return(err);
  if(err == ABORT ) {
    print_spclose("End of checking this germ.");
    print_spres("u1-v1, u2-v2 = %d-%d, %d-%d is NOT a kkvm-germ.",
		 u1, v1, u2, v2);
    return(NO);
  }
  if(err == NO_ERROR ) {
    print_spclose("End of checking this germ.");
    print_dpresh("u1-v1, u2-v2 = %d-%d, %d-%d is a kkvm-germ.",
		 u1, v1, u2, v2);
    return(YES);
  }
  return(ERROR);
}

static int
check_kkvm(NEW_TUPLE *ntp, void *s)
/* size must be 4, we do not check 
   If new tuple is (vvxy) or (xyvv) with xvy NOT all equal,
   then we ABORT. Otherwise return NO_ERROR.
*/
{
  VEC vec;
  int v;

  vec=((ntp->gen)->list)[ntp->offset];
  v=((KKVM_CHECK *)s)->v;
  if(vec[0]== v && vec[1]==v) {
    if(vec[2]!=v || vec[3]!=v) return(ABORT);
  }
  if(vec[2]== v && vec[3]==v) {
    if(vec[0]!=v || vec[1]!=v) return(ABORT);
  }
  return(NO_ERROR);
}

