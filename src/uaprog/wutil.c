#define EXTERN extern
#include "includes.h"

static void vprint_dev( int, char *, va_list );
static void print_dev( int , char *,...);
static void print_indent( int, int );
static void vprint_proc( char *, char *, va_list );
static void vprint_pres( char *, char *, va_list );
#if 0
static void print_proc( char *,... );
#endif
static void print_pres( char *,... );
static int set_proc_device( void );
static int set_pres_device( void );

/* output bitfields: */

#define NO_DEV           0 /* no output */
#define LOG_DEV          1 /* log file */
#define PROC_PROGR_DEV   2 /* standard output */
#define ERR_DEV          4 /* error output */
#define RUN_PROGR_DEV    8 /* runtime progress output */
#define RESULT_DEV      16 /* result output */


int
intpow(int x, int y)
/* Assumes that x, y are POSITIVE integers. Returns x^y, or if this is
   bigger than POWER_LIMIT, it returns -1. */
{
  long power, dx;
  int i;

  dx = (long)x;
  power = (long)1;
  for (i= 1;i<= y;i++) {
    power *= dx;
    if (power <= (long)0 || power > (long)POWER_LIMIT ) {
      return(-1);
    }
  }
  return((int)power);
}

int
is_eof( FILE *fp)
/* returns YES or NO. Assumes that fp is open for reading
   This is different form feof, because feof does not return
   YES until an unsuccesful read has been performed.
   The ungetc() may be dangerous, though.
 */
{
  int ch;

  ch=fgetc(fp);
  if(ch==EOF) return(YES);
  ungetc(ch, fp);
  return(NO);
}

int
get_next_line(char *buffer, int limit, FILE *fp)
/* reads next line from fp (up to \n).
   At most limit-1 characters read. Appends \0 at the end
   TO REPLACE \n.
   Lines starting with % are ignored.
   linenum is incremented with each line read.
   Returns ERROR on error, LONG_LINE, if no \n read,
   NOT_FOUND, if empty line read at EOF, NO_ERROR if OK,
   Prints error message, and a warning, if no \n
   found at EOS.
*/
{
  for(;;) {
    linenum++;
    if(fgets( buffer, limit, fp ) == NULL) {
      if( feof(fp) ) {
        print_derr("Unexpected EOF after line %d of the parameter file.",
                                                                linenum-1);
        return(NOT_FOUND);
      }
      print_derr("Error reading line %d of the parameter file.", linenum);
      return(ERROR);
    }
    if(*buffer=='%') continue;
    if( cut_tr_n(buffer)==NO ) {/* no trailing newline */
      if( feof(fp) ) {
        print_serr("Line %d in the parameter file is not terminated\
 by <RETURN>.", linenum);
        return(NO_ERROR);
      }
      print_derr("Line %d too long in parameter file.", linenum);
      return(LONG_LINE);
    }
    return(NO_ERROR);
  }
}

int
cut_tr_n(char *str)
/* cut trailing \n from the end of the string `str' 
   Returns YES, if there was a trailing \n, NO otherwise. */
{
  char c;

  while((c= *str)!= EOS) {
    str++;
    if(c== '\n' && *str== EOS) {
      str--;
      *str= EOS;
      return(YES);
    }
  }
  return(NO);
}

int
comp_command_name(char *str, char *name)
/* Finds the first occurrence of { in str.
   If not found, or there is a newline before, returns ERROR.
   Otherwise it compares the string up and without this { to 'name'.
   Returns EQUAL, if they are the same, NOT_EQUAL if not.
   Does not change str.
 */
{
  char *st;
  int flag;

  st=str;
  while( *st != EOS) {
    if(*st=='\n') return(ERROR);
    if(*st== '{') {
      *st=EOS;
      flag=strcmp(name, str);
      *st= '{';
      if(flag==0) return(EQUAL);
      else return(NOT_EQUAL);
    }
    st++;
  }
  return(ERROR);
}

char *
get_command_arg(char *str, char *out)
/* It finds the first {, and its matching } in str, while
   skipping matching {} pairs inside. If not found, or if \n is found first,
   returns NULL. Otherwise it copies the inside of this { }
   to out, and returns a pointer at the first character after this }.
   Does not change str. On error, *out is always NULL.
 */
{
  int count;
  char *st;

  while( *str != EOS) {
    if(*str=='\n') return(NULL);
    if(*str== '{') break;
    str++;
  }
  if(*str!='{') return(NULL);
  count=0;
  str++;
  st=str;
  while( *st != EOS) {
    if(*st=='\n') return(NULL);
    if(*st== '{') count++;
    if(*st== '}') {
      if(count!=0) count--;
      else {
        *st=EOS;
        strcpy(out, str);
        *st= '}';
        st++;
        return(st);
      }
    }
    st++;
  }
  return(NULL);
}

int
get_integer(FILE *fp, char *delim)
/* Reads the next non-negative integer from fp.
   That is, it reads characters as long as they are digits,
   but less than NUM_LEN_LIMIT characters. Skips starting whitespace.
   If the digits are followed by the a character in delim,
   the integer corresponding to these digits is returned.
   Otherwise, or on error, BAD_NUMBER is returned.
   EOF cannot be in delim.
   The file pointer points to the first character after the delimiter,
   if there was no error.
 */
{
  char st[NUM_LEN_LIMIT+1];
  int ch, count, num;
  int sr;

  for(;;) {
    ch=fgetc(fp);
    if(ch == ' ') continue;
    if(ch == '\n') continue;
    if(ch == '\r') continue;
    if(ch == '\t') continue;
    if(ch==EOF) return(BAD_NUMBER);
    if(isdigit(ch)) break;
    return(BAD_NUMBER);
  }
  for(count=0; count<NUM_LEN_LIMIT-1; count++) {
    st[count]=(char)ch;
    ch=fgetc(fp);
    if(ch==EOF) return(BAD_NUMBER);
    if(!isdigit(ch)) break;
  }
  if(count>=NUM_LEN_LIMIT-1) return(BAD_NUMBER);
                                         /* NUM_LEN_LIMIT digits read */
  if( strchr(delim, ch) == NULL ) return(BAD_NUMBER);
/* OK, parse number */
  st[count+1]=EOS;
  sr=sscanf(st, "%d", &num);
  if(sr<=0 || sr== EOF) {
    return(BAD_NUMBER);
  }
  return(num);
}

int
append_alloc_str( char **out, char *buf)
/* reallocates out, and appends copies buf. Returns ERROR on error 
   Nothing is freed on ERROR, but *out is not changed then.
*/
{
  char *new;
  int n;

  n=strlen(buf)+1;
  if(*out!=NULL) n += strlen(*out);
  new= (char *)(realloc(*out, (n)*sizeof(char)));
  if (new== NULL) return(ERROR);
  if(*out==NULL) *new=EOS;
  strcat(new, buf);
  *out=new;
  return(NO_ERROR);
}

int
print_part( int size, VEC part, char **string )
/* size is the size of the set
** The partition is displayed by listing its blocks separated by | .
** The numbers inside a block are separated by -.
** The result is returned in *string.
** Returns the number of blocks, or ERROR.
** The algoritm works with Matt's format, so we convert first.
** wrcong is always freed, string never.
*/
{
  int i, j, err;
  int flag, count;
  char buf[NUM_LEN_LIMIT+10];/* Must be able to accomodate " %d" */
  VEC wrcong;

  if ((wrcong= (VEC)(calloc(size,sizeof(int))))== NULL)
  {
    print_derr("Not enough memory to reformat congruence.");
    return(ERROR);
  }
  for(i=0; i< size; i++) {
    wrcong[i]=part[i];
  }
  arb2matt_con(size, wrcong);
  *string=NULL; /* realloc deals with this, see append_alloc_string() */
  err=append_alloc_str(string, "|");
  if(err!=NO_ERROR) {
    print_derr("Not enough memory to print congruence.");
    free((void *)wrcong);
    return(ERROR);
  }
  count=0;
  for(i=0; i<size; i++) {
    flag=YES;
    for(j=i; j<size; j++) {
      if(wrcong[j]==i) {
        if(flag==NO) { /* not a first element in a block */
          err=append_alloc_str(string, "-");
          if(err!=NO_ERROR) {
            print_derr("Not enough memory to print congruence.");
            free((void *)wrcong);
            return(ERROR);
          }
        }
        flag=NO;
        sprintf(buf, "%d", j);
        err=append_alloc_str(string, buf);
        if(err!=NO_ERROR) {
          print_derr("Not enough memory to print congruence.");
          free((void *)wrcong);
          return(ERROR);
        }
      }
    }
    if(flag==NO) { /* non-empty block */
      count++;
      err=append_alloc_str(string, "|");
      if(err!=NO_ERROR) {
        print_derr("Not enough memory to print congruence.");
        free((void *)wrcong);
        return(ERROR);
      }
    }
    else break;
  }
  free((void *)wrcong);
  return(count);
}

int
hash(VEC array, int len)  /* compute hashing number of array[len] */
{                    /* len must be < 2^{15} */
  int i, hashn;

  hashn = 0;
  i= 0;
  for(;;)          /* mult[i] written explicitely for max speed */
  {
    if(i== len) break;
    hashn += MULT0*array[i]; i++;
    if(i== len) break;
    hashn += MULT1*array[i]; i++;
    if(i== len) break;
    hashn += MULT2*array[i]; i++;
    if(i== len) break;
    hashn += MULT3*array[i]; i++;
    if(i== len) break;
    hashn += MULT4*array[i]; i++;
    if(i== len) break;
    hashn += MULT5*array[i]; i++;
    if(i== len) break;
    hashn += MULT6*array[i]; i++;
    if(i== len) break;
    hashn += MULT7*array[i]; i++;
    if(i== len) break;
    hashn += MULT8*array[i]; i++;
    if(i== len) break;
    hashn += MULT9*array[i]; i++;
    if(i== len) break;
    hashn += MULT10*array[i]; i++;
    if(i== len) break;
    hashn += MULT11*array[i]; i++;
    if(i== len) break;
    hashn += MULT12*array[i]; i++;
    if(i== len) break;
    hashn += MULT13*array[i]; i++;
    if(i== len) break;
    hashn += MULT14*array[i]; i++;
    if(i== len) break;
    hashn += MULT15*array[i]; i++;
  }
  hashn = (int)((unsigned int)hashn % HASH_SIZE);
  return(hashn);
}

int
find_vec(VEC vc, VECTORS *vecs)
/* Returns the index of the first occurrence of vc in vecs
   if it is in there, NOT_FOUND otherwise.
   It is assumed that the length of v is vecs->lvecs.
   Optimized for speed. */
{
  int len;
  VEC *vp, *ep;
  VEC i, j, ej;

  len= vecs->lvecs;
  vp= vecs->list;
  ep= vp+(vecs->nvecs);
  ej= vc+len;
  for (;vp<ep; vp++) {
    for (i= *vp,j= vc; j<ej; i++,j++)  /* is this a different tuple? */
    {
      if (*i != *j)  goto WLnextvec;  /* They are different */
    }
    return(vp-(vecs->list));    /* They are equal */
WLnextvec:
    continue;
  }
  return(NOT_FOUND);
}

int
add_vec(VEC *vc, VECTORS *vecs)
/* Adds *vc to vecs if it is not there. 
   If it is in there, then *vc is set to point to the
   new location, AND THE ORIGINAL *vc IS FREED.

   Returns the index of vc if it is in there, NOT_FOUND
   if it is not in there, ERROR on error.
   Reallocates memory. Does not copy *vc.
   It is assumed that the length of v is vecs->lvecs. 

   IMPORTANT: what is freed and what is not.
   A typical usage of this function is to allocate
   storage for *vc, then calling add_vec() to put
   vc into vecs. If it is already there, then
   *vc is not needed, and therefore it has to be freed.
   Therefore we do it here.

   The routine does not free vecs or *vc on ERROR.
   HOWEVER, AN ERROR RETURN ENSURES THAT vc IS NOT ADDED,
   AND NOT CHANGED.
   This is important, because we would not want to free
   *vc twice. So on error return, the caller should free both
   vecs and *vc.
*/
{
  int err;

  err=find_vec( *vc, vecs );
  if(err != NOT_FOUND ) { /* so vc is in vecs */
    free((void *)(*vc)); /* IMPORTANT! */
    *vc=(vecs->list)[err];
    return(err);
  }
  err=concat_vec(*vc, vecs);
  if( err!=NO_ERROR) {
    return(ERROR);
  }
  return(NOT_FOUND);
}

int
concat_vec(VEC vc, VECTORS *vecs)
/* Concatenates vc to vecs.
   Returns ERROR on error, NO_ERROR if succesful.
   Reallocates memory. Does not copy vc.
   Usually, the length of v is vecs->lvecs. 

   The routine does not free vecs or vc on ERROR.
   HOWEVER, AN ERROR RETURN ENSURES THAT vc IS NOT ADDED,
   AND NOT CHANGED.
   This is important, because we would not want to free
   vc twice. So on error return, the caller should free both
   vecs and vc.
*/
{
  VEC *new;

  if( (vecs->nvecs)+1 >= VEC_NUM_LIMIT) {
    print_derr("Illegal number %d of vectors (should be in [%d, %d]).",
                                     (vecs->nvecs)+1, 0, VEC_NUM_LIMIT-1);
    return(ERROR);
  }
  new= (VEC *)(realloc(vecs->list,((vecs->nvecs)+1)*sizeof(VEC)));
  if (new== (VEC *)NULL) {
    print_derr("Not enough continuous memory to add vector.");
    return(ERROR);
  }
  vecs->list= new;
  (vecs->list)[vecs->nvecs]= vc;
  (vecs->nvecs)++;
  return(NO_ERROR);
}

int
concat_arg_st(ARGUMENTS *arg_st, ARGSLIST *argl)
/* Concatenates arg_st to argl.
   Returns ERROR on error, NO_ERROR if succesful.
   This is different from concat_vec, because
   everything is allocated and copied.
   Reallocates memory. 

   On ERROR, argl is unchanged, and everything that
   has been allocated is freed.
*/
{
  int i;
  ARGUMENTS *ap;
  VEC args;
  ARGUMENTS *new;

  if( (argl->nargs_st)+1 >= VEC_NUM_LIMIT) {
    print_derr("Illegal number %d of structures (should be in [%d, %d]).",
                                 (argl->nargs_st)+1, 0, VEC_NUM_LIMIT-1);
    return(ERROR);
  }
/* we must allocate args first, because we do not want to change
   argl->list, if there is no room to copy the arguments  */

  args=NULL;
  if(arg_st->args!=NULL) {                /* there are arguments */
    args= (VEC)(calloc(arg_st->arity,sizeof(int)));
    if (args== NULL) {                    /* arity can be zero here! */
      print_derr("Not enough memory to store arguments.");
      return(ERROR);
    }
  }
  new= (ARGUMENTS *)(realloc(argl->list,
                           ((argl->nargs_st)+1)*sizeof(ARGUMENTS)));
  if (new == NULL) {
    print_derr("Not enough memory to add argument structure.");
    if(args!=NULL) free((void *)args);
    return(ERROR);
  }
  argl->list= new;
  (argl->nargs_st)++;
  ap= &((argl->list)[(argl->nargs_st)-1]);
  ap->result=arg_st->result;
  if(arg_st->args==NULL) { /* no arguments, arity, opno */
    ap->args=NULL;
    return(NO_ERROR);
  }
  ap->args=args;
/* copy things */
  ap->opno=arg_st->opno;
  ap->arity=arg_st->arity;
  for( i=0; i<arg_st->arity; i++) {
    args[i]=(arg_st->args)[i];
  }
  return(NO_ERROR);
}

int
find_con(VEC vc, PARTS *vecs)
/* same as find_vec, but for congruences.
*/
{
  return( find_vec(vc, (VECTORS *)vecs) );
}

int
add_con(VEC *vc, PARTS *vecs)
/* same as add_vec, but for congruences.
*/
{
  return( add_vec(vc, (VECTORS *)vecs) );
}

int
cr_vectors(VECTORS *v, int *maxval )
/* This routine puts into v all vectors of length v->lvecs
   having values at most maxval[i] at the i-th coordinate.
   Returns ERROR if memory error.
*/
{
  int i,j;
  int flag, tmp;

  v->list=NULL;
  v->nvecs=1;
  for(i=0; i<v->lvecs; i++) {
    v->nvecs *= maxval[i];
    if(v->nvecs >= VEC_NUM_LIMIT || v->nvecs <=0) {
      print_derr("Illegal number %d of vectors (should be in [%d, %d]).",
                                     v->nvecs, 0, VEC_NUM_LIMIT-1);
      return(ERROR);
    }
  }
  if ((v->list= (VEC *)(calloc(v->nvecs,sizeof(VEC))))== NULL) {
    print_derr("Not enough memory to point to vectors.");
    return(ERROR);
  }
  for(i= 0;i<v->nvecs;i++) {
    (v->list)[i]= (VEC)NULL;  /* to ensure that free_vectors works OK */
  }
  for(i= 0;i<v->nvecs;i++) {
    if(((v->list)[i]= (VEC)(calloc(v->lvecs,sizeof(int))))== NULL) {
      print_derr("Not enough memory to store vectors.");
      free_vectors(v);
      return(ERROR);
    }
  }

  for(j= 0;j<v->lvecs;j++) {
    ((v->list)[0])[j]= 0;
  }
  for(i= 1;i<v->nvecs;i++) {
    flag=0;
    for(j= (v->lvecs)-1;j>=0;j--) {
      tmp=((v->list)[i-1])[j];
      if(flag==1) {            /* already incremented */
        ((v->list)[i])[j]= tmp;
      } else {
        if(tmp==maxval[j]-1) {
          ((v->list)[i])[j]= 0;
        } else {
          ((v->list)[i])[j]= tmp+1;
          flag=1;          /* copy the rest */
        }
      }
    }
  }
  return(NO_ERROR);
}

int
add_consts(VECTORS *v, int maxval )
/* This routine adds to v all constant vectors of length v->lvecs
   having values at most maxval.
   Returns ERROR if memory error.
   WARNING: If ERROR, v might have already been expanded with
   some constant vectors!
*/
{
  int i, j;
  VEC vec;

  for(i=0; i<maxval; i++) {
    vec= (VEC)(calloc(v->lvecs,sizeof(int)));
    if(vec== NULL) {
      print_derr("Not enough memory to store constant vector.");
      return(ERROR);
    }
    for(j=0; j<v->lvecs; j++ ) {
      vec[j]= i;
    }
    if(add_vec(&vec, v)==ERROR) {
      free((void *)vec);
      return(ERROR);
    }
  }
  return(NO_ERROR);
}

int
check_vectors( VECTORS *v, int *limits)
/* The vector entries are checked, namely whether
   the i-th component is between 0 and limits[i]-1.
   Returns ERROR on error, NO_ERROR otherwise */
{
  int i,j, val;

  for(i= 0;i<v->nvecs;i++) {
    for(j= 0;j<v->lvecs;j++) {
      val=((v->list)[i])[j];
      if(val < 0 || val>= limits[j]) {
        print_derr("Illegal entry %d of a vector (should be in [%d, %d]).",
                                              val, 0, limits[j]-1);

        return(ERROR);
      }
    }
  }
  return(NO_ERROR);
}

void
maxc_vectors( VECTORS *v, int **limits)
/* The maximum value m[j] is found in every component
   and m[j]+1 is put into *limits.
*/
{
  int i,j, val;

  for(i=0; i<v->lvecs; i++) {
    (*limits)[i]=0;
  }
  for(i=0;i<v->nvecs;i++) {
    for(j=0;j<v->lvecs;j++) {
      val=((v->list)[i])[j];
      if( val >= (*limits)[j] ) (*limits)[j]=val+1;
    }
  }
}

void
free_alg(ALG *A)
/*  This function frees up memory occupied by an algebra */
{
  FUNCT *k, *l;

  if(A->func== (FUNCT *)NULL) return;
  k= A->func;
  while(k!= (FUNCT *)NULL)
  {
    if(k->values != NULL) free((void *)(k->values));
    l= k->next;
    free((void *)k);
    k= l;
  }
  A->func=(FUNCT *)NULL;
}

void
free_vectors(VECTORS *v)
/* This function frees up memory occupied by the entries in v */
{
  int i;

  if(v->list== (VEC *)NULL) return;
  for(i= 0; i<v->nvecs; i++)
  {
    if( (v->list)[i] != (VEC)NULL ) free((void *)((v->list)[i]));
  }
  if( (v->list) != NULL ) free((void *)(v->list));
  v->list=(VEC *)NULL;
}

void
free_conlist(PARTS *v)
{
  free_vectors((VECTORS *)v);
}

void
free_argslist(ARGSLIST *v)
/* This function frees up memory occupied by the entries in v */
{
  int i;
  ARGUMENTS *arg_stp;

  if(v->list== NULL) return;
  for(i= 0; i<v->nargs_st; i++) {
    arg_stp = &((v->list)[i]);
    if( (arg_stp->args) != NULL )  free((void *)(arg_stp->args));
  }
  if( (v->list) != NULL ) free((void *)(v->list));
  v->list=NULL;
}

int
get_max_arity( ALG *A)
/* get maximum arity in A. Returns ERROR, or max arity. */
{
  int max_arity;
  FUNCT *op;

  max_arity= 0;
  op= A->func;
  while(op != NULL)
  {
    max_arity = ( ( max_arity > op->arity ) ? max_arity : op->arity ) ;
    op= op->next;            /* computing maximum */
  }
  if( max_arity >= ARITY_LIMIT )
  {
    print_derr("Arity too big, impossible!");
    return(ERROR);
  }
  return(max_arity);
}

int
get_func_value(int size, int arity, int *values, int *args)
/* returns values(args). Uses the Horner scheme.
   WARNING: there are places in the code, where a function values
   is set directly, not by using this function. Mainly, when
   working with vectors, and not with elements.
*/
{
  int ar, offset;

  offset=0;
  for(ar=arity-1; ar>=0; ar--) {
    offset = size*offset+args[ar];
  }
  return(values[offset]);
}

void
set_func_value(int size, int arity, int *values, int *args, int set)
/* sets values(args)=set. Uses the Horner scheme.
   WARNING: there are places in the code, where a function values
   is got directly, not by using this function.
*/
{
  int ar, offset;

  offset=0;
  for(ar=arity-1; ar>=0; ar--) {
    offset = size*offset+args[ar];
  }
  values[offset]=set;
}

int
find_range( int size, VEC func, VEC set)
/* puts the range of func into set, in increasing order.
   Returns the size of the range of func.
*/
{
  int i, j;
  int min, max;

  min=func[0];
  max=func[0];
  for(j=0; j<size; j++) {
    if( func[j] < min ) min=func[j];
    if( func[j] > max ) max=func[j];
  }
  set[0]=min;
/* now set[0] is correctly set to the minimum value of func. */
  for(i=1; ; i++) {
    min=max+1;
    for(j=0; j<size; j++) {
      if( (func[j] > set[i-1]) && (func[j] < min) ) min=func[j];
    }
    if(min==max+1) {
      return(i); /* no new range element found */
    }
    set[i]=min;
  }
}

/* Routines to print messages. See descriptions in WUTIL.H */

/* Low level */

static void
vprint_dev( int dev, char *fmt, va_list marker)
/* There is a problem here. The program prints the \n character
   at the beginning of the lines, and not at the end.
   The reason is this. Obviously, we do not want explicit \n
   characters in the program. Every print_ function starts
   with a newline, except possibly the functions print_r_ (printing
   composite objects). This way we can have only a few
   object printing functions, and still have flexibility
   at the beginning of the lines containing them.

   However, a line is not printed to the screen until a \n is read.
   Suppose for example that message X goes to stdout, Y to stdin,
   Z to stdin, U to stdout. Then the order on the screen will be
   YX..., and not XY..., because the starting \n of Z forces
   Y to be written before the starting \n of U forces X to be written.

   This is a problem only if both stderr and stdout go
   to the same file. In practice, this is the screen
   (and this is the only thing we can check). This explains
   the organization of the routine below:
   if stdout and stderr are both character devices, then
   we print only to stdout.

   This is a problem, for example, if the user directs output
   to a printer (or a serial port), because then the progress
   messages will go to instead of the screen. An obvious remedy is
   to direct output to a file, and then send it or print it.
   This is certainly less burdensome than reading everything
   twice on the screen.
 */
{
    char guibuf[1024];
/* Should be enough, we only write one line at a time */

    int sr;

    if( ((dev & LOG_DEV) || (dev & ERR_DEV)) && (log_file_p != 0) ) {
	sr = vfprintf( log_file_p, fmt, marker );
	if(sr<0 || sr== EOF)
	{
	    print_derr("Error writing log file.");
	}
    }
    if( (dev & RESULT_DEV) && (log_file_p != 0)) {
	sr = vfprintf( results_file_p, fmt, marker );
	if(sr<0 || sr== EOF)
	{
	    print_derr("Error writing result file.");
	}
    }
    if( GUI_mode == YES ) {
	if( (dev & RESULT_DEV)  ) {
	    vsprintf( guibuf, fmt, marker );
	//    C_ResultLogAppend(guibuf);
	}
	if( (dev & ERR_DEV) || (dev & RUN_PROGR_DEV) || 
	    (dev & PROC_PROGR_DEV)) {
	    vsprintf( guibuf, fmt, marker );
	//    C_ProgressLogAppend(guibuf);
	}
    } else {
        if(isatty(fileno(stdout))!=0 && isatty(fileno(stderr))!=0 ) {
	    /* stdout=console=stderr */
	    if( (dev & PROC_PROGR_DEV) || (dev & RUN_PROGR_DEV)
		|| (dev & ERR_DEV) || (dev & RESULT_DEV) ) {
		vfprintf( stdout, fmt, marker );  /* write ONLY to stdout */
	    }
	} else {
	    if( (dev & PROC_PROGR_DEV) || (dev & RESULT_DEV) ) {
		vfprintf( stdout, fmt, marker );
	    }
	    if( dev & ERR_DEV ) {
		vfprintf( stderr, fmt, marker );
	    }
	}
    } /* GUI_mode */
}

static void
print_dev( int device, char *fmt,...)
{
  va_list marker;

  va_start( marker, fmt );
  vprint_dev(device, fmt, marker);
  va_end( marker );
}

static void
print_indent( int device, int length )
{
  int i;

  print_dev( device, "\n" );
  for(i=0; i< length; i++) {
    print_dev( device, LEVEL_INDENT_STRING );
  }
}

static int
set_proc_device( )
/* find the correct device according to the level situation */
{
    int dev = 0;

    if( log_level <= PROC_LEVEL_LIMIT ) {
	dev = dev | PROC_PROGR_DEV;
    }
    if( log_level <= log_level_limit ) {
	dev = dev | LOG_DEV;
    }
    return(dev);
}

static int
set_pres_device( )
/* find the correct device according to the level situation */
{
    int dev = 0;

    if( log_level <= RESULT_LEVEL_LIMIT ) {
	dev = dev | RESULT_DEV;
    }
    if( log_level <= log_level_limit ) {
	dev = dev | LOG_DEV;
    }
    return(dev);
}

static void
print_pres( char *fmt,... )
/* general printing routine for result messages */
{
  int dev;
  va_list marker;

  dev = set_pres_device();
  va_start( marker, fmt );
  vprint_dev( dev, fmt, marker );
  va_end( marker );
  return;
}

static void
vprint_proc( char *prompt, char *fmt, va_list marker )
/* general printing routine for proc progress messages */
{
//  int dev;

//  dev = set_proc_device();
//  print_indent( dev, log_level );
//  print_dev( dev, "%s", prompt);
//  vprint_dev( dev, fmt, marker );
  return;
}

static void
vprint_pres( char *prompt, char *fmt, va_list marker )
/* general printing routine for result messages */
{
  int dev;

  dev=set_pres_device();
  print_indent( dev, log_level );
  print_dev( dev, "%s", prompt);
  vprint_dev( dev, fmt, marker );
  return;
}

/* Print functions called from the program */

/* Error messages */

void
print_derr(char *fmt, ...)
{
  va_list marker;

  print_dev( ERR_DEV, "\n!! ");
  va_start( marker, fmt );
  vprint_dev( ERR_DEV, fmt, marker );
  va_end( marker );
}

void
print_serr(char *fmt, ...)
{
  va_list marker;

  print_dev( ERR_DEV,"\n ! WARNING: ");
  va_start( marker, fmt );
  vprint_dev( ERR_DEV, fmt, marker );
  va_end( marker );
}

/* help messages */

void
print_nerr(char *fmt, ...)
{
  va_list marker;

  print_dev( ERR_DEV,"\n   ");
  va_start( marker, fmt );
  vprint_dev( ERR_DEV, fmt, marker );
  va_end( marker );
}

void
print_dhlp(char *fmt, ...)
{
  va_list marker;

  print_dev( ERR_DEV,"\n?? ");
  va_start( marker, fmt );
  vprint_dev( ERR_DEV, fmt, marker );
  va_end( marker );
}

void
print_shlp(char *fmt, ...)
{
  va_list marker;

  print_dev( ERR_DEV,"\n ? ");
  va_start( marker, fmt );
  vprint_dev( ERR_DEV, fmt, marker );
  va_end( marker );
}

void
print_nhlp(char *fmt, ...)
{
  va_list marker;

  print_dev( ERR_DEV, "\n   ");
  va_start( marker, fmt );
  vprint_dev( ERR_DEV, fmt, marker );
  va_end( marker );
}

/* process progress messages */

void
print_spopen(char *fmt, ...)
{
  va_list marker;
  char prompt[6];

  sprintf(prompt, " [ ");
  va_start( marker, fmt );
  vprint_proc( prompt, fmt, marker );
  va_end( marker );
  log_level++;
}

void
print_spclose(char *fmt, ...)
{
  va_list marker;
  char prompt[6];

  log_level--;
  sprintf(prompt, " ] ");
  va_start( marker, fmt );
  vprint_proc( prompt, fmt, marker );
  va_end( marker );
}

void
print_spdo(char *fmt, ...)
{
  va_list marker;
  char prompt[6];

  sprintf(prompt, " o ");
  va_start( marker, fmt );
  vprint_proc( prompt, fmt, marker );
  va_end( marker );
}

void
print_spinput(char *fmt, ...)
{
  va_list marker;
  char prompt[6];

  sprintf(prompt, " < ");
  va_start( marker, fmt );
  vprint_proc( prompt, fmt, marker );
  va_end( marker );
}

void
print_npinput(char *fmt, ...)
{
  va_list marker;
  char prompt[6];

  sprintf(prompt, "   ");
  va_start( marker, fmt );
  vprint_proc( prompt, fmt, marker );
  va_end( marker );
}

void
print_dpres(char *fmt, ...)
{
  va_list marker;
  char prompt[6];

  sprintf(prompt, ">> ");
  va_start( marker, fmt );
  vprint_pres( prompt, fmt, marker );
  va_end( marker );
}

void
print_dpresh(char *fmt, ...)
/* It is the caller's responsibility to ensure that
   this routine is called only when log_level > 0
*/
{
  va_list marker;
  char prompt[6];

  log_level--;
  sprintf(prompt, ">> ");
  va_start( marker, fmt );
  vprint_pres( prompt, fmt, marker );
  va_end( marker );
  log_level++;
}

void
print_spres(char *fmt, ...)
{
  va_list marker;
  char prompt[6];

  sprintf(prompt, " > ");
  va_start( marker, fmt );
  vprint_pres( prompt, fmt, marker );
  va_end( marker );
}

void
print_spresl(char *fmt, ...)
{
  va_list marker;
  char prompt[6];

  log_level++;
  sprintf(prompt, " > ");
  va_start( marker, fmt );
  vprint_pres( prompt, fmt, marker );
  va_end( marker );
  log_level--;
}

void
print_npres(char *fmt, ...)
{
  va_list marker;
  char prompt[6];

  sprintf(prompt, "   ");
  va_start( marker, fmt );
  vprint_pres( prompt, fmt, marker );
  va_end( marker );
}

void
print_npresh(char *fmt, ...)
/* It is the caller's responsibility to ensure that
   this routine is called only when log_level > 0
*/
{
  va_list marker;
  char prompt[6];

  log_level--;
  sprintf(prompt, "   ");
  va_start( marker, fmt );
  vprint_pres( prompt, fmt, marker );
  va_end( marker );
  log_level++;
}

void
print_r_values( int size, int algsize, int opno, int arity, VEC v)
/* This function DOES print an indent */
{
  int i, j, k;

  if(arity==1) {
    print_npinput( "     ");
    for(j=0; j<algsize; j++) { /* print elements of algebra */
      print_pres( "%2d ", j);
    }
      print_npinput( "   : " );
    for(j=0; j<algsize; j++) {
      print_pres( "%2d ", v[j]);
    }
    return;
  }
  if(arity==2) {
    print_npinput( "     ");
    for(j=0; j<algsize; j++) { /* print elements of algebra */
      print_pres( "%2d ", j);
    }
    for(i=0 ; i<algsize ; i++) {
      print_npinput( "%3d: ", i );
      for(j=0; j<algsize; j++) {
        print_pres( "%2d ", v[i+algsize *j]);
      }
    }
    return;
  }
  if(arity==3) {
    for(i=0; i<algsize; i++) {
      print_npinput( "f%d( , ,%2d)-table:", opno, i );
      print_npinput( "     ");
      for(j=0; j<algsize; j++) { /* print elements of algebra */
        print_pres( "%2d ", j);
      }
      for(j=0; j<algsize; j++) {
        print_npinput( "%3d: ", j );
        for(k=0; k<algsize; k++) {
          print_pres( "%2d ", v[i+algsize*(j+algsize*k)]);
        }
      }
    }
    return;
  }
/* arity is not 2, not 3, just print values as they come. */
  print_npinput("Value(s): ");
  for(i=0 ; i<size ; i++) {
    print_pres( "%2d ", v[i]);
  }
}

void
print_r_vector( int size, VEC v)
{
  int i;

  print_pres( "(");
  for(i=0; i< size-1; i++) {
    print_pres( "%2d,", v[i]);
  }
  print_pres( "%2d)", v[size-1]);
}

void
print_r_set( int size, VEC v)
{
  int i;

  print_pres( "{");
  for(i=0; i< size-1; i++) {
    print_pres( "%2d,", v[i]);
  }
  print_pres( "%2d}", v[size-1]);
}

void
print_r_cong( int size, VEC v)
{
  char *str;
  int err;

  err=print_part( size, v, &str );
  if(err==ERROR) {
    print_nerr("Printing congruence as a vector:");
    print_npres(""); /* do indentation, prompt */
    print_r_vector(size, v);
    if(str!=NULL) free((void *)str);
    return;
  }
  print_pres( "%s", str);
  print_pres( " (%2d block(s))", err); /* number of blocks */
  if(str!=NULL) free((void *)str);
  return;
}

/* run time progress messages */

#ifdef RUN_PROGRESS
void
print_rratio(char *fmt, ...)
/* Used both inside and outside #ifdef TIME_DIAG */
{
  va_list marker;

  print_indent( RUN_PROGR_DEV, log_level );
  print_dev( RUN_PROGR_DEV, " * ");
  va_start( marker, fmt );
  vprint_dev(RUN_PROGR_DEV, fmt, marker);
  va_end( marker );
}
#else
void
print_rshort( void )
{
  print_dev( RUN_PROGR_DEV, "*");
}
#endif /* RUN_PROGRESS */

/* High level */

#ifdef RUN_PROGRESS
void
print_time_progr( 
#ifdef TIME_DIAG
       time_t start_time, time_t *last_time, 
#endif /* TIME_DIAG */
       double ratio, char * subprocess_name )
/*  Prints progress message, containing the elapsed time,
    and an estimated time to go.
    start_time is the start of the process.
    *last_time is the last time when a progress message was printed.
    It is updated by this routine.
    ratio is provided by the caller, its meaning is this:
    if the whole process requires 1 unit of time, then
    ratio has already been done (0< 1 < ratio).
    subproc_name is the name of the subprocess (like `Berman's routine').
    It should be sort (not to wrap lines), and should end in a period
    with no trailing space.
*/
{
#ifdef TIME_DIAG
  time_t curr_time;
  double elapsed_time, time_to_go;
  char elapsedstr[NUM_LEN_LIMIT+3+3+1+20]; /* 20: for " several years." */
  char togostr[NUM_LEN_LIMIT+3+3+1+20];
              /* hours:minutes:seconds EOS */
  int err;

  curr_time=time( NULL );
  if(  difftime(curr_time, *last_time) < PROGR_TIME_OUT ) return;
  *last_time=time( NULL );
#endif /* TIME_DIAG */

  if(ratio <= (double)0 || ratio > (double)1 ) {
    print_rratio("%s Cannot calculate times (%g%% completed?).",
                                subprocess_name, 100*ratio);
    return;
  }
#ifdef TIME_DIAG
  elapsed_time=difftime(curr_time, start_time);
  time_to_go=elapsed_time * ( (double)1-ratio ) / ratio;
  err=time2str(elapsed_time, elapsedstr);
  if(err==ERROR) {
    sprintf(elapsedstr, "several years.");
  }
  err=time2str(time_to_go, togostr);
  if(err==ERROR) {
    sprintf(togostr, "several years.");
  }
  print_rratio("%s Elapsed: %s. To go: %s (%g%% done)", 
                     subprocess_name, elapsedstr, togostr, 100*ratio);
#else
  print_rratio("%s %g%% complete", subprocess_name, 100*ratio);
#endif /* TIME_DIAG */
  return;
}

#ifdef TIME_DIAG
int
time2str( double secnum, char *timestr )
/* converts ELAPSED time to a string.
   if secnum is bigger than TIME_LIMIT, then returns ERROR,
   and timestr is undefined. Otherwise, NO_ERROR is returned.
*/
{
  long secnuml;
  long min, sec;

  if(secnum > TIME_LIMIT ) {
    return(ERROR);
  }
  secnuml=(long)secnum;
  if(secnuml <0) return(ERROR);
  sec=secnuml % 60;
  secnuml=(long)(secnuml/60); /* in minutes */
  min=secnuml % 60;
  secnuml=(long)(secnuml/60); /* in hours */
  sprintf(timestr,  "%02ld:%02ld:%02ld", secnuml, min, sec);
  return(NO_ERROR);
}
#endif /* TIME_DIAG */
#endif /* RUN_PROGRESS */

