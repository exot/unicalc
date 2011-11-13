#define EXTERN extern
#include "includes.h"

int
ps_alg( char *name, ALG *A )
{
  char str[NAME_LEN_LIMIT+1];

  if(comp_command_name(name,"\\algebra_file")==EQUAL)
  {
    if( (get_command_arg(name,str)) != NULL) {
      return(rd_alg(str, A));
    }
  }
  print_derr("Algebra description expected.");
  hlp_alg_line();
  return(ERROR);
}

int
rd_alg( char *fname, ALG *A )
/* This function attempts to load in an algebra from file `fname'.
   If the file is not there, it returns ERROR. */
{
  int algsize;
  FUNCT *k;
  FILE *fp;
  int err;
  int opno;

  print_spopen("Reading algebra `%s'.", fname);
  fp = fopen(fname,"r");
  if(fp == NULL)
  {
    print_derr("Cannot open algebra file.");
    hlp_alg_line();
    return(ERROR);
  }
  algsize= get_integer(fp,"\n\r");
  if(algsize==BAD_NUMBER)
  {
    print_derr("Error reading size of algebra (bad delimiter?).");
    hlp_alg_file();
    fclose(fp);
    return(ERROR);
  }
  if(algsize <= 0 || algsize >= ALG_SIZE_LIMIT)
  {
    print_derr("Illegal algebra size %d (should be in [%d, %d]).",
                                       algsize, 1, ALG_SIZE_LIMIT-1);
    hlp_alg_file();
    fclose(fp);
    return(ERROR);
  }
  print_spinput("Number of elements: %d", algsize);
  A->size = algsize;
  A->func = (FUNCT *)NULL;
  opno=0;

  err= rd_funct(fp,algsize,&(A->func), opno);
  if(err == NOT_FOUND)
  {
    print_serr("No operations defined in algebra.");
    hlp_alg_file();
    print_spclose("End of reading algebra.");
    fclose(fp);
    return(NO_ERROR);
  }
  if(err!=NO_ERROR)
  {
    fclose(fp);
    return(ERROR);
  }
  k= A->func;
  opno=1;
  for(;;opno++)
  {
    err=rd_funct(fp,algsize,&(k->next),opno);
    if(err==NOT_FOUND) break;
    if(err==ERROR)
    {
      free_alg(A);
      fclose(fp);
      return(ERROR);
    }
    k= k->next;
  }
  fclose(fp);
  print_spclose("End of reading algebra.");
  return(NO_ERROR);
}

int
rd_funct( FILE *fp, int algsize, FUNCT **k, int opno )
/* Reads the next function from FILE *fp. `algsize' is the size of the
   algebra. Returns the result in **k, ERROR on error, 
   NOT_FOUND if there are no more functions, NO_ERROR otherwise. */
{
  int arity, len;
  int *ip, *ipe;

  arity= get_integer(fp,"\n\r");
  if(arity==BAD_NUMBER) {
    if(feof(fp)) {
      return(NOT_FOUND);
    }
    print_derr("Error reading arity of operation (bad delimiter?).");
    hlp_alg_file();
    return(ERROR);
  }
  if(arity < 0 || arity >= ARITY_LIMIT)
  {
    print_derr("Illegal arity %d (should be in [%d, %d]).",
                                              arity, 0, ARITY_LIMIT-1);
    hlp_alg_file();
    fclose(fp);
    return(ERROR);
  }
  if( ((*k)= (FUNCT *) (malloc(sizeof(FUNCT))))== NULL)
  {
    print_derr("Not enough memory to define an operation.");
    return(ERROR);
  }
  print_spinput("Reading f%d. Arity is %d.", opno, arity);
  (*k)->arity= arity;
  (*k)->next= (FUNCT *)NULL;
  len = intpow(algsize, arity);
  if(len == -1)
  {
    print_derr("(Algebra size)^(arity) too big.");
    hlp_alg_file();
    if((*k)!= (FUNCT *)NULL) free((void *)(*k));
    return(ERROR);
  }
  if (((*k)->values= (int *) (calloc(len,sizeof(int))))== NULL)
  {
    print_derr("Not enough memory to store an operation.");
    hlp_alg_file();
    if((*k)!= (FUNCT *)NULL) free((void *)(*k));
    return(ERROR);
  }
  ip = (*k)->values;
  ipe = ip + len;
  for ( ;ip<ipe; ip++)
  {
    *ip= get_integer(fp,"\n\r "); /* space can also delimit values */
    if(*ip==BAD_NUMBER)
    {
      print_derr("Error reading value of operation (bad delimiter?).");
      hlp_alg_file();
      if(((*k)->values)!= (int *)NULL) free((void *)((*k)->values));
      if((*k)!= (FUNCT *)NULL) free((void *)(*k));
      return(ERROR);
    }
    if(*ip < 0 || *ip >= algsize)
    {
      print_derr("Illegal operation value %d (should be in [%d, %d]).",
                                              *ip, 0, algsize-1);
      hlp_alg_file();
      fclose(fp);
      if((*k)!= (FUNCT *)NULL) free((void *)(*k));
      return(ERROR);
    }
  }
  print_r_values(len, algsize, opno, (*k)->arity, (VEC)((*k)->values) );
  return(NO_ERROR);
}

int
wr_alg(char *fname, ALG *A )
/* This routine writes an algebra to a file.
   The output is in Matt's *.alg (or *.lat) format.
   Returns ERROR if file error, NO_ERROR otherwise. */
{
  FILE *fp;
  int i, sr, len;
  FUNCT *k;


  print_spdo("Writing algebra to `%s'.", fname);
  fp = fopen(fname,"w");
  if(fp == NULL)
  {
    print_derr("Cannot open output file.");
    return(ERROR);
  }
  sr= fprintf(fp,"%d\n",A->size);
  if(sr<0 || sr== EOF)
  {
    print_derr("Error writing file.");
    fclose(fp);
    return(ERROR);
  }
  k=A->func;
  while(k!=NULL) {
    sr= fprintf(fp,"%d\n",k->arity);
    if(sr<0 || sr== EOF)
    {
      print_derr("Error writing file.");
      fclose(fp);
      return(ERROR);
    }
    len = intpow(A->size, k->arity);
    if(len == -1)
    {
      print_derr("Cannot write algebra: (size)^(arity) too big.");
      fclose(fp);
      return(ERROR);
    }
    for(i=0; i<len; i++) {
      sr= fprintf(fp,"%d\n", (k->values)[i]);
      if(sr<0 || sr== EOF)
      {
        print_derr("Error writing file.");
        fclose(fp);
        return(ERROR);
      }
    }
    k=k->next;
  }
  fclose(fp);
  return(NO_ERROR);
}

int
ps_vectors(char *name, VECTORS *v, int limit)
/* parameters described in rd_vectors() */
{
  char str[NAME_LEN_LIMIT+1];

  if(comp_command_name(name,"\\vectorlist_file")==EQUAL)
  {
    if( (get_command_arg(name,str)) != NULL) {
      return(rd_vectors(str, v, limit));
    }
  }
  print_derr("Vectorlist description expected.");
  hlp_vector_line();
  return(ERROR);
}

int
rd_vectors(char *fname, VECTORS *v, int limit)
/* This routine reads vectors from file `fname'.
   On output, v is filled in. See hlp_vector_file() for file syntax.
   The vecor entries are checked to be between 0 and limits-1.
   Returns ERROR on error, NO_ERROR otherwise */
{
  int i,j;
  FILE *fp;
  int inp;
  int ch;

  print_spopen("Reading vectorlist file `%s'.", fname);
  fp = fopen(fname,"r");
  if(fp == NULL)
  {
    print_derr("Cannot open vectorlist file.");
    hlp_vector_line();
    return(ERROR);
  }
  v->nvecs= get_integer(fp,",");
  if(v->nvecs==BAD_NUMBER)
  {
    print_derr("Error reading number of vectors (bad delimiter?).");
    hlp_vector_file();
    fclose(fp);
    return(ERROR);
  }
  v->lvecs= get_integer(fp,"\n\r");
  if(v->lvecs==BAD_NUMBER)
  {
    print_derr("Error reading length of vectors (bad delimiter?).");
    hlp_vector_file();
    fclose(fp);
    return(ERROR);
  }
  if(v->nvecs < 0 || v->nvecs >=VEC_NUM_LIMIT)
  {
    print_derr("Illegal number %d of vectors (should be in [%d, %d]).",
                                        v->nvecs, 0, VEC_NUM_LIMIT-1);
  }
  if(v->lvecs <= 0 || v->lvecs >= VEC_LEN_LIMIT)
  {
    print_derr("Illegal length %d of vectors (should be in [%d, %d]).",
                                          v->lvecs, 1, VEC_LEN_LIMIT-1);
    hlp_vector_file();
    fclose(fp);
    return(ERROR);
  }
  ch= fgetc(fp);
  if((char)ch != '\n')
  {
    print_derr("The second line of a vectorlist file must be empty.");
    hlp_vector_file();
    fclose(fp);
    return(ERROR);
  }
  print_spinput("Number of vectors: %d", v->nvecs);
  print_npinput("Length of vectors: %d", v->lvecs);
  v->list=NULL;  /* to ensure that free_vectors works OK */
  if(v->nvecs==0) {
    print_serr("No vectors in vectorlist file, is this OK?");
    goto  WLrd_vectors;
  }


  if ((v->list= (VEC *)(calloc(v->nvecs,sizeof(VEC))))== NULL)
  {
    print_derr("Not enough memory to point to vectors.");
    hlp_vector_file();
    fclose(fp);
    return(ERROR);
  }
  for(i= 0;i<v->nvecs;i++)
  {
    (v->list)[i]= (VEC)NULL;  /* to ensure that free_vectors works OK */
  }
  for(i= 0;i<v->nvecs;i++)
  {
    if(((v->list)[i]= (VEC)(calloc(v->lvecs,sizeof(int))))== NULL)
    {
      print_derr("Not enough memory to store vectors.");
      hlp_vector_file();
      free_vectors(v);
      fclose(fp);
      return(ERROR);
    }
    for(j= 0;j<(v->lvecs)-1;j++)
    {
      inp= get_integer(fp,",");
      if(inp==BAD_NUMBER)
      {
        print_derr("Error reading vector entries (bad delimiter?).");
        hlp_vector_file();
        free_vectors(v);
        fclose(fp);
        return(ERROR);
      }
      if(inp < 0 || inp >=limit)
      {
        print_derr("Illegal vector entry %d (should be in [%d, %d]).",
                                                         inp, 0, limit-1);
        hlp_vector_file();
        free_vectors(v);
        fclose(fp);
        return(ERROR);
      }
      ((v->list)[i])[j]= inp;
    }
    /* now j= (v->lvecs)-1 */
    inp= get_integer(fp,"\n\r");
    if(inp==BAD_NUMBER)
    {
      print_derr("Error reading vector entries (bad delimiter?).");
      hlp_vector_file();
      free_vectors(v);
      fclose(fp);
      return(ERROR);
    }
    if(inp < 0 || inp >=limit)
    {
      print_derr("Illegal vector entry %d (should be in [%d, %d]).",
                                                         inp, 0, limit-1);
      hlp_vector_file();
      free_vectors(v);
      fclose(fp);
      return(ERROR);
    }
    ((v->list)[i])[j]= inp;
    print_npinput("V%5d: ", i);
    print_r_vector(v->lvecs, v->list[i]);
  }
  if(fgetc(fp)!=EOF)
  {
    print_serr("Vectorlist file longer than expected.");
  }
WLrd_vectors:
  print_spclose("End of reading vectors.");
  fclose(fp);
  return(NO_ERROR);
}

int
wr_vectors(char *fname, VECTORS *v)
/* This routine writes a set of vectors to fname.
   Returns ERROR if file error, NO_ERROR otherwise. */
{
  int i,j;
  FILE *fp;
  int sr;

  print_spdo("Writing vector(s) to `%s'.", fname);
  fp = fopen(fname,"w");
  if(fp == NULL)
  {
    print_derr("Cannot open output file.");
    hlp_gen_subalg();
    return(ERROR);
  }
  sr= fprintf(fp,"%d,%d\n\n",v->nvecs,v->lvecs);
  if(sr<0 || sr== EOF)
  {
    print_derr("Error writing file");
    hlp_gen_subalg();
    fclose(fp);
    return(ERROR);
  }
  for (i= 0;i<v->nvecs;i++)
  {
    for (j= 0;j<(v->lvecs)-1;j++)
    {
      sr= fprintf(fp,"%d, ",((v->list)[i])[j]);
      if(sr<0 || sr== EOF)
      {
        print_derr("Error writing file.");
        fclose(fp);
        return(ERROR);
      }
    }
    sr= fprintf(fp,"%d\n",((v->list)[i])[j]);
    if(sr<0 || sr== EOF)
    {
      print_derr("Error writing file.");
      fclose(fp);
      return(ERROR);
    }
  }
  fclose(fp);
  return(NO_ERROR);
}

int
wr_conlist(char *fname, PARTS *cong )
/* This routine writes a congruence-list to fname.
   This congruence is transformed to matt encoding before output.
   The output is in Matt's *.con (or *.pri) format, but the permutability
   value is set to 0.
   Returns ERROR if file error, NO_ERROR otherwise. */
{
  int i,j;
  FILE *fp;
  int sr;
  VEC wrcong;

  if ((wrcong= (VEC)(calloc(cong->lparts,sizeof(int))))== NULL)
  {
    print_derr("Not enough memory for writing congruences.");
    return(ERROR);
  }
  print_spdo("Writing congruence(s) to `%s'.", fname);
  fp = fopen(fname,"w");
  if(fp == NULL)
  {
    print_derr("Cannot open output file.");
    free((void *)wrcong);
    return(ERROR);
  }
  sr= fprintf(fp,"%d,%d\n",cong->lparts,0);/* 0=invalid perm argument */
  if(sr<0 || sr== EOF)
  {
    print_derr("Error writing file.");
    fclose(fp);
    free((void *)wrcong);
    return(ERROR);
  }
  for (i= 0;i<cong->nparts;i++)
  {
    for(j=0; j<cong->lparts; j++) {
      wrcong[j]=((cong->list)[i])[j];
    }
    arb2matt_con(cong->lparts, wrcong);
    for (j= 0;j<(cong->lparts);j++)
    {
      sr= fprintf(fp,",%d", wrcong[j]);
      if(sr<0 || sr== EOF)
      {
        print_derr("Error writing file.");
        fclose(fp);
        free((void *)wrcong);
        return(ERROR);
      }
    }
    sr= fprintf(fp,"\n");
    if(sr<0 || sr== EOF)
    {
      print_derr("Error writing file.");
      fclose(fp);
      free((void *)wrcong);
      return(ERROR);
    }
  }
  fclose(fp);
  free((void *)wrcong);
  return(NO_ERROR);
}

int
wr_typelist(char *fname, VECTORS *types )
/* This routine writes a type-list to fname.
   It is almost identical to the conlist format,
   but there is no permutablity argument, and the second line
   is empty (as in vectorlist files). 
   This differs from Matt's *.typ format only in one respect:
   we add a trailing \n in the last line, too.
   Returns ERROR if file error, NO_ERROR otherwise. */
{
  int i,j;
  FILE *fp;
  int sr;

  print_spdo("Writing type label(s) to `%s'.", fname);
  fp = fopen(fname,"w");
  if(fp == NULL)
  {
    print_derr("Cannot open output file.");
    return(ERROR);
  }
  sr= fprintf(fp,"%d\n\n",types->nvecs);
  if(sr<0 || sr== EOF)
  {
    print_derr("Error writing file.");
    fclose(fp);
    return(ERROR);
  }
  for (i= 0;i<types->nvecs;i++)
  {
    for (j= 0;j<(types->lvecs);j++)
    {
      sr= fprintf(fp,",%d", ((types->list)[i])[j] );
      if(sr<0 || sr== EOF)
      {
        print_derr("Error writing file.");
        fclose(fp);
        return(ERROR);
      }
    }
    sr= fprintf(fp,"\n");
    if(sr<0 || sr== EOF)
    {
	print_derr("Error writing file.");
	fclose(fp);
	return(ERROR);
    }
  }
  fclose(fp);
  return(NO_ERROR);
}

int
ps_out(char *name, char *out)
{
  FILE *fp;

  if(comp_command_name(name,"\\output_file")==EQUAL)
  {
    if( (get_command_arg(name,out)) != NULL) {
      fp = fopen(out,"w");
      if(fp == NULL)
      {
        print_derr("Cannot open output file `%s'.", out);
        hlp_output_line();
        return(ERROR);
      }
      fclose(fp);
      unlink(out); /* Do not leave empty files on error. */
      print_spinput("The output file is `%s'.", out);
      return(NO_ERROR);
    }
  }
  print_derr("Output description expected.");
  hlp_output_line();
  return(ERROR);
}

int
ps_number(char *name, int low, int high, int *out)
/* gets a number between low and high */
{
  char str[NAME_LEN_LIMIT+1];

  if(comp_command_name(name,"\\number")==EQUAL)
  {
    if( (get_command_arg(name,str)) != NULL) {
      return(rd_number(str, low, high, out));
    }
  }
  print_derr("Number description expected.");
  hlp_number_line();
  return(ERROR);
}

int
rd_number(char *num, int low, int high, int *out)
/* read a number in [low,high] from *num */
{
  int sr;

  sr= sscanf(num,"%d", out);
  if(sr<=0 || sr== EOF)
  {
    print_derr("Cannot read number `%s'.", num);
    hlp_number_line();
    return(ERROR);
  }
  if(*out<low || *out> high)
  {
    print_derr("Illegal number %d (should be in [%d, %d]).",
                                                    *out, low, high);
    return(ERROR);
  }
  print_spinput("The number read is %d.", *out);
  return(NO_ERROR);
}

int
ps_conlist(char *name, PARTS *v, int algsize)
/* The meaning of algsize is this:
     if algsize > 0, then the vector entries must be algsize long;
     if algsize < 0, then the vector entries must be >=-algsize long;
     if algsize==0, then the length of vector entries is not checked.
*/
{
  char str[NAME_LEN_LIMIT+1];

  if(comp_command_name(name,"\\conlist_file")==EQUAL)
  {
    if( (get_command_arg(name,str)) != NULL) {
      return(rd_conlist(str, v, algsize));
    }
  }
  print_derr("Conlist description expected.");
  hlp_conlist_line();
  return(ERROR);
}

int
ps_con_or_list(char *name, PART *cong, PARTS *v, int algsize)
/* The meaning of algsize is this:
     if algsize > 0, then the vector entries must be algsize long;
     if algsize < 0, then the vector entries must be >=-algsize long;
     if algsize==0, then the length of vector entries is not checked.
   The value 0 is illegal for \zero and \one.
*/
{
  char str[NAME_LEN_LIMIT+1];

  cong->func=NULL;
  v->list=NULL;

  if(comp_command_name(name,"\\conlist_file")==EQUAL)
  {
    if( (get_command_arg(name,str)) == NULL) {
      print_derr("Conlist argument expected.");
      hlp_conlist_line();
      return(ERROR);
    }
    return(rd_conlist(str, v, algsize));
  }
  if(comp_command_name(name,"\\congruence")==EQUAL)
  {
    if( (get_command_arg(name,str)) == NULL) {
      print_derr("Congruence argument expected.");
      hlp_cong_line();
      return(ERROR);
    }
    return(ps_cong_arg(str, cong, algsize));
  }
  print_derr("Congruence or conlist description expected.");
  hlp_cong_line();
  hlp_conlist_line();
  return(ERROR);
}

int
ps_cong(char *name, PART *cong, int algsize)
/* The meaning of algsize is this:
     if algsize > 0, then the vector entries must be algsize long;
     if algsize < 0, then the vector entries must be >=-algsize long;
     if algsize==0, then the length of vector entries is not checked.
   The value 0 is illegal for \zero and \one.
*/
{
  char str[NAME_LEN_LIMIT+1];

  if(comp_command_name(name,"\\congruence")==EQUAL)
  {
    if( (get_command_arg(name,str)) != NULL) {
      return(ps_cong_arg(str, cong, algsize));
    }
  }
  print_derr("Congruence description expected.");
  hlp_cong_line();
  return(ERROR);
}

int
ps_cong_arg(char *arg, PART *cong, int algsize)
/* Everything is freed on return, except cong->func on NO_ERROR.
   The meaning of algsize is this:
     if algsize > 0, then the vector entries must be algsize long;
     if algsize < 0, then the vector entries must be >=-algsize long;
     if algsize==0, then the length of vector entries is not checked.
   The value 0 is illegal for \zero and \one.
*/
{
  PARTS conlist;
  int congnum;
  char *next_c;
  char str[NAME_LEN_LIMIT+1];

  conlist.list=NULL;
  cong->func=NULL;
  if(comp_command_name(arg,"\\zero_congruence")==EQUAL) {
    if(algsize==0) {
      print_derr("Must have a set to create the zero congruence on.");
      return(ERROR);
    }
    if(algsize < 0) algsize = -algsize;
    cong->size= algsize;
    if(zero_cong(cong)!=NO_ERROR) return(ERROR);
    print_spinput("The congruence is the zero_congruence\
 on a %d element set.", algsize);
    return(NO_ERROR);
  }
  if(comp_command_name(arg,"\\one_congruence")==EQUAL) {
    if(algsize==0) {
      print_derr("Must have a set to create the one congruence on.");
      return(ERROR);
    }
    if(algsize < 0) algsize = -algsize;
    cong->size= algsize;
    if(one_cong(cong)!=NO_ERROR) return(ERROR);
    print_spinput("The congruence is the one_congruence\
 on a %d element set.", algsize);
    return(NO_ERROR);
  }
  if(comp_command_name(arg,"\\conlist_file")==EQUAL) {
    if( (next_c=get_command_arg(arg,str)) == NULL) {
      print_derr("Conlist argument expected.");
      hlp_cong_arg();
      return(ERROR);
    }
    if( rd_conlist(str, &conlist, algsize) != NO_ERROR) return(ERROR);
    if(comp_command_name(next_c,"\\number") != EQUAL) {
      print_derr("Number description expected.");
      free_conlist(&conlist);
      hlp_cong_arg();
      return(ERROR);
    }
    if( (get_command_arg(next_c,str)) == NULL) {
      print_derr("Number argument expected.");
      free_conlist(&conlist);
      hlp_cong_arg();
      return(ERROR);
    }
    if(rd_number(str, 0, conlist.nparts-1, &congnum)!=NO_ERROR) {
      free_conlist(&conlist);
      hlp_cong_arg();
      return(ERROR);
    }
/* everything read succesfully, now move the congruence over. */
    cong->size=conlist.lparts;
    cong->func=(conlist.list)[congnum];
    (conlist.list)[congnum]=NULL;
/* we MUST do this for free_conlist not to free cong->func! */
    free_conlist(&conlist);
    print_spinput("The congruence is congruence C%d.", congnum);
    return(NO_ERROR);
  }
  print_derr("Congruence argument expected.");
  hlp_cong_arg();
  return(ERROR);
}

int
ps_binrel_line(char *name, VECTORS *v, int algsize)
{
  char str[NAME_LEN_LIMIT+1];

  if(comp_command_name(name,"\\bin_rel")==EQUAL) {
    if( (get_command_arg(name,str)) != NULL) {
      return(ps_binrel_arg(str, v, algsize));
    }
  }
  print_derr("Binary relation description expected.");
  hlp_binrel_line();
  return(ERROR);
}

int
ps_binrel_arg(char *arg, VECTORS *v, int algsize)
{
  char str[NAME_LEN_LIMIT+1];
  PART cong;
  int err;

  cong.func=NULL;
  if(comp_command_name(arg,"\\congruence")==EQUAL) {
    if( (get_command_arg(arg,str)) == NULL) {
      print_derr("Congruence argument expected.");
      hlp_binrel_arg();
      return(ERROR);
    }
    if( ps_cong_arg(str, &cong, algsize)!=NO_ERROR ) {
      return(ERROR);
    }
    err=cong2pairs(v, &cong);
    if(cong.func != NULL) free((void *)(cong.func));
    if(err!=NO_ERROR) return(ERROR);
    print_spinput("The binrel is this congruence.");
    return(NO_ERROR);
  }
  if(comp_command_name(arg,"\\vectorlist_file")==EQUAL) {
    if( (get_command_arg(arg,str)) == NULL) {
      print_derr("Vectorlist file argument expected.");
      hlp_binrel_arg();
      return(ERROR);
    }
    if(rd_vectors(str, v, algsize)!=NO_ERROR) return(ERROR);
    if(v->lvecs!=2) {
      print_derr("The length of these vectors must be 2, and not %d.", 
                                                            v->lvecs);
      hlp_binrel_arg();
      return(ERROR);
    }
    print_spinput("The binrel is the set of these pairs.");
    return(NO_ERROR);
  }
  print_derr("Binary relation argument expected.");
  hlp_binrel_arg();
  return(ERROR);
}

int
rd_conlist(char *fname, PARTS *v, int algsize)
/* This routine reads congruences from file `fname'.
   On output, v is filled in. See hlp_conlist_file() for file syntax
   (conforms to Matt Valeriote's *.con format).
   The input is converted to ewk format.
   The meaning of algsize is this:
     if algsize > 0, then the vector entries must be algsize long;
     if algsize < 0, then the vector entries must be >=-algsize long;
     if algsize==0, then the length of vector entries is not checked.
   Returns ERROR on error, NO_ERROR otherwise */
{
  int i,j;
  FILE *fp;
  int inp;
  int ch;

  print_spopen("Reading conlist file `%s'.", fname);
  fp = fopen(fname,"r");
  if(fp == NULL)
  {
    print_derr("Cannot open conlist file.");
    hlp_conlist_line();
    return(ERROR);
  }
  v->lparts= get_integer(fp,",");
  if(v->lparts==BAD_NUMBER)
  {
    print_derr("Error reading length of con-vectors (bad delimiter?).");
    hlp_conlist_file();
    fclose(fp);
    return(ERROR);
  }
/* check v->lparts */
  if (algsize == 0) {
    if(v->lparts <= 0 || v->lparts >= VEC_LEN_LIMIT ) {
      print_derr("Illegal length %d of congruences (should be in [%d, %d]).",
                                       v->lparts, 1, VEC_LEN_LIMIT-1);
      hlp_conlist_file();
      fclose(fp);
      return(ERROR);
    }
  } else if( algsize < 0 ) {
    if( v->lparts < -algsize ) {
      print_derr("Illegal length %d of congruences (should be at least %d).",
                                       v->lparts, -algsize);
      hlp_conlist_file();
      fclose(fp);
      return(ERROR);
    }
  } else { /* algsize > 0 */
    if(v->lparts != algsize) {
      print_derr("Illegal length %d of congruences (should be %d).",
                                       v->lparts, algsize);
      hlp_conlist_file();
      fclose(fp);
      return(ERROR);
    }
  }

/* now discard the rest of the first line */
  for(;;) {
    ch= fgetc(fp);
    if(ch== EOF)
    {
      print_derr("Unexpected EOF when reading congruence-file.");
      hlp_conlist_file();
      fclose(fp);
      return(ERROR);
    }
    if((char)ch=='\n') break;
  }
/* first line read, read congruences. Instead of doing reallocs,
*  we read the file twice, first determining the number of congruences. */
  v->nparts=0;
  for(;;) {
    ch= fgetc(fp);
    if(ch== EOF) break;
    if((char)ch=='\n') (v->nparts)++;
  }
  rewind(fp);
/* read first line, and discard it */
  for(;;) {
    ch= fgetc(fp);
    if(ch== EOF)
    {
      print_derr("Unexpected EOF when reading congruence-file.");
      hlp_conlist_file();
      fclose(fp);
      return(ERROR);
    }
    if((char)ch=='\n') break;
  }
  if(v->nparts >= VEC_NUM_LIMIT)
  {
    print_derr("Illegal number %d of congruences (should be in [%d, %d]).",
                                       v->nparts, 0, VEC_NUM_LIMIT-1);

    hlp_conlist_file();
    fclose(fp);
    return(ERROR);
  }
/* now v->nparts is correctly set */
  print_spinput("The number of congruences is %d.", v->nparts);
  v->list=NULL;  /* to ensure that free_vectors works OK */
  if(v->nparts==0) {
    print_serr("No congruences in the file, is this OK?");
    fclose(fp);
    return(NO_ERROR);
  }

  if ((v->list= (VEC *)(calloc(v->nparts,sizeof(VEC))))== NULL)
  {
    print_derr("Not enough memory to point to congruences.");
    hlp_conlist_file();
    fclose(fp);
    return(ERROR);
  }
  for(i= 0;i<v->nparts;i++)
  {
    (v->list)[i]= (VEC)NULL;  /* to ensure that free_vectors works OK */
  }
  for(i= 0;i<v->nparts;i++)
  {
    if(((v->list)[i]= (VEC)(calloc(v->lparts,sizeof(int))))== NULL)
    {
      print_derr("Not enough memory to store congruences.");
      hlp_conlist_file();
      free_conlist(v);
      fclose(fp);
      return(ERROR);
    }
/* Read starting `,' */
    ch= fgetc(fp);
    if(ch== EOF || (char)ch != ',')
    {
      print_derr("Line %d of the conlist file must start with `,'.", i+2 );
      hlp_conlist_file();
      free_conlist(v);
      fclose(fp);
      return(ERROR);
    }
    for(j=0;j<(v->lparts)-1;j++)
    {
      inp= get_integer(fp,",");
      if(inp==BAD_NUMBER)
      {
        print_derr("Error reading congruences (bad delimiter?).");
        hlp_conlist_file();
        free_conlist(v);
        fclose(fp);
        return(ERROR);
      }
      if(inp < 0 || inp >= v->lparts)
      {
        print_derr("Illegal entry %d of a congruence\
 (should be in [%d, %d]).", inp, 0, v->lparts-1);
        hlp_conlist_file();
        free_conlist(v);
        fclose(fp);
        return(ERROR);
      }
      ((v->list)[i])[j]= inp;
    }
    /* now j= (v->lparts)-1 */
    inp= get_integer(fp,"\n\r");
    if(inp==BAD_NUMBER)
    {
      print_derr("Error reading congruence vectors (bad delimiter?).");
      hlp_conlist_file();
      free_conlist(v);
      fclose(fp);
      return(ERROR);
    }
    if(inp < 0 || inp >= v->lparts)
    {
      print_derr("Illegal entry %d of a congruence\
 (should be in [%d, %d]).", inp, 0, v->lparts-1);
      hlp_conlist_file();
      free_conlist(v);
      fclose(fp);
      return(ERROR);
    }
    ((v->list)[i])[j]= inp;
    arb2ewk_con( v->lparts, (v->list)[i] );
    print_npinput("C%3d: ", i);
    print_r_cong(v->lparts, v->list[i]);
  }
  if( fgetc(fp)!=EOF )
  {
    print_serr("Conlist file longer than expected.");
  }
  print_spclose("End of reading congruences.");
  fclose(fp);
  return(NO_ERROR);
}

