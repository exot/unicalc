#define EXTERN 
#include "includes.h"

char name[NAME_LEN_LIMIT+1];

static int ps_project_name( const char *, FILE ** );
static int rd_pars( FILE * );

static int gen_subalg( FILE * );
static int gen_cong( FILE * );
static int cr_dir_prod( FILE * );
static int cr_fact_alg( FILE * );
static int cr_subpr_alg( FILE * );
static int cr_prod_cong( FILE * );
static int find_congnum( FILE * );
static int gen_conlat( FILE * );
static int is_centr( FILE *, int );
static int rlr( FILE * );
static int min_sets( FILE * );
static int un_pol( FILE * );
static int type_q( FILE * );
static int label_conlat( FILE * );
static int kkvm_germ( FILE * );

int do_it(const char * project_name) {
/* Main entry point. It is assumed that log_level_limit 
   and GUI_mode are already set. */

  int err, first, iseof;
  FILE *fp;

  if(ps_project_name(project_name, &fp)!=NO_ERROR) return(ERROR);

  first=YES;
  err = NO_ERROR;
  linenum=0;
  for(;;) {
    log_level=0;
    iseof=is_eof(fp);
    if(iseof==YES) {
      if( first!=YES ) break;
      else {
        print_derr("Empty parameter file?");
        fclose(fp);
	goto WLe_do_it;
      }
    }
    /* so there is something to read */
    err = get_next_line(name, NAME_LEN_LIMIT, fp);
    if( err!=NO_ERROR ) {
      fclose(fp);
      goto WLe_do_it;
    }
    first=NO;
    if(strcmp(name, "\\begin_command")!= 0) {
      print_derr("Missing \\begin_command.");
      hlp_par_format();
      fclose(fp);
      err = ERROR;
      goto WLe_do_it;
    }
    err = rd_pars(fp);
    if(err != NO_ERROR ) {
      fclose(fp);
      goto WLe_do_it;
    }
    if( get_next_line(name, NAME_LEN_LIMIT, fp) != NO_ERROR) {
      fclose(fp);
      err = ERROR;
      goto WLe_do_it;
    }
    if(strcmp(name, "\\end_command")!= 0) {
      print_derr("Missing \\end_command.");
      hlp_par_format();
      fclose(fp);
      err = ERROR;
      goto WLe_do_it;
    }
  }
  fclose(fp);
WLe_do_it:

  fprintf( log_file_p, "\n" );
  fflush( log_file_p );
  fprintf( results_file_p, "\n" );
  fflush( results_file_p );
  if( err == INTERRUPT ) {
      print_derr("Calculation interrupted.\n");
      return(err);
  }
  if( err == NO_ERROR ) {
      print_spdo("Calculation terminated succesfully.\n");
      return(err);
  }
  print_derr("Calculation terminated on error.\n");
  return(err);

}

static int
ps_project_name(const char *project_name, FILE **fp)
/* sets the file pointer fp, and opens the log and results files.
  Returns ERROR or NO_ERROR.
*/
{
  strcpy(name, project_name);
  strcat(name, ".par");
  *fp = fopen(name,"r");
  if(*fp == NULL)
  {
    print_derr("Cannot open parameter file `%s'.", name);
    hlp_usage();
    return(ERROR);
  }

  strcpy(name, project_name);
  strcat(name, ".log");
  log_file_p = fopen(name,"w");
  if(log_file_p == NULL)
  {
    print_derr("Cannot open log file `%s' for writing.", name);
    hlp_usage();
    return(ERROR);
  }

  strcpy(name, project_name);
  strcat(name, ".dat");
  results_file_p = fopen(name,"a");
  if(results_file_p == NULL)
  {
    print_derr("Cannot open file `%s' to append the results to.", name);
    hlp_usage();
    return(ERROR);
  }
  return(NO_ERROR);
}

static int
rd_pars( FILE *fp )  /* returns NO_ERROR if no error, ERROR otherwise */
{
  if( get_next_line(name, NAME_LEN_LIMIT, fp) != NO_ERROR) {
    return(ERROR);
  }

  if(strcmp(name, "\\generate_subalgebra")== 0)
  {
    return(gen_subalg(fp));
  }
  if(strcmp(name, "\\create_direct_product")== 0)
  {
    return(cr_dir_prod(fp));
  }
  if(strcmp(name, "\\create_factor_algebra")== 0)
  {
    return(cr_fact_alg(fp));
  }
  if(strcmp(name, "\\create_subproduct_algebra")== 0)
  {
    return(cr_subpr_alg(fp));
  }
  if(strcmp(name, "\\product_congruence")== 0)
  {
    return(cr_prod_cong(fp));
  }
  if(strcmp(name, "\\find_congruence_number")== 0)
  {
    return(find_congnum(fp));
  }
  if(strcmp(name, "\\generate_congruence")== 0)
  {
    return(gen_cong(fp));
  }
  if(strcmp(name, "\\compute_congruence_lattice")== 0)
  {
    return(gen_conlat(fp));
  }
  if(strcmp(name, "\\centrality")== 0)
  {
    return(is_centr(fp, CENTR_NORMAL));
  }
  if(strcmp(name, "\\weak_centrality")== 0)
  {
    return(is_centr(fp, CENTR_WEAK));
  }
  if(strcmp(name, "\\strong_centrality")== 0)
  {
    return(is_centr(fp, CENTR_STRONG));
  }
  if(strcmp(name, "\\rectangular_centrality")== 0)
  {
    return(is_centr(fp, CENTR_RECT));
  }
  if(strcmp(name, "\\compute_R(L,R)")== 0)
  {
    return(rlr(fp));
  }
  if(strcmp(name, "\\compute_min_sets")== 0)
  {
    return(min_sets(fp));
  }
  if(strcmp(name, "\\unary_polynomials")== 0)
  {
    return(un_pol(fp));
  }
  if(strcmp(name, "\\compute_type")== 0)
  {
    return(type_q(fp));
  }
  if(strcmp(name, "\\label_congruence_lattice")== 0)
  {
    return(label_conlat(fp));
  }
  if(strcmp(name, "\\find_kkvm_germ")== 0)
  {
    return(kkvm_germ(fp));
  }
  print_derr("Unrecognized command `%s'.", name);
  hlp_commands();
  return(ERROR);
}

static int
gen_subalg( FILE *fp )
{
  ALG A;
  VECTORS v;
  int err;
  char outf[NAME_LEN_LIMIT+1];

  print_spopen("Generating a subpower.");
  A.func= (FUNCT *)NULL;
  v.list= (VEC *)NULL;

  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_gen_subalg;
  }
  err= ps_alg(name, &A);
  if(err!= NO_ERROR)
  {
    hlp_gen_subalg();
    goto WLe_gen_subalg;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_gen_subalg;
  }
  err = ps_vectors(name, &v, A.size);
  if(err!= NO_ERROR)
  {
    hlp_gen_subalg();
    goto WLe_gen_subalg;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_gen_subalg;
  }
  err=ps_out(name, outf);
  if(err != NO_ERROR)
  {
    hlp_gen_subalg();
    hlp_vector_file();
    goto WLe_gen_subalg;
  }
  err= spf(&A, &v, NULL, NULL, NULL);
  if(err!= NO_ERROR)
  {
    goto WLe_gen_subalg;
  }
  err = wr_vectors(outf, &v);
  if(err!= NO_ERROR)
  {
    goto WLe_gen_subalg;
  }
  free_alg(&A);
  free_vectors(&v);
  print_spclose("End of generating a subpower.");
  return(NO_ERROR);

WLe_gen_subalg:
  free_alg(&A);
  free_vectors(&v);
  return(err);
}

static int
cr_subpr_alg( FILE *fp )
{
  ALG *algs;
  ALG P;
  VECTORS v;
  int i, err, errf;
  char outf[NAME_LEN_LIMIT+1];
  int *sizes;

  errf=NO_ERROR;
  print_spopen("Creating an algebra in a subproduct.");
  v.list= (VEC *)NULL;
  P.func=NULL;
  sizes=NULL;
  algs=NULL;
/* These lines for free_ to work properly */

  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    errf=ERROR;
    goto WLe_cr_subpr_alg;
  }
  err = ps_vectors(name, &v, ALG_SIZE_LIMIT); /* must have a third arg. */
  if(err!= NO_ERROR)
  {
    errf=ERROR;
    hlp_cr_subpr_alg();
    goto WLe_cr_subpr_alg;
  }
  if(v.lvecs>=PROD_LIMIT)
  {
    print_derr("Illegal number %d of components (should be at most %d).",
                                                v.lvecs, PROD_LIMIT-1 );
    errf=ERROR;
    goto WLe_cr_subpr_alg;
  }
  if(v.nvecs == 0)
  {
    print_dpres("The universe of this algebra is empty, it cannot be created.");
    errf=ERROR;
    goto WLe_cr_subpr_alg;
  }
/* allocate room for algebras */
  algs= (ALG *)(calloc(v.lvecs,sizeof(ALG)));
  if(algs== NULL) {
    print_derr("Not enough memory to store algebras.");
    errf=ERROR;
    hlp_cr_subpr_alg();
    goto WLe_cr_subpr_alg;
  }
  for(i=0; i<v.lvecs; i++) {
    (algs[i]).func= (FUNCT *)NULL;
  }
  for(i=0; i<v.lvecs; i++) {
    if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
      errf=ERROR;
      goto WLe_cr_subpr_alg;
    }
    err= ps_alg(name, &(algs[i]));
    if(err!= NO_ERROR)
    {
      errf=ERROR;
      print_nerr("%d algebra description lines needed.", v.lvecs);
      hlp_cr_subpr_alg();
      goto WLe_cr_subpr_alg;
    }
  }
/* algebras read, now check vector entries */
  sizes= (int *) (calloc(v.lvecs,sizeof(int)));
  if(sizes== NULL) {
    print_derr("Not enough memory to check vector entries.");
    errf=ERROR;
    hlp_cr_subpr_alg();
    goto WLe_cr_subpr_alg;
  }
  for(i=0; i<v.lvecs; i++) {
    sizes[i]=(algs[i]).size;
  }
  err=check_vectors(&v,sizes);
  if(err!= NO_ERROR)
  {
    errf=ERROR;
    goto WLe_cr_subpr_alg;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    errf=ERROR;
    goto WLe_cr_subpr_alg;
  }
  err=ps_out(name, outf);
  if(err != NO_ERROR)
  {
    errf=ERROR;
    print_nerr("%d algebra lines and an output line needed.", v.lvecs);
    hlp_cr_subpr_alg();
    hlp_alg_file();
    goto WLe_cr_subpr_alg;
  }
  err= sub_prod_alg(algs, &v, &P);
  if(err!= NO_ERROR)
  {
    errf=ERROR;
    goto WLe_cr_subpr_alg;
  }
  err = wr_alg(outf, &P);
  if(err!= NO_ERROR)
  {
    errf=ERROR;
    goto WLe_cr_subpr_alg;
  }
WLe_cr_subpr_alg:
  if(sizes!=NULL) free((void *)sizes);
  if(algs!=NULL) {
    for(i=0; i<v.lvecs; i++) {
      free_alg(&(algs[i]));
    }
    free((void *)algs);
  }
  free_alg(&P);
  free_vectors(&v);
  if(errf==NO_ERROR) {
    print_spclose("End of creating an algebra in a subproduct.");
  }
  return(errf);
}

static int
cr_prod_cong( FILE *fp )
{
  VECTORS v;
  PART *fact_c;
  PARTS res;
  int i, err, errf;
  char outf[NAME_LEN_LIMIT+1];
  int *sizes;

  errf=NO_ERROR;
  print_spopen("Creating a product congruence.");
  v.list= (VEC *)NULL;
  res.list=NULL;
  fact_c=NULL;
  sizes=NULL;
/* These lines for free_ to work properly */

  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    errf=ERROR;
    goto WLe_cr_prod_cong;
  }
  err = ps_vectors(name, &v, ALG_SIZE_LIMIT); /* must have a third arg. */
  if(err!= NO_ERROR)
  {
    errf=ERROR;
    hlp_cr_prod_cong();
    goto WLe_cr_prod_cong;
  }
  if(v.lvecs>=PROD_LIMIT)
  {
    print_derr("Illegal number %d of components (should be at most %d).",
                                                v.lvecs, PROD_LIMIT-1 );
    errf=ERROR;
    goto WLe_cr_prod_cong;
  }
/* compute the sizes of the components */
  sizes= (int *) (calloc(v.lvecs,sizeof(int)));
  if(sizes== NULL) {
    print_derr("Not enough memory to store component sizes.");
    errf=ERROR;
    hlp_cr_prod_cong();
    goto WLe_cr_prod_cong;
  }
  maxc_vectors(&v,&sizes);
/* allocate room for partitions */
  fact_c= (PART *)(calloc(v.lvecs,sizeof(PART)));
  if( fact_c == NULL) {
    print_derr("Not enough memory to store partitions.");
    errf=ERROR;
    hlp_cr_prod_cong();
    goto WLe_cr_prod_cong;
  }
  for(i=0; i<v.lvecs; i++) {
    (fact_c[i]).func= NULL;
  }
  for(i=0; i<v.lvecs; i++) {
    if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
      errf=ERROR;
      goto WLe_cr_prod_cong;
    }
    err = ps_cong(name, &(fact_c[i]), -sizes[i]); /* don't know exact size */
    if(err!= NO_ERROR)
    {
      errf=ERROR;
      print_nerr("%d congruence description lines needed.", v.lvecs);
      hlp_cr_prod_cong();
      goto WLe_cr_prod_cong;
    }
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    errf=ERROR;
    goto WLe_cr_prod_cong;
  }
  err=ps_out(name, outf);
  if(err != NO_ERROR)
  {
    errf=ERROR;
    print_nerr("%d congruence lines and an output line needed.", v.lvecs);
    hlp_cr_prod_cong();
    hlp_conlist_file();
    goto WLe_cr_prod_cong;
  }
  if ((res.list= (VEC *)(calloc(1,sizeof(VEC))))== NULL)
  {
    errf=ERROR;
    print_derr("Not enough memory to point to product congruence.");
    hlp_cr_prod_cong();
    goto WLe_cr_prod_cong;
  }
  err= prod_cong(&v, fact_c, &((res.list)[0]) );
  if(err!= NO_ERROR)
  {
    errf=ERROR;
    goto WLe_cr_prod_cong;
  }
  res.nparts= 1;
  res.lparts= v.nvecs;
  err = wr_conlist(outf, &res);
  if(err!= NO_ERROR)
  {
    errf=ERROR;
    goto WLe_cr_prod_cong;
  }

WLe_cr_prod_cong:
  if(sizes!=NULL) free((void *)sizes);
  if(fact_c!=NULL) {
    for(i=0; i<v.lvecs; i++) {
      if( (fact_c[i]).func!= NULL ) free((void *)((fact_c[i]).func));
    }
    free((void *)fact_c);
  }
  free_vectors(&v);
  free_conlist(&res);
  if(errf==NO_ERROR) {
    print_spclose("End of creating a product congruence.");
  }
  return(errf);
}

static int
cr_dir_prod( FILE *fp )
{
  ALG *algs;
  ALG P;
  VECTORS v;
  int i, err, errf;
  char outf[NAME_LEN_LIMIT+1];
  char outuni[NAME_LEN_LIMIT+1];

  errf=NO_ERROR;
  print_spopen("Creating a direct product.");
  v.list= (VEC *)NULL;
  P.func=NULL;
  algs=NULL;
/* These lines for free_ to work properly */

  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    errf=ERROR;
    goto WLe_dir_prod;
  }
  err = ps_number(name, 0, PROD_LIMIT-1, &(v.lvecs));
  if(err!= NO_ERROR)
  {
    errf=ERROR;
    hlp_dir_prod();
    goto WLe_dir_prod;
  }
  v.nvecs=0; /* unnecessary */
/* allocate room for algebras */
  algs= (ALG *)(calloc(v.lvecs,sizeof(ALG)));
  if(algs== NULL) {
    print_derr("Not enough memory to store algebras.");
    errf=ERROR;
    hlp_dir_prod();
    goto WLe_dir_prod;
  }
  for(i=0; i<v.lvecs; i++) {
    (algs[i]).func= (FUNCT *)NULL;
  }
  for(i=0; i<v.lvecs; i++) {
    if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
      errf=ERROR;
      goto WLe_dir_prod;
    }
      err= ps_alg(name, &(algs[i]));
    if(err!= NO_ERROR)
    {
      errf=ERROR;
      print_nerr("%d algebra description lines needed.", v.lvecs);
      hlp_dir_prod();
      goto WLe_dir_prod;
    }
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    errf=ERROR;
    goto WLe_dir_prod;
  }
  err=ps_out(name, outf);
  if(err != NO_ERROR)
  {
    errf=ERROR;
    print_nerr("%d algebra lines and two output lines needed.", v.lvecs);
    hlp_dir_prod();
    hlp_alg_file();
    goto WLe_dir_prod;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    errf=ERROR;
    goto WLe_dir_prod;
  }
  err=ps_out(name, outuni);
  if(err != NO_ERROR)
  {
    errf=ERROR;
    print_nerr("%d algebra lines and two output lines needed.", v.lvecs);
    hlp_dir_prod();
    hlp_vector_file();
    goto WLe_dir_prod;
  }
  err= sub_prod_alg(algs, &v, &P);
  if(err!= NO_ERROR)
  {
    errf=ERROR;
    goto WLe_dir_prod;
  }
  err = wr_alg(outf, &P);
  if(err!= NO_ERROR)
  {
    errf=ERROR;
    goto WLe_dir_prod;
  }
  err = wr_vectors(outuni, &v);
  if(err!= NO_ERROR)
  {
    errf=ERROR;
    goto WLe_dir_prod;
  }

WLe_dir_prod:
  if(algs!=NULL) {
    for(i=0; i<v.lvecs; i++) {
      free_alg(&(algs[i]));
    }
    free((void *)algs);
  }
  free_alg(&P);
  free_vectors(&v);
  if(errf==NO_ERROR) {
    print_spclose("End of creating a direct product.");
  }
  return(errf);
}

static int
cr_fact_alg( FILE *fp )
{
  ALG A;
  ALG F;
  PART cong;
  int err;
  char outf[NAME_LEN_LIMIT+1];

  A.func= NULL;
  F.func= NULL;
  cong.func= NULL;

  print_spopen("Creating factor algebra.");
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_cr_fact_alg;
  }
  err= ps_alg(name, &A);
  if(err!= NO_ERROR)
  {
    hlp_cr_fact_alg();
    goto WLe_cr_fact_alg;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_cr_fact_alg;
  }
  err = ps_cong(name, &cong, A.size);
  if(err != NO_ERROR) {
    hlp_cr_fact_alg();
    goto WLe_cr_fact_alg;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_cr_fact_alg;
  }
  err=ps_out(name, outf);
  if(err != NO_ERROR)
  {
    hlp_cr_fact_alg();
    hlp_alg_file();
    goto WLe_cr_fact_alg;
  }
  err= factor_alg(&A, cong.func, &F );
  if(err!= NO_ERROR)
  {
    goto WLe_cr_fact_alg;
  }
  err = wr_alg(outf, &F);
  if(err!= NO_ERROR)
  {
    goto WLe_cr_fact_alg;
  }
  free_alg(&A);
  free_alg(&F);
  if(cong.func!=NULL) free((void *)(cong.func));
  print_spclose("End of creating factor algebra.");
  return(NO_ERROR);

WLe_cr_fact_alg:
  free_alg(&A);
  free_alg(&F);
  if(cong.func!=NULL) free((void *)(cong.func));
  return(err);
}

static int
gen_cong( FILE *fp )
{
  ALG A;
  VECTORS vecps;
  PARTS cong;
  int err;
  char outf[NAME_LEN_LIMIT+1];

  print_spopen("Generating a congruence.");
  A.func= (FUNCT *)NULL;
  vecps.list= (VEC *)NULL;
  cong.list= (VEC *)NULL;

  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_gen_cong;
  }
  err= ps_alg(name, &A);
  if(err!= NO_ERROR)
  {
    hlp_gen_cong();
    goto WLe_gen_cong;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_gen_cong;
  }
  err = ps_vectors(name, &vecps, A.size);
  if(err!= NO_ERROR)
  {
    hlp_gen_cong();
    goto WLe_gen_cong;
  }
  if(vecps.lvecs != 2 )
  {
    print_derr("The length of vectors in the `pairs' file must be 2,\
 %d is illegal.", vecps.lvecs);
    hlp_gen_cong();
    goto WLe_gen_cong;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_gen_cong;
  }
  err=ps_out(name, outf);
  if(err != NO_ERROR)
  {
    hlp_gen_cong();
    hlp_conlist_file();
    goto WLe_gen_cong;
  }

  if ((cong.list= (VEC *)(calloc(1,sizeof(VEC))))== NULL)
  {
    print_derr("Not enough memory to point to congruence.");
    hlp_gen_cong();
    goto WLe_gen_cong;
  }
  err= comp_cong(&A, &vecps, &((cong.list)[0]) );
  if(err!= NO_ERROR)
  {
    goto WLe_gen_cong;
  }

  cong.nparts= 1;
  cong.lparts= A.size;
  err = wr_conlist(outf, &cong);
  if(err!= NO_ERROR)
  {
    goto WLe_gen_cong;
  }
  free_alg(&A);
  free_vectors(&vecps);
  free_conlist(&cong);
  print_spclose("End of generating a congruence.");
  return(NO_ERROR);

WLe_gen_cong:
  free_alg(&A);
  free_vectors(&vecps);
  free_conlist(&cong);
  return(err);
}

static int
gen_conlat( FILE *fp )
{
  ALG A, L;
  PARTS congs, mi;
  int err;
  char outlat[NAME_LEN_LIMIT+1];
  char outcon[NAME_LEN_LIMIT+1];
  char outprin[NAME_LEN_LIMIT+1];
  char outmi[NAME_LEN_LIMIT+1];

  A.func= (FUNCT *)NULL;
  L.func= (FUNCT *)NULL;
  congs.list= (VEC *)NULL;
  mi.list= (VEC *)NULL;

  print_spopen("Computing a congruence lattice.");
  A.func= (FUNCT *)NULL;

  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_gen_conlat;
  }
  err= ps_alg(name, &A);
  if(err!= NO_ERROR)
  {
    hlp_gen_conlat();
    goto WLe_gen_conlat;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_gen_conlat;
  }
  if( ps_out(name, outlat) != NO_ERROR ) {
    hlp_gen_conlat();
    hlp_alg_file();
    goto WLe_gen_conlat;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_gen_conlat;
  }
  if( ps_out(name, outcon) != NO_ERROR ) {
    hlp_gen_conlat();
    hlp_conlist_file();
    goto WLe_gen_conlat;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_gen_conlat;
  }
  if( ps_out(name, outprin) != NO_ERROR ) {
    hlp_gen_conlat();
    hlp_conlist_file();
    goto WLe_gen_conlat;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_gen_conlat;
  }
  if( ps_out(name, outmi) != NO_ERROR ) {
    hlp_gen_conlat();
    hlp_conlist_file();
    goto WLe_gen_conlat;
  }
  err=comp_all_prin_cong( &A, &congs );
  if(err!= NO_ERROR)
  {
    goto WLe_gen_conlat;
  }
  err=wr_conlist(outprin, &congs );
  if(err!= NO_ERROR)
  {
    goto WLe_gen_conlat;
  }
  err=comp_conlat( &A, &L, &congs );
  if(err!= NO_ERROR)
  {
    goto WLe_gen_conlat;
  }
  err=wr_conlist(outcon, &congs );
  if(err!= NO_ERROR)
  {
    goto WLe_gen_conlat;
  }
  if(L.func != NULL) {
    err=wr_alg(outlat, &L );
    if(err!= NO_ERROR)
    {
      goto WLe_gen_conlat;
    }
  }
  err=comp_mi_congs( &congs, &mi );
  if(err!= NO_ERROR)
  {
    goto WLe_gen_conlat;
  }
  err=wr_conlist(outmi, &mi );
  if(err!= NO_ERROR)
  {
    goto WLe_gen_conlat;
  }
  free_alg(&A);
  free_alg(&L);
  free_conlist(&congs);
  free_conlist(&mi);
  print_spclose("End of computing a congruence lattice.");
  return(NO_ERROR);

WLe_gen_conlat:
  free_alg(&A);
  free_alg(&L);
  free_conlist(&congs);
  free_conlist(&mi);
  return(err);
}

static int
un_pol( FILE *fp )
{
  ALG A;
  VECTORS un, id;
  int err;
  char outun[NAME_LEN_LIMIT+1];
  char outid[NAME_LEN_LIMIT+1];

  A.func= (FUNCT *)NULL;
  un.list= (VEC *)NULL;
  id.list= (VEC *)NULL;
  A.func= (FUNCT *)NULL;

  print_spopen("Computing unary polynomials.");

  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_un_pol;
  }
  err= ps_alg(name, &A);
  if(err!= NO_ERROR)
  {
    hlp_un_pol();
    goto WLe_un_pol;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_un_pol;
  }
  if( ps_out(name, outun) != NO_ERROR ) {
    hlp_un_pol();
    hlp_vector_file();
    goto WLe_un_pol;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_un_pol;
  }
  if( ps_out(name, outid) != NO_ERROR ) {
    hlp_un_pol();
    hlp_vector_file();
    goto WLe_un_pol;
  }
  err=comp_un_pol( &A, &un, &id );
  if(err!= NO_ERROR)
  {
    goto WLe_un_pol;
  }
  err=wr_vectors(outun, &un );
  if(err!= NO_ERROR)
  {
    goto WLe_un_pol;
  }
  err=wr_vectors(outid, &id );
  if(err!= NO_ERROR)
  {
    goto WLe_un_pol;
  }
  free_alg(&A);
  free_vectors(&un);
  free_vectors(&id);
  print_spclose("End of computing unary polynomials.");
  return(NO_ERROR);

WLe_un_pol:
  free_alg(&A);
  free_vectors(&un);
  free_vectors(&id);
  return(err);
}

static int
is_centr( FILE *fp, int centr)
{
  ALG A;
  VECTORS left;
  VECTORS right;
  PART cong;
  PARTS conlist;
  int err;

  A.func= NULL;
  left.list= NULL;
  right.list= NULL;
  cong.func= NULL;
  conlist.list= NULL;

  switch(centr) {
    case CENTR_NORMAL :
      print_spopen("Checking centrality C(left, right, cong).");
      break;
    case CENTR_WEAK :
      print_spopen("Checking weak centrality CW(left, right, cong).");
      break;
    case CENTR_STRONG :
      print_spopen("Checking strong centrality C*(left, right, cong).");
      break;
    case CENTR_RECT :
      print_spopen("Checking rectangulation CR(left, right, cong).");
      break;
    default:
      print_derr("Internal error: invalid centr value.");
      exit(1);
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_is_centr;
  }
  err= ps_alg(name, &A);
  if(err!= NO_ERROR)
  {
    hlp_is_centr(centr);
    goto WLe_is_centr;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_is_centr;
  }
  err = ps_binrel_line(name, &left, A.size);
  if(err != NO_ERROR) {
    hlp_is_centr(centr);
    goto WLe_is_centr;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_is_centr;
  }
  err = ps_binrel_line(name, &right, A.size);
  if(err != NO_ERROR) {
    hlp_is_centr(centr);
    goto WLe_is_centr;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_is_centr;
  }
  err = ps_con_or_list(name, &cong, &conlist, A.size);
  if(err != NO_ERROR) {
    hlp_is_centr(centr);
    goto WLe_is_centr;
  }
  err= is_centrf(&A, &left, &right, cong.func, &conlist, centr );
  if(err!= NO_ERROR)
  {
    goto WLe_is_centr;
  }
  free_alg(&A);
  free_vectors(&left);
  free_vectors(&right);
  if(cong.func!=NULL) free((void *)(cong.func));
  free_conlist(&conlist);
  switch(centr) {
    case CENTR_NORMAL :
      print_spclose("End of checking centrality.");
      break;
    case CENTR_WEAK :
      print_spclose("End of checking weak centrality.");
      break;
    case CENTR_STRONG :
      print_spclose("End of checking strong centrality.");
      break;
    case CENTR_RECT :
      print_spclose("End of checking rectangulation.");
      break;
    default:
      print_derr("Internal error: invalid centr value.");
      exit(1);

  }
  return(NO_ERROR);

WLe_is_centr:
  free_alg(&A);
  free_vectors(&left);
  free_vectors(&right);
  if(cong.func!=NULL) free((void *)(cong.func));
  free_conlist(&conlist);
  return(err);
}


static int
rlr( FILE *fp )
{
  ALG A;
  VECTORS left;
  VECTORS right;
  PART cong;
  int err;
  VECTORS rlr_res;
  char outf[NAME_LEN_LIMIT+1];

  A.func= NULL;
  left.list= NULL;
  right.list= NULL;
  cong.func= NULL;
  rlr_res.list=NULL;

  print_spopen("Computing R(left, right) modulo cong.");
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_rlr;
  }
  err= ps_alg(name, &A);
  if(err!= NO_ERROR)
  {
    hlp_rlr();
    goto WLe_rlr;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_rlr;
  }
  err = ps_binrel_line(name, &left, A.size);
  if(err != NO_ERROR) {
    hlp_rlr();
    goto WLe_rlr;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_rlr;
  }
  err = ps_binrel_line(name, &right, A.size);
  if(err != NO_ERROR) {
    hlp_rlr();
    goto WLe_rlr;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_rlr;
  }
  err = ps_cong(name, &cong, A.size);
  if(err != NO_ERROR) {
    hlp_rlr();
    goto WLe_rlr;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_rlr;
  }
  err=ps_out(name, outf);
  if(err != NO_ERROR)
  {
    hlp_rlr();
    hlp_vector_file();
    goto WLe_rlr;
  }
  err= wr_rlr(&A, &left, &right, cong.func, &rlr_res );
  if(err!= NO_ERROR)
  {
    goto WLe_rlr;
  }
  err = wr_vectors(outf, &rlr_res);
  if(err!= NO_ERROR)
  {
    goto WLe_rlr;
  }
  free_alg(&A);
  free_vectors(&left);
  free_vectors(&right);
  if(cong.func!=NULL) free((void *)(cong.func));
  free_vectors(&rlr_res);
  print_spclose("End of computing R(L,R).");
  return(NO_ERROR);

WLe_rlr:
  free_alg(&A);
  free_vectors(&left);
  free_vectors(&right);
  if(cong.func!=NULL) free((void *)(cong.func));
  free_vectors(&rlr_res);
  return(err);
}

static int
min_sets( FILE *fp )
{
  ALG A;
  PART alpha;
  PART beta;
  int err;
  VECTORS msets;
  char outf[NAME_LEN_LIMIT+1];

  A.func= NULL;
  alpha.func= NULL;
  beta.func= NULL;
  msets.list=NULL;

  print_spopen("Computing minimal sets for a quotient.");
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_min_sets;
  }
  err= ps_alg(name, &A);
  if(err!= NO_ERROR)
  {
    hlp_min_sets();
    goto WLe_min_sets;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_min_sets;
  }
  err = ps_cong(name, &alpha, A.size);
  if(err != NO_ERROR) {
    hlp_min_sets();
    goto WLe_min_sets;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_min_sets;
  }
  err = ps_cong(name, &beta, A.size);
  if(err != NO_ERROR) {
    hlp_min_sets();
    goto WLe_min_sets;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_min_sets;
  }
  err=ps_out(name, outf);
  if(err != NO_ERROR)
  {
    hlp_min_sets();
    hlp_vector_file();
    goto WLe_min_sets;
  }
  err= min_sets_np1(&A, alpha.func, beta.func, &msets );
  if(err!= NO_ERROR)
  {
    goto WLe_min_sets;
  }
  err = wr_vectors(outf, &msets);
  if(err!= NO_ERROR)
  {
    goto WLe_min_sets;
  }
  free_alg(&A);
  if(alpha.func!=NULL) free((void *)(alpha.func));
  if(beta.func!=NULL) free((void *)(beta.func));
  free_vectors(&msets);
  print_spclose("End of computing minimal sets.");
  return(NO_ERROR);

WLe_min_sets:
  free_alg(&A);
  if(alpha.func!=NULL) free((void *)(alpha.func));
  if(beta.func!=NULL) free((void *)(beta.func));
  free_vectors(&msets);
  return(err);
}

static int
type_q( FILE *fp )
{
  ALG A;
  PART alpha;
  PART beta;
  int type;
  int err;

  A.func= NULL;
  alpha.func= NULL;
  beta.func= NULL;

  print_spopen("Computing the type of a quotient.");
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_type_q;
  }
  err= ps_alg(name, &A);
  if(err!= NO_ERROR)
  {
    hlp_type_q();
    goto WLe_type_q;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_type_q;
  }
  err = ps_cong(name, &alpha, A.size);
  if(err != NO_ERROR) {
    hlp_type_q();
    goto WLe_type_q;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_type_q;
  }
  err = ps_cong(name, &beta, A.size);
  if(err != NO_ERROR) {
    hlp_type_q();
    goto WLe_type_q;
  }
  err= type_quotient(&A, alpha.func, beta.func, &type );
  if(err!= NO_ERROR)
  {
    goto WLe_type_q;
  }
  free_alg(&A);
  if(alpha.func!=NULL) free((void *)(alpha.func));
  if(beta.func!=NULL) free((void *)(beta.func));
  print_spclose("End of computing type.");
  return(NO_ERROR);

WLe_type_q:
  free_alg(&A);
  if(alpha.func!=NULL) free((void *)(alpha.func));
  if(beta.func!=NULL) free((void *)(beta.func));
  return(err);
}

static int
label_conlat( FILE *fp )
{
  ALG A;
  PARTS conlat;
  VECTORS types;
  int err;
  char outf[NAME_LEN_LIMIT+1];

  A.func= NULL;
  conlat.list=NULL;
  types.list=NULL;

  print_spopen("Labeling a congruence lattice.");
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_label_conlat;
  }
  err= ps_alg(name, &A);
  if(err!= NO_ERROR)
  {
    hlp_label_conlat();
    goto WLe_label_conlat;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_label_conlat;
  }
  err = ps_conlist(name, &conlat, A.size);
  if(err != NO_ERROR) {
    hlp_label_conlat();
    goto WLe_label_conlat;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_label_conlat;
  }
  err=ps_out(name, outf);
  if(err != NO_ERROR)
  {
    hlp_label_conlat();
    hlp_type_file();
    goto WLe_label_conlat;
  }
  err= type_conlat(&A, &conlat, &types );
  if(err!= NO_ERROR)
  {
    goto WLe_label_conlat;
  }
  err = wr_typelist(outf, &types);
  if(err!= NO_ERROR)
  {
    goto WLe_label_conlat;
  }
  free_alg(&A);
  free_conlist(&conlat);
  free_vectors(&types);
  print_spclose("End of labeling a congruence lattice.");
  return(NO_ERROR);

WLe_label_conlat:
  free_alg(&A);
  free_conlist(&conlat);
  free_vectors(&types);
  return(err);
}

static int
find_congnum( FILE *fp )
{
  PART cong;
  PARTS conlist;
  int err;

  cong.func=NULL;
  conlist.list=NULL;

  print_spopen("Looking for a congruence number.");
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_find_congnum;
  }
  err = ps_conlist(name, &conlist, -1); /* we don't know the size */
  if(err != NO_ERROR) {
    hlp_find_congnum();
    goto WLe_find_congnum;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_find_congnum;
  }
  err = ps_cong(name, &cong, conlist.lparts);
  if(err != NO_ERROR)
  {
    hlp_find_congnum();
    goto WLe_find_congnum;
  }
  err= find_con( cong.func, &conlist );
  if(err== ERROR)
  {
    goto WLe_find_congnum;
  }
  if(err==NOT_FOUND) {
    print_dpres("RESULT: The given congruence is not on the list.");
  } else { /* found */
    print_dpres("RESULT: The given congruence is C%d.", err);
  }
  free_conlist(&conlist);
  if(cong.func!=NULL) free((void *)(cong.func));
  print_spclose("End of looking for a congruence number.");
  return(NO_ERROR);

WLe_find_congnum:
  free_conlist(&conlist);
  if(cong.func!=NULL) free((void *)(cong.func));
  return(err);
}

static int
kkvm_germ( FILE *fp )
{
  ALG A;
  VECTORS v;
  VECTORS rtt;
  int err;

  print_spopen("Looking for kkvm-germs.");
  A.func= (FUNCT *)NULL;
  v.list= (VEC *)NULL;

  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_kkvm_germ;
  }
  err= ps_alg(name, &A);
  if(err!= NO_ERROR)
  {
    hlp_kkvm_germ();
    goto WLe_kkvm_germ;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_kkvm_germ;
  }
  err = ps_binrel_line(name, &v, A.size);
  if(err!= NO_ERROR)
  {
    hlp_kkvm_germ();
    goto WLe_kkvm_germ;
  }
  if( (err = get_next_line(name, NAME_LEN_LIMIT, fp)) != NO_ERROR) {
    goto WLe_kkvm_germ;
  }
  err = ps_binrel_line(name, &rtt, A.size);
  if(err!= NO_ERROR)
  {
    hlp_kkvm_germ();
    goto WLe_kkvm_germ;
  }

  err= find_kkvm_germ(&A, &v, &rtt);
  if(err!= NO_ERROR)
  {
    goto WLe_kkvm_germ;
  }
  free_alg(&A);
  free_vectors(&v);
  print_spclose("End of looking for kkvm germs.");
  return(NO_ERROR);

WLe_kkvm_germ:
  free_alg(&A);
  free_vectors(&v);
  return(err);
}

