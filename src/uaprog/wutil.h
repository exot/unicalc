#define HASH_SIZE 8192
#define		MULT0		3141
#define		MULT1		5926
#define		MULT2		5358
#define		MULT3		9793
#define		MULT4		2384
#define		MULT5		6264
#define		MULT6		3383
#define		MULT7		2795
#define		MULT8		288
#define		MULT9		4197
#define		MULT10		1693
#define		MULT11		9937
#define		MULT12		5105
#define		MULT13		8209
#define		MULT14		7494
#define		MULT15		4592

/* input utilities */
int is_eof(FILE *);
int comp_command_name(char *, char *);
char *get_command_arg(char *, char *);
int get_next_line(char *, int, FILE * );
int get_integer(FILE *, char * );

/* The various print_ routines can be categorized thus:
         (the possible combinations do not all exist)
   1. Attention getting level:
        --  double prompt, ??   letter: d    (like print_dhlp())
        --  single prompt,  ?   letter: s    (like print_shlp())
        --  no prompt,          letter: n    (like print_nhlp())
   2. Type:
        -- help message: hlp
        -- error message: err (note: serr is WARNING)
        -- runtime progress message: r
           Subtypes:
             -- short: short (just prints a dot)
             -- progress ratio: ratio
                (the format depending on #if defined(TIME_DIAG) )
        -- process progress message: p
           Subtypes:
             -- open process: open
             -- input data: input
             -- doing process: do
             -- close process: close
        -- result message: pres

   NOTE: print_spresl()=print_spres(), but the log_level is one bigger.
         It is used to print results of secondary importance.
	 Similarly, print_dpresh() and print_npresh()
	 have log_level one smaller.
     3. Indentation:
        Normally, indentation is printed with \n at the beginning.
        The exceptions are print_pres and print_proc.
        These are used when a line is assembled by parts.

  Altogether four types:
   PROC_PROGR_DEV
   RUN_PROGR_DEV
   ERR_DEV
   RESULT_DEV
  Help goes to ERR_DEV.

  Output strategy:
    We write to five places.
      1. to a log file.
      2. to the standard output place (Event).
      3. to the standard error place.
      4. to the run time progress place.
      5. to the results place and file.
    If there is a GUI, then 3=4, the others are all different.
    If there is no GUI then 2, 3 and 4 become the same,
    which requires some care to avoid sync problems.

    The user can regulate the level of output to the log file.

    The logo is printed to both stderr and stdout.
    All help and error messages are printed to both stderr and stdout.
    Run time progress messages are printed only to stderr.

    The printing of process progress messages depends on their level.
    popen  increases log_level;
    pclose decreases log_level;
    Process progress messages are printed to log if and only if
    their current level is >= log_level_limit (set by the user).
    They are printed to Event if and only if their level 
    is <= EVENT_LEVEL_LIMIT (it is 1).
    Similarly for result messages.

    Indentation.
    Error and help messages are top level indented.
    Process progress messages and _rratio are indented
    according to their level. (log_level=1 : top level).
    _rshort prints a single dot.
*/

void print_dhlp(char *, ...);
void print_shlp(char *, ...);
void print_nhlp(char *, ...);

void print_derr(char *, ...);
void print_serr(char *, ...);
void print_nerr(char *, ...);

void print_spopen(char *, ...);
void print_spclose(char *, ...);
void print_spdo(char *, ...);
void print_spinput(char *, ...);
void print_npinput(char *, ...);
void print_dpres(char *, ...);
void print_spres(char *, ...);
void print_spresl(char *, ...);
void print_dpresh(char *, ...);
void print_npresh(char *, ...);
void print_npres(char *, ...);
void print_r_values( int, int, int, int, VEC);
void print_r_vector( int, VEC);
void print_r_set( int, VEC);
void print_r_cong( int, VEC);

#ifdef RUN_PROGRESS
void print_rratio(char *, ...);
#else
void print_rshort( void );
#endif /* RUN_PROGRESS */

#ifdef RUN_PROGRESS
void print_time_progr( 
#ifdef TIME_DIAG
     time_t , time_t *, 
#endif /* TIME_DIAG */
     double , char * );
#ifdef TIME_DIAG
int time2str( double , char * );
#endif /* TIME_DIAG */
#endif /* RUN_PROGRESS */

/* string utilities */
int append_alloc_str( char **, char *);
int cut_tr_n(char *);

/* utilities to maintain data structures */
int find_vec( VEC , VECTORS * );
int add_vec( VEC *, VECTORS * );
int concat_vec( VEC , VECTORS * );
int concat_arg_st(ARGUMENTS *, ARGSLIST *);
int find_con( VEC , PARTS * );
int add_con( VEC *, PARTS *);
int add_consts( VECTORS *, int );
int cr_vectors(VECTORS *, int * );
int check_vectors(VECTORS *, int * );
void maxc_vectors(VECTORS *, int ** );

/* converters (the congruence utilities are in WCONG) */
int find_range( int , VEC , VEC );

/* utilities to free data structures */
void free_alg(ALG *);
void free_vectors( VECTORS * );
void free_conlist( PARTS * );
void free_argslist( ARGSLIST * );

/* algebras and functions */
int get_max_arity( ALG * );
int get_func_value(int , int , int *, int *);
void set_func_value(int , int , int *, int *, int );

/* misc. */
int hash( VEC , int );
int intpow(int, int);
int print_part( int , VEC , char ** );

