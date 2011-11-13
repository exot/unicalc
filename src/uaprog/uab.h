#define YES            -22
#define NO             -23

/* all messages are negative, so a function can return information
   simultaneously with an error message */

#define  NO_ERROR        -1
#define  ERROR           -2
#define  ABORT           -3
#define  NOT_FOUND       -4
#define  NOT_SUBUNIVERSE -5
#define  UNDEFINED       -6
#define  BAD_NUMBER      -7
#define  LONG_LINE       -8
#define  EQUAL           -9
#define  NOT_EQUAL      -10

#define  CENTR_WEAK     -11
#define  CENTR_NORMAL   -12
#define  CENTR_STRONG   -13
#define  CENTR_RECT     -14

#define  NOTYP 0
#define  UTYP 1
#define  ATYP 2
#define  BTYP 3
#define  LTYP 4
#define  STYP 5

#define  O_EQUAL     -15
#define  O_INCOMP    -16
#define  O_S_COVER   -17
#define  O_B_COVER   -18
#define  O_S_N_COVER -19
#define  O_B_N_COVER -20

#define  INTERRUPT      -21

#define  EOS       (char)0

#if defined(BIG_INT)
#define POWER_LIMIT  10000000  /* max size of a computed power */
#define ALG_SIZE_LIMIT  10000  /* max size of the underlying set of an alg.*/
#else
#define POWER_LIMIT     32000  /* max size of a computed power */
#define ALG_SIZE_LIMIT    166  /* max size of the underlying set of an alg.*/
#endif

#define ARITY_LIMIT        10  /* max arity of a basic operation */
#define PROD_LIMIT        200  /* max # of components in a dir. prod. */
#define CONLAT_SIZE_LIMIT ALG_SIZE_LIMIT 
                           /* max number of congruences of an algebra */
#define VEC_LEN_LIMIT  ALG_SIZE_LIMIT /* max length of a vector or part.*/
#define VEC_NUM_LIMIT  POWER_LIMIT    /* max number of vectors in an array */


#define NAME_LEN_LIMIT    300  /* max length of a file or command name */
#define NUM_LEN_LIMIT      20  /* max length of an integer */

#ifdef RUN_PROGRESS
#ifdef TIME_DIAG
#define WSPF_PROGR_TICKS   2000 /* Regulates the frequency of progress output
                                   to stedrr in Berman's routine. A bigger
                                   number gives less frequent output. */
#define JM_PROGR_TICKS      500 /* same in join or meet computing routine */
#define PROGR_TIME_OUT (double)10  /* The number of seconds after which a
                                      process should print a new progress
                                      report message. */

#define TIME_LIMIT  (double)1000000000 /* a little over 31 years */
/* if a time is smaller than this, then we can convert it to a long */
#else
#define WSPF_PROGR_TICKS  30000 /* Regulates the frequency of progress output
                                   to stedrr in Berman's routine. A bigger
                                   number gives less frequent output. */
#endif /* TIME_DIAG */
#endif /* RUN_PROGRESS */

/* Global variables */

EXTERN int linenum;
/* in the parameter file. Later, this won't be global. */

EXTERN FILE *log_file_p;
EXTERN FILE *results_file_p;
EXTERN int log_level; /* measures, how deep we are in the program */
EXTERN int log_level_limit; /* if we are not deeper than this, log is written */
EXTERN int GUI_mode; /* YES if we are under a GUI, NO otherwise */
EXTERN int StopIt; /* YES if an INTERRUPT is requested, NO otherwise */
#define LOG_LEVEL_DEFAULT 2
#define PROC_LEVEL_LIMIT 1           /* limit for printing to event */
#define RESULT_LEVEL_LIMIT 1           /* limit for printing to event/result */
#define LEVEL_INDENT_STRING " ."   
                  /* string repeated at the beginnig of the lines */
/* indexing a two-dimensional dynamic array */

#define BIN_ARR_INDEX(a,b) ((a)+(BIN_ARR_SIZE)*(b))
/* Is there a better way to access elements of b[][],
   where the size of this array is not known in advance?
   We want a macro for speed, and a readable notation. */

