/* functions for main commands */
int comp_cong( ALG *, VECTORS *, VEC * );
int comp_prin_cong( ALG *, int, int, VEC *);
int comp_conlat( ALG *, ALG *, PARTS * );
int comp_mi_congs( PARTS *, PARTS * );

/* composite utilities */
int comp_all_prin_cong( ALG *, PARTS * );
int meet_table(int *, PARTS *);
int order_congs( PARTS *, int ** );
int find_pers_mi( int , int *, int, int, int *, int * );

/* simple utilities */
void ins_pair( int, int, int, VEC);
int is_smaller_part( int , VEC , VEC );
int join_part( int, VEC, VEC, VEC *);
int meet_part( int, VEC, VEC, VEC *);
int is_mi( int, int*, int);

/* converters */
void arb2matt_con( int, VEC );
void arb2ewk_con( int, VEC );
int cong2pairs(VECTORS *, PART * );

int one_cong(PART *);
int zero_cong(PART *);

