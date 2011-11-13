int ps_alg( char *, ALG * );
int ps_vectors(char *, VECTORS *, int);
int ps_binrel_line(char *, VECTORS *, int );
int ps_binrel_arg(char *, VECTORS *, int );
int ps_cong(char *, PART *, int );
int ps_conlist(char *, PARTS *, int);
int ps_con_or_list(char *, PART *, PARTS *, int);
int ps_cong_arg(char *, PART *, int );
int ps_number(char *, int, int, int * );
int ps_out(char *, char * );

int rd_alg( char *, ALG * );
int rd_funct( FILE *, int, FUNCT **, int );
int rd_vectors(char *, VECTORS *, int);
int rd_conlist(char *, PARTS *, int);
int rd_number(char *, int , int , int *);

int wr_alg(char *, ALG * );
int wr_vectors(char *, VECTORS * );
int wr_conlist(char *, PARTS * );
int wr_typelist(char *, VECTORS * );

