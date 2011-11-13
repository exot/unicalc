#define EXTERN extern
#include "includes.h"

void
wr_title()
{
  print_nhlp("     UNIVERSAL ALGEBRA CALCULATOR");
  print_nhlp("       ---  batch version  ---");
}

void
hlp_usage()
{
  print_dhlp("HELP (usage).");
  print_nhlp("   uab <project name> [verbosity]");
  print_nhlp("The parameter file is     <project name>.par,");
  print_nhlp("the log file name is      <project name>.log,");
  print_nhlp("the results file is       <project name>.dat.");
  print_shlp("The program describes the parameter file format");
  print_nhlp("(just create an empty file, and put it to the command line).");
  print_shlp("The optional verbosity argument is a positive integer.");
  print_nhlp("The bigger it is, the more verbose the output (default=%d).",
                                                      LOG_LEVEL_DEFAULT);

}

void
hlp_par_format()
{
  print_dhlp("HELP (parameter file format).");
  print_nhlp("   \\begin_command");
  print_nhlp("   <command name>");
  print_nhlp("   <lines of data>");
  print_nhlp("   \\end_command");
  print_nhlp("The program describes the data format for each command");
  print_nhlp("(just type something - anything - that is obviously wrong).");
  print_nhlp("There can be several commmand blocks in a parameter file.");
  print_nhlp("Lines starting with %% are comments, and are ignored.");
}

void
hlp_commands()
{
  print_dhlp("HELP (commands). The list of commands currently available:");
  print_nhlp("\\create_direct_product      \\create_factor_algebra");
  print_nhlp("\\generate_subalgebra        \\create_subproduct_algebra");
  print_nhlp("\\generate_congruence        \\compute_congruence_lattice");
  print_nhlp("\\product_congruence         \\find_congruence_number");
  print_nhlp("\\centrality                 \\strong_centrality");
  print_nhlp("\\weak_centrality            \\rectangular_centrality");
  print_nhlp("\\compute_type               \\label_congruence_lattice");
  print_nhlp("\\compute_min_sets           \\unary_polynomials");
  print_nhlp("\\compute_R(L,R)             \find_kkvm_germ.");
}

void
hlp_gen_subalg( void )
{
  print_dhlp("HELP (\\generate_subalgebra).");
  print_nhlp("Generate a subuniverse in a subpower.");
  print_shlp("Description of arguments (on separate lines):");
  print_nhlp("1: input algebra.");
  print_nhlp("2: generating vectors.");
  print_nhlp("3: resulting vectors (vectorlist file).");
}

void
hlp_cr_subpr_alg( void )
{
  print_dhlp("HELP (\\create_subproduct_algebra).");
  print_nhlp("Create algebra from a subuniverse in a product.");
  print_shlp("Description of arguments (on separate lines):");
  print_nhlp("1:     vectors forming the subuniverse.");
  print_nhlp("2,...: component algebras.");
  print_nhlp("last : resulting algebra (algebra file).");
}

void
hlp_cr_prod_cong( void )
{
  print_dhlp("HELP (\\product_congruence).");
  print_nhlp("Compute the product of congruences.");
  print_shlp("Description of arguments (on separate lines):");
  print_nhlp("1:     vectors forming the subuniverse.");
  print_nhlp("2,...: component congruences.");
  print_nhlp("last : resulting congruence (conlist file with one vector).");
}

void
hlp_find_congnum( void )
{
  print_dhlp("HELP (\\find_congruence_number).");
  print_nhlp("Find a given congruence on a list.");
  print_shlp("Description of arguments (on separate lines):");
  print_nhlp("1: conlist file.");
  print_nhlp("2: congruence description.");
  print_shlp("The answer is written to the terminal.");
}

void
hlp_dir_prod( void )
{
  print_dhlp("HELP (\\create_direct_product).");
  print_nhlp("Create and store the direct product of algebras.");
  print_shlp("Description of arguments (on separate lines):");
  print_nhlp("1:      number of components.");
  print_nhlp("2,... : component algebras.");
  print_nhlp("last-1: resulting algebra (algebra file).");
  print_nhlp("last  : resulting universe (vectorlist file).");
}

void
hlp_cr_fact_alg( void )
{
  print_dhlp("HELP (\\create_factor_algebra).");
  print_nhlp("Create the factor F=A/cong.");
  print_shlp("Description of arguments (on separate lines):");
  print_nhlp("1: input algebra A.");
  print_nhlp("2: congruence `cong'.");
  print_nhlp("3: resulting algebra F (algebra file).");
}

void
hlp_gen_cong( void )
{
  print_dhlp("HELP (\\generate_congruence).");
  print_nhlp("Generate a congruence from a set of pairs.");
  print_shlp("Description of arguments (on separate lines):");
  print_nhlp("1: input algebra.");
  print_nhlp("2: pairs from 'B'.");
  print_nhlp("   Formally this is a vector file, with vectors of length 2.");
  print_nhlp("3: resulting congruence (conlist file with only one vector).");
}

void
hlp_gen_conlat( void )
{
  print_dhlp("HELP (\\compute_congruence_lattice).");
  print_nhlp("Compute all congruences of an algebra.");
  print_shlp("Description of arguments (on separate lines):");
  print_nhlp("1: input algebra.");
  print_nhlp("2: resulting congruence lattice.");
  print_nhlp("3: resulting list of all congruences.");
  print_nhlp("4: resulting list of principal congruences.");
  print_nhlp("5: resulting list of meet-irred congruences.");
  print_shlp("The conlists are in Matt's *.pri(n)=*.con format.");
  print_nhlp("The lattice is in Matt's *.alg format (L, join, meet).");
}

void
hlp_rlr( void )
{
  print_dhlp("HELP (\\compute_R(L,R)).");
  print_nhlp("Compute the relation R(L,R) mod a congruence.");
  print_nhlp("This is the set of all pairs (a,b) for which");
  print_nhlp("there exists an L,R-matrix (a,b,c,d) with (a,d) in cong.");
  print_shlp("Description of arguments (on separate lines):");
  print_nhlp("1: input algebra.");
  print_nhlp("2: binary relation `left'.");
  print_nhlp("3: binary relation `right'.");
  print_nhlp("4: congruence `cong'.");
  print_nhlp("5: resulting list of pairs in R(L,R).");
  print_shlp("The result is formally a vectorlist file.");
  print_nhlp("The details of generation are written to the log file.");
}

void
hlp_is_centr( int centr )
{
  switch(centr) {
    case CENTR_NORMAL :
      print_dhlp("HELP (\\centrality).");
      print_nhlp("The relation C(left, right; cong) is checked.");
      print_nhlp("(if the top edge of a left-right rectangle is in cong,");
      print_nhlp("then the bottom edge must also be in cong).");
      break;
    case CENTR_WEAK :
      print_dhlp("HELP (\\weak_centrality).");
      print_nhlp("The relation CW(left, right; cong) is checked.");
      print_nhlp("(if any three vertices of a left-right rectangle are in\
 cong,");
      print_nhlp("then all four vertices must be in cong).");
      break;
    case CENTR_STRONG :
      print_dhlp("HELP (\\strong_centrality).");
      print_nhlp("The relation C*(left, right; cong) is checked.");
      print_nhlp("(C(left, right; cong) and CR(left, right; cong)).");
      break;
    case CENTR_RECT :
      print_dhlp("HELP (\\rectangular_centrality).");
      print_nhlp("The rectangularity CR(left, right; cong) is checked.");
      print_nhlp("(if a diagonal of a left-right rectangle is in cong,");
      print_nhlp("then all four vertices must be in cong).");
      break;
    default:
      print_derr("Internal error: invalid centr value.");
      exit(1);
  }
  print_shlp("Description of arguments (on separate lines):");
  print_nhlp("1: input algebra.");
  print_nhlp("2: binary relation `left'.");
  print_nhlp("3: binary relation `right'.");
  print_nhlp("4: congruence `cong', OR");
  print_nhlp("        congruence-list for `cong'.");
  print_nhlp("   If a list is given, all of its elements are checked,");
  print_nhlp("   and the program stops at the first failure.");
  print_shlp("The answer is written to the terminal.");
}

void
hlp_min_sets( void )
{
  print_dhlp("HELP (\\compute_min_sets).");
  print_nhlp("Compute all <alpha,beta>-minimal sets");
  print_nhlp("(if <alpha,beta> is not tame, we may not get all).");
  print_shlp("Description of arguments (on separate lines):");
  print_nhlp("1: input algebra A.");
  print_nhlp("2: congruence `alpha'.");
  print_nhlp("3: congruence `beta'.");
  print_nhlp("4: resulting sets (in a vectorlist file).");
}

void
hlp_un_pol( void )
{
  print_dhlp("HELP (\\unary_polynomials).");
  print_nhlp("Compute all, and all idempotent, unary polynomials.");
  print_shlp("Description of arguments (on separate lines):");
  print_nhlp("1: input algebra A.");
  print_nhlp("2: resulting list of unary polynomials.");
  print_nhlp("3: resulting list of idempotent unary polynomials.");
  print_shlp("These are in vectorlist format (like Matt's *.p1).");
}

void
hlp_type_q( void )
{
  print_dhlp("HELP (\\compute_type).");
  print_nhlp("Compute the type of the quotient <alpha,beta>");
  print_nhlp("(if <alpha,beta> is not tame, one can get nonsense results).");
  print_shlp("Description of arguments (on separate lines):");
  print_nhlp("1: input algebra A.");
  print_nhlp("2: congruence `alpha'.");
  print_nhlp("3: congruence `beta'.");
  print_shlp("The answer is written to the terminal.");
}

void
hlp_label_conlat( void )
{
  print_dhlp("HELP (\\label_congruence_lattice).");
  print_nhlp("Compute the types of all prime quotients.");
  print_shlp("Description of arguments (on separate lines):");
  print_nhlp("1: input algebra A.");
  print_nhlp("2: congruence lattice of A.");
  print_nhlp("   WARNING: it is not checked if this is indeed Con(A)!");
  print_nhlp("3: resulting labels (a type file, that is, triplets ,a,b,t");
/* There is intentionally no , at the end of the previous line. */
  print_nhlp("   where Ca-<Cb, and typ<Ca,Cb>=t).");
}

void
hlp_kkvm_germ( void )
{
  print_dhlp("HELP (\\find_kkvm_germ).");
  print_nhlp("Find kkvm germs with respect to a tolerance.");
  print_shlp("Description of arguments (on separate lines):");
  print_nhlp("1: input algebra.");
  print_nhlp("2: the tolerance T.");
  print_nhlp("3: the tolerance R(T,T).");
  print_shlp("The answer is written to the terminal.");
}

void
hlp_alg_line()
{
  print_nhlp("Line %d of the current parameter file.", linenum);
  print_dhlp("HELP: Algebra description:");
  print_nhlp("\\algebra_file{<file name>}.");
}

void
hlp_alg_file()
{
  print_dhlp("HELP: Algebra file format (Matt's *.alg, *.lat):");
  print_nhlp("line 1: number of elements.");
  print_nhlp("line 2: arity of operation.");
  print_nhlp("...  : values lexicographically, on separate lines.");
  print_shlp("On input, the values can also be separated by spaces.");
  print_nhlp("WARNING: the tables get transposed on input.");
  print_shlp("The operations are repeated until the end of the file.");
}

void
hlp_vector_line()
{
  print_nhlp("Line %d of the current parameter file.", linenum);
  print_dhlp("HELP: Vector-list description:");
  print_nhlp("\\vectorlist_file{<file name>}");
}

void
hlp_vector_file()
{
  print_dhlp("HELP: Vectorlist file format (Matt's *.p1):");
  print_nhlp("line 1: <number of vectors>,<length of vectors>.");
  print_nhlp("line 2: empty.");
  print_nhlp("...   : vectors on separate lines.");
  print_shlp("The components of the vectors are separated by `, '.");
}

void
hlp_type_file()
{
  print_dhlp("HELP: Type file format (Matt's *.typ):");
  print_nhlp("line 1: <number of triplets>.");
  print_nhlp("line 2: empty.");
  print_nhlp("...   : triplets ,a,b,t on separate lines,");
  print_nhlp("        Ca-<Cb in the congruence lattice, and typ<Ca,Cb>=t");
}

void
hlp_conlist_line()
{
  print_nhlp("Line %d of the current parameter file.", linenum);
  print_dhlp("HELP: Congruence-list description:");
  print_nhlp("\\conlist_file{<file name>}");
}

void
hlp_cong_line()
{
  print_nhlp("Line %d of the current parameter file.", linenum);
  print_dhlp("HELP: Congruence description:");
  print_nhlp("\\congruence{<congruence argument>}");
}
void
hlp_cong_arg()
{
  print_nhlp("Line %d of the current parameter file.", linenum);
  print_dhlp("HELP: Congruence argument description:");
  print_nhlp("\\zero_congruence{} OR");
  print_nhlp("\\one_congruence{} OR");
  print_nhlp("\\conlist_file{<file name>}\\number{<congnum>}}");
  print_nhlp("   The desired congruence is C<congnum>");
  print_nhlp("   in the congruence-list file <file name>");
}


void
hlp_binrel_line()
{
  print_nhlp("Line %d of the current parameter file.", linenum);
  print_dhlp("HELP: Binary relation description:");
  print_nhlp("\\bin_rel{<binrel argument>}");
}

void
hlp_binrel_arg()
{
  print_nhlp("Line %d of the current parameter file.", linenum);
  print_dhlp("HELP: Binary relation argument description:");
  print_nhlp("\\vectorlist_file{<file name>} OR");
  print_nhlp("\\congruence{<congruence argument>}");
}

void
hlp_number_line()
{
  print_nhlp("Line %d of the current parameter file.", linenum);
  print_dhlp("HELP: Number description:");
  print_nhlp("\\number{<integer>}");
}

void
hlp_conlist_file()
{
  print_dhlp("HELP: Congruence-list file format (Matt's *.con, *.pri):");
  print_nhlp("line 1: <length of vectors>,[<perm>].");
  print_nhlp("...   : congruences on separate lines.");
  print_shlp("The components of the vectors are preceeded by `,'.");
  print_nhlp("Conforms to *.con file format of Matt Valeriote's program.");
}

void
hlp_output_line()
{
  print_nhlp("Line %d of the current parameter file.", linenum);
  print_dhlp("HELP: Output file description:");
  print_nhlp("\\output_file{<file name>}.");
  print_serr("The original contents of this file is destroyed.");
}

