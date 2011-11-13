#define EXTERN extern
#include "includes.h"

int
main(int argc, char *argv[])
{
  int sr, err;

  GUI_mode = NO;
  StopIt = NO;

  wr_title();
  if( (argc != 2) && (argc != 3) )
  {
    hlp_usage();
    exit(1);
  }
  if(argc==2) {
    log_level_limit=LOG_LEVEL_DEFAULT;
  } else {
      sr=sscanf(argv[2], "%d", &log_level_limit);
      if(sr<=0 || sr== EOF) {
	  print_derr(
	      "Wrong number for verbosity level, set to %d.",
	      LOG_LEVEL_DEFAULT);
	  log_level_limit=LOG_LEVEL_DEFAULT;
      }
  }
  err = do_it(argv[1]);
  if (err != NO_ERROR ) {
      exit(1);
  }
  exit(0);
}

