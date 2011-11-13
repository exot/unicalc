#include <stdio.h>
#include <string.h>
#include <memory.h>
#include <malloc.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdarg.h>

#ifdef RUN_PROGRESS
#include <math.h>
#ifdef TIME_DIAG
#include <time.h>
#endif /* TIME_DIAG */
#endif /* RUN_PROGRESS */


#if defined(MSC) || defined(EMX)
#include <conio.h>
#include <io.h>
#endif

#if defined(UNIX)
#include <unistd.h>
#endif

#if defined(DJGPP)
#include <std.h>
#endif

#if defined(BORLANDC)
#include <io.h>
#endif

#include "uab.h"
#include "wparse.h"
#include "wal.h"
#include "whelp.h"
#include "winput.h"
#include "wutil.h"
#include "wspf.h"
#include "wcong.h"
#include "wprod.h"
#include "wabel.h"
#include "wmin.h"
#include "wtype.h"
