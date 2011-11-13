PROJ = uab

CC  = gcc
CFLAGS_G  = -Wall -DRUN_PROGRESS -DTIME_DIAG -DBIG_INT -DUNIX -O2\
-I$(JDK_ROOT)/include\
-I$(JDK_ROOT)/include/linux -fPIC\
-ggdb
LFLAGS_G  = -lm
LINKER	= gcc

OBJS  = wparse.o whelp.o winput.o wutil.o wspf.o\
 wcong.o wprod.o wabel.o wmin.o wtype.o

HEADERS = includes.h uab.h wparse.h wal.h whelp.h winput.h wutil.h wspf.h\
	wcong.h wprod.h wabel.h wmin.h wtype.h uacalc_Compute.h

all: $(PROJ) libua.so

.SUFFIXES:
.SUFFIXES: .o .c

libua.so: $(OBJS)
	  $(LINKER) -shared -o libua.so $(OBJS) $(LFLAGS_G)

$(PROJ) : uab.o $(OBJS)
	$(LINKER) uab.o $(OBJS) -o $(PROJ) $(LFLAGS_G)

uab.o : uab.c $(HEADERS)

wparse.o : wparse.c $(HEADERS)

whelp.o : whelp.c $(HEADERS)

winput.o : winput.c $(HEADERS)

wutil.o : wutil.c $(HEADERS)

wspf.o : wspf.c $(HEADERS)

wcong.o : wcong.c $(HEADERS)

wprod.o : wprod.c $(HEADERS)

wabel.o : wabel.c $(HEADERS)

wmin.o : wmin.c $(HEADERS)

wtype.o : wtype.c $(HEADERS)

.c.o :
	$(CC) -c $(CFLAGS_G) $<


