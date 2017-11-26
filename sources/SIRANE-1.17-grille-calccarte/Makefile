#*********************************************#
#                                             #
# Modele SIRANE version 1.15    LMFA/ECL 2002 #
#                                             #
# Auteur = Lionel SOULHAC                     #
# Date de derniere modification 26/10/2004    #
#                                             #
# Makefile --> Compilation du modele          #
#                                             #
#*********************************************#

PROGRAM = sirane
MAKE = make
RM = rm -f
TARGET = All
LIBS =  -lm
CC= cc
CFLAGS= -g
WIN_FLAGS = \
		MACHINE="WIN" \
		CFLAGS="-O3 -finline-functions -ffast-math" \
		CC="gcc"

LINUX_FLAGS = \
		MACHINE="LINUX" \
		CFLAGS="-O3 -finline-functions -ffast-math" \
		CC="gcc"

DEC_FLAGS = \
                MACHINE="LINUX" \
                CFLAGS="-O3" \
                CC="cc"

DEBUG_FLAGS = \
		MACHINE="LINUX" \
		CFLAGS="-g3 -Wall" \
		CC="gcc"

PROF_FLAGS = \
		MACHINE="DEC" \
		CFLAGS="-g3 -p" \
		CC="cc"

SOURCES = 	Bessel.c \
		Calcul-Conc-Fond.c \
		Calcul-Conc-Grd.c \
		Chimie.c \
		Fonct-Univ.c \
		Gauss.c \
		Hauteur-CLA.c \
		Init.c \
		Init-Disp-Similitude.c \
		Init-Meteo.c \
		Intersection.c \
		Io.c \
		Io-Don.c \
		Io-Resul.c \
		Io-Surfer.c \
		Main.c \
		Particules.c \
		Profil.c \
		Sigma.c \
		Sigma-Similitude.c \
		Solv-Conc.c \
		Solv-Conc-Noeud.c \
		Solv-Conc-Rue.c \
		Solv-Debit.c \
		Stat.c \
		Stat-Grd.c \
		Stat-Rue-Recept.c \
		Surhauteur.c \
		Us-Lmo.c \
		Util.c \
		Util-Date.c \
		Util-Mem.c

OBJS= $(SOURCES:.c=.o)

.c.o:
	$(CC) -c $(CFLAGS) -D$(MACHINE) $*.c

win:
		$(MAKE) $(WIN_FLAGS) $(OBJS) $(TARGET)
linux:
		$(MAKE) $(LINUX_FLAGS) $(OBJS) $(TARGET)
dec:
		$(MAKE) $(DEC_FLAGS) $(OBJS) $(TARGET)
debug:
		$(MAKE) $(DEBUG_FLAGS) $(OBJS) $(TARGET)
prof:
		$(MAKE) $(PROF_FLAGS) $(OBJS) $(TARGET)
clean:
	 $(RM) $(OBJS) $(PROGRAM) $(PROGRAM).exe *~*
fast:
	$(MAKE) $(FAST_FLAGS) $(OBJS) $(TARGET)

All:	$(PROGRAM)

$(PROGRAM) : $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) $(LIBS) -o $(PROGRAM)

Bessel.o: Fonc.h Def.h Makefile
Calcul-Conc-Fond.o: Fonc.h Def.h Makefile
Calcul-Conc-Grd.o: Fonc.h Def.h Makefile
Chimie.o: Fonc.h Def.h Makefile
Fonct-Univ.o: Fonc.h Def.h Makefile
Gauss.o: Fonc.h Def.h Makefile
Hauteur-CLA.o: Fonc.h Def.h Makefile
Init.o: Fonc.h Def.h Makefile
Init-Disp-Similitude.o: Fonc.h Def.h Makefile
Init-Meteo.o: Fonc.h Def.h Makefile
Intersection.o: Fonc.h Def.h Makefile
Io.o: Fonc.h Def.h Donnees.h Makefile
Io-Don.o: Fonc.h Def.h Donnees.h Makefile
Io-Resul.o: Fonc.h Def.h Donnees.h Makefile
Io-Surfer.o: Fonc.h Def.h Makefile
Main.o: Fonc.h Def.h Makefile
Particules.o: Fonc.h Def.h Makefile
Profil.o: Fonc.h Def.h Makefile
Sigma.o: Fonc.h Def.h Makefile
Sigma-Similitude.o: Fonc.h Def.h Makefile
Solv-Conc.o: Fonc.h Def.h Makefile
Solv-Conc-Noeud.o: Fonc.h Def.h Makefile
Solv-Conc-Rue.o: Fonc.h Def.h Makefile
Solv-Debit.o: Fonc.h Def.h Makefile
Stat.o: Fonc.h Def.h Makefile
Stat-Grd.o: Fonc.h Def.h Makefile
Stat-Rue-Recept.o: Fonc.h Def.h Makefile
Surhauteur.o: Fonc.h Def.h Makefile
Us-Lmo.o: Fonc.h Def.h Makefile
Util.o: Fonc.h Def.h Makefile
Util-Date.o: Fonc.h Def.h Makefile
Util-Mem.o: Fonc.h Def.h Makefile
