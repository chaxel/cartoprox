.IGNORE:

.SUFFIXES: .o .f90

target: mailleur.exe

OBJS= params.o mailleur_optimal.o read_mif.o calcul_brins.o calcul_grille.o \
      calcul_recept.o read_args.o projection.o write_output.o

FC=$(F90)

LIBS=-L$(PROJECTIONLIB) -lproj

FCFLAGS= $(FCOPT) 

RM=rm

RM_LIST = $(OBJS) *.mod 

######################################################################

mailleur.exe: $(OBJS)
	($(FC) -O3 -I/. -o $@ $(OBJS) $(FCFLAGS) $(LIBS))

#-----------------------------------------------------------------------------
# Don't touch anything below this line
#-----------------------------------------------------------------------------

.f90.o:
	$(FC) -o $*.o -c $(FCFLAGS) $*.f90
		
params.o:
	$(FC) -o params.o -c $(FCFLAGS) params.f90
	$(FC) -c $(FCFLAGS) params.f90	
	
clean:
	$(RM) $(RM_LIST)
