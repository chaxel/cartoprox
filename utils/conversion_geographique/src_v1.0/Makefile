.IGNORE:

.SUFFIXES: .o .f90

target: conversion.exe

OBJS= params.o conversion.o read_ascii.o  \
      read_args.o projection.o write_output.o #calcul_grille.o

FC=ifort #$(F90)
LIBS=-L$(PROJECTIONLIB_IFORT) -lproj

#FC=$(F90)
#LIBS=-L$(PROJECTIONLIB) -lproj

FCFLAGS= $(FCOPT) 

RM=rm

RM_LIST = $(OBJS) *.mod 

######################################################################

conversion.exe: $(OBJS)
	($(FC) -I/. -o $@ $(OBJS) $(FCFLAGS) $(LIBS))

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
