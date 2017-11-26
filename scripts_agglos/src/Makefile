#        Lower-level Makefile for tovis5d

#        Suffix rules and commands

.IGNORE:

.SUFFIXES: .o .f90

target: fill_cdf.e

OBJS= 

FC=$(F90) $(FCOPT)

LIBS=-L$(NETCDFHOME)/lib -lnetcdf 

FCFLAGS=-I$(NETCDFHOME)/include

RM=rm -rf

######################################################################

fill_cdf.e  : fill_cdf.o $(OBJS)
	($(FC) -o $@ fill_cdf.o $(OBJS) $(LIBS))
		
#-----------------------------------------------------------------------------
# Don't touch anything below this line
#-----------------------------------------------------------------------------

.f90.o:
	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f90
