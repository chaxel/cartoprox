#        Lower-level Makefile for tovis5d

#        Suffix rules and commands

.IGNORE:

.SUFFIXES: .o .f90

target: create_cdf.e write_cdf_sirane_Iter.e write_cdf_sirane_Grille.e 

OBJS= calendar.o

FC=$(F90) $(FCOPT)

LIBS=-L$(NETCDFHOME)/lib -lnetcdf 

FCFLAGS=-I$(NETCDFHOME)/include

RM=rm

RM_LIST = *.o *.mod *.e

######################################################################

create_cdf.e  : create_cdf.o $(OBJS)
	($(FC) -o $@ create_cdf.o $(OBJS) $(LIBS))
	
write_cdf_sirane_Evol.e  : write_cdf_sirane_Evol.o $(OBJS)
	($(FC) -o $@ write_cdf_sirane_Evol.o $(OBJS) $(LIBS))
	
write_cdf_sirane_Iter.e  : write_cdf_sirane_Iter.o $(OBJS)
	($(FC) -o $@ write_cdf_sirane_Iter.o $(OBJS) $(LIBS))	

write_cdf_sirane_Grille.e  : write_cdf_sirane_Grille.o $(OBJS)
	($(FC) -o $@ write_cdf_sirane_Grille.o $(OBJS) $(LIBS))		
	
#-----------------------------------------------------------------------------
# Don't touch anything below this line
#-----------------------------------------------------------------------------

.f90.o:
	$(FC) -c $(FCFLAGS) $*.f90

clean:
	$(RM) $(RM_LIST)
