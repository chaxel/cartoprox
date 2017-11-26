.IGNORE:

.SUFFIXES: .o .f90

target: cartes_to_nc.e

OBJS=	cartes_to_nc.o check.o 

FC=${F90}

LIBS=-L$(NETCDFHOME)/lib -lnetcdf 

FCFLAGS= -I$(NETCDFHOME)/include

RM=rm

######################################################################

cartes_to_nc.e: $(OBJS)
	($(FC) -o $@ $(OBJS) $(LIBS) $(FCFLAGS))

#-----------------------------------------------------------------------------
# Don't touch anything below this line
#-----------------------------------------------------------------------------

.f90.o:
#	$(RM) $@
	$(FC) -c $(FCFLAGS) $(LIBS) $*.f90
