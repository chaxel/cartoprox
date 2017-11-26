.IGNORE:

.SUFFIXES: .o .f90

target: get_wdot_nc_stations.e

OBJS=  wcal2.o julian.o check.o daymon.o calcvent.o

FC=${F90}

LIBS=-L$(NETCDFHOME)/lib -L$(PROJECTIONLIB) -lnetcdf -lproj 

FCFLAGS= -I$(NETCDFHOME)/include

RM=rm

######################################################################

get_wdot_nc_stations.e: $(OBJS) get_wdot_nc_stations.o
	($(FC) -o $@  get_wdot_nc_stations.o $(OBJS) $(LIBS) $(FCFLAGS))

#-----------------------------------------------------------------------------
# Don't touch anything below this line
#-----------------------------------------------------------------------------

.f90.o:
	$(FC) -c $(FCFLAGS) $*.f90
