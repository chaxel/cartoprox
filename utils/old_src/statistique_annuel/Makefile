.IGNORE:

.SUFFIXES: .o .f90

target: sirane_statistics.exe

OBJS=	params.o sirane_statistics.o check.o io_netcdf.o allocation.o

FC=$(F90) $(FCOPT)

LIBS=-L$(NETCDFHOME)/lib -L$(PROJECTIONLIB) -lnetcdf -lproj 

FCFLAGS=-I$(NETCDFHOME)/include

RM=rm

RM_LIST = $(OBJS) *.mod *.exe 

######################################################################

sirane_statistics.exe: $(OBJS)
	($(FC) -o $@ $(OBJS) $(LIBS))

#-----------------------------------------------------------------------------
# Don't touch anything below this line
#-----------------------------------------------------------------------------

params.o:
	$(FC) -o params.o -c $(FCFLAGS) params.f90
	$(FC) -c $(FCFLAGS) params.f90	

.f90.o:
#	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f90

clean:
	$(RM) $(RM_LIST)
