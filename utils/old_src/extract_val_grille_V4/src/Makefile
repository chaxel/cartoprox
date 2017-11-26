.IGNORE:

.SUFFIXES: .o .f90

target: extract_val_grille.exe

OBJS= params.o check.o read_args.o io_netcdf.o 

FC=${F90}

LIBS=-L$(NETCDFHOME)/lib -lnetcdf

FCFLAGS= -O2 -I$(NETCDFHOME)/include

RM=rm

RM_LIST = $(OBJS)  extract_val_grille.o *.mod 

######################################################################

extract_val_grille.exe: $(OBJS) extract_val_grille.o
	($(FC) -o $@  extract_val_grille.o $(OBJS) $(LIBS) $(FCFLAGS))

#-----------------------------------------------------------------------------
# Don't touch anything below this line
#-----------------------------------------------------------------------------

.f90.o:
#	$(RM) $@
	$(FC) -c $(FCFLAGS) $*.f90
				
params.o:
	$(FC) -o params.o -c $(FCFLAGS) params.f90
	$(FC) -c $(FCFLAGS) params.f90	
clean:
	$(RM) $(RM_LIST) extract_val_grille.exe
