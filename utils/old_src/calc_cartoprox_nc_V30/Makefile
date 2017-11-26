.IGNORE:

.SUFFIXES: .o .f90

target: calc_cartoprox_nc.exe

OBJS= calc_cartoprox_nc.o 

FC=${F90}

LIBS=-L$(NETCDFHOME)/lib -lnetcdf

FCFLAGS= -I$(NETCDFHOME)/include

RM=rm

RM_LIST = $(OBJS)  extract_val_grille.o 

######################################################################

calc_cartoprox_nc.exe: $(OBJS) 
	($(FC) -o $@  $(OBJS) $(LIBS) $(FCFLAGS))

#-----------------------------------------------------------------------------
# Don't touch anything below this line
#-----------------------------------------------------------------------------

.f90.o:
	$(FC) -c $(FCFLAGS) $*.f90

clean:
	$(RM) $(RM_LIST) extract_val_grille.exe
