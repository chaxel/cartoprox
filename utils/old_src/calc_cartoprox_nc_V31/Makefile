
.IGNORE:

.SUFFIXES: .o .f90

target: calc_cartoprox_nc.exe calc_suremis_nc.exe

OBJS= calc_cartoprox_nc.o  io_netcdf.o

FC=${F90}

LIBS=-L$(NETCDFHOME)/lib -lnetcdf

FCFLAGS= -O2 -I$(NETCDFHOME)/include

RM=rm

RM_LIST = $(OBJS)  extract_val_grille.o io_netcdf.o

######################################################################

calc_cartoprox_nc.exe: calc_cartoprox_nc.o  
	($(FC) -o $@  calc_cartoprox_nc.o  io_netcdf.o $(LIBS) $(FCFLAGS))

calc_suremis_nc.exe: calc_suremis_nc.o  io_netcdf.o
	($(FC) -o $@  calc_suremis_nc.o  io_netcdf.o $(LIBS) $(FCFLAGS))	

#-----------------------------------------------------------------------------
# Don't touch anything below this line
#-----------------------------------------------------------------------------

.f90.o:
	$(FC) -c $(FCFLAGS) $*.f90

clean:
	$(RM) $(RM_LIST) extract_val_grille.exe
