.IGNORE:

.SUFFIXES: .o .f90

target: genere_grille.exe elimine_doublons_grille.exe grille2nc.exe grille2VM.exe grille2grd.exe 

OBJS= 

FC=$(F90)

LIBS=-L$(PROJECTIONLIB) -lproj -L$(NETCDFHOME)/lib -lnetcdf

FCFLAGS= $(FCOPT) -I$(NETCDFHOME)/include

RM=rm

RM_LIST = *.o *.exe *.mod 

######################################################################

genere_grille.exe: genere_grille.o 
	($(FC) -I/. -o $@ genere_grille.o  $(FCFLAGS) $(LIBS))

elimine_doublons_grille.exe: elimine_doublons_grille.o 
	($(FC) -I/. -o $@ elimine_doublons_grille.o  $(FCFLAGS) $(LIBS))

grille2nc.exe: grille2nc.o 
	($(FC) -I/. -o $@ grille2nc.o  $(FCFLAGS) $(LIBS))

grille2VM.exe: grille2VM.o 
	($(FC) -I/. -o $@ grille2VM.o  $(FCFLAGS))
#ifort  grille2VM.f90 -I/. -o grille2VM.exe  -O2 -xW -fno-alias -prec_div -ip -I/opt/netcdf-4.0.1-ifort-gcc/include Mng
grille2grd.exe: grille2grd.o
	($(FC) -I/. -o $@ grille2grd.o $(FCFLAGS))

#-----------------------------------------------------------------------------
# Don't touch anything below this line
#-----------------------------------------------------------------------------

.f90.o:
	$(FC) -o $*.o -c $(FCFLAGS) $*.f90
			
clean:
	$(RM) $(RM_LIST)
