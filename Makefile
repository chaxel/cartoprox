#-*-makefile-*-

# Cuisine et dependances ...

include		Makefile.hdr

utils = ./utils

RM_LIST =	*.a *.o *.e *.exe *.mod

NCFLIB =	 $(shell [ -e $(NETCDFLIB)/libnetcdff.a ]&& echo twolibs)
ifeq ($(NCFLIB),twolibs)
LDFLAGS =	-L.  -L${NETCDFLIB} -lnetcdff -lnetcdf
else
LDFLAGS =	-L.  -L${NETCDFLIB} -lnetcdf
endif

.PHONY:		all calc_cartoprox_nc extract_val_grille mailleur_optimal mosaique_grille statistics_netcdf recept2cdf recept2grid

all:		calc_cartoprox_nc extract_val_grille mailleur_optimal mosaique_grille statistics_netcdf recept2cdf recept2grid

calc_cartoprox_nc :
		( $(CD) $(utils)/calc_cartoprox_nc_V33 ; $(MAKE)  )
		
extract_val_grille :
		( $(CD) $(utils)/extract_val_grille_V5/src ; $(MAKE)  )
		
mailleur_optimal :
		( $(CD) $(utils)/mailleur_optimal/src_V1.03 ; $(MAKE)  )

mosaique_grille :
		( $(CD) $(utils)/mosaique_grille/src ; $(MAKE)  )

statistics_netcdf :
		( $(CD) $(utils)/statistics_netcdf/src_v1.1 ; $(MAKE)  )

projection :


recept2cdf :
		( $(CD) $(utils)/recept2cdf/src ; $(MAKE)  )

recept2grid :
		( $(CD) $(utils)/recept2grid/src ; $(MAKE)  )

clean:
	$(RM) $(RM_LIST)
	( $(CD) $(utils)/calc_cartoprox_nc_V32      ; $(MAKE) clean )
	( $(CD) $(utils)/extract_val_grille_V5/src  ; $(MAKE) clean )
	( $(CD) $(utils)/mailleur_optimal/src_V1.03 ; $(MAKE) clean )
	( $(CD) $(utils)/mosaique_grille/src        ; $(MAKE) clean )
	( $(CD) $(utils)/statistics_netcdf/src_v1.1 ; $(MAKE) clean )	
	( $(CD) $(utils)/recept2cdf/src             ; $(MAKE) clean )
	( $(CD) $(utils)/recept2grid/src            ; $(MAKE) clean )


