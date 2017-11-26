#!/bin/ksh
#
# post-traitement des fichiers stat aux rues
# extraction de la moyenne
# MLNG 2009/09/02
################################
#
dorues=1
donoeuds=1

grid=$1


mkdir -p ${ChemWork}

#########################################"
#...pour domaine rouge
${ChemProg}/miniruesnoeudsplus.sh ${grid}
#
#...pour tous les noeuds
${ChemProg}/mininoeuds.sh ${grid}


#creation des emis
${ChemProg}/miniemisrues.sh ${grid}
${ChemProg}/miniemisnoeuds.sh ${grid}

