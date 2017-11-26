#!/bin/bash
#
# CARTOPROX
# Preparation du reseau SIRANE et des emissions ANNUELLES des rues
# En theorie ce script doit etre lancé une fois par an et par reseau
#
localdir=`pwd`

case $# in
2|3)
code_domaine=$1
periode=$2
liste_polluant=$3
;;
*)
echo "indiquer $0 code_domaine periode"
exit
;;
esac

if [ "${liste_polluant}" == "" ] ; then
liste_polluant="1 2 3"
fi

for pol in ${liste_polluant} ; do

export polluant=${pol}

############################################################################################
# POLLUANT
############################################################################################
case ${polluant} in
0|1|2|3)echo "polluant=${polluant}";; 
*)
echo "$0: Fixer polluant="
exit 1
;;
esac

############################################################################################
# INCLUDE
############################################################################################
# declaration de la periode de calcul 
if [ "${periode}" == "" ] ; then
  periode=${deb_j}_${fin_j}
fi

deb_j=`echo ${periode} | gawk -F "_" '{print $1}'`
fin_j=`echo ${periode} | gawk -F "_" '{print $2}'`
annee=`date -d ${deb_j} +%Y`

#Include
source ${cartoprox}/cartoprox.inc ${periode} || \
  { echo "Erreur dans $0: ${cartoprox}/cartoprox.inc" ; exit 1 ; }
############################################################################################

### dependant du domaine de calcul#####################################
source ./utils/infos_domaine.sh ${cartoprox_domaine} ${code_domaine} ${periode} #> /dev/null || \
#  { echo "Erreur dans $0: ./utils/infos_domaine.sh" ; exit 1 ; }
#######################################################################

ficnoeuds=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/noeuds.txt
ficrues=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/rues.txt
ficponct=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/ponct.txt

ficemisnoeuds=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/emis_${vcadastre}_noeuds_${polluant}.txt
#ficemisrues=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/emis_${vcadastre}_rues_${polluant}.txt
ficemisponct=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/emis_${vcadastre}_ponct_${polluant}.txt

mkdir -p ${sirane_geom}/${cartoprox_domaine}/${code_domaine}

# Taille du mini-domaine etendu
grillesizex=`echo ${dx} ${cadre_dx} | gawk '{print $1+2*$2}'`
grillesizey=${grillesizex}

Xmin=`echo ${xl2c} ${grillesizex} | gawk '{print $1-$2/2.}'`
Xmax=`echo ${xl2c} ${grillesizex} | gawk '{print $1+$2/2.}'`

Ymin=`echo ${yl2c} ${grillesizey} | gawk '{print $1-$2/2.}'`
Ymax=`echo ${yl2c} ${grillesizey} | gawk '{print $1+$2/2.}'`

echo "***info: ${code_domaine} pour periode ${periode} : debut du traitement `date` "
if [ ! -d ${ChemRes} ] ; then
    mkdir -p ${ChemRes} 
fi

echo "-> GEOMETRIE GRILLE "
ficres=${ChemRes}/infoGEOM_${code_domaine}_pol${polluant}.sh
echo "-> ${ficres}"
echo "Xmin=${Xmin}" >  ${ficres}
echo "Xmax=${Xmax}" >> ${ficres}
echo "Ymin=${Ymin}" >> ${ficres}
echo "Ymax=${Ymax}" >> ${ficres}

echo "ChemRes=${ChemRes}" >> ${ficres}

echo "ChemProg=${ChemProg}" >> ${ficres}

echo "ficnoeuds=${ficnoeuds}" >> ${ficres}
echo "ficrues=${ficrues}" >> ${ficres}
echo "ficponct=${ficponct}" >> ${ficres}

echo "ficemisnoeuds=${ficemisnoeuds}" >> ${ficres}
#echo "ficemisrues=${ficemisrues}" >> ${ficres}
echo "ficemisponct=${ficemisponct}" >> ${ficres}

echo "ficnoeudsrc=${ficnoeudsrc}" >> ${ficres}
echo "ficruessrc=${ficruessrc}" >> ${ficres}
#echo "ficemisruesrc=${ficemisruesrc}" >> ${ficres}

echo "***info: ${ficres} OK"

echo "-> RUES+NOEUDS+EMISSIONS"

#...pour tous les rues
${ChemProg}/miniruesnoeudsplus.sh ${code_domaine} ${ChemRes} > /dev/null
# 
#...pour tous les noeuds
${ChemProg}/mininoeuds.sh ${code_domaine} ${ChemRes} > /dev/null

#creation des emis (VERSION 3 : dans 5_run)
#case ${polluant} in
#0)${ChemProg}/miniemisrues.sh ${code_domaine} ${ChemRes} PM10;; # passif PM10
#1)${ChemProg}/miniemisrues.sh ${code_domaine} ${ChemRes} NOx ;; # reactif NOx
#2)${ChemProg}/miniemisrues.sh ${code_domaine} ${ChemRes} PM10;; # reactif PM10
#esac

${ChemProg}/miniemisnoeuds.sh ${code_domaine} ${ChemRes} > /dev/null

echo "***info: GEOM OK"

echo "-> PONCTUELLES ${ficponct}"

if [ "${ficemisponctsrc_AN}" != "" ] ; then
  
  #Genere le fichier d'EMISSIONS
  if [ ! -f ${ficemisponctsrc_AN} ] ; then
    #Genere le fichier des metadonnees ponctuelles (emplacement geo + émissions)
    ${cartoprox}/ponctuelles/bdsirane/export_emis_ponct_cartoprox.sh > ${ficemisponctsrc_AN}
    echo "${cartoprox}/ponctuelles/bdsirane/export_emis_ponct_cartoprox.sh > ${ficemisponctsrc_AN}"
  fi
  echo "ficemisponctsrc_AN=${ficemisponctsrc_AN} OK"
  head -n 5 ${ficemisponctsrc_AN}  

  #Genere le fichier de METADONNEES
  if [ "${ficponctsrc}" != "" ] ; then
    if [ ! -f ${ficponctsrc} ] ; then
    # Genere le fichiers des emissions annuelles ponctuelles (emplacement geo + émissions)
    ${cartoprox}/ponctuelles/bdsirane/export_ponct_cartoprox.sh > ${ficponctsrc}
    echo "ficpontsrc=${ficponctsrc} OK"
    fi
    ln -sf ${ficponctsrc} ${ficponct}
    echo "${ficponctsrc} -> ${ficponct}"
    head -n 5 ${ficponctsrc}      
  fi

else

  echo "Pas de ponctuelles"
  echo 1	>  ${ficponct}
  echo 0	866000	2026000	0	0	1	1	25  >>  ${ficponct}

  echo 1       >  ${ficemisponct}
  echo 0	0.0  >> ${ficemisponct}
  head -n 5 ${ficponct}
fi  


echo "***info: Emissions ponctuelles OK"

done # polluant

echo "Fin normale de $0 a `date`"

exit 0
