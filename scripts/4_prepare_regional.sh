#!/bin/ksh
#
# CARTOPROX
# Preparation des donnees du modele REGIONAL
#

localdir=`pwd`

case $# in
#2)
#code_domaine=$1
#periode_loc=$2
#Charge les paramètres pour l'année N
#source ${cartoprox}/cartoprox.inc ${periode_loc} || \
#  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
#dependant du domaine de calcul
#source ./utils/infos_domaine.sh ${cartoprox_domaine} ${code_domaine} ${periode_loc} 
#Syntaxe
#syntaxe="${script_regional} ${cartoprox_domaine} ${code_domaine} ${lonb} ${latb} ${sirane_prevalp}/${cartoprox_domaine}/${periode_loc} ${periode_loc}"
#;;
2)
fic_domaines=$1
periode_loc=$2
#Charge les paramètres pour l'année N
source ${cartoprox}/cartoprox.inc ${periode_loc} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
#Syntaxe
syntaxe="${script_regional} ${cartoprox_domaine} ${fic_domaines} ${sirane_prevalp}/${cartoprox_domaine}/${periode_loc} ${periode_loc}"
;;
*)
echo "indiquer $0 fic_domaines periode" 
exit
;;
esac

cd ${interface_regional}
pwd
echo ${syntaxe}
${syntaxe} || \
  { echo "Erreur dans ${syntaxe}" ; exit 1 ; }
