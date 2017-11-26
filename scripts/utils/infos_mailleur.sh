#!/bin/ksh
#
# CARTOPROX
# infos sur mini-domaine (specifique au mailleur)
#
## INCLUDE ###########################################
localdir=`pwd`

case $# in
3)
code_maille=$1
code_domaine=$2
params_mailles_loc=""
periode_loc=$3
;;
4)
code_maille=$1
code_domaine=$2
params_mailles_loc=$3
periode_loc=$4
;;
*)
echo "indiquer $0 code_maille code_domaine"
echo "mailles -> ${params_mailles}"
exit
;;
esac

# Charge les paramètres pour l'année N
source ${cartoprox}/cartoprox.inc ${periode_loc} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }

if [ "${params_mailles_loc}" != "" ] ; then
params_mailles=${params_mailles_loc}
echo "params_mailles=${params_mailles_loc}"
echo "+++++++++++++++++ MAILLE ${code_maille} ++++++++++DOMAINE ${code_domaine} ++++++++++++++++++++++++++++++++"
fi

#echo "+++++++++++++++++ MAILLE ${code_maille} ++++++++++DOMAINE ${code_domaine} ++++++++++++++++++++++++++++++++"

# Repertoires dependant de code_domaine
ChemRes=${ChemRes1}/${code_domaine}

# Lit les infos sur la maille/domaine ##########
xc=`gawk          '( $1 == "'${code_maille}'" && $2 == "'${code_domaine}'" ) { print      $7 }' ${params_mailles}`   # X utm 31
yc=`gawk          '( $1 == "'${code_maille}'" && $2 == "'${code_domaine}'" ) { print      $8 }' ${params_mailles}`   # Y utm 31
dx=`gawk          '( $1 == "'${code_maille}'" && $2 == "'${code_domaine}'" ) { print int($9) }' ${params_mailles}`
cadre_dx=`gawk    '( $1 == "'${code_maille}'" && $2 == "'${code_domaine}'" ) { print int($10)}' ${params_mailles}`
emis_profil=`gawk '( $1 == "'${code_maille}'" && $2 == "'${code_domaine}'" ) { print      $5 }' ${params_mailles}`

###  Recepteurs USER pour MAILLEUR
recept_mailleur=recept_pts_dx${dx}_brin${brin_dx}_recept${recept_dx}_cadre${cadre_dx}.txt
brin_mailleur=brin_pts_brin${brin_dx}.txt
sirane_recept_mailleur=${sirane_recept}/${code_maille}/${code_domaine}
ficrecept_mailleur=${sirane_recept_mailleur}/${recept_mailleur}
ficbrin_mailleur=${sirane_recept_mailleur}/${brin_mailleur}
##########
