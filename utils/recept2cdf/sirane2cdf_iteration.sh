#!/bin/sh
##################################################
# Export SIRANE vers NetCDF			 #
# E. Chaxel (echaxel@atmo-rhonealpes.org)	 #
# mars 2009					 #
##################################################
# Utilisation: Le script lance un programme fortran qui exportent les 
# donnees de SIRANE aux recepteurs au format NetCDF
# Bouble sur les fichiers iterations (1 fichier par iteration)
#la methode=Evol n'est plus a jour
#Travaille pour une annee ${annee}
# 07/12/2010 : ajoute l'information sur le fond de SIRANE $fichier_fond

##########################################
# Modifications utilisateur
##########################################
methode=Iter # Evol/Iter
fichier_cdf=defaut_iter.nc

##########################################
# A partir de la ne plus rien modifier
##########################################

script=sirane2cdf.sh

. ${cartoprox}/cartoprox.inc
#conversion_exe=${cartoprox}/utils/conversion_geographique/src/conversion.exe

case $# in
2)
# entrees utilisateur
periode=$1
repertoire_sorties=$2 #/data/SIRANE/m2008_diag/RESULTAT_${annee}
fichier_recepteur=${repertoire_sorties}/Recepteurs.dat
;;
3)
# entrees utilisateur
periode=$1
repertoire_sorties=$2
fichier_recepteur=$3
;;
4)
# entrees utilisateur
periode=$1
repertoire_sorties=$2
fichier_recepteur=$3
fichier_cdf=$4
;;
5)
# entrees utilisateur
periode=$1
repertoire_sorties=$2
fichier_recepteur=$3
fichier_cdf=$4
polluant=$5
;;
6)
# entrees utilisateur
periode=$1
repertoire_sorties=$2
fichier_recepteur=$3
fichier_cdf=$4
polluant=$5
fichier_fond=$6
;;
*)
echo "Syntaxe: $0 repertoire_sorties [fichier_recepteur] "
exit
;;
esac

echo "polluant=${polluant}"

#periode
deb_j=`echo ${periode} | gawk -F "_" '{print $1}'`
fin_j=`echo ${periode} | gawk -F "_" '{print $2}'`

echo "deb_j=${deb_j}"
echo "fin_j=${fin_j}"

#ATTENTION A L'ORDRE DES POLLUANTS
case ${polluant} in
0)pol_liste="NOX";;
1)pol_liste="NOX NO NO2 O3";;
2|3)pol_liste="${var_pm}";;
*)
polluant=1
pol_liste="NOX NO NO2 O3"
;;
esac

# repertoire temporaire
TMPDIR=${repertoire_sorties}/${script}.$RANDOM
while [ -d ${repertoire_sorties}/${script}.$RANDOM ] ; do
TMPDIR=${repertoire_sorties}/${script}.$RANDOM
done
echo "Travaille dans ${TMPDIR}"
mkdir -p ${TMPDIR}

#ETAPE 1 :Liste les recepteurs et 
nrecepts=`gawk '(NR==1) {print}' ${fichier_recepteur}`

echo "nrecepts="${nrecepts}

if [ ${nrecepts} -gt 0 ] ; then
  echo "Trouve ${nrecepts} recepteurs"
else
  echo "ERREUR en lisant ${fichier_recepteur}"
  exit
fi

echo "-> Conversion du format SIRANE tab au format csv..."
gawk '( NR!=1 && NR <= '${nrecepts}'+1 ) {print $1" "$2" "$3}' ${fichier_recepteur} > ${TMPDIR}/recept_tmp.list1
gawk '{print $2" "$3}' ${TMPDIR}/recept_tmp.list1 > ${TMPDIR}/recept_tmp.xylamb2

# Conversion de coordonnées Lambert 2 -> UTM 31
if [ ! -f ${conversion_exe} ] ; then
  echo "ERREUR : ne trouve pas l utilitaire de conversion geographique"
  echo "->"${conversion_exe}
  exit
fi
 
${conversion_exe} -l2 -geoid NTF -i ${TMPDIR}/recept_tmp.xylamb2 -utm 31 -geoid WGS84 -o ${TMPDIR}/recept_tmp.xyutm || \
      { echo "$0: ERREUR dans la conversion de coordonnées" ; exit 1  ; }

# Ecrit un fichier X, Y, Z 
paste ${TMPDIR}/recept_tmp.list1 ${TMPDIR}/recept_tmp.xyutm | gawk '{print $1" "$2" "$3" "$4" "$5}' > ${TMPDIR}/recept_tmp.list
rm ${TMPDIR}/recept_tmp.list1 ${TMPDIR}/recept_tmp.xyutm

case ${methode} in
Iter)
echo "->methode Iteration: aucune action a realiser"
;;
esac # methode

echo  ${TMPDIR}/recept_tmp.list

# Conversion de format TAB -> CSV du fichier FOND maximum de 5 colonnes date, heure, NO, NO2, O3
if [ -f ${fichier_fond} ] ; then
#polluant passif
case ${polluant} in
0)nvar_fond=1;;
1)nvar_fond=3;;
2|3)nvar_fond=1;;
esac
echo "-> Conversion du format SIRANE tab au format csv..."
echo ${nvar_fond} > ${TMPDIR}/fond_tmp.csv
gawk '{print $3" "$4" "$5}' ${fichier_fond} >> ${TMPDIR}/fond_tmp.csv
head ${TMPDIR}/fond_tmp.csv
fi

# COMPILATION
echo "-> Compilation"
cd src;make;cd ..

#ETAPE 2 : CREE UN FICHIER NetCDF AVEC LES RECEPTEURS, les COORDONNEES et les polluants
echo "-> Creation du NetCDF ${fichier_cdf}"
rm -rf ${TMPDIR}/pol.txt ; touch ${TMPDIR}/pol.txt
for pol in ${pol_liste} ; do 
echo ${pol} >>  ${TMPDIR}/pol.txt
done
./src/create_cdf.e ${TMPDIR}/recept_tmp.list ${TMPDIR}/pol.txt ${fichier_cdf}

#ETAPE 3 : REMPLI FICHIER NetCDF AVEC LES CONCENTRATIONS DES POLLUANTS
echo "-> remplissage NetCDF"
case ${methode} in
#Evol)syntaxe="src/write_cdf_sirane_Evol.e ${repertoire_sorties}  ${fichier_cdf}";;
Iter)syntaxe="src/write_cdf_sirane_Iter.e ${deb_j} ${repertoire_sorties} ${fichier_cdf} ${TMPDIR}/fond_tmp.csv";;
esac

${syntaxe} || \
      { echo "$0: ERREUR de l executable de conversion SIRANE->${fichier_cdf}" ; exit 1 ; } #>> ${log} 2>&1

niter=`${cartoprox}/scripts/utils/info_netcdf.sh ${fichier_cdf}`
if [ ${niter} -eq 0 ] ; then
  echo "$0: Pas de temps incorrect=0"
  exit 1
fi

# SUPPRESSION
rm -rf ${TMPDIR}/

echo "OK... Fichier de sortie : ${fichier_cdf}"
