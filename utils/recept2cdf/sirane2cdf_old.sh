#!/bin/sh
##################################################
# Export SIRANE vers NetCDF			 #
# E. Chaxel (echaxel@atmo-rhonealpes.org)	 #
# mars 2009					 #
##################################################
# Utilisation: Le script lance un programme fortran qui exportent les 
# donnees de SIRANE aux recepteurs au format NetCDF
##########################################
# Modifications utilisateur
##########################################
pol_liste="NO NO2 O3"
annee=2009
fichier_cdf=defaut2.nc
methode=Iter # Evol/Iteration
##########################################
# A partir de la ne plus rien modifier
##########################################

script=sirane2cdf.sh

case $# in
2)
# entrees utilisateur
fichier_recepteur=$1
repertoire_sorties=$2
;;
*)
echo "Syntaxe: $0 fichier_recepteur repertoire_sorties"
fichier_recepteur=/appli/SIRANE/SRC1.16diag/GEOM/Recepteurs.dat
repertoire_sorties=/data/SIRANE/m2008_diag/RESULTAT_${annee}
#exit
;;
esac

# repertoire temporaire
TMPDIR=${SCRATCH}/${script} #.$RANDOM

#fichier_recepteur=fichier avec les coordonnées des récepteurs
# FORMAT  
#17
#berthelot	794486.05	2086041.92
#garibaldi	795527.01	2088440.39
#st_just	793225.8	2086940.82
#vaise	791851.34	2089316.26


#repertoire_sorties=repertoire ou sont stocké les répertoires RESULTAT_200801
# FORMAT
#i	Date	Heure	C	C_NO	C_NO2	C_O3
#1	01/01/2008	00:00	28.761937	2.273337	25.276154	30.544143
#2	01/01/2008	01:00	21.914983	1.590690	19.475926

#ETAPE 1 :Liste les recepteurs et 
nrecepts=`gawk '(NR==1) {print}' ${fichier_recepteur}`

if [ ${nrecepts} -gt 0 ] ; then
  echo "Trouve ${nrecepts} recepteurs"
  echo "Travaille dans ${TMPDIR}"
  mkdir -p ${TMPDIR}
else
  echo "ERREUR en lisant ${fichier_recepteur}"
  exit
fi

liste_mois=""
# liste les mois
echo "-> Conversion du format SIRANE tab au format csv..."

for mois in 01 02 03 04 05 06 07 08 09 10 11 12 ; do
if [ -d ${repertoire_sorties}${mois} ] ; then
liste_mois="${liste_mois} ${mois}"
fi
done

#echo ${liste_mois} ; exit

# liste les recepteurs (limite a nrecepts)
gawk '( NR!=1 && NR <= '${nrecepts}'+1 ) {print $1","$2","$3}' ${fichier_recepteur} > ${TMPDIR}/recept_tmp

i=1
#while [ $i -le ${nrecepts} ]  ; do
cat ${TMPDIR}/recept_tmp | gawk -F "," '{print $1}' | while read name ; do
recept_n[$i]=${name}

#recept_n[$i]=`gawk -F "," ' (NR=='${i}') { print $1 }' ${TMPDIR}/recept_tmp`
#recept_x[$i]=`gawk -F "," ' (NR=='${i}') { print $2 }' ${TMPDIR}/recept_tmp`
#recept_y[$i]=`gawk -F "," ' (NR=='${i}') { print $3 }' ${TMPDIR}/recept_tmp`
#echo "recepteur ${i}/${nrecepts} nom=${recept_n[$i]} x=${recept_x[$i]} y=${recept_y[$i]}"
#echo "${recept_n[$i]},${recept_x[$i]},${recept_y[$i]}"

# traite le fichier pour le mois n -> ecrit un fichier CSV avec des entiers
for mois in ${liste_mois} ; do
ficrecept_root=${repertoire_sorties}${mois}/Evol-recept-${recept_n[$i]}
ficrecept_dat=${ficrecept_root}.dat
ficrecept_csv=${ficrecept_root}.csv
if [ -f ${ficrecept_dat} ]  &&  [ ! -f ${ficrecept_csv} ] ; then
gawk -F "(\t|/|:)" ' ( $3 == "'${mois}'" ) { print $1","$2","$3","$4","$5","$6","$7","$8","$9","$10 }' \
    ${ficrecept_dat} > ${ficrecept_csv}
#echo "${ficrecept_csv} OK"
fi
done
i=`expr $i \+ 1`
done

echo  ${TMPDIR}/recept_tmp

# COMPILATION
cd src;make;cd ..

#ETAPE 2 : CREE UN FICHIER NetCDF AVEC LES RECEPTEURS, les COORDONNEES et les polluants
rm -rf ${TMPDIR}/pol.txt ; touch ${TMPDIR}/pol.txt
for pol in ${pol_liste} ; do 
echo ${pol} >>  ${TMPDIR}/pol.txt
done
src/create_cdf.e ${TMPDIR}/recept_tmp ${TMPDIR}/pol.txt ${fichier_cdf}

#ETAPE 3 : REMPLI FICHIER NetCDF AVEC LES CONCENTRATIONS DES POLLUANTS
echo "-> remplissage NetCDF en cours..."
src/write_cdf_sirane_v1.e ${repertoire_sorties}  ${fichier_cdf}

echo "OK... Fichier de sortie : ${fichier_cdf}"

exit

# supprime les fichiers temporaires pour le mois n 
for mois in ${liste_mois} ; do
ficrecept_root=${repertoire_sorties}${mois}/Evol-recept-${recept_n[$i]}
ficrecept_csv=${ficrecept_root}.csv
if [ -f ${ficrecept_csv} ] ; then
rm ${ficrecept_csv}
#echo "${ficrecept_csv} detruit"
fi
done

# SUPPRESSION
rm -rf ${TMPDIR}/pol.txt ${TMPDIR}/recept_tmp

