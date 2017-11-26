#!/bin/ksh
#
# Statistiques de CARTOPROX aux recepteurs
# 
#############################################
localdir=`pwd`
export cartoprox=`pwd | gawk -F '/scripts' '{print $1}'`
${cartoprox}/scripts/utils/header.sh

#Liste des polluants a traiter
liste_polluant="2" #1=no2 2=pm10 3=pm25

#heures par defaut
deb_h=00
fin_h=23

#Parametres entree
case $# in
5)
nom_camp=$1
ficrecept_camp=$2
deb_j=$3
fin_j=$4
deb_camp_j=
deb_camp_h=
fin_camp_j=
fin_camp_h=
typ_op=$5  #avg/h
;;
7)
nom_camp=$1
ficrecept_camp=$2
deb_camp_j=$3
deb_camp_h=$4
fin_camp_j=$5
fin_camp_h=$6
annee_campagne=`date -d ${deb_camp_j} +%Y`
annee=${annee_campagne}
deb_j=${annee}0101
fin_j=${annee}1231
typ_op=$7  #avg/h
;;
11)
nom_camp=$1
ficrecept_camp=$2
deb_j=$3
deb_h=$4
fin_j=$5
fin_h=$6
deb_camp_j=$7
deb_camp_h=$8
fin_camp_j=$9
fin_camp_h=${10}
typ_op=${11}  #avg/h
annee_campagne=`date -d ${deb_camp_j} +%Y`
;;
9)
ficrecept_camp=$1
deb_j=$2
deb_h=$3
fin_j=$4
fin_h=$5
deb_camp_j=$6
deb_camp_h=$7
fin_camp_j=$8
fin_camp_h=$9
;;
10)
ficrecept_camp=$1
deb_j=$2
deb_h=$3
fin_j=$4
fin_h=$5
deb_camp_j=$6
deb_camp_h=$7
fin_camp_j=$8
fin_camp_h=$9
typ_op=${10}  #avg/h
;;
*)
echo "Nombre d arguments incorrect"
echo "Syntaxe $0 : voir script"
exit
;;
esac

if [ "${typ_op}" == "" ] ; then
  typ_op=avg
fi

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
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
############################################################################################
echo "periode="${periode}

#<<<<<<<<<<<<<<<<<<< Boucle sur polluant 1/2>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
for polluant in ${liste_polluant} ; do

# ATTENTION: NE PAS CHANGER l'ORDRE DES POLLUANTS !!!
case ${polluant} in
0)pol_list=TRACEUR;;
1)pol_list="NO2 NO";;   #O3
2)pol_list="PM10";;
3)pol_list="PM25";;
*)
echo "$0: Fixer le polluant: polluant=" 
exit 1
;;
esac

### dependant du domaine de calcul####################################
#source ./utils/infos_domaine.sh ${cartoprox_domaine} ${code_domaine} ${periode}
######################################################################

### ETAPE 1: traite les données des récepteurs
#ficrecept_tmp=${sirane_recept_user}/${code_domaine}_recept.tmp # format X Y Z (lambert 2)

# Recepteurs USER (Marie-Laure)
#ficrecept=${sirane_recept_user}/${code_domaine}_sites.dat

# Recepteurs MAILLEUR = ${ficrecept_mailleur}
#if [ ! -f ${ficrecept} ] || [ "${irecept}" == "no"  ] ; then
#ficrecept=${ficrecept_mailleur}
#echo "Script non valide avec recepteur MAILLEUR"
#exit
#cp ${ficrecept}  ${ficrecept_tmp}
#else
# nombre de recepteur au cas ou on utilise USER
nrecepts=`cat ${ficrecept_camp} | wc -l`
#gawk '( NR != 1 ) {print}' ${ficrecept} > ${ficrecept_tmp}
#fi

# il faut convertir Lambert  2 en UTM 31...
#gawk '{print $1     }' ${ficrecept_tmp} > tmp_${code_domaine}.id
#gawk '{print $2" "$3}' ${ficrecept_tmp} > tmp_${code_domaine}.xy_l2c

# conversion en UTM
#syntaxe="${conversion_exe} -l2 -geoid NTF -i tmp_${code_domaine}.xy_l2c -utm 31 -geoid WGS84 -o tmp_${code_domaine}.xy_utm"
#${syntaxe}
#paste tmp_${code_domaine}.id tmp_${code_domaine}.xy_utm > ${ficrecept_tmp} # fichier ID X_UTM Y_UTM

#
#echo "***info : conversion geo OK"

### ETAPE 2: traite les données du RUN
run=${deb_j}${deb_h}_${fin_j}${fin_h}
nrs=`./utils/date2iter.sh ${deb_j} ${deb_h} ${deb_camp_j} ${deb_camp_h}`
nre=`./utils/date2iter.sh ${deb_j} ${deb_h} ${fin_camp_j} ${fin_camp_h}`
if [ ${nre} -eq 1 ] ; then
  fin_j_1=`date -d "${fin_camp_j} 1 day ago" +%Y%m%d`
  nre=`./utils/date2iter.sh  ${fin_j_1} 23`
fi

### ETAPE 3: extrait les données aux recepteurs
#sdate=`./utils/iter2date.sh ${nrs} ${annee} yyyymmddhh`
#edate=`./utils/iter2date.sh ${nre} ${annee} yyyymmddhh`
sdate=${deb_camp_j}${deb_camp_h}
edate=${fin_camp_j}${fin_camp_h}

stat_result=${StatRes}/stat_${sdate}_${edate}_${nom_camp} # StatRes dans cartoprox.inc
mkdir -p ${stat_result}
echo "Resultats dans: ${stat_result}"

case ${typ_op} in
avg)
# une stat pour tous les recepteurs
stat_recept=${stat_result}/statistiques_sites_campagne-${nom_camp}_pol${polluant}.dat
rm -f ${stat_recept}
touch ${stat_recept}
;;
esac

irecept=0
gawk '{print $1}' ${ficrecept_camp} > ${ficrecept_camp}.nom
echo "Recepteurs :"
cat ${ficrecept_camp}.nom

#<<<<<<<<<<<<<<<<<<< Boucle sur les recepteurs>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cat ${ficrecept_camp}.nom | while read id_recept ; do
irecept=`expr ${irecept} \+ 1`

case ${typ_op} in
h)
# un evol par recepteur
evol_recept=${stat_result}/evol_H-recept-${id_recept}_pol${polluant}.dat
rm -f ${evol_recept}
;;
j)
# un evol par recepteur
evol_recept=${stat_result}/evol_J-recept-${id_recept}_pol${polluant}.dat
rm -f ${evol_recept}
;;
esac

#<<<<<<<<<<<<<<<<<<< Boucle sur les polluants>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
for pol in ${pol_list} ; do

#coordonnées du mini-domaine
x_utm=`gawk '($1 == "'${id_recept}'") {print int($2)}' ${ficrecept_camp}`
y_utm=`gawk '($1 == "'${id_recept}'") {print int($3)}' ${ficrecept_camp}`

case ${typ_op} in
avg)echo "***info: traite recepteur ${id_recept}(${irecept}/${nrecepts}) dates ${deb_camp_j}->${fin_camp_j}(${nom_camp}) polluant ${pol}"

# extrait les resultats de CARTOPROX non horaire
syntaxe="./utils/export_au_point.sh ${x_utm} ${y_utm} ${pol} ${deb_j} ${deb_h} ${fin_j} ${fin_h} avg ${nrs} ${nre} std"
pwd
echo ${syntaxe}

#concatenation des colonnes STAT
case ${pol} in 
NO2|PM*)
echo "recepteur;x_utm;y_utm;C_prevalp_${pol};C_sirane_${pol};C_fond_${pol};C_prox_${pol};C_cartoprox_${pol}" > ${stat_recept}.header1
echo "${id_recept};${x_utm};${y_utm};`${syntaxe}`" > ${stat_recept}.tmp1
;;
*)
echo "C_prevalp_${pol};C_sirane_${pol};C_fond_${pol};C_prox_${pol};C_cartoprox_${pol}" > ${stat_recept}.header2
paste -d ";" ${stat_recept}.header1 ${stat_recept}.header2 > ${stat_recept}.header
mv ${stat_recept}.header ${stat_recept}.header1
rm ${stat_recept}.header2
${syntaxe} > ${stat_recept}.tmp
paste -d ";" ${stat_recept}.tmp1 ${stat_recept}.tmp > ${stat_recept}.tmp2
mv ${stat_recept}.tmp2 ${stat_recept}.tmp1
rm ${stat_recept}.tmp
;;
esac
;;

h|j)echo "***info: traite recepteur ${id_recept}(${irecept}/${nrecepts}) dates ${deb_camp_j}->${fin_camp_j}(${nom_camp}) polluant ${pol} (horaire)"
# extrait les resultats de CARTOPROX horaire

#concatenation des colonnes EVOL

case ${pol} in 
NO2|PM*)
#typ_op=j/h
syntaxe=" ./utils/export_au_point.sh ${x_utm} ${y_utm} ${pol} ${deb_j} ${deb_h} ${fin_j} ${fin_h} ${typ_op} ${nrs} ${nre}  std"
echo "date_heure;C_prevalp_${pol};C_sirane_${pol};C_fond_${pol};C_prox_${pol};C_cartoprox_${pol}" > ${evol_recept}
pwd
echo ${syntaxe}
${syntaxe} | gawk '{print $0 }' >> ${evol_recept}
;;
*)
syntaxe=" ./utils/export_au_point.sh ${x_utm} ${y_utm} ${pol} ${deb_j} ${deb_h} ${fin_j} ${fin_h} ${typ_op} ${nrs} ${nre} no_date"
echo "C_prevalp_${pol};C_sirane_${pol};C_fond_${pol};C_prox_${pol};C_cartoprox_${pol}" > ${evol_recept}.tmp
pwd
echo ${syntaxe}
${syntaxe} >> ${evol_recept}.tmp
mv ${evol_recept} ${evol_recept}.tmp1
paste -d ";" ${evol_recept}.tmp1 ${evol_recept}.tmp > ${evol_recept}
rm ${evol_recept}.tmp ${evol_recept}.tmp1
;;
esac

;;

esac #typ_op


done 
#<<<<<<<<<<<<<<<<<<< Boucle sur les polluants>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


case ${typ_op} in
h)
# un evol par recepteur
echo "-> ${evol_recept}"
;;
avg)
cat ${stat_recept}.tmp1  >> ${stat_recept}
;;
esac

done 
#<<<<<<<<<<<<<<<<<<< Boucle sur les recepteurs>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

rm -f ${ficrecept_camp}.nom

case ${typ_op} in
avg)
mv ${stat_recept} ${stat_recept}.tmp1
cat ${stat_recept}.header1 ${stat_recept}.tmp1 > ${stat_recept} 
rm  ${stat_recept}.header1 ${stat_recept}.tmp1
echo "-> ${stat_recept}"
;;
esac

done 
#<<<<<<<<<<<<<<<<<<< Boucle sur polluant 1/2>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#rm ${ficrecept_tmp} tmp_${code_domaine}.id tmp_${code_domaine}.xy_l2c tmp_${code_domaine}.xy_utm

# do_stat

echo "***info: Fin du script $0"
