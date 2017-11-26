#!/bin/bash

. ../include/common.inc

export cartoprox=`pwd | gawk -F '/scripts' '{print $1}'`

# NCO
if [ "${NCO}" == "" ] ; then
  export NCO=/opt/nco
fi

if [ "${ncks}" == "" ] ; then
  ncks=${NCO}/bin/ncks
fi

deb_h=00
fin_h=23

############################################################################################
# Inputs
############################################################################################
case $# in
2)
deb_j=$1
fin_j=$2
;;
4|5|6|7)
deb_j=$1
deb_h=$2
fin_j=$3
fin_h=$4
fond_fic_maille=$5
sure_fic_maille=$6
stat_fic_maille=$7
;;
*)
echo -e "$BLEU" "Syntaxe $0 deb_j (deb_h) fin_j (fin_h)" "$NORMAL"

exit
;;
esac

############################################################################################
# INCLUDE
############################################################################################
# declaration de la periode de calcul 
if [ "${periode}" == "" ] ; then
  export periode=${deb_j}_${fin_j}
fi

deb_j=`echo ${periode} | gawk -F "_" '{print $1}'`
fin_j=`echo ${periode} | gawk -F "_" '{print $2}'`
annee=`date -d ${deb_j} +%Y`

#Include
source ${cartoprox}/include/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
############################################################################################

case ${polluant} in
1)liste_variable="NO2 NO O3";oper="_h";;
2)liste_variable="${var_pm}";oper="_avg24";;
3)liste_variable="${var_pm}";oper="_h" ;;
*)echo "Specifier le polluant : 0/1/2/3"
exit
;;
esac

##############################################################################################################################
#Extrait PREVALP sur un voisinage (pratique pour les petits domaines)
##############################################################################################################################
echo "Script $0"
echo "Extrait un voisinage de PREVALP correspondant aux recepteurs (reduit le temps de calcul pour les petits domaines)"
#PREVALP est defini sur une grille reguliere ${fond_xmin} ${fond_ymin} ${fond_dx}  ${fond_nx} ${fond_ny}

echo "maille_xmin ${maille_xmin}"
echo "maille_ymin ${maille_ymin}"
echo "fond_xmin ${fond_xmin}"
echo "fond_ymin ${fond_ymin}"
echo "fond_dx ${fond_dx}"

if [ "${maille_xmin}" == "" ] || [ "${maille_ymin}" == "" ] ; then
  echo "Specifier maille_xmin et maille_ymin dans ../include/domaine_${cartoprox_domaine}.inc"
  exit 1
fi

if [ "${fond_xmin}" == "" ] || [ "${fond_ymin}" == "" ] || [ "${fond_dx}" == "" ] ; then
  echo "Specifier fond_xmin, fond_ymin et fond_dx dans ${cartoprox}/include/cartoprox.inc"
  exit 1
fi

#calcule le voisinage de PREVALP                                                          NCO  voisinage pour interpolation
i_min=`echo ${maille_xmin} ${fond_xmin} ${fond_dx} | awk '{print int(($1+$3/2-$2)/$3) + 1 - 1 - 1 }'`
j_min=`echo ${maille_ymin} ${fond_ymin} ${fond_dx} | awk '{print int(($1+$3/2-$2)/$3) + 1 - 1 - 1 }'`
i_max=`echo ${maille_xmax} ${fond_xmin} ${fond_dx} | awk '{print int(($1-$3/2-$2)/$3) + 1 - 1 + 1 }'`
j_max=`echo ${maille_ymax} ${fond_ymin} ${fond_dx} | awk '{print int(($1-$3/2-$2)/$3) + 1 - 1 + 1 }'`
fond_xmin_maille=`echo ${maille_xmin} ${fond_dx} | awk '{print int( $1 - $2 ) }'`
fond_ymin_maille=`echo ${maille_ymin} ${fond_dx} | awk '{print int( $1 - $2 ) }'`
nx_maille=`echo ${i_min} ${i_max} | awk '{print $2 - $1 + 1 }'`
ny_maille=`echo ${j_min} ${j_max} | awk '{print $2 - $1 + 1 }'`

echo "Indices dans la norme NCO (indices de 0 a n)"
echo "i_min ${i_min}"
echo "j_min ${j_min}"
echo "i_max ${i_max}"
echo "j_max ${j_max}"

#Nom du fichier de sortie
rep_sorties=${prevalp_raster}/${code_maille}
if [ "${fond_fic_maille}" == "" ] ; then
  mkdir -p ${rep_sorties}
  fond_fic_maille=${rep_sorties}/out${oper}.${deb_j}${deb_h}_${fin_j}${fin_h}${selection}.nc
fi
if [ "${sure_fic_maille}" == "" ] ; then
  mkdir -p ${rep_sorties}
  sure_fic_maille=${rep_sorties}/suremis${oper}.${deb_j}${deb_h}_${fin_j}${fin_h}${selection}.nc
fi
if [ "${stat_fic_maille}" == "" ] ; then
  mkdir -p ${rep_sorties}
  stat_fic_maille=${rep_sorties}/stat${oper}.${deb_j}${deb_h}_${fin_j}${fin_h}${selection}.nc
fi

#Liste des variables
liste_variable_csv=`echo ${liste_variable} | sed -e "s/ /,/g"`

#Date de PREVALP
nsteps=`ncdump -h ${fond_fic} | grep "currently" | gawk -F '( // |  )' '{print $2}' | sed 's#(##g' | sed 's# currently)##g'`
echo "Time=${nsteps}"
#Liste les dates dans le fichier NetCDF (format AAAAMMJJ_HH)
echo "liste_idate="
#echo ${ncks} -H -s '%c' -v Times ${fond_fic} | sed -e "s/:00:00/\n/g" | sed -e "s/-//g"
${ncks} -H -s '%c' -v Times ${fond_fic} | sed -e "s/:00:00/\n/g" | sed -e "s/-//g" | grep "_" > liste_idate
head -n 1 liste_idate
echo "    (...)"
tail -n 1 liste_idate
echo "--------------------------"
it1=`awk '( $1 == "'${deb_j}_${deb_h}'") {print NR-1}' liste_idate`
it2=`awk '( $1 == "'${fin_j}_${fin_h}'") {print NR-1}' liste_idate`

if [ "${it1}" == "" ] ; then
  echo "Ne trouve pas la date ${deb_j}_${deb_h}:00:00 dans le fichier PREVALP"
  exit 1
fi

if [ "${it2}" == "" ] ; then
  echo "Ne trouve pas la date ${fin_j}_${fin_h}:00:00 dans le fichier PREVALP"
  j1=`date -d "${deb_j}" +%Y%m%d`
  j2=`date -d "${fin_j}" +%Y%m%d`
  diff_j=`expr ${j2} \- ${j1}`
  fin_j=`date -d "${deb_j} ${diff_j} day" +%Y%m%d`
  it2=`awk '( $1 == "'${fin_j}_00'") {print NR-1}' liste_idate`  
  #exit 1
fi
echo "Extrait Times,lon,lat,${liste_variable_csv} it=${it1}->${it2} de ${fond_fic}"
echo -n "${fond_fic_maille}..."

#Extrait PREVALP
syntaxe="${ncks} -O -o ${fond_fic_maille} \
     -d Time,${it1},${it2} \
     -d west_east,${i_min},${i_max} -d south_north,${j_min},${j_max} \
     -v Times,lon,lat,${liste_variable_csv} ${fond_fic}"

#echo $syntaxe
if [ ! -f ${fond_fic} ] ; then
  echo -e "$ROUGE" "ATTENTION: fichier regional n existe pas !" "$NORMAL"
fi

${syntaxe} || { echo  -e "$ROUGE" "ERREUR extraction. Dimensions OK ?" "$NORMAL"; exit 1; }
if [ -f ${fond_fic_maille} ] ; then
echo -e "$VERT" "OK" "$NORMAL"
else
echo -e "$ROUGE" "ECHOUE" "$NORMAL"
exit
fi

#Statistiques
${statistics_exe} ${fond_fic_maille} ${stat_fic_maille} || \
  { echo -e "$ROUGE" "ERREUR du calcul statistique" "$NORMAL"  ; exit 1 ; }

#Suremissions
echo -n "Extrait Times,lon,lat it=${it1}->${it2} de ${fond_fic}"
echo -n "${sure_fic_maille}..."
syntaxe="${ncks} -O -o ${sure_fic_maille} \
     -d Time,${it1},${it2} \
     -d west_east,${i_min},${i_max} -d south_north,${j_min},${j_max} -d Time,0,0 \
     -v Times,lon,lat ${fond_fic}"
#echo $syntaxe
${syntaxe} || { echo  -e "$ROUGE" "ERREUR extraction. Dimensions OK ?" "$NORMAL"; exit 1; }
if [ -f ${sure_fic_maille} ] ; then
echo -e "$VERT" "OK" "$NORMAL"
else
echo -e "$ROUGE" "ECHOUE" "$NORMAL"
exit
fi

echo "xmin=${maille_xmin}"
echo "ymin=${maille_ymin}"
echo "nx=${nx_maille}"
echo "ny=${ny_maille}"
echo "dx=${fond_dx}"
