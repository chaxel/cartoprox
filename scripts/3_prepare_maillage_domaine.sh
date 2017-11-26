#!/bin/ksh
#
# CARTOPROX
# Preparation du reseau SIRANE et des emissions des rues
#

localdir=`pwd`

annee_defaut=2010 # dans cartoprox.inc
deb_j=${annee_defaut}0101
fin_j=${annee_defaut}1231
deb_h=00
fin_h=23

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

case $# in
1)
code_domaine=$1
;;
*)
echo "indiquer $0 code_domaine"
exit
;;
esac

### dependant du domaine de calcul#######################################################
source ./utils/infos_domaine.sh ${cartoprox_domaine} ${code_domaine} #> /dev/null 2>&1 || \
     #{ echo "Erreur dans ./utils/infos_domaine.sh" ; exit 1 ; }
#########################################################################################
echo "sirane_recept_mailleur=${sirane_recept_mailleur}"
mkdir -p ${sirane_recept_mailleur}

# mini-domaine/full-domaine
case ${domaine} in
full)
syntaxe_mailleur="-optimal -i ${ficmif} -o ${ficrecept_mailleur} -brin_dx ${brin_dx} -recept_dx ${recept_dx} -cadre_dx ${cadre_dx}";;
mini)
case ${mailleur_version} in
1.2)
#-i ${fic_mif} -brin_dx 5 -recept_dx ${recept_dx} -cadre_dx 1000 -fac_geom ${fac_recept} \
#         -utm 31 -xc 649500 -yc 4977500 -dx 3000 -d \
#         -o ${txtfile}
fac_recept=1.6 #voir cast-test du mailleur (rapport optimal)
syntaxe_mailleur="-optimal -i ${ficmif} -o ${ficrecept_mailleur} \
                  -brin_dx ${brin_dx} -recept_dx ${recept_dx} -fac_geom ${fac_recept} \
		  -utm 31 -xc ${xc} -yc ${yc} -dx ${dx} -cadre_dx ${cadre_dx}"
;;
*)
syntaxe_mailleur="-optimal -i ${ficmif} -o ${ficrecept_mailleur} \
                  -brin_dx ${brin_dx} -recept_dx ${recept_dx} \
		  -utm 31 -xc ${xc} -yc ${yc} -dx ${dx} -cadre_dx ${cadre_dx}"
;;
esac
;;
esac


echo "***info: ${code_domaine} : debut du traitement `date` "

case ${mailleur} in
optimal)
if [ -f ${ficmif} ] ; then

  rm -rf ${ficrecept_mailleur}

  if [ ! -f  ${ficrecept_mailleur} ] ; then
    echo ${mailleur_exe} ${syntaxe_mailleur} 
    echo "-> ${sirane_recept_mailleur}"
    cd ${sirane_recept_mailleur}
    ${mailleur_exe} ${syntaxe_mailleur} #> ${localdir}/mailleur.log
    cd ${localdir}  
  else 
     echo "*** info: maillage optimal existe dans ${ficrecept_mailleur}..."   
     echo "*** info: `wc -l  ${ficrecept_mailleur} | gawk '{print $1}'` recepteurs"
     #sleep 3
  fi
  if [ -f  ${ficrecept_mailleur} ] ; then
    echo "-> ficrecept_mailleur=${ficrecept_mailleur}"
  else
    echo "ERREUR: mailleur optimal echoue"
    exit    
  fi
  if [ -f ${sirane_recept_mailleur}/brin_pts.txt ] ;then
    mv ${sirane_recept_mailleur}/brin_pts.txt ${ficbrin_mailleur}
    echo "-> ficbrin_mailleur=${ficbrin_mailleur}"
  else
    echo "ERREUR: mailleur optimal ne produit pas ${sirane_recept_mailleur}/brin_pts.txt"
    exit    
  fi  
  
else
  echo "ERREUR: pas de fichier MIF ${ficmif}"
  exit
fi
;;
esac

# grahique GMT
psfile=${sirane_recept_mailleur}/maillage
cat ${ficbrin_mailleur} ${ficrecept_mailleur} | awk  '{print $4/1000." "$5/1000.}' > ${psfile}.all
cat ${ficrecept_mailleur}                     | awk  '{print $4/1000." "$5/1000.}' > ${psfile}.recept
cat ${ficbrin_mailleur}                       | awk  '{print $4/1000." "$5/1000.}' > ${psfile}.brin 
xmin_km=`echo ${xc} ${dx} | awk '{print ($1-$2/2.)/1000.}'`
xmax_km=`echo ${xc} ${dx} | awk '{print ($1+$2/2.)/1000.}'`
ymin_km=`echo ${yc} ${dx} | awk '{print ($1-$2/2.)/1000.}'`
ymax_km=`echo ${yc} ${dx} | awk '{print ($1+$2/2.)/1000.}'`
dx_km=`echo ${dx} | awk '{print $1/1000.}'`
for ext in all recept brin ; do  
  if [ `cat ${psfile}.${ext} | wc -l` -gt 1 ] ; then
    psxy ${psfile}.${ext} -R${xmin_km}/${xmax_km}/${ymin_km}/${ymax_km} -Jx${dx_km}/${dx_km} -Sp -B.5/.5 > ${psfile}.${ext}.ps
    ps2raster -A -P -Tg -E1000 ${psfile}.${ext}.ps    
    echo "${psfile}.${ext}.png OK"
    rm ${psfile}.${ext}.ps
  fi
  rm ${psfile}.${ext} 
done
