#!/bin/bash
#############################################
# Fournit les propriétés des mini-domaines
#############################################
localdir=`pwd`

export cartoprox=`pwd | gawk -F '/scripts' '{print $1}'`

annee_defaut=2010 # dans cartoprox.inc
deb_j=${annee_defaut}0101
fin_j=${annee_defaut}1231
deb_h=00
fin_h=23

case $# in
1)selection=$1;;
*)
echo "Syntaxe $0 selection"
echo "Continue avec tous les recepteurs de la selection ${selection}? ENTER"
read ok
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
annee=`date -d "${deb_j}" +%Y`

#Include
source ${cartoprox}/include/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
############################################################################################

id=`date +%Y%m%d"-"%H%M%S`
liste_domaines_run=${localdir}/tmp/liste_domaines_run.${id}
while [ -f ${liste_domaines_run} ] ; do
id=`date +%Y%m%d"-"%H%M%S`
liste_domaines_run=${localdir}/tmp/liste_domaines_run.${id}
done

##########################################################################
echo "Domaines actifs pour ${cartoprox_domaine} extrait de ${params_mailles} --> ${liste_domaines_run}"

# Liste + classement
#gawk '( $1 =="'${cartoprox_domaine}'" && $5 != 0 ) { print $2 }'  ${params_mailles} | sort > ${liste_domaines_run}
awk '( $1 =="'${cartoprox_domaine}'"             ) { print $2 }'  ${params_mailles} | sort > ${liste_domaines_run}

# nombre de mini domaines
ndom=`cat ${liste_domaines_run} | wc -l` 

echo "Selection${selection} --> ${ndom} mini-domaines"

# Fichier info

fic_infos=${cartoprox}/infos/infos_domaine_${cartoprox_domaine}${selection}.txt
mkdir -p ${cartoprox}/infos
rm -f ${fic_infos}
echo "num;cartoprox_domaine;code_domaine;OccupSol;emis_profil;recepteurs;xc;yc;xb;yb;rues;noeuds;ponct" > ${fic_infos}

echo "***info: remplissage de ${fic_infos}..."

# <<<<< DEBUT BOUCLE SUR LES MINI-DOMAINES >>>>>>
idom=0
cat ${liste_domaines_run} | while read code_domaine ; do
# <<<<< DEBUT BOUCLE SUR LES MINI-DOMAINES >>>>>>
idom=`expr ${idom} \+ 1`

 echo -n -e "\r" "${idom}/${ndom}"
### dependant du domaine de calcul####################################
source ./utils/infos_domaine.sh ${cartoprox_domaine} ${code_domaine} ${params_mailles} ${periode} > /dev/null
######################################################################
#rm log.txt
#echo "${idom}/${ndom}"
echo "${idom};${cartoprox_domaine};${code_domaine};${OccupSol_nom};${emis_profil};${nrecepts};${xc};${yc};${xb};${yb};${nrues};${nnoeuds};${nponct}" >> ${fic_infos}

# <<<<< FIN BOUCLE SUR LES MINI-DOMAINES >>>>>>
done
# <<<<< FIN BOUCLE SUR LES MINI-DOMAINES >>>>>>
echo ""
echo "${fic_infos} OK"
rm ${liste_domaines_run}
