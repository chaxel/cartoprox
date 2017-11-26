#!/bin/ksh
#
# creation de donnees.dat pour sirane LYON 
# + lancement de run mensuel
# 
# usage : applique le masque de calcul pour les agglos
#########################################################

### FICHIERS ENTREES DE SIRANE
localdir=`pwd`

debug=0

##### debut d'intervention utilisateur
case $# in
3)
periode=$1
agglo=$2
code_domaine=$3
;;
4)
periode=$1
agglo=$2
code_domaine=$3
debug=1
;;
*)
echo "Syntaxe $0 code_maille code_domaine"
exit
;;
esac

annee_loc=9999

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


# test le nombre de recepteurs dans le fichier STATISTIQUES
source ./utils/infos_mailleur.sh ${code_maille} ${code_domaine} ${periode}

#fichier_masque="../inputs/MASQUES/infos_recepteurs_lyon_utm_masque.txt" # cartoprox.inc
#Extrait les recepteurs du masque qui sont dans la maille ${code_domaine}
if [ -f ${ficrecept_mailleur} ] && [ -f ${ficbrin_mailleur} ] ; then

if [ -f ${fichier_masque} ] ; then
sed 's/"//g' ${fichier_masque} | gawk '( $2 == "'${code_domaine}'" ) {print $1" "$3 }' > masque_${code_domaine}.txt
cp ${ficrecept_mailleur} recept_${code_domaine}.txt
cp ${ficbrin_mailleur}   brin_${code_domaine}.txt
gawk -f./utils/masque.awk -v domaine=${code_domaine} masque_${code_domaine}.txt recept_${code_domaine}.txt brin_${code_domaine}.txt
rm recept_${code_domaine}.txt brin_${code_domaine}.txt  masque_${code_domaine}.txt
else
cat ${ficrecept_mailleur} ${ficbrin_mailleur}
fi

fi

