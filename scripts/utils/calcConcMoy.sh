#!/bin/sh
#
# ..calcul de la concentration moyenne
###########################################################
cartoprox=/appli/CARTOPROX

# Charge les paramètres pour l'année N
source ${cartoprox}/cartoprox.inc > /dev/null 2>&1 || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; } 

case $# in
3)
polluant=$1
xp=$2
yp=$3
;;
5)
polluant=$1
xp=$2
yp=$3
nrs=$4
nre=$5
it_syntaxe="-it1 ${nrs} -it2 ${nre}"
;;
*)
echo "Syntaxe: $0 polluant X Y (metres UTM31) [itstart itstop]"
echo "***infos sur fichier CHIMERE:"
echo fond_fic=${fond_fic}
echo fond_xmin=${fond_xmin}
echo fond_ymin=${fond_ymin}
echo fond_dx=${fond_dx}
exit
;;
esac

# extrait le niveau de fond
case ${polluant} in
NO)pol_fac=1.25;;
NO2)pol_fac=1.91;;
O3)pol_fac=2.0;;
*)echo "Polluant inconnu"
exit
;;	 
esac

syntaxe="${extract_val_grille_exe} \
    -i ${fond_fic} -xmin ${fond_xmin} -ymin ${fond_ymin} -dx ${fond_dx} -x ${xp} -y ${yp} ${it_syntaxe} -var ${polluant}"

#echo "Extraction en cours..."

${syntaxe} | gawk '($4 != ""){print $4*'${pol_fac}'}'
