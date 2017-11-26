#!/bin/ksh
#
# Crerer une selection pour CARTOPROX 
# 
# usage : plot_CARTOPROX.sh + le mois voulu
#############################################

localdir=`pwd`
cartoprox=`pwd | gawk -F '/scripts' '{print $1}'`
cartoprox_domaine=region_ARN

# Selections UTILISATEUR
##########################################
case $1 in
# Lyon
# etend de 20 km au nord pour villefranche
all)selection=all
xmin=-9999999
xmax=9999999
ymin=-9999999
ymax=9999999
;;
lyon)selection=lyon
# petit lyon
xmin=635000
xmax=655000
ymin=5060000
ymax=5075000
# moyen lyon
xmin=635000
xmax=658000
ymin=5048000
ymax=5075000
;;
grandlyon)selection=grandlyon
# grand lyon
xmin=620000
xmax=666000
ymin=5045000
ymax=5090000
;;
valence)selection=valence
xmin=638000
xmax=660000
ymin=4966000
ymax=4991000
;;
stetienne)
# stetienne
selection=stetienne
xmin=602000
xmax=621000
ymin=5026000
ymax=5042000
;;
grenoble)
# grenoble
selection=grenoble
xmin=698000
xmax=730000
ymin=4990000
ymax=5021000
;;
arve)
# vallée arve
selection=arve
xmin=749000
xmax=813000
ymin=5083000
ymax=5147000
;;
villefranche)
# villefranche
selection=villefranche
xmin=629000
xmax=639000
ymin=5086000
ymax=5102000
;;
AR_geants)
# villefranche
selection=AR_geants
xmin=683000
xmax=726000
ymin=5101000
ymax=5123000
;;
bourgoin)
# bourgoin
selection=bourgoin
xmin=671000
xmax=687000
ymin=5044000
ymax=5057000
;;
chambery)
# bourgoin
selection=chambery
xmin=722000
xmax=731000
ymin=5045000
ymax=5066000
;;
annecy)
# annecy
selection=annecy
xmin=731000
xmax=750000
ymin=5080000
ymax=5099000
;;
annemasse)
# annemasse
selection=annemasse
xmin=737000
xmax=756000
ymin=5107000
ymax=5129000
;;
*)
echo "Indiquer la selection"
exit
;;
esac
#########################################
# Fin des sections UTILISATEUR
params_mailles_brut=${cartoprox}/selections/mailles_${cartoprox_domaine}.txt
params_mailles_selection=${cartoprox}/selections/mailles_${cartoprox_domaine}_${selection}.txt

gawk '( $1 =="'${cartoprox_domaine}'" && (($7>='${xmin}')&&($7<='${xmax}')&&($8>='${ymin}')&&($8<='${ymax}'))) { print $0 }'\
       ${params_mailles_brut} > ${params_mailles_selection}
  
nlignes=`wc -l ${params_mailles_selection} | gawk '{print $1}'`
echo "-> ${params_mailles_selection} avec ${nlignes} domaines"
echo "Modifier cartoprox.inc avec "
echo "params_mailles=${params_mailles_selection}"
