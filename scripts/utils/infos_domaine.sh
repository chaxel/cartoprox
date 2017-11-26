#!/bin/bash
#
# CARTOPROX
#
localdir=`pwd`

case $# in
2)
code_maille=$1
code_domaine=$2
params_mailles_loc=""
;;
3)
code_maille=$1
code_domaine=$2
periode_loc=$3
params_mailles_loc=""
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
exit 1
;;
esac

# Charge les paramètres pour l'année N
source ${cartoprox}/cartoprox.inc ${periode_loc} || \
  { echo "Erreur dans $0: ${cartoprox}/cartoprox.inc" ; exit 1 ; }

if [ "${params_mailles_loc}" != "" ] ; then
  params_mailles=${params_mailles_loc}
#  echo "params_mailles=${params_mailles_loc}"
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

# VERIFICATIONS ##########
if [ "${xc}" == "" ] || [ "${yc}" == "" ] || [ "${dx}" == "" ] || [ "${cadre_dx}" == "" ] ; then
  echo "ERREUR : ne trouve pas domaine ${code_maille} mini-domaine ${code_domaine} dans ${params_mailles}"
  exit 1
else
  # Conversion geographique UTM 31 -> long/lat
  lonc=`${conversion_exe} -xc ${xc} -yc ${yc} -utm 31 -geoid WGS84 -geo | gawk '{print $1}'`
  latc=`${conversion_exe} -xc ${xc} -yc ${yc} -utm 31 -geoid WGS84 -geo | gawk '{print $2}'`

  # Conversion geographique UTM 31 -> L2
  xl2c=`${conversion_exe} -xc ${xc} -yc ${yc} -utm 31 -geoid WGS84 -l2 -geoid NTF | gawk '{print $1}'`
  yl2c=`${conversion_exe} -xc ${xc} -yc ${yc} -utm 31 -geoid WGS84 -l2 -geoid NTF | gawk '{print $2}'`
 
# echo "***Infos sur le mini-domaine***"
# echo "xc=${xc} m"
# echo "yc=${yc} m" 
# echo "dx=${dx} m"
# echo "lonc=${lonc} deg E"
# echo "latc=${latc} deg N" 
# echo "cadre_dx=${cadre_dx} m (TAMPON)"
# echo "profil emissions (V2)=${emis_profil}"  
fi


if [ -f ${ficrecept_mailleur} ] ; then
#  echo "+++++++++++++++++++++++++++++++++  Maillage OPTIMAL OK ++++++++++++++++++++++++++++++++"
  maillage_ok=1
else
#  echo "+++++++++++++++++++++++++++++++++  Maillage OPTIMAL PB ++++++++++++++++++++++++++++++++"
  maillage_ok=0
fi
#echo "->${ficrecept_mailleur}"
#echo "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"

###  Calcul du baricentre
if [ ${maillage_ok} -eq 1 ] ; then

nrecepts=`wc -l ${sirane_recept_mailleur}/${recept_mailleur} | gawk '{print $1}'`
nrecepts_brin=`wc -l ${sirane_recept_mailleur}/${brin_mailleur} | gawk '{print $1}'`

echo "infos_domaines.sh: recepteurs champ=${nrecepts}"
echo "infos_domaines.sh: recepteurs brins=${nrecepts_brin}" 

xb=`./utils/calcBaricentre.sh ${sirane_recept_mailleur}/${recept_mailleur}  X`
yb=`./utils/calcBaricentre.sh ${sirane_recept_mailleur}/${recept_mailleur}  Y`

lonb=`${conversion_exe} -xc ${xb} -yc ${yb} -utm 31 -geoid WGS84 -geo | gawk '{print $1}'`
latb=`${conversion_exe} -xc ${xb} -yc ${yb} -utm 31 -geoid WGS84 -geo | gawk '{print $2}'`

#echo "***Calcul du barycentre MAILLAGE***"
#echo "xb=${xb} m"
#echo "yb=${yb} m"
#echo "lonb=${lonb} deg E"
#echo "latb=${latb} deg N"
else
nrecepts=1 # 1 recepteur virtuel PREVALP
xb=${xc}
yb=${yc}
#echo "***info: pas de recepteurs ! point barycentre est le centre de la maille"
fi

####################### GEOM ##########################
ficnoeuds=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/noeuds.txt
ficrues=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/rues.txt
ficponct=${sirane_geom}/${cartoprox_domaine}/${code_domaine}/ponct.txt

###  Nb brins
if [ -f ${ficrues} ] ; then
  nrues=`head -n 1 ${ficrues}`
else
  nrues=0
fi

###  Nb rues
if [ -f ${ficnoeuds} ] ; then
  nnoeuds=`head -n 1 ${ficnoeuds}`
else
  nnoeuds=0
fi

###  Nb ponct
if [ -f ${ficponct} ] ; then  
  nponct=`head -n 1 ${ficponct}`
else
  nponct=0
fi
######################################################

###  Calcul de l'occupation des sols (0 = rural, 1 = urbain)
if [ ${maillage_ok} -eq 1 ] ; then
OccupSol=`./utils/calcOccupSol.sh ${sirane_recept_mailleur}/${recept_mailleur}`
#Affichage
#./utils/calcOccupSol.sh ${sirane_recept_mailleur}/${recept_mailleur} -v
#echo "***Calcul d occupation des sols***"
OccupSol_nom=${OccupationSol[${OccupSol}]}
case ${OccupSol} in
0|1)
echo "OccupSol=${OccupSol} --> maille ${OccupSol_nom}" > /dev/null
;;
*)
echo "OccupSol=${OccupSol} --> ERREUR"
exit 1
;;
esac
else
#  echo "***info: pas de recepteurs ! la maille est RURALE par defaut"
  OccupSol=0
  #OccupSol_nom non defini
fi

### Parametres physiques
#echo "***Parametres physiques***"
Latitude=${latc}
Rug_quartier=${Rug_quartier_lu[${OccupSol}]}
Epaisseur_deplacement=${Epaisseur_deplacement_lu[${OccupSol}]}
Hmoyenne=${Hmoyenne_lu[${OccupSol}]}
Rugo_bati=${Rugo_bati_lu[${OccupSol}]}

#echo "Latitude=${Latitude}" 
#echo "Rugosite quartier=${Rug_quartier}"
#echo "Epaisseur deplacement=${Epaisseur_deplacement}"
#echo "Hauteur moyenne bati=${Hmoyenne}"
#echo "Rugosite bati=${Rugo_bati}"


######################################################################
