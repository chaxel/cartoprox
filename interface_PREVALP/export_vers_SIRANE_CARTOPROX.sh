#!/bin/sh
######################################################################################################
# INTERFACE PREVALP/SIRANE
# methode d'interpolation : bilineaire
# GIE Atmo Rhone-Alpes (juillet 2009)
######################################################################################################
# Utilisation : 1 .modifier le fichier ./src/Makefile avec vos options de compilation ifort et NetCDF
#               2. indiquer un label pour votre extraction ${label}
#               3. creer un fichier de stations (${station_file}) dans ./coordonnees_stations
#               4. indiquer le fichier de sortie wrfout_ de WRF dans ${fichier_mto} 
#                  et le type de sortie ${type_sorties}
######################################################################################################
# 2010/11/29 : ECh : ajoute la meteo meteo.dat pour la prevision et le fichier PM
# 2010/08/24 : ECh : corrige bug sur extraction de la pression
# 2010/07/23 : ECh : VERSION 2
# 2010/03/22 : ECh : choix FTP/copie + crrections bugs d'export
# 2009/11/12 : FTr : ChemArchMet et ChemArchChim pour facilitÃ© si dÃ©placement de fichiers...
#                    ChemOut=$4  -> ajout dÃ©placement du rÃ©sultat vers $ChemOut   
######################################################################################################
. ${cartoprox}/include/cartoprox.inc

echo -e "$VERT" "**** Entre dans le script $0 ****" "$NORMAL"

ulimit -s unlimited

localdir=`pwd`
#conversion_exe=${prevalphome}/utils/conversion_geographique/src/conversion.exe

#cartoprox=/mnt/mod4/appli/CARTOPROX
scripts=${cartoprox}/scripts

nstationsmax=10000

option_chimie=2
# Dans version 2, ajoute une option pour la CHIMIE (23/07/2010)
# option 1 = option de la version 1 : utilise l'interpolation au centre de la maille
# option 2 = utilise la moyenne sur n mailles autour du point
option_chimie_n=1

case $# in
3)
code_maille_loc=$1
ficstation=${scripts}/mailles_region_ARN.txt
ChemOut=$2
periode=$3
ificstation=yes
;;
4)
code_maille_loc=$1
ficstation=$2
ChemOut=$3
periode=$4
ificstation=yes
;;
6)
code_maille_loc=$1
code_domaine=$2
lon=$3
lat=$4
ChemOut=$5
periode=$6
ificstation=no
;;
7)
code_maille_loc=$1
code_domaine=$2
lon=$3
lat=$4
ChemOut=$5
deb_user=$6
ndays_user=$7
periode=operationnel
ificstation=no
;;
*)
echo "Syntaxe $0 code_maille_loc fic_stations (code_domaine lon lat) repertoire_sorties periode[YYYYMMDD_YYYYMMDD]"
exit
;;
esac

#SIMULATIONS
case ${periode} in
20*)
echo "Utilise le fichier ${cartoprox}/cartoprox.inc"
#sleep 3
;;
operationnel)
version_simulation="op_mm5.aerosols"
dom_mto_wrf="aucun" 
dom_mto_chimere="REG06KM" 
dom_chimere="REG06KM"
ChemPrefixChim=out
;;
esac

echo ChemOut=${ChemOut}

type_sorties=horaire                                   # choisir donnees en sortie : moyjour, maxjour, horaire
coordonnees_stations=${localdir}/coordonnees_stations  # repertoire ou sont stockees les coordonnes 
 
##################################################################################
# Fichiers PREVALP
##################################################################################
case ${periode} in
20*) #Cas pour calcul annuel
# Charge données CARTOPROX
echo "periode=${periode}"
echo "Utilise ${cartoprox}/cartoprox.inc pour les noms de fichiers ENTREE"
source ${cartoprox}/cartoprox.inc ${periode}
#sleep 3

#Periode d'export (calcul SIRANE)
deb_j=`echo ${periode} | gawk -F "_" '{print $1}'`
fin_j=`echo ${periode} | gawk -F "_" '{print $2}'`
deb_h=00
fin_h=23

#CARTOGRAPHIE ANNUELLE PREVALP
#Noms des fichiers
label=CARTOPROX_${periode}
stations_file=CARTOPROX_coordonnees_stations_SIRANE.txt
#Supprime les fichiers temporaires ?
do_clean=1
;;
*)
echo "Specifier la periode dans $0: periode="
exit 1
;;
esac

if [ "${fichier_chimie}" == "" ] ; then
  echo "ERREUR : indiquer fichier_chimie="
  exit 1
fi  

# repertoire temporaire
tmpdir=${SCRATCH}/tmp.interface_prevalp_cartoprox.${code_maille_loc}
mkdir -p ${tmpdir}
echo "Repertoire temporaire: ${tmpdir}"

# repertoire sortie
tmpdir=${SCRATCH}/tmp.interface_prevalp_cartoprox.${code_maille_loc}


####################################################################################
# genere le fichier stations
####################################################################################
fic_domaines=${tmpdir}/domaines.${code_maille_loc}

# long/lat/alti des stations
case ${ificstation} in
yes)
echo "Lit les stations dans ${ficstation}"
gawk '( $7 > 0 && $7 < 900000. && $8 > 0 && $8 < 6000000.){print}' ${ficstation} > ficstations.tmp
gawk '( $1 =="'${code_maille_loc}'" ) { print $2      }' ficstations.tmp > ${fic_domaines}
gawk '( $1 =="'${code_maille_loc}'" ) { print $7" "$8 }' ficstations.tmp > coord_utm.txt
gawk '( $1 =="'${code_maille_loc}'" ) { print "0. "$2 }' ficstations.tmp > infos.txt

${conversion_exe} -utm 31 -i coord_utm.txt -geo -o coord_geo.txt
paste coord_geo.txt infos.txt | gawk '($4 != ""){print}'> ${coordonnees_stations}/CARTOPROX_coordonnees_stations_SIRANE.txt
rm    coord_geo.txt infos.txt coord_utm.txt 
echo "${coordonnees_stations}/CARTOPROX_coordonnees_stations_SIRANE.txt OK"
#gawk '( $1 =="'${code_maille_loc}'" ) { print $2 }'  ${ficstation} > minidomaines.txt
nstations=`wc -l ${fic_domaines} | gawk '{print $1}'`
nstations_brutes=`wc -l ${ficstation} | gawk '{print $1}'`
if [ ${nstations} -gt ${nstationsmax} ] ; then
  echo "ERREUR : Augmente le nombre de stations max (${nstationsmax}) dans les programmes d'extraction"
  exit
fi
echo "Trouve ${nstations} stations valides sur ${nstations_brutes} dans ${ficstation}"
#sleep 5
;;
*)
echo ${lon} ${lat} 0. ${code_domaine} > ${coordonnees_stations}/CARTOPROX_coordonnees_stations_SIRANE.txt
echo ${code_domaine} > ${fic_domaines}
stations_file=CARTOPROX_coordonnees_stations_SIRANE.txt
;;
esac

############################################################
if [ -f ${fichier_mto_wrf} ] || [ "${periode}" != "operationnel" ] ; then
liste_type="chimie mto_diag mto_wrf"
liste_fic="${fichier_mto_wrf} ${fichier_mto_diag} ${fichier_chimie}"
else
liste_type="chimie mto_diag"
liste_fic="${fichier_mto_diag} ${fichier_chimie}"
fi
############################################################
for fic in ${liste_fic} ; do
  if [ ! -f ${fic} ] ; then
    echo "ERREUR : pas de fichier ${fic}"
    exit 1
  else
    echo "-> ${fic} OK"
  fi
done

domaine1=`gawk '(NR==1){ print $4 }' ${coordonnees_stations}/${stations_file}`

for type in ${liste_type} ; do
fic_csv=${tmpdir}/resultats/${type}/comparaison_${label}_${domaine1}.csv
if [  -f ${fic_csv} ] ; then
 echo "***attention: le fichier existe ->${fic_csv} le supprime dans 1 seconde(s)..."
 sleep 1
 rm ${fic_csv}
fi
done

echo "coordonnees_stations stations_file : ${coordonnees_stations}/${stations_file}"
echo "nlabel=$nlabel stations_file=${stations_file}"

######################################################################################################
# FIN DE MODIFICATIONS UTILSATEUR
######################################################################################################

echo "label="${label}
#sleep 2s

#compile
echo "*** Programmes en cours de compilation pour max.stations=${nstationsmax}"
rm -rf ${localdir}/logs/make.log;touch ${localdir}/logs/make.log
for src in ${liste_type} ; do
echo "-> src_${src}"
cd ${localdir}/src_${src}
rm *.o *.e
make >> ${localdir}/logs/make.log 2>&1
done

# genere les entrees
#wc -l ${coordonnees_stations}/${stations_file}
deb_str=`date -d "${deb_j}" +%Y-%m-%d`"_${deb_h}:00:00"
fin_str=`date -d "${fin_j}" +%Y-%m-%d`"_${fin_h}:00:00"

echo "Dates extraction : ${deb_str} --> ${fin_str}"

echo "'"${deb_str}"'" "'"${fin_str}"'"    >  ${tmpdir}/OUT_MTO.txt
echo "'"${fichier_mto_wrf}"'"             >> ${tmpdir}/OUT_MTO.txt

echo "'"${deb_str}"'" "'"${fin_str}"'"    >  ${tmpdir}/OUT_CHIMERE.txt
echo "'"${fichier_chimie}"'"              >> ${tmpdir}/OUT_CHIMERE.txt
echo ${option_chimie} ${option_chimie_n}  >> ${tmpdir}/OUT_CHIMERE.txt

echo "'"${deb_str}"'" "'"${fin_str}"'"    >  ${tmpdir}/OUT_DIAG.txt
echo "'"${fichier_mto_diag}"'"            >> ${tmpdir}/OUT_DIAG.txt

ln -sf ${coordonnees_stations}/${stations_file} ${tmpdir}/coordonnees_stations.txt

echo "***info : repertoire temporaire ${tmpdir}"

for type in ${liste_type} ; do

#mkdir -p ${localdir}/resultats/${code_maille_loc}/${type}
#fic_csv=${localdir}/resultats/${code_maille_loc}/${type}/comparaison_${label}_${domaine1}.csv
fic_csv=${tmpdir}/resultats/${type}/comparaison_${label}_${domaine1}.csv

# repertoire temporaire pour l'extraction des données aux stations
mkdir -p ${tmpdir}/resultats_stations

# creer le repertoire des sorties
mkdir -p ${tmpdir}/resultats/${type}

if [ ! -f ${fic_csv} ] ; then

cd ${tmpdir}
# Lance le programme d'export
#echo "get_wdot_nc_stations -> ${localdir}/logs/get_wdot_nc_stations_${type}.log"
echo "----------------------------------------------------------------------------"
echo "${localdir}/src_${type}/get_wdot_nc_stations.e RUN..."
echo "----------------------------------------------------------------------------"

${localdir}/src_${type}/get_wdot_nc_stations.e #> ${localdir}/logs/get_wdot_nc_stations_${type}.log 2>&1

cp WDOT ${localdir}/WDOT_${type}

########################################################################################
# Boucle sur les domaines ${fic_domaines}
########################################################################################
idom=0
ndom=`cat ${fic_domaines} | wc -l`
cat ${fic_domaines} | while read code_domaine ; do

idom=`expr ${idom} \+ 1`

ns2=${idom}
if [ ${idom} -le 999 ] ; then
ns2="0"${idom}
fi
if [ ${idom} -le 99 ] ; then
ns2="00"${idom}
fi
if [ ${idom} -le 9 ] ; then
ns2="000"${idom}
fi

#Ecrit l entete du fichier (indique si le traitement a ete efecctue)
ficin=${tmpdir}/resultats_stations/OUT.STATION.${ns2}.csv

#Fichier out
#ficout=${localdir}/resultats/${code_maille_loc}/${type}/comparaison_${label}_${code_domaine}.csv
ficout=${tmpdir}/resultats/${type}/comparaison_${label}_${code_domaine}.csv

# Ecrit l entete du fichier (indique si le traitement a ete efecctue)
cat ${ficin} | sed 's/ //g' \
| gawk '( FNR == 1 ) { print  "code_maille_loc;code_domaine;"$0 }'   \
> ${ficout}

cat ${ficin} | sed 's/ //g' \
| gawk -F ';' ' ( FNR != 1 ) { print  "'${code_maille_loc}';'${code_domaine}';"$0 }'  \
>> ${ficout}

# Supprime le fichier
rm ${ficin}

echo "Mise en forme ${type} ${code_domaine} (${idom}/${ndom}) OK -> ${ficout}" >> ${localdir}/logs/get_wdot_nc_stations_${type}.log 2>&1

done # code_domaine
########################################################################################
# Fin boucle sur les mini-domaines
########################################################################################

else

echo "${fic_csv} OK"

fi

cd ${tmpdir}
rm -rf ./resultats_stations

done # for type in ${liste_type} ; do

cd ${localdir}
########################################################################################
# Boucle sur les mini-domaines
########################################################################################
idom=0
ndom=`cat ${fic_domaines} | wc -l`
cat ${fic_domaines} | while read code_domaine ; do
idom=`expr ${idom} \+ 1`

case ${periode} in
20*)
# Charge données CARTOPROX
echo "periode=${periode}"
source ${cartoprox}/cartoprox.inc ${periode}
ficout_dir=${ChemOut}/${code_domaine}
ficout_pression=${ficout_dir}/${prefix_pression}_${periode}_prevalp.dat
ficout_evolmto=${ficout_dir}/${prefix_evolmeteo}_${periode}_prevalp.dat
ficout_mto=${ficout_dir}/${prefix_meteo}_${periode}_prevalp.dat
ficout_fond_gaz=${ficout_dir}/${prefix_fond}_${periode}_prevalp.dat
ficout_fond_pm=${ficout_dir}/${prefix_fond}_pm10_${periode}_prevalp.dat
mkdir -p ${ficout_dir}
;;
esac

# Sorties existent ?
#if [ ! -f ${ficout_pression} ] || [ ! -f ${ficout_evolmto} ] || [ ! -f ${ficout_mto} ] || [ ! -f ${ficout_fond_gaz} ] || [ ! -f ${ficout_fond_pm} ] ; then

echo "Domaine ${code_domaine} (${idom}/${ndom})"

ns2=${idom}
if [ ${idom} -le 999 ] ; then
ns2="0"${idom}
fi
if [ ${idom} -le 99 ] ; then
ns2="00"${idom}
fi
if [ ${idom} -le 9 ] ; then
ns2="000"${idom}
fi

#fic_diag=${localdir}/resultats/${code_maille_loc}/mto_diag/comparaison_${label}_${code_domaine}.csv
#fic_mto=${localdir}/resultats/${code_maille_loc}/mto_wrf/comparaison_${label}_${code_domaine}.csv
#fic_chim=${localdir}/resultats/${code_maille_loc}/chimie/comparaison_${label}_${code_domaine}.csv

fic_diag=${tmpdir}/resultats/mto_diag/comparaison_${label}_${code_domaine}.csv
fic_mto=${tmpdir}/resultats/mto_wrf/comparaison_${label}_${code_domaine}.csv
fic_chim=${tmpdir}/resultats/chimie/comparaison_${label}_${code_domaine}.csv

fic_fond_gaz=conc_fond-${code_domaine}.txt
fic_fond_pm=conc_fond_pm-${code_domaine}.txt
fic_evolmeteo=evol-meteo-${code_domaine}.dat
fic_meteo=meteo-${code_domaine}.dat
fic_pres=pression_${code_domaine}.dat

type=mto_diag
#simulation;station;i;date;t(deg);sreh(%);dd10;ff10(m/s);obuk;hght(m);usta(m/s);topc(mm);atte;uh(m/s),sigmatheta,cld,k1(s/ppb),k3(s/ppb)
gawk -F ";" '                             {print $3";"$4}' ${fic_diag} | sed 's#-#/#g' > .date_evolmto
gawk -F ";" '                             {print ";"$8";"$7";"$14";"$5";"$10";"$11";"$15";"$16";"$9";"$17";"$18}' ${fic_diag} > .col1_evolmto
gawk -F ";" '                             {print $4}' ${fic_diag} | sed 's#-#/#g'                > .date_mto
gawk -F ";" '                             {print ";"$8";"$7";"$5";"$16";0." }' ${fic_diag}> .col1_mto
echo "-> ${type} OK"

type=mto_wrf
if [ -f ${fic_mto} ] ; then
#simulation;station;i;date;t(deg);q(g/kg);p(hPa);pluie(mm);dd;ff;snow(kg/m2);sw(W/m2);lw(W/m2);u*(m/s);Hcl(m);hfx(W/m2);qfx(kg/m2/s);lh(W/m2);thetas
gawk -F ";" '                             {print ";"$16";"$19}' ${fic_mto} > .col2_evolmto
else
gawk -F ";" '                             {print ";0.;0."}' ${fic_diag}    > .col2_evolmto
fi
echo "-> ${type} OK"

type=chimie
#simulation;station;i;date;NO;NO2;O3;PM10;PM25
#..NO n'est pas nul
gawk -F ";" '( FNR != 1 && ($2 == "'${code_domaine}'" )  ) {print $3";"$4";"$5";"$6";"$7";"$8}' ${fic_chim} \
         | sed 's/_/;/g'| sed 's/:00:00/:00/g'| sed 's/\t//g' | sed 's/ //g' > .tmp1
gawk -F '(;|-)' '{print $4"/"$3"/"$2" "$5";"$6";"$7";"$8}' .tmp1 \
| sed 's/;/\t/g' > ${fic_fond_gaz}
gawk -F '(;|-)' '{print $4"/"$3"/"$2" "$5";"$9}' .tmp1 \
| sed 's/;/\t/g' > ${fic_fond_pm}

echo "-> ${fic_fond_gaz} OK"
echo "-> ${fic_fond_pm} OK"
rm .tmp1

case ${ftp} in
yes)send2mod2.sh ${fic_fond_gaz} ${ChemOut};;
yes)send2mod2.sh ${fic_fond_pm} ${ChemOut};;
esac
#...

paste .date_mto     .col1_mto                   > .tmp_mto
paste .date_evolmto .col1_evolmto .col2_evolmto > .tmp_evolmto
rm .date_mto .col1_mto 
rm .date_evolmto .col1_evolmto .col2_evolmto

# mise en forme EVOL-METEO
# Remplace les ********** par 1.000
# Remplace les NaN par 1.000 (longueur de Monin-Obukov le 01/01 à 00:00
gawk -F ";" '{print $1";"$2";"$3";"$4";"$5";"$6";"$7";"$8";"$9";"$10";"$14";"$11";"$15";"$12";"$13}'  .tmp_evolmto \
| sed 's/_/;/g'          | sed 's/:00:00/:00/g'    | sed 's/\t//g' | sed 's#\*\*\*\*\*\*\*\*\*\*#1.000#g'  | sed 's#NaN#1.000#g' \
| sed 's#date#Date#g'    | sed 's#heure#Heure#g'   | sed 's#ff10(m/s)#Uext#g'  \
| sed 's#dd10#Direct#g'  | sed 's#uh(m/s)#Uh#g'    | sed 's#t(deg)#To#g'       \
| sed 's#hght(m)#h#g'    | sed 's#usta(m/s)#us#g'  | sed 's#cld#Cld#g'         \
| sed 's#hfx(W/m2)#Fo#g' | sed 's#obuk#Lmo#g'      | sed 's#k1(s/ppb)#k1#g' | sed 's#k3(s/ppb)#k3#g' | sed 's# ##g'  > .tmp1
# N'utilise pas la 1ere ligne (header)
gawk -F '(;|/)' '( FNR != 1 ){print $1";"$4"/"$3"/"$2";"$5";"$6";"$7";"$8";"$9";"$10";"$11";"$12";"$13";"$14";"$15";"$16";"$17";"$18";"$19}' .tmp1 \
| sed 's/;/\t/g' > ${fic_evolmeteo}
# Ligne 1 : header
gawk -F '(;|/)' '( FNR == 1 ){print $1";"$4"/"$3"/"$2";"$5";"$6";"$7";"$8";"$9";"$10";"$11";"$12";"$13";"$14";"$15";"$16";"$17";"$18";"$19}' .tmp1 \
| sed 's/;/\t/g' > ${fic_evolmeteo}.header
rm .tmp1 .tmp_evolmto

# mise en forme METEO
# Remplace les ********** par 1.000
# Remplace les NaN par 1.000 (longueur de Monin-Obukov le 01/01 à 00:00
gawk -F ";" '{print $1";"$2";"$3";"$4";"$5";"$6";"$7";"$8";"$9";"$10";"$14";"$11";"$15";"$12";"$13}'  .tmp_mto \
| sed 's/_/;/g'          | sed 's/:00:00/:00/g'    | sed 's/\t//g' | sed 's#\*\*\*\*\*\*\*\*\*\*#1.000#g'  | sed 's#NaN#1.000#g' \
| sed 's#date#Date#g'    | sed 's#heure#Heure#g'   | sed 's#ff10(m/s)#Uext#g'  \
| sed 's#dd10#Direct#g'  | sed 's#uh(m/s)#Uh#g'    | sed 's#t(deg)#To#g'       \
| sed 's#hght(m)#h#g'    | sed 's#usta(m/s)#us#g'  | sed 's#cld#Cld#g'         \
| sed 's#hfx(W/m2)#Fo#g' | sed 's#obuk#Lmo#g'      | sed 's#k1(s/ppb)#k1#g' | sed 's#k3(s/ppb)#k3#g' | sed 's# ##g'  > .tmp1
# N'utilise pas la 1ere ligne (header)
gawk -F '(;|/)' '( FNR != 1 ){print $3"/"$2"/"$1";"$4";"$5";"int($6)";"$7";"int($8)";"$9 }' .tmp1 \
| sed 's/;/\t/g' > ${fic_meteo}
# HEADER
gawk -F '(;|/)' '( FNR == 1 ){print $3"/"$2"/"$1";"$4";"$5";"int($6)";"$7";"int($8)";"$9 }' .tmp1 \
| sed 's/;/\t/g' > ${fic_meteo}.header
rm .tmp1 .tmp_mto


echo "evol-meteo-${code_maille_loc}.dat OK"
case ${ftp} in
yes)send2mod2.sh ${fic_evolmeteo} ${ChemOut};;
esac


###PRESSION
if [ -f ${fic_mto} ] ; then
echo "Export pression..." # depuis ${fic_diag} -> ${fic_pres} "
./export_pression.sh ${fic_mto}       > ${fic_pres}
else
echo "Export pression DEFAUT=1024 hPa..." # depuis ${fic_diag} -> ${fic_pres} "
./export_pression.sh ${fic_meteo} 1024. > ${fic_pres}
fi

case ${ftp} in
yes)send2mod2.sh ${fic_pres} $ChemOut;;
esac

# Ecrit les sorties
mv ${fic_pres} ${ficout_pression}
echo "-> ${ficout_pression}"
mv ${fic_evolmeteo}        ${ficout_evolmto}
mv ${fic_evolmeteo}.header ${ficout_evolmto}.header
echo "-> ${ficout_evolmto}"
mv ${fic_meteo}         ${ficout_mto}
mv ${fic_meteo}.header  ${ficout_mto}.header
echo "-> ${ficout_mto}"
mv ${fic_fond_gaz} ${ficout_fond_gaz}
echo "-> ${ficout_fond_gaz}"
mv ${fic_fond_pm}  ${ficout_fond_pm}
echo "-> ${ficout_fond_pm}"

#rm -rf .col1 .col2 .tmp .tmp1

#fi # sorties existent ?

done # code_domaine

#Supprime la liste des domaines
rm ${fic_domaines}

#Supprime les fichiers temporaires
if [ ${do_clean} -eq 1 ] ; then
rm -rf ${tmpdir}
fi
