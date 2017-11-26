#!/bin/sh
#
# CARTOPROX 
# 
# Export des résultats aux récepteurs vers la base de données
###############################################################

source $HOME/.bashrc

localdir=`pwd`

# BDD
bd_serveur=172.16.30.91 # MOD-BASE
bd_user=appli
bd_pwd=appli
bd_data=/data/bdcartoprox

# chemins des fichiers entrees/resultats
export ChemData=/data/CARTOPROX

deb_j=20090101
deb_h=00
fin_j=20100101
fin_h=00

############################################################################################
# domaine de cartographie
############################################################################################
# bourgoin 
xc_p=677500.
yc_p=5053500.
dx_p=9000.
dy_p=${dx_p}

# defini le domaine de PLOT
xmin_p=`echo ${xc_p} | gawk '{print $1 - '${dx_p}'/2. }'`
xmax_p=`echo ${xc_p} | gawk '{print $1 + '${dx_p}'/2. }'`
ymin_p=`echo ${yc_p} | gawk '{print $1 - '${dy_p}'/2. }'`
ymax_p=`echo ${yc_p} | gawk '{print $1 + '${dy_p}'/2. }'`

# automatique : avec tout domaine dans cartoprox.inc
xmin_p=-9999999
ymin_p=-9999999
xmax_p=9999999
ymax_p=9999999

list_var="no2_moy_an pm10_moy_an nb_dep_50_jour"

case $# in
2)
deb_j=$1
deb_h=00
fin_j=$2
fin_h=00
;;
3)
code_domaine_loc=$1
deb_j=$2
deb_h=00
fin_j=$3
fin_h=00
;;
4)
deb_j=$1
deb_h=$2
fin_j=$3
fin_h=$4
;;
5)
code_domaine_loc=$1
deb_j=$2
deb_h=$3
fin_j=$4
fin_h=$5
;;
*)
echo "Syntaxe $0 cartoprox_domaine (code_domaine) debut[YYYYMMJJ (HH)] fin[YYYYMMJJ (HH)]"
exit
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
annee=`date -d ${deb_j} +%Y`

#Include
source ${cartoprox}/cartoprox.inc ${periode} || \
  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
############################################################################################

# Seuil de concentration
var_list="no2_moy_an pm10_moy_an nb_dep_50_jour"
var_list_csv=`echo ${var_list} | sed 's/ /,/g`

version_simulation=beta
prefix_fnin=stat
prefix_nc=stat

facteur_conversion=1.00

cartoprox_BD=${ChemRes1}/export_BD
bd_script=${cartoprox_BD}/export_BD/${prefix_fnin}/${cartoprox_domaine}_${version_simulation}${selection}_import.sql
bd_gridfile=${cartoprox_BD}/export_BD/${prefix_fnin}/${cartoprox_domaine}_${version_simulation}${selection}_utm31.csv
bd_domaine=${cartoprox_BD}/export_BD/${prefix_fnin}/domaine${selection}.csv

bd_gridfile_base=${bd_data}/${cartoprox_domaine}_${version_simulation}${selection}_utm31.csv
bd_script_base=${bd_data}/${cartoprox_domaine}_${version_simulation}${selection}_import.sql
bd_domaine_base=${bd_data}/domaine${selection}.csv

table_domaine_sql=domaine${selection}
table_sql=cartoprox_${cartoprox_domaine}${selection}

mkdir -p ${cartoprox_BD}/export_BD/${prefix_fnin}
rm -rf ${bd_gridfile}
touch ${bd_gridfile}

rm -rf ${bd_domaine}
touch ${bd_domaine}

#################################################################################
# Fin des modifications USER
################################################################################

# Optimisation PREVALP ###########################################################
fic_grille_nc=${SCRATCH}/tmp_export_BDD.nc
echo "Genere ${SCRATCH}/tmp_${var}.nc a partir de ${fond_fic_stat}"
ncks -O -v lon,lat,${var_list_csv} ${fond_fic_stat} ${fic_grille_nc}
# Optimisation PREVALP ###########################################################

echo "Parametre du domaine de PLOT (UTM 31) :"
echo "xmin=${xmin_p}"
echo "ymin=${ymin_p}"
echo "xmax=${xmax_p}"
echo "ymax=${ymax_p}"

#selectionne les minidomaines dans le domaine de PLOT
if [ "${code_domaine_loc}" == "" ] ; then
gawk '( $1 =="'${cartoprox_domaine}'" && \
(($7+'${dx}'/2>'${xmin_p}')&&($7-'${dx}'/2<'${xmax_p}')&&($8+'${dx}'/2>'${ymin_p}')&&($8-'${dx}'/2<'${ymax_p}')) \
) { print $2 }' ${params_mailles} > ${localdir}/minidomaines.txt
else
gawk '( $1 =="'${cartoprox_domaine}'" && $2 =="'${code_domaine_loc}'" && \
(($7+'${dx}'/2>'${xmin_p}')&&($7-'${dx}'/2<'${xmax_p}')&&($8+'${dx}'/2>'${ymin_p}')&&($8-'${dx}'/2<'${ymax_p}')) \
) { print $2 }' ${params_mailles} > ${localdir}/minidomaines.txt
fi

ndomaines=`wc -l ${localdir}/minidomaines.txt | gawk '{print $1}'`
echo "Liste des domaines : "${ndomaines}
cat ${localdir}/minidomaines.txt
if [ ${ndomaines} -eq 0 ] ; then
echo "***info : aucun domaine a traiter - STOP"
exit
fi

#echo "Plot CARTOPROX sur minidomaine ${code_domaine} dans 5 secondes..."
run=${deb_j}${deb_h}_${fin_j}${fin_h}
nrs=`./utils/date2iter.sh  ${deb_j} ${deb_h}`
nre=`./utils/date2iter.sh  ${fin_j} ${fin_h}`

if [ ${nre} -eq 1 ] ; then
  fin_j_1=`date -d "${fin_j} 1 day ago" +%Y%m%d`
  nre=`./utils/date2iter.sh  ${fin_j_1} 23`
fi

##################################################################################
echo "-- Script genere automatiquement par $0 le `date`" >  ${bd_script}

echo "-- DROP TABLE ${table_domaine_sql};" >> ${bd_script}
echo "CREATE TABLE ${table_domaine_sql}("  >> ${bd_script}
echo "  cartoprox_domaine  character varying(16) NOT NULL," >> ${bd_script}
echo "  code_domaine character varying(16) NOT NULL," >> ${bd_script}
echo "  id_domaine integer NOT NULL,"   >> ${bd_script}
echo "  emis_profil  integer NOT NULL," >> ${bd_script}
echo "  xc_utm31 integer NOT NULL,"         >> ${bd_script}
echo "  yc_utm31 integer NOT NULL,"         >> ${bd_script}
echo "  xb_utm31 integer NOT NULL,"    >> ${bd_script}
echo "  yb_utm31 integer NOT NULL,"    >> ${bd_script}
echo "  xmin_utm31 integer NOT NULL,"         >> ${bd_script}
echo "  ymin_utm31 integer NOT NULL,"         >> ${bd_script}
echo "  xmax_utm31 integer NOT NULL,"    >> ${bd_script}
echo "  ymax_utm31 integer NOT NULL,"    >> ${bd_script}
echo "  dx real NOT NULL,"              >> ${bd_script}
echo "  cadre_dx real NOT NULL"         >> ${bd_script}
echo "  )" >> ${bd_script}

echo "WITH OIDS;" >> ${bd_script}
echo "ALTER TABLE ${table_domaine_sql} OWNER TO postgres;" >> ${bd_script}
echo "DELETE FROM ${table_domaine_sql};"   >> ${bd_script}
echo "COPY  ${table_domaine_sql} "         >> ${bd_script}
echo "FROM '${bd_domaine_base}' " >> ${bd_script}
echo "WITH DELIMITER AS ';' ;"    >> ${bd_script}

echo "${bd_domaine} OK" 
echo "">> ${bd_script}

echo "-- DROP TABLE ${table_sql};" >>  ${bd_script}
echo "CREATE TABLE ${table_sql}("  >> ${bd_script}
echo "  id_domaine integer NOT NULL," >> ${bd_script}
echo "  x_utm31 real NOT NULL,"       >> ${bd_script}
echo "  y_utm31 real NOT NULL"        >> ${bd_script}
first=1 # Garanti une seul ecriture dans ${bd_script}

##################################################################################

ndom=`wc -l ${localdir}/minidomaines.txt | gawk '{print $1}'`

################################################################################## Boucle sur les mini domaines>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
idom=0
cat ${localdir}/minidomaines.txt | while read code_domaine ; do
################################################################################## Boucle sur les mini domaines>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
idom=`expr ${idom} \+ 1`

# Pour chaque mini-domaine contruit un fichier ${bd_gridfile}.tmp

code_domaine_loc=${code_domaine}

### dependant du domaine de calcul####################################
source ./utils/infos_domaine_BD.sh ${cartoprox_domaine} ${code_domaine} ${periode} #> log.txt
######################################################################

stat_nc=${ChemRes}/${prefix_nc}.${run}.nc

dx_2=`expr ${dx} \/ 2` 
xmin=`expr ${xc} \- ${dx_2}` 
xmax=`expr ${xc} \+ ${dx_2}` 
ymin=`expr ${yc} \- ${dx_2}` 
ymax=`expr ${yc} \+ ${dx_2}` 

echo "${cartoprox_domaine};${code_domaine};${idom};${emis_profil};${xc};${yc};${xb};${yb};${xmin};${ymin};${xmax};${ymax};${dx};${cadre_dx}"
echo "${cartoprox_domaine};${code_domaine};${idom};${emis_profil};${xc};${yc};${xb};${yb};${xmin};${ymin};${xmax};${ymax};${dx};${cadre_dx}"  >> ${bd_domaine}

echo "[${idom}/${ndom}] export sur domaine ${cartoprox_domaine} minidomaine ${code_domaine}  var ${var} seuil ${seuil_var} agregation ${typ_op} profil ${emis_profil}"

echo ficrecept_mailleur=${ficrecept_mailleur}

modele=0
if [ -f ${stat_nc} ] ; then # SIRANE
modele=sirane
else
if [ -f ${ficrecept_mailleur} ] && [ -f ${fic_grille_nc} ] ; then # SIRANE
modele=prevalp
else
echo "[${idom}/${ndom}] introuvable : "
echo ${fic_grille_nc}
echo ${ficrecept_mailleur}
exit
fi
fi

case ${modele} in
sirane)
# EASTING NORTHING
ncks -s '%f \n' -H -C -v easting_pts   ${stat_nc} | gawk '{ print $1 }' > .easting
ncks -s '%f \n' -H -C -v northing_pts  ${stat_nc} | gawk '{ print $1 }' > .northing
;;
prevalp)
gawk '{ print $4 }' ${ficrecept_mailleur} > .easting
gawk '{ print $5 }' ${ficrecept_mailleur} > .northing
gawk '{ print $4" "$5" 0." }' ${ficrecept_mailleur}  > prevalp.tmp
;;
esac

paste  .easting   .northing | gawk '{ print "'${idom}';"$1";"$2}' > ${bd_gridfile}.tmp

for var in ${var_list} ; do 

case ${modele} in
sirane)
echo "Extrait ${var} <- ${stat_nc}"
ncks -s '%f \n' -H -C -v ${var} ${stat_nc} > .list \
   || { echo "$0: ERREUR de l extraction SIRANE de ${var}" ; exit 1 ; }  
;;
prevalp)
echo "Extrait ${var} <- ${fic_grille_nc}"
var_fac=1.
syntaxe="${extract_val_grille_exe} \
    -i ${fic_grille_nc} -xmin ${fond_xmin} -ymin ${fond_ymin} -dx ${fond_dx} -s prevalp.tmp -var ${var}"  
${syntaxe} | gawk '{print $4*'${var_fac}'}' > .list \
|| { echo "$0: ERREUR de l extraction PREVALP de ${var}" ; exit 1 ; }  
;;
esac

mv ${bd_gridfile}.tmp .tmp
paste -d";" .tmp .list | sed 's/ //g' | sed '/;;;/d' >  ${bd_gridfile}.tmp

if [ ${first} -eq 1 ] ; then
echo "  , ${var} real NOT NULL" >> ${bd_script}
fi

done

# supprime ligne invalide
cat ${bd_gridfile}.tmp | sed '/;;;/d' | sed '$d' >>  ${bd_gridfile}
rm .list .tmp
rm .easting .northing

done # Boucle sur les domaines

echo "WITH OIDS;" >> ${bd_script}
echo "ALTER TABLE ${table_sql} OWNER TO postgres;" >> ${bd_script}
echo "DELETE FROM ${table_sql};"   >> ${bd_script}
echo "COPY  ${table_sql} "         >> ${bd_script}
echo "FROM '${bd_gridfile_base}' " >> ${bd_script}
echo "WITH DELIMITER AS ';' ;"     >> ${bd_script}

##################################################################
echo "Resultats dans ${bd_gridfile}"
echo "Script SQL dans ${bd_script}"
##################################################################

##################################################################
# FTP
##################################################################
cd ${cartoprox_BD}/export_BD
cat  << EOF > ftp.tmp
user ${bd_user} ${bd_pwd}
prompt
mkdir ${bd_data}
cd ${bd_data}
put ${bd_domaine}  ${bd_domaine_base}
put ${bd_gridfile} ${bd_gridfile_base}
put ${bd_script}   ${bd_script_base}
EOF
echo '....FTP'
ftp -inv "${bd_serveur}" < ftp.tmp 

echo "Donnees sur ${bd_serveur}"
echo "Repertoire ${bd_data}"
