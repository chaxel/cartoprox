#!/bin/sh -f

#Requete des villes de la BD PREVALP
#E. Chaxel, nov 2010

. ${cartoprox}/include/cartoprox.inc

user=postgres
host=172.16.30.91 #mob-base
base=previ

case $# in
1)
mesure=$1
format=kml
;;
2)
mesure=$1
format=$2
;;
*)
echo "!Extraction de stations de la BD PREVI"
echo "!Usage $0 mesure format"
exit 0
;;
esac

coord=latlong


mesures_par_site=mesures_par_site
case ${mesure} in
FF10|T2)mesures_par_site=mesures_par_site_mto;;
esac


# REQUETE SQL
psql -q -A -h ${host}  -U ${user} -d ${base} <<EOF | sed 's\|\;\g' > ${SCRATCH}/tmp_stations.export
\pset tuples_only off

select 
utmx, utmy, libelle_site, datdeb, datfin

from mesure,${mesures_par_site},site

where 
${mesures_par_site}.id_mes=mesure.id_mes
and ${mesures_par_site}.org_site=site.org_site
and mesure.nom_mes = '${mesure}'
and utmx is not null
and utmy is not null

EOF

#head ${SCRATCH}/tmp_stations.export

case ${coord} in
latlong)
#conversion_exe=${cartoprox}/utils/conversion_geographique/src/conversion.exe
# transformation UTM -> long/lat
# il faut convertir UTM 31 en lat/long...
gawk -F ";" '{print $1" "$2     }' ${SCRATCH}/tmp_stations.export >  ${SCRATCH}/tmp_stations.xy
gawk -F ";" '{print $3";"$4";"$5}' ${SCRATCH}/tmp_stations.export >  ${SCRATCH}/tmp_stations.infos
## conversion en UTM
syntaxe="${conversion_exe} -utm 31 -geoid WGS84 -i ${SCRATCH}/tmp_stations.xy -geo -geoid WGS84 -o ${SCRATCH}/tmp_stations.xy" 
${syntaxe} > quiet
gawk '{ print $1";"$2 }' ${SCRATCH}/tmp_stations.xy > ${SCRATCH}/tmp_stations.latlong 
paste -d ";"  ${SCRATCH}/tmp_stations.latlong  ${SCRATCH}/tmp_stations.infos > ${SCRATCH}/tmp_stations.export
rm ${SCRATCH}/tmp_stations.latlong ${SCRATCH}/tmp_stations.xy ${SCRATCH}/tmp_stations.infos quiet
;;
esac

#cat ${SCRATCH}/tmp_stations.export

#exit

###FORMAT#############################
case ${format} in
ferret*)
gawk -F ";"  '{print "LABEL "$1/'${fac_km}'","$2/'${fac_km}'",0,0,0.1,"$3}' ${SCRATCH}/tmp_stations.export 
#LABEL 4.834,45.767, 0, 0,0.1 "Lyon"
;;
kml)
/bin/cat << EOM
<?xml version="1.0" encoding="UTF-8"?>
<kml xmlns="http://www.opengis.net/kml/2.2">
<!--Fichier genere automatiquement le `date` par $HOST script `pwd`$0  -->
<name>Stations mesures QA</name>
EOM
echo "<name>BD stations</name>"
echo "<Folder>"
echo "<name>Stations de mesures ${mesure}</name>"
gawk -F ";" '{print "<Placemark>\n<name>"$3"</name>\n<description>\nMesures de '${mesure}' du "$4" au "$5"\n</description>\n<Point>\n<coordinates>"$1","$2",0</coordinates>\n</Point>\n</Placemark>"}' ${SCRATCH}/tmp_stations.export
echo "</Folder>"
echo "</kml>"
;;
*)
echo "!Format inconnu"
;;
esac

rm ${SCRATCH}/tmp_stations.export

exit 0


