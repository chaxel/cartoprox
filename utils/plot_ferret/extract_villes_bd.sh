#!/bin/sh -f

#Requete des villes de la BD PREVALP
#E. Chaxel, nov 2010

user=postgres
host=172.16.30.91 #mob-base
base=prevalp

case $# in
4|5)
xmin=$1
ymin=$2
xmax=$3
ymax=$4
format=$5
#NOMBER VILLES############################
limit=10      # Nombre de villes a afficher
;;
6)
xmin=$1
ymin=$2
xmax=$3
ymax=$4
format=$5
limit=$6     # Nombre de villes a afficher
;;
*)
echo "!Extraction de noms de villes de la BD PREVI"
echo "!Usage $0 xmin ymin xmax ymax format"
exit 0
;;
esac

#NOMBER VILLES############################
#limit=10      # Nombre de villes a afficher

#FORMAT DES DONNEES############################
case ${format} in
ferret_utm31_km)
coord=utm31   # Coordonnées latlong, utm31, ... a definir plus bas
fac_km=1000.  # Facteur de passage metres -> kilometres
;;
*)
echo "!Format inconnu"
exit 1
;;
esac

###FORMAT#############################
#case ${format} in
#ferret*)
#gawk -F ";" '{print "LABEL "$1/'${fac_km}'","$2/'${fac_km}'",0,0,0.1,"$3}' ${SCRATCH}/tmp_villes.export 
#LABEL 4.834,45.767, 0, 0,0.1 "Lyon"
#;;
#*)
#echo "Format inconnu"
#;;
#esac

#psql -q -A -h ${host}  -U ${user} -d ${base} <<EOF | sed 's\|\;\g' > ${SCRATCH}/tmp_villes.export
psql -q -A -h ${host}  -U ${user} -d ${base} <<EOF | \
   sed 's\|\;\g' | \
   gawk -F ";" '{print "LABEL "$1/'${fac_km}'","$2/'${fac_km}'",0,0,0.1,"$3}'
\pset tuples_only off

select 
cast(avg(maille_region01km.x_utm31) as integer) as x_centre,
cast(avg(maille_region01km.y_utm31) as integer) as y_centre,
lower(nom_commune),
count(nom_commune) as superficie ,
sum( pop_06_reparti_bati) as population ,
cast(commune.code_insee/1000 as integer) as dept

from commune, communes_region01km, maille_region01km

where 
commune.code_insee=communes_region01km.code_insee
and maille_region01km.x_utm31>${xmin}*${fac_km} and maille_region01km.x_utm31<${xmax}*${fac_km}
and maille_region01km.y_utm31>${ymin}*${fac_km} and maille_region01km.y_utm31<${ymax}*${fac_km}
and maille_region01km.x_utm31 = communes_region01km.x_utm31
and maille_region01km.y_utm31 = communes_region01km.y_utm31

group by nom_commune, dept

order by population desc

limit ${limit}

EOF

#head ${SCRATCH}/tmp_villes.export

#case ${coord} in
#latlong)
# transformation UTM -> long/lat
# il faut convertir UTM 31 en lat/long...
#gawk -F ";" '{print $1" "$2}' tmp_villes.export >  tmp_villes.xy
#gawk -F ";" '{print $3" "$4" "$5" "$6}' tmp_villes.export >  tmp_villes.infos
## conversion en UTM
#syntaxe="${conversion_exe} -utm 31 -geoid WGS84 -i tmp_villes.xy -geo -geoid WGS84 -o tmp_villes.latlong" 
#${syntaxe}> quiet
#paste tmp_villes.latlong tmp_villes.infos > tmp_villes.export
#rm tmp_villes.latlong tmp_villes.xy tmp_villes.infos quiet
#;;
#esac


exit 0


