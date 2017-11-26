#!/bin/sh

# exporte les émissions sources ponctuelles pour CARTOPROX
NomBase="-h mod-base -U postgres -d sirane"
Schema=public
Tablesources="${Schema}.sources_ponctuelles"
Tableemis="${Schema}.emis_sources_ponct"
RepExport=${cartoprox}/inputs/PONCTUELLES/lyon_metro

max_h=9999

annee=2007

emis_type=annuelle #annuelle

# Suppression du fichier evol
#rm -f ${RepExport}/*.*

#echo "EXPORT des emissions en cours"

case ${emis_type} in
#############################################################################################
horaire)
#############################################################################################
# formatage de la date
echo "liste des dates"
psql -A -q  -P fieldsep=" " ${NomBase} <<EOF > liste_dates #| while read ${d_emis} ; do 
\pset pager 0
\pset footer off
\pset tuples_only off
select date_emis from emis_sources_ponct group by date_emis order by date_emis limit ${max_h};
EOF

mkdir -p ${RepExport}

cat liste_dates | while read d_emis ; do 

ExtFic=`echo ${d_emis} | sed -e "s/2010/${annee}/g" | sed -e "s/ //g" | sed -e "s/-//g" | awk -F ':' '{print $1}'`
NomFicExport=${RepExport}/emis_ponct_${ExtFic}.txt

# export des émissions de NOX  
echo "emission des ponctuelles ${d_emis} --> ${NomFicExport}"
psql -A -q  -P fieldsep=" " ${NomBase} <<EOF > ${NomFicExport}
\pset pager 0
\pset footer off
\pset tuples_only off
select count(id_sources) from emis_sources_ponct 
where date_emis = '${d_emis}' and id_sources > 63 ;
EOF

#Format Marie-Laure NOx, NO2, PM10, PM2.5
psql -A -q  -P fieldsep=" " ${NomBase} <<EOF >> ${NomFicExport}
\pset pager 0
\pset footer off
\pset tuples_only off
select id_sources, emis_no2_gs+46/30*emis_no_gs as emis_nox_gs,emis_no_gs,emis_no2_gs,emis_PM10_gs
from emis_sources_ponct 
where date_emis = '${d_emis}' 
and id_sources > 63
order by id_sources;
EOF
done # liste des dates
;;
#############################################################################################
annuelle)
#############################################################################################
#NomFicExport=${RepExport}/emis_ponct_${annee}.txt
#echo "emission des ponctuelles ${d_emis} --> ${NomFicExport}"
#Sortie en direct

#Format Marie-Laure NOx, NO2, PM10 
psql -A -q  -P fieldsep=" " ${NomBase} <<EOF #> ${NomFicExport}
\pset pager 0
\pset footer off
\pset tuples_only off
select count(id_sources) from
(select id_sources from emis_sources_ponct where id_sources > 63 group by id_sources ) as a ; 
EOF
psql -A -q  -P fieldsep=" " ${NomBase} <<EOF #>> ${NomFicExport}
\pset pager 0
\pset footer off
\pset tuples_only off
select id_sources, 
avg(emis_no2_gs)+46/30*avg(emis_no_gs) as emis_nox_gs,
avg(emis_no2_gs) as emis_no2_gs,
avg(emis_PM10_gs) as emis_PM10_gs
from emis_sources_ponct 
where id_sources > 63 
group by id_sources
order by id_sources;
EOF
;;
esac
