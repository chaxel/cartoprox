#!/bin/bash
#
# CARTOPROX
#
#Scripts utilisé pour exporter au format ASCII les résultats des grilles cartesiennes de CARTOPROX
#Prerequis : genere les grilles de resultats avec 6b_zoom
localdir=`pwd`

export cartoprox_domaine=A7
export periode=20100101_20101231

#rep_cdf=/mnt/mod4/data/CARTOPROX/carto_V31/20070101_20071231/lyon_metro/zoom7_3000_3000_10m/cdf
#rep_ascii=/mnt/mod4/data/CARTOPROX/carto_V31/20070101_20071231/lyon_metro/zoom7_3000_3000_10m/ascii

rep_cdf=/mnt/mod4/data/CARTOPROX/carto_V31/${periode}/${cartoprox_domaine}/zoom7_3000_3000_10m/cdf
rep_ascii=/mnt/mod4/data/CARTOPROX/carto_V31/${periode}/${cartoprox_domaine}/zoom7_3000_3000_10m/ascii

liste_var="no2_moy_an" # nb_dep_50_jour"


ls ${rep_cdf}/*500_*500_3000_3000.cdf > ./liste_cdf

ific=0
nfic=`cat ./liste_cdf| wc -l`

cat ./liste_cdf | while read nom_cdf ; do
ific=`expr ${ific} \+ 1`

#Nom complet du NetCDF
nom_cdf=`basename ${nom_cdf}`
fic_cdf=${rep_cdf}/${nom_cdf}

#Nom et nom complet de l'ASCII
nom_ascii=`echo ${nom_cdf} | sed -e "s/.cdf/.ascii/"`
fic_ascii=${rep_ascii}/${nom_ascii}

echo "[${ific}/${nfic}] fic_cdf=${fic_cdf}"
echo "[${ific}/${nfic}] fic_ascii=${fic_ascii}"

echo "[${ific}/${nfic}] ${cartoprox}/utils/carto_gmt/export_cdf_ascii_V2.sh ${fic_cdf} "${liste_var}" > ${fic_ascii}"
mkdir -p ${rep_ascii}
${cartoprox}/utils/carto_gmt/export_cdf_ascii_V2.sh ${fic_cdf} "${liste_var}" | awk '{print $1";"$2";"$3";"$4}' > ${fic_ascii}

done

rm ./liste_cdf

