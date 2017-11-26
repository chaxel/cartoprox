#!/bin/ksh
# Importe les sites des sources ponctuelles
NomBase="-U postgres sirane"
Schema=public
RepertoireData="/data/bdsirane/import"
Ficsources="sources_ponct.csv"
Tablesources="${Schema}.sources_ponctuelles"
Tableemis="${Schema}.emis_sources_ponct"


INSERTION_BD()
{  
echo "liste des sources ponctuelles"

ListeSourcesPonct=`psql -A -q -P fieldsep=" " ${NomBase} <<EOF
\pset pager 0
\pset footer off
\pset tuples_only off
select nom 
from $Tablesources;
EOF`

echo "${ListeSourcesPonct}"
for source in  ${ListeSourcesPonct} 
do

Ficemis=emis-ponct-${source}-2010.txt
echo "${Ficemis}"

idsources=`psql -A -q -P fieldsep=" " ${NomBase} <<EOF
\pset pager 0
\pset footer off
\pset tuples_only off
select id_sources 
from $Tablesources
where lower(nom) like lower('${source}');
EOF`

echo "${idsources}"

  sed -e 's# #/#g' ${RepertoireData}/2010/${Ficemis} > fic1 
  sed -e 's#\t#/#g' fic1>fic2
  
 gawk -F "/" '
    { if(NR>1) print ('${idsources}'";"$3"-"$2"-"$1" "$4":00;"$5";"$6";"$7";"$8)}' \
     fic2 > fictmp.csv
 
 rm fic1
 rm fic2    
 
 mv fictmp.csv ${RepertoireData}/2010/fictmp.csv
     
psql ${NomBase} <<EOF
copy ${Tableemis}(id_sources,date_emis,emis_no2_gs,emis_no_gs,emis_o3_gs,emis_pm10_gs)
from '${RepertoireData}/2010/fictmp.csv'
using delimiters ';'
with null as '';
EOF
done
}




# Programme principal


echo "Import de ${Ficsources} dans ${Tablesources} en cours"
INSERTION_BD
