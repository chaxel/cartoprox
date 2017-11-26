# Importe les sites des sources ponctuelles
NomBase="-U postgres sirane"
Schema=public
RepertoireData="/data/bdsirane/import"
Ficsources="sources_ponct.csv"
Tablesources="${Schema}.sources_ponctuelles"

INSERTION_BD()
{
psql ${NomBase} <<EOF
copy ${Tablesources}
from '${RepertoireData}/${Ficsources}'
using delimiters ';'
with null as ''
csv header;
EOF
}
#delete from ${TableValPrevalp};

# Programme principal


echo "Import de ${Ficsources} dans ${Tablesources} en cours"
INSERTION_BD
