#!/bin/sh

# exporte les metadonnées sources ponctuelles pour CARTOPROX
NomBase="-h mod-base -U postgres -d sirane"
Schema=public
Tablesources="${Schema}.sources_ponctuelles"
Tableemis="${Schema}.emis_sources_ponct"

psql -A -q  -P fieldsep=" " ${NomBase} <<EOF #> ${NomFicExport}
\pset pager 0
\pset footer off
\pset tuples_only off
select count(id_sources) from
(select id_sources from sources_ponctuelles where id_sources > 63 group by id_sources ) as a ; 
EOF

# Attention: SEPARATEUR espace
# voir les champs dans Io.c de SIRANE
psql -A -q  -P fieldsep=" " ${NomBase} <<EOF | sed -e "s/ //g" | sed -e "s/;/\t/g"
\pset pager 0
\pset footer off
\pset tuples_only off
\pset fieldsep ;
select id_sources, round(cast(xlamb2 as numeric),2) as xlamb2, round(cast(ylamb2 as numeric),2) as ylamb2, 
hauteur, 0 as hauteur_sol, 0 as rs, uz, temp
from sources_ponctuelles 
where id_sources > 63
order by id_sources;
EOF

