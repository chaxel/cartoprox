#!/bin/ksh
# extraction du fichier noeuds pour 1 minidomaine
# argument d'appel : $1 = grid
#
# FT 2009/09/03
############################################################

code_domaine=$1
ChemRes=$2

echo "================================================="
echo "====== extraction  noeuds pour ${code_domaine} ========="
echo "================================================="

#extrait les coordonnees du centre de la grille  
ficres=${ChemRes}/infoGEOM_${code_domaine}_pol${polluant}.sh
. ${ficres}

echo "extraction noeuds maille ${code_domaine}"

#...cree la liste des noeuds
echo "'"${ficrues}"'"       >  ${ficnoeuds}.input
echo "'"${ficnoeuds}.tmp"'" >> ${ficnoeuds}.input
 
echo "fichier input : " ${ficnoeuds}.input
#
if [ ! -f ${ChemProg}/extractmininode.exe ] ; then
${F90} -o ${ChemProg}/extractmininode.exe ${ChemProg}/extractmininode.f90 
fi

${ChemProg}/extractmininode.exe <  ${ficnoeuds}.input

listnoeuds=`awk '{ if(NR>1) {printf("%s\n",$1)}  }' ${ficnoeuds}.tmp`
nbnoeuds=`awk '{ if(NR==1) {printf("%s\n",$1)}  }'  ${ficnoeuds}.tmp`
#
echo "nombre de noeuds à recuperer : " ${nbnoeuds}
#
#...recupere les noeuds & leurs caracteristiques dans le fichier global
echo ${nbnoeuds} > ${ficnoeuds}

for num in ${listnoeuds} ; do
  echo "recupere num ${num}"
    awk '{ if (NR>1) { if ($1==num) print($0) } }' num=${num} ${ficnoeudsrc} >> ${ficnoeuds}
done #num

rm ${ficnoeuds}.input ${ficnoeuds}.tmp

echo "fichier cree : ${ficnoeuds}"
echo "fin normale de $0"
