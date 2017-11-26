#!/bin/ksh
#
# FT 2009/09/03
#############################
code_domaine=$1
ChemRes=$2

echo "================================================="
echo "====== extraction emissions noeuds pour ${code_domaine} ======="
echo "================================================="

ficres=${ChemRes}/grille_${code_domaine}.sh  
. ${ficres}

#..Recup num des rues
if [ ! -f ${ficnoeuds} ] ; then
 echo "fichier inexistant !! ${ficnoeuds}"
 echo "bye bye"
 exit
fi
 
list=`awk '{ if(NR>1) {printf("%s\n",$1)}  }' ${ficnoeuds}`
nb=`awk '{ if(NR==1) {printf("%s\n",$1)}  }' ${ficnoeuds}`

rm -f ${ficemisnoeuds}
rm -rf ${ficemisnoeuds}.tmp
touch ${ficemisnoeuds}.tmp

for node in $list ; do
  echo ${node} 0
  echo ${node} 0 >> ${ficemisnoeuds}.tmp
done 
#
#...format sirane
echo ${nb}               >  ${ficemisnoeuds}
cat ${ficemisnoeuds}.tmp >> ${ficemisnoeuds}

rm ${ficemisnoeuds}.tmp
#..menage
echo "-> ${ficemisnoeuds}"
head -n 5 ${ficemisnoeuds}
