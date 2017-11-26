#!/bin/ksh
#
# FT 2009/09/03
#############################
code_domaine=$1
ChemRes=$2

echo "================================================="
echo "====== extraction emissions pour ${code_domaine} ======="
echo "================================================="

ficres=${ChemRes}/grille_${code_domaine}.sh  
. ${ficres}

mkdir -p ${ChemRes}

#..Recup num des rues
if [ ! -f ${ficrues} ] ; then
 echo "fichier inexistant !! ${ficrues}"
 echo "bye bye"
 exit
fi
 
list=`awk '{ if(NR>1) {printf("%s\n",$1)}  }' ${ficrues}`
nb=`awk '{ if(NR==1) {printf("%s\n",$1)}  }' ${ficrues}`

echo "fichier source : ${ficemisruesrc}"
rm -f ${ficemisrues}
rm -rf ${ficemisrues}.tmp
touch ${ficemisrues}.tmp
for nrue in $list ; do
   awk '{ if($1==nrue) print($0) }' nrue=${nrue} ${ficemisruesrc} >> ${ficemisrues}.tmp
done #ntyp
#
#...format sirane
echo ${nb}              > ${ficemisrues}
cat  ${ficemisrues}.tmp >> ${ficemisrues}
rm   ${ficemisrues}.tmp

#..menage
echo "-> ${ficemisrues}"
head -n 5 ${ficemisrues}
