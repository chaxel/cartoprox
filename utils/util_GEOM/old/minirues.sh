#!/bin/ksh
# creation des rues.txt
#
# MLNG 2009/09/02
################################
#
grid=$1



mkdir -p ${ChemRes} ${ChemWork}

#ficrue=rues.txt
#ficgrid=liste_mailles3km.txt

 echo "================================================="
 echo "====== extraction  rues pour $minidom ========="
 echo "================================================="

if [ ! -f ${ficruessrc} ] ;  then
 echo "fichier inexistant : ${ficruessrc}"
 echo "bye bye"
 exit
fi

if [ ! -f ${ficgrid} ] ;  then
 echo "fichier inexistant : ${ficgrid}"
 echo "bye bye"
 exit
fi
listgrid=`awk '{ if(NR>1) {printf("%s\n",$1)}  }' ${ficgrid}`

echo $listgrid

   echo "extraction des rues contenues dans la maille ${grid}"
   
   fictmp=rue_${grid}.tmp
   awk '{ if($7==grid) {print($1,$2,$3,$4,$5,$6)} }' grid=$grid ${ficruessrc} > $ChemRes/$fictmp

 echo "compte le nombre de rues dans la maille $grid"
 fictmp=rue_${grid}.tmp
 awk ' END { print(NR) }' $ChemRes/$fictmp > ${ficrues}
 cat $ChemRes/$fictmp >> ${ficrues}

 echo "fichier ecrit : ${ficrues}"

rm -f $ChemRes/*.tmp
