#!/bin/sh -f

#source $HOME/.bashrc

# export les resultats d'un fichier en Lambert2 en UTM

if [ $# -lt 3 ] ; then
echo "Specifier $0 fichier_ascii colonne typ_op[max,avg] pas_de_temps1 pas_de_temps2"
exit
fi

fichier_ascii=$1
icol=$2
typ_op=$3
it1=$4
it2=$5

if [ "${it1}" == "" ] || [ "${it2}" == "" ]  ; then
  it1=1
  it2=`cat ${fichier_ascii} | wc -l`
fi

case ${typ_op} in
max)gawk 'BEGIN { max = -1.E9 };( NR >='${it1}' && NR <='${it2}' && $'${icol}' >= max && $'${icol}' != "" ) { max =  $'${icol}'      }; END { print max   }' ${fichier_ascii};;
avg)gawk 'BEGIN { avg =  0.00 };( NR >='${it1}' && NR <='${it2}'					  ) { avg += $'${icol}';n+=1 }; END { print avg/n }' ${fichier_ascii};;
*)echo "Type inconnu :"${typ_op}
esac
