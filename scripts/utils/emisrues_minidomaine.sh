#!/bin/bash
case $# in
5)
nom_polluant=$1    # Nom du polluant (NO2, PM10, ...)
code_domaine=$2    # Code du mini-domaine
fic_rues_noeuds=$3 # Fichier de rues/noeuds du mini-domaine
ficemissrc=$4      # Fichiers d'emissions du domaine ENTIER
ficemis=$5         # Fichier resultat
;;
*)
echo "$0: Mauvais nombre d arguments - STOP"
exit 1
;;
esac
#echo "==========================================================="
#echo "====== extraction emissions ${polluant} pour ${code_domaine} ======="
#echo "==========================================================="
if [ ! -f ${ficrues} ] ; then
 echo "fichier inexistant: ${ficrues}"
 exit 1
fi
#..Recup num des rues
list=`awk '{ if(NR>1) {printf("%s\n",$1)}  }' ${fic_rues_noeuds}`
nb=`head -n 1 ${fic_rues_noeuds} | awk '{ if(NR==1) {printf("%s\n",$1)} }' `
nom_fic=`basename ${fic_rues_noeuds}`
case ${nom_fic} in
*rues*)type_emis=rues;;
*noeuds*)type_emis=noeuds;;
*ponct*)type_emis=ponct;;
*)
echo "$0: Impossible de determiner le type d emissions - STOP"
exit 1
;;
esac
#...format sirane
echo ${nb} >  ${ficemis}
case ${nom_polluant} in
NOx)col_num=2;;
NO2)col_num=3;;
PM10)col_num=4;;
PM25)col_num=5;;
esac

case ${type_emis} in
rues|ponct) #RUES ou PONCTUELLES
#A optimiser pour lire une seule fois le fichier de rue
#Creer la condition
condition="\$1==-999"
for rue in ${list} ; do
  condition="${condition}||\$1==${rue}"
done
echo ${condition} 
#for rue in ${list} ; do
#awk '{ if($1==nrue) print($1"\t"$'${col_num}') }' nrue=${rue} ${ficemissrc} >> ${ficemis}
#done
awk '( '${condition}' ) {print($1"\t"$'${col_num}') }' ${ficemissrc} >> ${ficemis}
;;
noeuds)#NOEUDS
for noeud in ${list} ; do
  echo "${noeud};0" | sed -e "s/;/\t/g" >> ${ficemis}
done 
;;
esac
#echo "fin normale de $0"
