#!/bin/ksh
#Pour un domaine
#Calcul des coordonnees de la grille de calcul a partir du centre du domaine
#selection des rues contenues dans cette grille 
#MLNG 03/09/2009
#####################################################
code_domaine=$1
ChemRes=$2

echo "================================================="
echo "====== creation rue+noeud pour ${code_domaine} ========="
echo "================================================="
                                                                                                                           
#extrait les coordonnees du centre de la grille  
ficres=${ChemRes}/infoGEOM_${code_domaine}_pol${polluant}.sh
. ${ficres}

# recupère les valeurs de grille_${grid}.sh cree par minigrille.sh

#a partir des coordonnees x y de la grille, recupere les id noeuds 
awk ' { if($2>="'${Xmin}'" && $2<="'${Xmax}'" && $3>="'${Ymin}'" && $3<="'${Ymax}'" ) {printf("%s\n",$0)} }' ${ficnoeudsrc} > ${ficnoeuds}.tmp

nbnoeuds=`awk ' END {print(NR)} ' ${ficnoeuds}.tmp`
echo ${nbnoeuds}     >  ${ficnoeuds}
cat ${ficnoeuds}.tmp >> ${ficnoeuds}

echo "fichier cree ${ficnoeuds}"

#a partir de la liste des noeuds recupere les id des rues 
listenoeuds=`awk '{ if(NR>=1) {printf("%s\n",$1)}  }' ${ficnoeuds}.tmp`

rm ${ficnoeuds}.tmp
#echo $listenoeuds

if [ -f ${ficrues}.tmp ] ; then 
rm ${ficrues}.tmp
fi

touch ${ficrues}.tmp
for noeud in ${listenoeuds} ; do
  awk ' { if($2=="'${noeud}'" || $3=="'${noeud}'" ) {printf("%s\n",$0)}  }' ${ficruessrc} >> ${ficrues}.tmp
done #noeud

sort -u -k1 ${ficrues}.tmp > ${ficrues}.tmp2

nbrues=`awk ' END {printf("%s\n",NR)} ' ${ficrues}.tmp2`

echo ${nbrues}             >  ${ficrues}.descr
cat ${ficrues}.tmp2 | sort >> ${ficrues}.descr

rm ${ficrues}.tmp ${ficrues}.tmp2 

awk '{ if(NR==1) {print $0} 
       if(NR>1) {printf("%s %s %s %s %s %s\n",$1,$2,$3,$4,$5,$6) }
     }' ${ficrues}.descr > ${ficrues}
     
rm ${ficrues}.descr

echo "fichier cree ${ficrues}"
echo "fin normale de $0"
