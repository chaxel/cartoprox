#!/bin/ksh
# extraction du fichier noeuds pour 1 minidomaine
# argument d'appel : $1 = grid
#
# FT 2009/09/03
############################################################


# CE SCRIPT N 'EST PAS UTILISE
grid=$1

 echo "================================================="
 echo "====== extraction  noeuds pour $grid ========="
 echo "================================================="

 echo "extraction noeuds maille $grid"
 ficmininoeuds=noeuds_$grid.txt

#...cree la liste des noeuds
 ln -sf ${ficrues} $ChemWork/rues_$grid.tmp
 echo rues_$grid.tmp > $ChemWork/inputnodes.tmp
 echo liste_noeuds.tmp >> $ChemWork/inputnodes.tmp
 echo $ficmininoeuds >> $ChemWork/inputnodes.tmp
 
 echo fichier input : $ChemWork/inputnodes.tmp
#
 ${F90} $ChemProg/extractmininode.f90 -o $ChemProg/extractmininode
 cd $ChemWork
 $ChemProg/extractmininode < $ChemWork/inputnodes.tmp

 listnoeuds=`awk '{ if(NR>1) {printf("%s\n",$1)}  }' $ChemWork/liste_noeuds.tmp`
 nbnoeuds=`awk '{ if(NR==1) {printf("%s\n",$1)}  }' $ChemWork/liste_noeuds.tmp`
#
 echo "nombre de noeuds à recuperer : " $nbnoeuds
#
#...recupere les noeuds & leurs caracteristiques dans le fichier global
 echo $nbnoeuds > $ChemWork/$ficmininoeuds
 for num in $listnoeuds
 do
   echo recupere num $num
   awk '{ if (NR>1) { if ($1==num) print($0) }
        }' num=$num ${ficnoeudsrc} >> $ChemWork/$ficmininoeuds
 done #num

#...warning si vide
 fic=$ChemWork/$ficmininoeuds
 $ChemProg/funcvide.sh $fic
#...range la chambre... 
 mv $ChemWork/$ficmininoeuds ${ficnoeuds}
 rm -r $ChemWork
 echo
 echo "fichier cree : ${ficnoeuds} mininoeuds.sh"
 echo

