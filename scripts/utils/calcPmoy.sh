#!/bin/ksh
#
# ..calcul de la pression moyenne depuis le fichier aaaa hh val
#
# FT 02/12/2004 appelé par dometeo.sh
#    05/02/2007 1 arg en input
#    23/07/2008 arg differents pourvalence
###########################################################
#
fic=$1
deb=$2
fin=$3

Pmoy=`awk -F" " '{

   if(NR>=deb && NR<=fin) {
                    if($3>0.) { som=som+$3
                                cpt=cpt+1 }
                    if(NR==fin) { som=(som/cpt)*100. 
                                  printf("%7.1f",som)}
                               }
            }' fin=$fin deb=$deb $fic `
echo $Pmoy

