#!/bin/ksh
# transfert par ftp vers une machine / répertoire
#
# FTr 2009/11/13
################################
fic=$1
ChemDist=$2
#ChemServ=$3
#.......................................
#cd $ChemServ
ftp -n -v << FINftp
open 172.16.30.89
user troude parapente
cd $ChemDist
prompt off
bin
mput $fic  
quit
FINftp

