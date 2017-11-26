#
# fonction de verification traitement ok
# argument d'appel : $?
# FT 24/08/2005
######################################################
etat=$1
step=$2
#
if test $etat -eq 0
 then
   echo "........ step $step  OK .... "
fi
if test $etat -eq 1
 then
   echo ""
   echo ""
   echo "........ step $step  BUG .... "
   echo "........................==> EXIT "
   echo ""
   echo ""
   echo ""
   exit
fi

