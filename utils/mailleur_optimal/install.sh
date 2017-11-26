#!/bin/ksh
#programme installation mailleur
# E. Chaxel - Atmo Rhone-Alpes - 2009

case $# in
1)export F90=$1;;
esac

if [ "${F90}" == "" ] ; then
  echo "Indiquer le compilateur FORTRAN 90 en declarant"
  echo "install.sh nom_du_compilateur (gfotran, ifort, ...)"
  exit
fi

# option de compilation personnalisée
export FCOPT="-O2"

objs="calcul_brins.o  calcul_recept.o  mailleur_optimal.o  params.o  read_args.o  read_mif.o params.mod  mailleur.e"

case $1 in 
clean)
rm -rf mailleur.e
cd src
rm -rf ${objs}
cd ..
echo "Desinstallation du mailleur complete. Au revoir."
;;
*)
cd src
rm -rf ${objs}
make
cd ..
ln -sf src/mailleur.e .
echo "Installation du mailleur complete. Merci."
;;
esac
