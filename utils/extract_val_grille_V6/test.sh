
fic_nc=ana_h.20090101_20100101_REG01KM.nc
xmin=553000.
ymin=4887000.
dx=1000.
x=653200.
y=4899200.

#cd src ; make clean; make; cd ..
cd src ; make; cd ..
debug= #-debug
#syntaxe="./extract_val_grille.exe -i ${fic_nc} -xmin ${xmin} -ymin ${ymin} -dx ${dx} -x ${x} -y ${y} -it1 4500 -it2 4800 -var $1 ${debug}"
syntaxe="./extract_val_grille.exe -i ${fic_nc} -xmin ${xmin} -ymin ${ymin} -dx ${dx} -s test.xy -it1 4500 -it2 4800 -var $1 ${debug}"
echo ${syntaxe}
${syntaxe} 
