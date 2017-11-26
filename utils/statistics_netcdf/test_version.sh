#!/bin/bash

fic_nc=/mnt/mod4/data/PREVALP_DATA/cartos/carto_2007_annuel/emi2007v31/out.20070101_20080101_REG06KM.nc

mkdir ./tests

for version in 1.0 1.1 ; do

if [ ! -f ./tests/test_v${version}.nc ] ; then
echo -n "Export version ${version} en cours..."
time src_v${version}/statistics.exe ${fic_nc} ./tests/test_v${version}.nc 1> /dev/null
echo "OK"
fi

for var in no2_moy_an o3_moy_an pm10_moy_an nb_dep_50_jour ; do
ncks -v ${var} ./tests/test_v${version}.nc > ./tests/test_${var}_${version}
done

done #version

for var in no2_moy_an o3_moy_an pm10_moy_an nb_dep_50_jour pm10_moy_jour ; do

echo "Test ./tests/test_${var}_1.0 ./tests/test_${var}_1.1"
diff ./tests/test_${var}_1.0 ./tests/test_${var}_1.1

#rm ./tests/test_${var}_1.0 ./tests/test_${var}_1.1

done

