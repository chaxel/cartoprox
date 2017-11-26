gdaldem color-relief NETCDF:"/mnt/mod3/scratch/cartoprox_user_oper/no2_moy_an_x_x_zoom7_lambert93.nc":no2_moy_an echelle_no2_moy_an.txt -of GTiff test.tif
convert ./test.tif -flip ./test.flip.tif
mv ./test.flip.tif ./test.tif
echo test.tif OK
