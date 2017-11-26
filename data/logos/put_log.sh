#!/bin/sh -f

logo_gif=/appli/PREVALP_200611/data/logos/logo_gie_300.gif

list=`find ./ -name \*.gif -print -maxdepth 1`

ls *.gif

exit

echo ${list} | while read file_gif ; do

echo ${file_gif}
composite ${logo_gif} ${file_gif} temp.gif
mv ${file_gif} ${file_gif}.old
mv temp.gif ${file_gif}

done
