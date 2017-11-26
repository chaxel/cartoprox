#!/bin/sh -f

logo_gif=${cartoprox}/data/logos/logo_gie_713.gif
composite_script=/usr/local/bin/composite

save_old=no

find . -name "*.gif" -print | while read file_gif ; do

echo "Ajoute logo GIE Atmo-RA sur ${file_gif}"

rm -rf ./temp.gif
${composite_script} ${logo_gif} ${file_gif} temp.gif
if  [ -f  ./temp.gif ] ; then
  if [ "${save_old}" == "yes" ] ; then
    mv ${file_gif} ${file_gif}.old
  fi
  mv ./temp.gif ${file_gif}
fi
done
