#!/bin/sh

echo Content-Type: text/html
echo ""

/bin/cat << EOM
<HTML>
<HEAD><TITLE>Export PREVALP/CARTOPROX en cours...</TITLE>
</HEAD>
<BODY bgcolor="#cccccc" text="#000000">
<HR SIZE=5>
<H1>Export PREVALP/CARTOPROX en cours...</H1>
<HR SIZE=5>
<P>
<SMALL>
<PRE>
EOM

commande=$( echo $QUERY_STRING | tr '&' ';')
#echo $commande
eval $commande
#echo ${x_utm} ${y_utm} ${var}
#echo La valeur de PREVALP au point x ${x_utm} y ${y_utm} est : 

source /home/oper/.bashrc
source /appli/CARTOPROX/cartoprox.inc 2009
syntaxe="${extract_val_grille_exe} -i ${fond_fic} -it1 ${it1} -it2 ${it2} -xmin ${fond_xmin} -ymin ${fond_ymin} -dx ${fond_dx} -x ${x_utm} -y ${y_utm} -var ${var} -hour"    
echo $syntaxe
echo Requete en cours...
fic_tmp=export_cartoprox.txt
dir_out=/data/web/export
fic_zip=export_cartoprox_$RANDOM.zip
cd /data/SCRATCH/
### HEADER
date > header
echo "Export de la valeur de PREVALP " >> header
echo "Annee 2009 (version=2009v08)" >> header
echo "Polluant ${var} unites=microg/m3" >> header
echo "Point UTM 31 WGS84 x=${x_utm} m y=${y_utm} m" >> header
echo "Moyenne horaire des heures ${it1} a ${it2} en 2009" >> header
### CORPS
rm ${fic_tmp} ${fic_tmp}.1
$syntaxe > ${fic_tmp}.1
if [ -f ${fic_tmp}.1  ] ; then
cat header ${fic_tmp}.1 >    ${fic_tmp}
rm ${fic_zip}
zip  ${dir_out}/${fic_zip} ${fic_tmp}
rm ${fic_tmp} ${fic_tmp}.1
#val=`$syntaxe | awk '{print $4}'`
#echo "$var = $val microg/m3"
echo OK
#affichage windows
echo "Export dans <a href=../export/${fic_zip}>fichier ZIP</a>"
else
echo "Export echoue contacter modelisation@atmo-rhonealpes.org</a>"
fi

/bin/cat << EOM
</PRE>
</SMALL>
<P>
</BODY>
</HTML>
EOM
