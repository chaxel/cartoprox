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
echo La valeur de PREVALP au point x ${x_utm} y ${y_utm} est : 

source /home/oper/.bashrc
source /appli/CARTOPROX/cartoprox.inc 2009
syntaxe="${extract_val_grille_exe} -i ${fond_fic} -xmin ${fond_xmin} -ymin ${fond_ymin} -dx ${fond_dx} -x ${x_utm} -y ${y_utm} -var ${var} -hour"    
echo $syntaxe
echo Requete en cours...
fic_out=/mnt/mod3/data/transfert_windows/export_cartoprox.txt
#$syntaxe > ${fic_out}
#val=`$syntaxe | awk '{print $4}'`
#echo "$var = $val microg/m3"
echo OK
#affichage windows
fic_out_windows=`echo ${fic_out} | sed 's#/#\#g'`
echo Export dans ${fic_out_windows}

/bin/cat << EOM
</PRE>
</SMALL>
<P>
</BODY>
</HTML>
EOM
