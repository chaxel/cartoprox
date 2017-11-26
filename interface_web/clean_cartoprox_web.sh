#!/bin/sh -f

source ${HOME}/.bashrc

#Supprime les sorties CARTOPROX dans $webdir
webdir=/mnt/mod3/data/www/html
cartoprox_export=${webdir}/cartoprox/export
cartoprox_log=${webdir}/cartoprox/log
 
day=$1

if [ "${day}" = "" ]  ; then
echo "Syntaxe: $0 [J ou J-n]"
exit
fi

if [ ${day} -lt 19000000 ] ; then
day_back=${day}
today=`date +%Y%m%d`  
day_delete=`date -d "${today} ${day_back} days ago" +%Y%m%d`  
else
day_delete=${day}
fi

echo "Jour traite=${day_delete}"

if [ ${day_delete} != "" ] ; then

if [ -d ${cartoprox_export} ] ; then
if [ `ls  ${cartoprox_export} | grep ${day_delete} | wc -l` -gt 1 ] ; then
cd ${cartoprox_export}
echo "Clean ${cartoprox_export} pour date ${day_delete}"
find ./ -name \*${day_delete}*.txt -delete
find ./ -name \*${day_delete}*.zip -delete
find ./ -name \*${day_delete}*.kmz -delete
find ./ -name \*${day_delete}*.gif -delete
echo "Done"
fi
fi

if [ -d ${cartoprox_log} ] ; then
if [ `ls  ${cartoprox_log} | grep ${day_delete} | wc -l` -gt 1 ] ; then
cd ${cartoprox_log}
echo "Clean ${cartoprox_log} pour date ${day_delete}"
find ./ -name \*${day_delete}*.log -delete
echo "Done"
fi
fi

fi #day_delete OK

echo "***info: fin du script $0"
