#!/bin/bash

## Cartoprox : calcul pour les tests de coupure
## cartoprox_sce = cartoprox_ref - (sirane_ref - sirane_sce)

pols="nox pm10" # pm25
ref=ref2011
sces="valence_ssE3"  #"sansA7 sansE3 sansPL"   #"za7 ze2 zpl"

echo "/**** `basename $0` ****/"
echo "cartoprox_domaine = ${cartoprox_domaine}"
echo "selection = ${selection}"
echo "periode = ${periode}"
echo
echo "waiting 5 sec."
sleep 5
echo

periodeh=`echo ${periode} | cut -d'_' -f1`00_`echo ${periode} | cut -d'_' -f2`23

for sce in ${sces}
do
	echo "scenario = ${sce}"

	for pol in ${pols}
	do

		echo "polluant = ${pol}"

		case ${pol} in
			nox|NOx) export polluant=1 ; pref=nox ;;
			pm10|PM10) export polluant=2 ; pref=PM10 ;;
			pm25|PM25) export polluant=3 ; pref=PM25 ;;
			*) echo "polluant ${pol} inconnu !"; exit 1 ;;
		esac
		
		# Include
		source ${cartoprox}/include/zoom.inc
		source ${cartoprox}/include/common.inc
		source ${cartoprox}/cartoprox.inc ${periode} || \
		  { echo "Erreur dans ${cartoprox}/cartoprox.inc" ; exit 1 ; }
		
		# Liste des mini-domaines
		fnmd=${cartoprox}/selections/mailles_${cartoprox_domaine}${selection}.txt
		echo $fnmd
		mds=`awk '{ if ($5 != 0) print $2 }' ${fnmd}`
		nmd=`awk '{ if ($5 != 0) print $2 }' ${fnmd} | wc -l`
		echo ">> ${nmd} mini-domaines avec sirane"
		
		# Boucle sur les mini-domaines
		for code_domaine in ${mds}
		do
		
			source ./utils/infos_domaine.sh ${cartoprox_domaine} ${code_domaine} ${periode}
		
			echo "====================================================================================================="
			echo "Domaine ${cartoprox_domaine} mini-domaine ${code_domaine} xc=${xc} yc=${yc} dx=${dx} cadre_dx=${cadre_dx}"
			echo "====================================================================================================="
			
			#refdir=${ChemRes1}.ref/${code_domaine}
			refdir=${ChemRes1}_${ref}/${code_domaine}
			scedir=${ChemRes1}_${sce}/${code_domaine}
			
			# Liste des fichiers
			cpxref=${refdir}/cartoprox_${pref}.${periodeh}.nc
			cpxsce=${scedir}/cartoprox_${pref}.${periodeh}.nc
			sirref=${refdir}/sirane_${pref}.${periodeh}.nc
			sirsce=${scedir}/sirane_${pref}.${periodeh}.nc
	
			ls -l ${cpxref} ${cpxsce} ${sirref} ${sirsce}
			
			# Renome l'ancien cartoprox si celui-ci est bien le fichier de départ (n'écrase pas le fichier qui existe déjà)
			if [ -e ${cpxsce} ] ; then
				testorig=`${NETCDFHOME}/bin/ncdump -h ${cpxsce} | grep "cartoprox-diff" | wc -l`
				[ ${testorig} -eq 0 ] && mv ${cpxsce} ${cpxsce}.orig
			fi

			# Calcul de la différence
			${cartoprox}/utils/cartoprox-diff/cartoprox-diff.py ${polluant} ${cpxref} ${sirref} ${sirsce} ${cpxsce}
			[ $? -eq 0 ] || { echo "erreur dans cartoprox-diff.py !"; exit 1; }

			echo

		done # fin mini-domaine

	done # fin polluant

done # fin scenario
