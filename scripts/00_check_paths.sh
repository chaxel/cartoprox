#!/bin/bash

source ../include/common.inc

############################################################################################
#Variables du PATH :
############################################################################################

#cartoprox
if [ "${cartoprox}" == "" ] ; then
  echo -e "$ROUGE""Definir chemin de CARTOPROX dans .bashrc: export cartoprox=" "$NORMAL"
else
  echo -e  "cartoprox="$VERT"${cartoprox}" "$NORMAL"
fi

#Repertoire local
test_localdir=`echo $PATH | grep "./"`
if [ "${test_localdir}" == "" ] ; then
  echo -e "$ROUGE" "Definir ./ dans le PATH dans .bashrc: export PATH=\$PATH:./"
else
  echo -e  "PATH="$VERT"./" "$NORMAL"
fi

#SCRATCH
if [ "${SCRATCH}" == "" ] ; then
  echo -e "$ROUGE""Definir chemin de SCRATCH dans .bashrc: export SCRATCH=" "$NORMAL"
else
  echo -e  "SCRATCH="$VERT"${SCRATCH}" "$NORMAL"
fi

#cartoprox SCRATCH
if [ "${cartoprox_scratch}" == "" ] ; then
  echo -e "$VIOLET""Definir cartoprox_scratch (facultatif): export cartoprox_scratch=" "$NORMAL"
else											      
  echo -e  "cartoprox_scratch="$VERT"${cartoprox_scratch}" "$NORMAL"						      
fi

#compilateur fortran
if [ "${F90}" == "" ] ; then
  echo -e "$ROUGE""Definir chemin de F90 dans .bashrc: export F90=" "$NORMAL"
else
  echo -e "F90="$VERT"${F90}" "$NORMAL"
fi

#librairie projection compilee avec IFORT
if [ "${NETCDFHOME}" == "" ] ; then
  echo -e "Definir chemin de NETCDFHOME dans .bashrc: ""$ROUGE"" export NETCDFHOME=" "$NORMAL"
else
  echo -e  "NETCDFHOME="$VERT"${NETCDFHOME}" "$NORMAL"
fi

#librairie projection compilee avec IFORT
if [ "${PROJECTIONLIB}" == "" ] ; then
  echo -e "$ROUGE""Definir chemin de PROJECTIONLIB_IFORT dans .bashrc: export PROJECTIONLIB=" "$NORMAL"
else
  echo -e "PROJECTIONLIB="$VERT"${PROJECTIONLIB}""$NORMAL"
fi

#librairie projection compilee avec IFORT (si ifort n'est pas le compilateur par defaut)
if  [ "${F90}" != "ifort" ] ; then
if [ "${PROJECTIONLIB_IFORT}" == "" ] ; then
  echo -e "$ROUGE""Definir chemin de PROJECTIONLIB_IFORT dans .bashrc: export PROJECTIONLIB_IFORT=" "$NORMAL"
else
  echo -e "PROJECTIONLIB_IFORT="$VERT"${PROJECTIONLIB_IFORT}" "$NORMAL"
fi
fi

#NCO
if [ "${NCO}" == "" ] ; then
  echo -e "$ROUGE""Definir chemin des utilitaires NCO dans le .bashrc: export NCO=" "$NORMAL"
else											      
  echo -e  "NCO="$VERT"${NCO}" "$NORMAL"						      
fi

############################################################################################
# AUTRES
############################################################################################
#cartoprox_domaine
if [ "${cartoprox_domaine}" == "" ] ; then
  echo -e "$ROUGE""Definir le domaine (region_ARN, A7 stexupery, stetienne) : export cartoprox_domaine=" "$NORMAL"
else											      
  echo -e  "cartoprox_domaine="$VERT"${cartoprox_domaine}" "$NORMAL"						      
fi

#selection
if [ "${selection}" == "" ] ; then
  echo -e "$VIOLET""Definir la selection(facultatif) : export selection=" "$NORMAL"
else											      
  echo -e  "selection="$VERT"${selection}" "$NORMAL"						      
fi
#periode
if [ "${periode}" == "" ] ; then
  echo -e "$VIOLET""Definir la periode(facultatif) : export periode=AAAMMJJ_AAAMMJJ" "$NORMAL"
else											      
  echo -e  "periode="$VERT"${periode}" "$NORMAL"						      
fi

