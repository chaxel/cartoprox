#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""

A7: test de coupure
Création des fichiers cartoprox.nc à partir de la référence cartoprox.nc et de la différence entre la référence sirane et scénario sirane

Exemple :
  cartoprox_sans_a7 = cartoprox_ref - (sirane_ref - sirane_sans_a7)

Usage :
  ./cartoprox-diff.py idpol cartoprox_ref.nc sirane_ref.nc sirane_sce.nc cartoprox_sce.nc

"""

__version__ = '1.0'

# Historique
#  1.0 : version de base

import os, sys, datetime, time, subprocess
import numpy
try:
	from Scientific.IO.NetCDF import NetCDFFile
except:
	try:
		from netCDF4 import Dataset as NetCDFFile
	except:
		print "erreur: module 'Scientific.IO.NetCDF' ou 'netCDF4' absent !"
		sys.exit(1)

# Fonctions
def color(text, color='red', style='regular'):
        colors = {
          'black': 30,
          'red': 31,
          'green': 32,
          'yellow': 33,
          'blue': 34,
          'purple': 35,
          'cyan': 36,
          'white': 37,
        }
        styles = {
          'regular': 0,
          'bold': 1,
          'underline': 4,
        }
        if color not in colors: color = 'red'
        if style not in styles: style = 'regular'
        return "\033[%s;%sm%s\033[0m" % (styles[style], colors[color], text)

def error(text):
        return color(text, color='red', style='bold')

# Intro
print color("***/ cartoprox-diff.py /***", color='blue', style='bold')

# Lecture des arguments
if len(sys.argv) != 6:
	print __doc__.strip()
	sys.exit(1)
idpol, fncr, fnsr, fnss, fncs = sys.argv[1:]

# Polluants
if idpol == '1': polluants = ['NO2', 'NO', 'O3']
elif idpol == '2': polluants = ['PM10',]
elif idpol == '3': polluants = ['PM25',]
else:
	print error("code polluant inconnu !")
	sys.exit(1)

# Ouverture des fichiers
print "ouverture du fichier cartoprox ref"
dcrs = []
ncf = NetCDFFile(fncr, 'r')
for pol in polluants:
	data = numpy.array(ncf.variables[pol][:])
	dcrs.append(data)
	print " > %s %s" % (pol, data.shape)
times = ncf.variables['Times'][:]
area_pts = ncf.variables['area_pts'][:]
easting_pts = ncf.variables['easting_pts'][:]
northing_pts = ncf.variables['northing_pts'][:]
x_pts = ncf.variables['x_pts'][:]
y_pts = ncf.variables['y_pts'][:]
ncf.close()

print "ouverture du fichier sirane ref"
dsrs = []
ncf = NetCDFFile(fnsr, 'r')
for pol in polluants:
	data = numpy.array(ncf.variables[pol][:])
	dsrs.append(data)
	print " > %s %s" % (pol, data.shape)
ncf.close()

print "ouverture du fichier sirane sce"
dsss = []
ncf = NetCDFFile(fnss, 'r')
for pol in polluants:
	data = numpy.array(ncf.variables[pol][:])
	dsss.append(data)
	print " > %s %s" % (pol, data.shape)
ncf.close()

# Vérification
for i, pol in enumerate(polluants):
	if dcrs[i].shape != dsrs[i].shape or dcrs[i].shape != dsss[i].shape:
		print error("incohérence dans les fichiers !")
		sys.exit(1)
nh, np = dcrs[0].shape

# Calcul
print "calcul cartoprox sce"
dcss = []
for i, pol in enumerate(polluants):
	data = dcrs[i] - (dsrs[i] - dsss[i])
	dcss.append(data)
	print " > %s (min = %.1f, max = %1.f)" % (pol, data.min(), data.max())
del dsrs, dsss, dcrs

# Enregistrement des résultats
print "enregistrement des données"
if fncs[-3:] != '.nc': fncs = "%s.nc" % fncs
ncf = NetCDFFile(fncs, 'w')

# dimensions
ncf.createDimension('Time', None)
ncf.createDimension('DateStrLen', 19)
ncf.createDimension('Point', np)
ncf.sync()
print " > dimensions"

# variables annexes
ncf.createVariable('Times', 'c', ('Time', 'DateStrLen'))
ncf.createVariable('area_pts', 'f', ('Point',))
ncf.createVariable('easting_pts', 'f', ('Point',))
ncf.createVariable('northing_pts', 'f', ('Point',))
ncf.createVariable('x_pts', 'f', ('Point',))
ncf.createVariable('y_pts', 'f', ('Point',))
ncf.sync()
print " > variables"

nctimes = ncf.variables['Times']
nctimes.assignValue(times)
setattr(nctimes, 'long_name', 'date au format CHIMERE/WRF')
ncf.sync()
del times
print " >> Times"

ncarea = ncf.variables['area_pts']
ncarea.assignValue(area_pts)
setattr(ncarea, 'long_name', 'Aire representative du point')
setattr(ncarea, 'units', 'm2')
setattr(ncarea, '_FillValue', -9999)
setattr(ncarea, 'missing_value', -9999)
ncf.sync()
del area_pts
print " >> area_pts"

nceasting = ncf.variables['easting_pts']
nceasting.assignValue(easting_pts)
setattr(nceasting, 'long_name', 'Coordonnees UTM easting des points')
setattr(nceasting, 'units', 'm')
setattr(nceasting, '_FillValue', -9999.)
setattr(nceasting, 'missing_value', -9999.)
ncf.sync()
del easting_pts
print " >> easting_pts"

ncnorthing = ncf.variables['northing_pts']
ncnorthing.assignValue(northing_pts)
setattr(ncnorthing, 'long_name', 'Coordonnees UTM northing des points')
setattr(ncnorthing, 'units', 'm')
setattr(ncnorthing, '_FillValue', -9999.)
setattr(ncnorthing, 'missing_value', -9999.)
ncf.sync()
del northing_pts
print " >> northing_pts"

ncx = ncf.variables['x_pts']
ncx.assignValue(x_pts)
setattr(ncx, 'long_name', 'Coordonnees X des points')
setattr(ncx, 'units', 'm')
setattr(ncx, '_FillValue', -9999.)
setattr(ncx, 'missing_value', -9999.)
ncf.sync()
del x_pts
print " >> x_pts"

ncy = ncf.variables['y_pts']
ncy.assignValue(y_pts)
setattr(ncy, 'long_name', 'Coordonnees Y des points')
setattr(ncy, 'units', 'm')
setattr(ncy, '_FillValue', -9999.)
setattr(ncy, 'missing_value', -9999.)
ncf.sync()
del y_pts
print " >> y_pts"

# polluants
for i, pol in enumerate(polluants):

	ncf.createVariable(pol, 'f', ('Time', 'Point'))
        ncf.sync()

        ncvar = ncf.variables[pol]
        ncvar.assignValue(dcss[i])
        setattr(ncvar, 'long_name', 'Concentration %s' % pol)
	setattr(ncvar, 'units', 'microg/m3')
	setattr(ncvar, '_FillValue', 0.)
	setattr(ncvar, 'missing_value', -9999.)
        ncf.sync()
	dcss[i] = None
	print " >> %s" % pol

# variables globales
setattr(ncf, 'Title', "cartoprox")
setattr(ncf, 'Generated-by', "cartoprox-diff.py")
ncf.sync()

ncf.close()
print "création de %s" % fncs

