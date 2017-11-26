#!/usr/bin/env python
# -*- coding: utf-8 -*-

from numpy import arange
from colormath.color_objects import RGBColor, HSVColor


# Configuration de l'échelle de couleur
echelle = {
	# %VL (R  , G  , B  ),
	0.00: (0  , 153, 255),
	0.25: (0  , 204, 255),
	0.50: (153, 255, 0  ),
	0.75: (255, 255, 0  ),
	1.00: (255, 153, 0  ),
	1.50: (255, 0  , 0  ),
	2.00: (204, 0  , 0  ),
	3.00: (0  , 0  , 0  ),
}


def eq(xi, xa, xb, ya, yb):
	""" Résouds l'équation yi = a xi + b à partir de 2 points A et B. """
	xi, xa, xb, ya, yb = float(xi), float(xa), float(xb), float(ya), float(yb)
	a = (yb - ya) / (xb - xa)
	b = ya - a * xa
	yi = a * xi + b
	return yi


def color(v, echelle, tohex=True):
	""" Retourne la couleur d'une valeur intermediaire. """
	# Utilisation d'un régression linéaire des valeurs HSV (hue, saturation, value)
	# de 2 couleurs (même méthode que l'algorithme Lab-LCH d'ArcGIS).
	
	keys = echelle.keys()
	keys.sort()

	if v < min(keys): v = min(keys)
	if v > max(keys): v = max(keys)	
	if v in keys:
		rgb = RGBColor(*echelle[v])
		if tohex: return rgb.get_rgb_hex()
		else: return rgb.get_value_tuple()
			
	kmin, kmax = None, None
	vmin, vmax = None, None
	for i in range(len(keys)-1):
		if v > keys[i] and v < keys[i+1]:
			kmin, kmax = i, i+1
			vmin, vmax = keys[i], keys[i+1]
			break
	if kmin is None or kmax is None or vmin is None or vmax is None: return None

	rgb_a = RGBColor(*echelle[vmin])
	hsv_a = rgb_a.convert_to('hsv')
	
	rgb_b = RGBColor(*echelle[vmax])
	hsv_b = rgb_b.convert_to('hsv')
	
	xa = keys[kmin]
	xb = keys[kmax]
	xi = v
	
	hi = eq(xi, xa, xb, hsv_a.hsv_h, hsv_b.hsv_h)
	si = eq(xi, xa, xb, hsv_a.hsv_s, hsv_b.hsv_s)
	vi = eq(xi, xa, xb, hsv_a.hsv_v, hsv_b.hsv_v)
	
	hsv_i = HSVColor(hi, si, vi)
	rgb_i = hsv_i.convert_to('rgb')
	
	if tohex: return rgb_i.get_rgb_hex()
	else: return rgb_i.get_value_tuple()
	
	
def create_echelle_gdal(echelle, vl):
	""" Création d'une échelle de couleur pour GDAL. """

	vl = float(vl)
	for v in arange(vl * 3., -vl * 0.05, -vl * 0.05).tolist():
		i = v / vl
		c = color(i, echelle, tohex=False)
#		print "{%.3f:<10}  {%.3f:<6}  {%.3f:<6}  {%.3f:<6}"%(v, c[0], c[1], c[2])
#	print "{%s:<10}  {%.3f:<6}  {%.3f:<6}  {%.3f:<6}"%('nv', 255, 255, 255)
		print "%.1f  %.1f  %.1f  %.1f"%(v, c[0], c[1], c[2])
	print "%s  %.1f  %.1f  %.1f"%('nv', 255, 255, 255)

if __name__ == '__main__':

	# Test	
	#for i in range(0, 305, 5):
	#	v = float(i) / 100.
	#	print "{pc:.2f} % VL : {couleur}".format(pc=v, couleur=color(v, echelle, tohex=True))

	# Création d'une échelle pour GDAL
	# Usage: ./colors.py vl
	
	def usage(e=0):
		print "usage: ./colors.py vl"
		sys.exit(e)
		
	import sys
	if len(sys.argv) != 2: usage(1)
	if sys.argv[1] in ['-h', '--help']: usage()
	try: vl = float(sys.argv[1])
	except: usage(1)
	
	create_echelle_gdal(echelle, vl)


