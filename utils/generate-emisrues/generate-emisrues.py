#!/usr/bin/env python
# -*- coding: utf-8 -*-

""" Génération des fichiers d'émissions pour Sirane. """

__version__ = '2.0'

## Historique
##  1.0 : version de base
##  1.1 : suppression des fonctions "format" pour être compatible avec python < 2.6
##  2.0 : version multiprocesseur

# Configuration
polluants = ['NOx' , 'NO2' , 'PM10', 'PM25' ]
export    = [ True ,  False,  True ,  True  ]
#emisdir = '/home/jv/cartoprox/input/2020/emissions' ## les identifiants doivent être continu et doivent commencer par 0 !
emisdir = '/mnt/mod2/appli/utilitaire-sirane/script_boucle/result_A7_2011v169' #emi_test_sansA7' ## les identifiants doivent être continu et doivent commencer par 0 !

# Fin de la configuration

import os, sys, datetime, time, multiprocessing
import numpy
from progressbar import ProgressBar

# Fonctions
def dd(s):
	return datetime.date(int(s[0:4]), int(s[4:6]), int(s[6:8]))

def periode2dts(periode):
	di, de = [dd(e) for e in periode.strip().split('_')]
	return (di, de)

def read_rues(fn):
	f = open(fn, 'r')
	ids = []
	n = int(f.readline().strip())
	for i in range(n):
		ids.append(f.readline().strip().split()[0])
	f.close()
	return ids

def liste_heures(di, de):
	dhs = []
	di = datetime.datetime(di.year, di.month, di.day, 0, 0, 0)
	de = datetime.datetime(de.year, de.month, de.day, 23, 0, 0)
	d = di
	while d <= de:
		dhs.append(d)
		d += datetime.timedelta(hours=1)
	return dhs

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

# Taille de la console et autres variables pour le formatage
rows, columns = [int(e) for e in os.popen('stty size', 'r').read().split()]
mxlp = max([len(e) for e in polluants])

# Intro
print color("***/ generate-emisrues.py /***", color='blue', style='bold')
print

# Lecture des paramètres (variable env.)
selection = ""
if 'selection' in os.environ: selection = os.environ['selection']
err = False
for nom in ['cartoprox', 'cartoprox_domaine', 'periode', 'emisdir']:
	if nom in globals():
		print "%s = %s" % (nom, color(globals()[nom], color='green', style='bold'))
	else:
		if not nom in os.environ:
			print "%s = %s" % (nom, error("non définit !"))
			err = True
		else:
			globals()[nom] = os.environ[nom]
			if not globals()[nom]:
				print "%s = %s" % (nom, error("non définit !"))
				err = True
			else:
				print "%s = %s" % (nom, color(globals()[nom], color='green', style='bold'))
print "selection = %s" % color(selection, color='green', style='bold')
print "polluants = %s" % color(", ".join([polluants[i] for i in range(len(polluants)) if export[i]]), color='green', style='bold')
if err:
	print "erreur !"
	sys.exit(1)
print

# Waiting a few seconds
waiting = 10 # sec
print "waiting %s sec." % waiting
try: time.sleep(waiting)
except KeyboardInterrupt:
	print
	sys.exit(0)

# Liste les minidomaines (plusieurs sélections possible)
selections = [e.strip() for e in selection.split(',')]
domaines = []
for selection in selections:
	params_mailles = os.path.join(cartoprox, 'selections', 'mailles_%s%s.txt' % (cartoprox_domaine, selection))
	if not os.path.isfile(params_mailles):
		print error("erreur dans la lecture du fichier de paramètres des mailles !")
		sys.exit(1)
	f = open(params_mailles, 'r')
	for line in f.readlines():
		line = line.strip().split()
		if line[4] == '1': domaines.append(line[1])
	f.close()
domaines.sort()

# pour forcer les domaines suivants... ici valence
domaines = [
#'647500_4981500',
'647500_4978500',
'647500_4975500',
'650500_4981500',
'650500_4978500',
'650500_4975500',
'653500_4981500',
'653500_4978500',
'653500_4975500',
]
print "--> %i mini-domaines" % len(domaines)

# Liste des heures
di, de = periode2dts(periode)
dhs = liste_heures(di, de)
print "--> %s à %s" % (dhs[0], dhs[-1])
print "    %i heures" % len(dhs)

# Création du tableau d'émissions (dh, rue, polluant)
fnemis = os.path.join(emisdir, 'emis-rues-%s.txt' % dhs[0].strftime('%Y%m%d%H'))
if not os.path.isfile(fnemis):
	print error("erreur dans la lecture des fichiers d'émissions !")
	sys.exit(1)
f = open(fnemis, 'r')
totrues = int(f.readline().strip().split()[0])
f.close()
emis = numpy.zeros((len(dhs), totrues, len(polluants)))
print "--> %i rues du réseau entier" % totrues

# Lecture des émissions sur le réseau entier
print "lecture des émissions de l'ensemble du réseau"
pb = ProgressBar(0, len(dhs), columns - 10)
for idh, dh in enumerate(dhs):
	fnemis = os.path.join(emisdir, 'emis-rues-%s.txt' % dh.strftime('%Y%m%d%H'))
	if not os.path.isfile(fnemis):
		print
		print error("erreur dans la lecture des fichiers d'émissions !")
		sys.exit(1)
	f = open(fnemis, 'r')
	n = int(f.readline().strip().split()[0])
	if n != totrues:
		print
		print error("erreur dans la lecture du fichier %s (nombre de rues différent)" % fnemis)
		sys.exit(1)
	for i in range(totrues):
		data = f.readline().strip().split()
		for j in range(len(polluants)):
			emis[idh,i,j] = float(data[j+1])
	f.close()
	#print "{dh} ok".format(dh=dh)
	pb(idh)
del pb

def create_emis(idom, domaine, ndom, cartoprox, cartoprox_domaine, polluants, export, dhs):
	""" Création des émissions pour un domaine. """
	
	# Lecture des identifiants des rues
	rues = os.path.join(cartoprox, 'inputs', 'GEOM', cartoprox_domaine, domaine, 'rues.txt')
	if not os.path.isfile(rues):
		print error("erreur: fichier rues.txt inexistant !")
		return
	ids_rues = read_rues(rues)
	
	try:
		# Création des fichiers d'émissions
		for ipol, pol in enumerate(polluants):
			if not export[ipol]: continue
		
			for idh, dh in enumerate(dhs):
				fnout = os.path.join(cartoprox, 'inputs', 'EMIS', cartoprox_domaine, 'emis_H_valence_ssE3', domaine, pol, 'emis-rues-%s_%s.txt' % (dh.strftime('%Y%m%d%H'), pol))
				if not os.path.isfile(fnout):
					if not os.path.isdir(os.path.dirname(fnout)): os.makedirs(os.path.dirname(fnout))
					f = open(fnout, 'w')
					f.write("%i\n" % len(ids_rues))
					for idrue in ids_rues:
						f.write("%s\t%s\n" % (idrue, emis[idh,idrue,ipol]))
					f.close()
		print ">> domaine %s (%i rues) [%i/%i]" % (domaine, len(ids_rues), idom+1, ndom)
	
	except:
		print error("domaine %s : erreur" % domaine)
		traceback.print_exc(file=sys.stdout)

	

# Boucle sur les minidomaines
print "enregistrement des émissions"
pool = multiprocessing.Pool() # open a pool
for idom, domaine in enumerate(domaines):
	pool.apply_async(create_emis, (idom, domaine, len(domaines), cartoprox, cartoprox_domaine, polluants, export, dhs))
pool.close()
pool.join() # launch jobs


