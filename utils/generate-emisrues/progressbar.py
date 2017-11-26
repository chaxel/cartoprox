#!/usr/bin/python
# -*- coding: utf-8 -*-

# source : http://code.activestate.com/recipes/168639/
# code modifié

import sys

class ProgressBar:
	""" ProgressBar
		
		Use :
			bp = ProgressBar(min, max, width)
			iBp = 0
			
			for xxxx:
				do something
				iBp += 1
				bp(iBp) # to update the bar
	"""
	
	def __init__(self, minValue = 0, maxValue = 10, totalWidth=12, prefix=None):
		if totalWidth > 100: totalWidth = 100
		totalWidth = totalWidth + 2
		self.progBar = "[]"   # This holds the progress bar string
		self.min = minValue
		self.max = maxValue
		self.span = maxValue - minValue
		self.width = totalWidth
		self.prefix = prefix
		self.amount = 0       # When amount == max, we are 100% done 
		self.updateAmount(0)  # Build progress bar string
		self._old_pbar = ''
	
	def updateAmount(self, newAmount = 0):
		''' Create/Update the progress bar '''
		if newAmount < self.min: newAmount = self.min
		if newAmount > self.max: newAmount = self.max
		self.amount = newAmount
	
		# Figure out the new percent done, round to an integer
		diffFromMin = float(self.amount - self.min)
		percentDone = (diffFromMin / float(self.span)) * 100.0
		percentDone = round(percentDone)
		percentDone = int(percentDone)
	
		# Figure out how many hash bars the percentage should be
		allFull = self.width - 2
		numHashes = (percentDone / 100.0) * allFull
		numHashes = int(round(numHashes))
	
		# build a progress bar with hashes and spaces
		if self.prefix: self.progBar = self.prefix + " "
		else: self.progBar = ""
		self.progBar += "[" + '.'*numHashes + ' '*(allFull-numHashes) + "]"
	
		# figure out where to put the percentage, roughly centered
		percentString = str(percentDone) + "%"
	
		# slice the percentage into the bar
		self.progBar = self.progBar + " " + percentString
	
	def __str__(self):
		''' Returns the progress bar '''
		return str(self.progBar)
	
	def __call__(self, value):
		''' Updates the count and writes to stdout (only if it has changed) '''
		self.updateAmount(value)
		if self.progBar != self._old_pbar:
			self._old_pbar = self.progBar
			print '\r',
			sys.stdout.write(str(self))
			sys.stdout.flush()
			
			# write a new line if max
			if self.amount == self.max:
				print
	
	def __del__(self):
		''' Close and remove the progress bar. '''
		print
	

if __name__ == '__main__':
	
	# Example

	import time
	sum = 200
	
	print "waiting something"
	pb = ProgressBar(0, sum, 80, "waiting")
	for i in range(sum):
		time.sleep(0.05)
		pb(i)
	del pb
	
