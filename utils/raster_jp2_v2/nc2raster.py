#!/usr/bin/env python

import sys
from Scientific.IO.NetCDF import NetCDFFile
from osgeo import gdal
import numpy

var = sys.argv[1]
fni = sys.argv[2]
fno = sys.argv[3]

# Read data
ncf = NetCDFFile(fni, 'r')
var = numpy.array(ncf.variables[var][:])
ncf.close()

# Flip Y axis
var2 = numpy.flipud(var)

# Data information
ny, nx = var2.shape

# Creation raster
driver = gdal.GetDriverByName("GTiff")
dst_ds = driver.Create(fno, nx, ny, 1, gdal.GDT_Float32)
dst_ds.GetRasterBand(1).WriteArray(var2)
dst_ds = None
