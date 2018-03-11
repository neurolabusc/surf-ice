#!/usr/bin/python
#Report contents of mz3 image
# https://github.com/neurolabusc/surf-ice/tree/master/mz3
#Examples:
# python ./mz3.py 3Mesh.mz3
# python ./mz3.py 15TemplateMesh.mz3
import os
import sys
import struct
import gzip
import array

def read(fnm, isVerbose):
	invalid_mz3 = (None, None, None, None)
	mz3_faces = [];
	mz3_verts = [];
	mz3_rbga = [];
	mz3_scalar = [];
	with open(fnm, 'rb') as f :
		hdrBytes = 16
		data = f.read(hdrBytes)
		#header is 16 bytes LITTLE-ENDIAN
		# UINT16 : MAGIC
		# UINT16 : ATTR
		# UINT32 : NFACE
		# UINT32 : NVERT
		# UINT32 : NSKIP
		hdr = struct.unpack_from('<HHIII', data)
		if hdr[0] != 23117 :  #incorrect magic: assume gzip
			f=gzip.open(fnm,'rb')
			data=f.read(hdrBytes)
			hdr = struct.unpack_from('<HHIII', data)
			if hdr[0] != 23117 :  #incorrect magic: assume gzip
				print "Not a valid MZ3 file"
				return invalid_mz3
			if isVerbose :
				print "GZ Compressed"
		MAGIC = hdr[0]
		ATTR = hdr[1]
		NFACE = hdr[2]
		NVERT = hdr[3]
		NSKIP = hdr[4]
		#read attributes
		isFACE = (ATTR & 1) != 0
		isVERT = (ATTR & 2) != 0
		isRGBA = (ATTR & 4) != 0
		isSCALAR = (ATTR & 8) != 0
		#report contents
		if isVerbose :
			print "MAGIC %d ATTR %d" % (MAGIC, ATTR)
			print "NFACE %d NVERT %d NSKIP %d" % (NFACE, NVERT, NSKIP)
			print " isFACE %r isVERT %r isRGBA %r isSCALAR %r" % (isFACE, isVERT, isRGBA, isSCALAR)
		#quit if file does not make sense
		if ATTR > 15 :
			print "Unable to read future version of MZ3 file"
			return invalid_mz3
		if NVERT < 1 :
			print "Unable to read MZ3 files without vertices"
			return invalid_mz3
		if (NFACE < 1) & (isFACE) :
			print "MZ3 files with isFACE must specify NFACE"
			return invalid_mz3
		#read faces
		f.seek(hdrBytes+NSKIP)
		if isFACE :
			data = f.read(12 * NFACE) #each vertex defined by 3 UINT32 vertex indices
			str = '<{}I'.format(3*NFACE)
			mz3_faces = struct.unpack_from(str, data)
		#read vertices
		f.seek(hdrBytes+NSKIP+(isFACE * NFACE * 3 * 4) )
		if isVERT :
			data = f.read(12 * NVERT) #each vertex defined by 3 FLOAT32 coordinates (xyz)
			str = '<{}f'.format(3*NVERT)
			mz3_verts = struct.unpack_from(str, data)
		#read RGBA colors
		f.seek(hdrBytes+NSKIP+(isFACE * NFACE * 12)+(isVERT * NVERT * 12) )
		if isRGBA :
			data = f.read(4 * NVERT) #each vertex has UINT32 RGBA value
			str = '<{}I'.format(NVERT)
			mz3_rbga = struct.unpack_from(str, data)
		#read Scalar Intensity
		f.seek(hdrBytes+NSKIP+(isFACE * NFACE * 12)+(isVERT * NVERT * 12)+(isRGBA * NVERT * 4) )
		if isSCALAR :
			data = f.read(4 * NVERT) #each vertex has FLOAT32 scalar value
			str = '<{}f'.format(NVERT)
			mz3_scalar = struct.unpack_from(str, data)
		return mz3_faces, mz3_verts, mz3_rbga, mz3_scalar;

def load_mz3_mesh(filepath, mz3_name, isVerbose):
	mz3_faces, mz3_verts, mz3_rbga, mz3_scalar = read(filepath, isVerbose)
	if mz3_verts is None:
		print('Invalid file')
		return
	#verbose reporting of results
	if isVerbose & (len(mz3_faces) > 0) :
		NFACE = len(mz3_faces) // 3
		j = 0
		for i in range(NFACE):
			print "%d face %d %d %d" % (i, mz3_faces[j], mz3_faces[j+1], mz3_faces[j+2])
			j = j + 3
	if isVerbose & (len(mz3_verts) > 0) :
		NVERT = len(mz3_verts) // 3
		j = 0
		for i in range(NVERT):
			print "%d xyz %g %g %g" % (i, mz3_verts[j], mz3_verts[j+1], mz3_verts[j+2])
			j = j + 3
	if isVerbose & (len(mz3_rbga) > 0) :
		for i in range(len(mz3_rbga)):
			rgba = struct.unpack("4B", struct.pack("I", mz3_rbga[i]))
			print "%d rgba %d %d %d %d" % (i, rgba[0], rgba[1], rgba[2], rgba[3])
	if isVerbose & (len(mz3_scalar) > 0) :
		for i in range(len(mz3_scalar)):
			print "%d scalar %g" % (i, mz3_scalar[i])

#main program follows:
if len(sys.argv) < 2 :
	print "Error: no input filename provided"
	quit()
fnm = sys.argv[1]
if not os.path.isfile(fnm) :
	print "Unable to find file named %s" % (fnm)
	quit()
load_mz3_mesh(fnm, "", True)