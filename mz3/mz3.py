#!/usr/bin/python
# Report contents of mz3 image
#  https://github.com/neurolabusc/surf-ice/tree/master/mz3
# Examples:
#  python ./mz3.py 3Mesh.mz3
#  python ./mz3.py 15TemplateMesh.mz3
import os
import sys
import struct
import gzip
import array


def read(fnm, isVerbose):
    invalid_mz3 = (None, None, None, None)
    faces = []
    verts = []
    rbga = []
    scalar = []
    with open(fnm, 'rb') as f:
        hdrBytes = 16
        data = f.read(hdrBytes)
        # header is 16 bytes LITTLE-ENDIAN
        #  UINT16 : MAGIC
        #  UINT16 : ATTR
        #  UINT32 : NFACE
        #  UINT32 : NVERT
        #  UINT32 : NSKIP
        hdr = struct.unpack_from('<HHIII', data)
        if hdr[0] != 23117:  # incorrect magic: assume gzip
            f = gzip.open(fnm, 'rb')
            data = f.read(hdrBytes)
            hdr = struct.unpack_from('<HHIII', data)
            if hdr[0] != 23117:  # incorrect magic: not mz3
                print "Not a valid MZ3 file"
                return invalid_mz3
            if isVerbose:
                print "GZ Compressed"
        MAGIC = hdr[0]
        ATTR = hdr[1]
        NFACE = hdr[2]
        NVERT = hdr[3]
        NSKIP = hdr[4]
        # read attributes
        isFACE = (ATTR & 1) != 0
        isVERT = (ATTR & 2) != 0
        isRGBA = (ATTR & 4) != 0
        isSCALAR = (ATTR & 8) != 0
        # report contents
        if isVerbose:
            print "MAGIC %d ATTR %d" % (MAGIC, ATTR)
            print "NFACE %d NVERT %d NSKIP %d" % (NFACE, NVERT, NSKIP)
            print " isFACE %r isVERT %r" % (isFACE, isVERT)
            print " isRGBA %r isSCALAR %r" % (isRGBA, isSCALAR)
        # quit if file does not make sense
        if ATTR > 15:
            print "Unable to read future version of MZ3 file"
            return invalid_mz3
        if NVERT < 1:
            print "Unable to read MZ3 files without vertices"
            return invalid_mz3
        if (NFACE < 1) & (isFACE):
            print "MZ3 files with isFACE must specify NFACE"
            return invalid_mz3
        # read faces
        f.seek(hdrBytes+NSKIP)
        if isFACE:
            data = f.read(12 * NFACE)  # each face is 3 UINT32 vertex indices
            str = '<{}I'.format(3*NFACE)
            faces = struct.unpack_from(str, data)
        # read vertices
        f.seek(hdrBytes+NSKIP+(isFACE * NFACE * 3 * 4))
        if isVERT:
            data = f.read(12 * NVERT)  # each vertex is 3 FLOAT32 (xyz)
            str = '<{}f'.format(3*NVERT)
            verts = struct.unpack_from(str, data)
        # read RGBA colors
        f.seek(hdrBytes+NSKIP+(isFACE * NFACE * 12)+(isVERT * NVERT * 12))
        if isRGBA:
            data = f.read(4 * NVERT)  # each vertex has UINT32 RGBA value
            str = '<{}I'.format(NVERT)
            rbga = struct.unpack_from(str, data)
        # read Scalar Intensity
        f.seek(hdrBytes+NSKIP+(isFACE * NFACE * 12)
               + (isVERT * NVERT * 12)+(isRGBA * NVERT * 4))
        if isSCALAR:
            data = f.read(4 * NVERT)  # each vertex has FLOAT32 scalar value
            str = '<{}f'.format(NVERT)
            scalar = struct.unpack_from(str, data)
    # Optional verbose reporting
    if isVerbose & (len(faces) > 0):
        NFACE = len(faces) // 3
        j = 0
        for i in range(NFACE):
            print "%d face %d %d %d" % (i, faces[j], faces[j+1], faces[j+2])
            j = j + 3
    if isVerbose & (len(verts) > 0):
        NVERT = len(verts) // 3
        j = 0
        for i in range(NVERT):
            print "%d vert %g %g %g" % (i, verts[j], verts[j+1], verts[j+2])
            j = j + 3
    if isVerbose & (len(rbga) > 0):
        for i in range(len(rbga)):
            rgba = struct.unpack("4B", struct.pack("I", rbga[i]))
            print "%d rgba %d %d %d %d" % (i, rgba[0],
                                           rgba[1], rgba[2], rgba[3])
    if isVerbose & (len(scalar) > 0):
        for i in range(len(scalar)):
            print "%d scalar %g" % (i, scalar[i])
    return faces, verts, rbga, scalar


def load_mz3_mesh(filepath, mz3_name, isVerbose):
    faces, verts, rbga, scalar = read(filepath, isVerbose)
    if verts is None:
        print('Invalid file')
        return


# main program follows:
if len(sys.argv) < 2:
    print "Error: no input filename provided"
    quit()
fnm = sys.argv[1]
if not os.path.isfile(fnm):
    print "Unable to find file named %s" % (fnm)
    quit()
load_mz3_mesh(fnm, "", True)
