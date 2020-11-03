import gl
import os
fnm = os.path.expanduser("~")+os.path.sep+'myLUT.clut';
f = open(fnm, "w")
f.write("[INT]\n")
f.write("numnodes=3\n")
f.write("[BYT]\n")
f.write("nodeintensity0=0\n")
f.write("nodeintensity1=128\n")
f.write("nodeintensity2=255\n")
f.write("[RGBA255]\n")
f.write("nodergba0=0|0|0|0\n")
f.write("nodergba1=128|128|0|64\n")
f.write("nodergba2=255|0|0|128\n")
f.close()
gl.meshload('BrainMesh_ICBM152.rh.mz3')
gl.overlayload('motor_4t95vol.nii.gz')
gl.overlaycolorname(1,fnm)
