import gl
gl.resetdefaults()
pth='/Users/chris/Downloads/MNI_Glasser_HCP_2019_03_26/'
gl.meshload(pth+'std.141.lh.pial.gii')
#gl.overlayload(pth+'lh.std.141.Glasser.gii.dset')
#gl.overlayload(pth+'lh.std.141.Glasser_HCP.niml.dset')
gl.overlayload(pth+'lh.std.141.Glasser_HCP.lbl.niml.dset')
#gl.atlas2node('/Users/chris/afni/pial/mynodes.node')
#gl.overlayload(pth+'lh.std.141.Glasser.1D.dset')


