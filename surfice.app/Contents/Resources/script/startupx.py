import gl
gl.resetdefaults()
gl.meshload('lh.inflated')
#gl.overlayload(pth+'lh.std.141.Glasser.gii.dset')
#gl.overlayload(pth+'lh.std.141.Glasser_HCP.niml.dset')
gl.overlayload('boggle.annot')
#gl.atlas2node('/Users/chris/afni/pial/mynodes.node')
#gl.overlayload(pth+'lh.std.141.Glasser.1D.dset')
gl.atlas2node('/Users/chris/test.node')

