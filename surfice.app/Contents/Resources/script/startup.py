import gl
gl.resetdefaults()
#gl.meshload('BrainMesh_ICBM152.rh.mz3')
#gl.meshcurv()

#gl.meshload('/Users/chris/Neuro/export/teapot.obj')
#gl.meshload('/Users/chris/Desktop/datasets/std.60.lh.white.gii');
#gl.overlayload('/Users/chris/Desktop/datasets/std.60.lh.Schaefer2018_400Parcels_17Networks_order.annot_REN.1D.dset');
#gl.meshload('/Users/chris/Neuro/Surfice/Surfice/fs/lh.pial')
#gl.overlayload('/Users/chris/Neuro/Surfice/Surfice/fs/motor_4t95mesh.mz3')

gl.hemispherevisible(1)
gl.meshload('/Users/chris/Desktop/datasets/std.60.rh.white.gii')
gl.overlayload('/Users/chris/Desktop/datasets/std.60.lh.Schaefer2018_400Parcels_17Networks_order.annot_REN.1D.dset');
gl.atlas2node('/Users/chris/test.node')
