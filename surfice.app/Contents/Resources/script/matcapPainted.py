import gl
gl.resetdefaults()
gl.meshload('BrainMesh_ICBM152.rh.mz3')
gl.overlayload('motor_4t95vol.nii.gz')
gl.overlayminmax(1,2,12)
gl.overlaycolorname(2, 'Red-Yellow')
gl.azimuthelevation(110, 15)
gl.shadername('MatCap');
gl.meshcurv()
gl.overlaytranslucent(2, 1)


