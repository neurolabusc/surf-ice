import gl
gl.resetdefaults()
gl.azimuthelevation(70, 15)
gl.meshload('BrainMesh_ICBM152Right.mz3')
gl.overlayload('motor_4t95vol.nii.gz')
gl.overlaycolorname(1, 'Gold')
