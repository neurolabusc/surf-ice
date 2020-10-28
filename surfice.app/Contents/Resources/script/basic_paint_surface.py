import gl
gl.resetdefaults()
gl.azimuthelevation(70, 15)
gl.meshload('BrainMesh_ICBM152.rh.mz3')
gl.overlayload('motor_4t95vol.nii.gz')
gl.overlayminmax(1,2,12)
gl.overlayload('motor_4t95vol.nii.gz')
gl.overlayminmax(2,-1,-2)
gl.colorbarvisible(1)
gl.overlaytransparencyonbackground(25)
gl.meshcurv()

