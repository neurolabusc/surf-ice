import gl
gl.resetdefaults()
gl.azimuthelevation(70, 15)
gl.meshloadbilateral('BrainMesh_ICBM152.rh.mz3')
gl.overlayload('motor_4t95vol.nii.gz')
gl.overlayminmax(1,2,12)
gl.overlayload('motor_4t95vol.nii.gz')
gl.overlayminmax(2,-1,-2)
gl.colorbarvisible(1)
gl.overlaytransparencyonbackground(25)
gl.meshcurv()
gl.shadername('MatCap');
gl.shadermatcap('Cortex3')
gl.hemispheredistance(0.62)
gl.hemispherepry(-35)
gl.azimuthelevation(180,0)
print('Emulate WalnutBrain https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/tutorials/WalnutBrain/WalnutBrain.html')
