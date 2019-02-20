import gl
gl.resetdefaults()
gl.meshload('BrainMesh_ICBM152Right.mz3')
gl.overlayload('motor_4t95mesh.mz3')
gl.overlaycolorname(1, 'red')
gl.shaderxray(1.0, 0.3)
gl.azimuthelevation(110, 15)
gl.meshcurv()