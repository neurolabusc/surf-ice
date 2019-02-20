import gl
gl.resetdefaults()
gl.meshload('mni152_2009.mz3')
gl.overlayload('motor_4t95mesh.mz3')
gl.clipazimuthelevation(0.37, 0, 140)
gl.shaderambientocclusion(0.5)
gl.azimuthelevation(90, 20)
gl.shaderxray(1.0, 0.3)

