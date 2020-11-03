import gl
gl.resetdefaults()
gl.meshload('BrainMesh_ICBM152.rh.mz3')
gl.overlayload('motor_4t95mesh.mz3')
gl.overlaycolorname(1, 'red')
gl.shaderxray(0.9, 0.5)
gl.azimuthelevation(110, 15)
gl.meshcurv()
gl.shadername('MatCap');
gl.shadermatcap('MetalShiny');
