import gl
gl.resetdefaults()
gl.meshload('mni152_2009AO.mz3')
gl.overlayload('CIT168.mz3')
gl.shadername('MatCap')
gl.shadermatcap('Cortex')
gl.shaderxray(1.0, 0.9)