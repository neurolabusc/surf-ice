import gl
gl.resetdefaults()
gl.meshload('BrainMesh_ICBM152_smoothed.lh.mz3')
gl.meshcurv()
gl.shadername('hidecurves')
gl.overlayload('CIT168.mz3')
gl.shaderforbackgroundonly(1)
gl.shaderadjust('curvthreshhi', 0.44)
