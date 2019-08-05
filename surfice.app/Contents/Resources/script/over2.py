import gl
gl.resetdefaults()
#opacity adjusts whether overlays are translucent (0)
opacity = 50
pth = '/Users/chris/Downloads/sf/'
gl.meshload('BrainMesh_ICBM152_smoothed.mz3')
gl.overlayload(pth+'region.mz3')
gl.overlayload(pth+'stat.mz3')
gl.overlayoverlapoverwrite(0)