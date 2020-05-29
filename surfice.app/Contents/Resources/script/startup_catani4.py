import gl
gl.meshload('lh.pial')
gl.overlayload('motor_4t95vol.nii.gz')
gl.overlayminmax(gl.overlaycount(),-2,-3)
gl.overlayopacity(gl.overlaycount(),20)
#create new overlay based on contour of motor map:
gl.contour(gl.overlaycount())
#next line: make border thicker
#gl.overlayminmax(gl.overlaycount(),0.1,0.1)
#next line: make border a gradient
#gl.overlayminmax(gl.overlaycount(),0.1,1.0)
