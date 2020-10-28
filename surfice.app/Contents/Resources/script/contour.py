import gl
gl.resetdefaults()
gl.meshload('lh.pial')
gl.overlayload('boggle.lh.annot')
#make contour at border of atlas
gl.contour(0)
#reduce atlas salience:
gl.atlassaturationalpha(0.2, 0.5)
gl.overlayload('motor_4t95vol.nii.gz')
gl.overlayminmax(gl.overlaycount(),-2,-3)
#hide statistical map:
gl.overlayopacity(gl.overlaycount(),0)
#draw outline of statistical threshold
gl.contour(gl.overlaycount())