import gl
gl.meshload('~/catani/fs_LR.L.inflated_va_avg.32k_fs_LR.surf.gii')
#load atlas
gl.overlayload('~/catani/Q1-Q6_RelatedParcellation210.CorticalAreas_dil_Final_Final_Areas_Group_Colors.32k_fs_LR.dlabel.nii')
#make atlas translucent
gl.atlassaturationalpha(0.2, 0.5)
#show borders
gl.contour(gl.overlaycount())
gl.overlaycolorname(gl.overlaycount(),'Grayscale')
#invert color scheme so borders are black not white
gl.overlayinvert(gl.overlaycount(),1);
gl.overlayopacity(gl.overlaycount(),50)
#load statistical map as image
gl.overlayload('motor_4t95vol.nii.gz')
gl.overlayminmax(gl.overlaycount(),-2,-3)
gl.overlayopacity(gl.overlaycount(),75)
#create new overlay based on contour of motor map:
gl.contour(gl.overlaycount())

