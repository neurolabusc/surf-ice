import gl
gl.resetdefaults()
gl.meshload('~/catani/fs_LR.L.inflated_va_avg.32k_fs_LR.surf.gii')
gl.overlayload('~/catani/Q1-Q6_RelatedParcellation210.CorticalAreas_dil_Final_Final_Areas_Group_Colors.32k_fs_LR.dlabel.nii')
gl.contour(0)
gl.atlassaturationalpha(0.9, 0.5)
gl.overlaycolorname(1,'Grayscale')
gl.overlayinvert(1,1);