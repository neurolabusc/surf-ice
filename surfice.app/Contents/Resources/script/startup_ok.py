import gl
gl.resetdefaults()
gl.meshload('/Users/chris/catani/fs_LR.L.inflated_va_avg.32k_fs_LR.surf.gii')
gl.overlayload('/Users/chris/catani/Q1-Q6_RelatedParcellation210.CorticalAreas_dil_Final_Final_Areas_Group_Colors.32k_fs_LR.dlabel.nii')
gl.contour(0)

#gl.overlayload('/Users/chris/catani/Q1-Q6_RelatedParcellation210.CorticalAreas_dil_Final_Final_Areas_Group_Colors.32k_fs_LR.dlabel.nii')
