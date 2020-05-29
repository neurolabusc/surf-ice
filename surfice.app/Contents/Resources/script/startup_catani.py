import gl
gl.resetdefaults()
gl.meshload('~/catani/fs_LR.L.inflated_va_avg.32k_fs_LR.surf.gii')
gl.overlayload('~/catani/Wang_2015.32k_fs_LR.dlabel.nii')
gl.contour(gl.overlaycount())
gl.overlaycolorname(gl.overlaycount(),'Grayscale')
#invert color scheme so borders are black not white
gl.overlayinvert(gl.overlaycount(),1);