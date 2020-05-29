import gl
gl.resetdefaults()
#gl.meshload('BrainMesh_ICBM152Right.mz3');
pth='/Users/chris/afni/FATCAT_DEMO/DTI/'
gl.trackload(pth+'o.NETS_OR_003.niml.tract');
gl.nodeload(pth+'o.NETS_AND_MINIP_003.niml.dset');
 

#pth='/Users/chris/Downloads/MNI_Glasser_HCP_2019_03_26/'
#pth='/Users/chris/Downloads/MNI_Glasser_HCP_2019_03_26/'
#gl.meshload(pth+'std.141.lh.pial.gii')
#gl.overlayload(pth+'lh.std.141.Glasser.1D.dset')
#gl.overlaycolorname(1,'Random')

#pth='/Users/chris/erosion_thickdir/thickdir/'
#gl.meshload(pth+'anat.gii')
#gl.overlayload(pth+'erosion_thick.niml.dset')


#pth='/Users/chris/Downloads/SUMA/'
#gl.meshload(pth+'std.141.lh.inflated.gii')
#gl.meshload(pth+'std.141.lh.inflated.gii')
#gl.overlayload(pth+'std.141.lh.curv.niml.dset')
#gl.overlayload(pth+'std.141.lh.sulc.niml.dset')

#pth='/Users/chris/Downloads/MNI_Glasser_HCP_2019_03_26/'
#gl.meshload(pth+'std.141.lh.pial.gii')
#gl.meshload(pth+'lh.std.141.Glasser.gii.dset')
#gl.overlayload(pth+'lh.std.141.Glasser_HCP.niml.dset')
#gl.overlayload(pth+'lh.std.141.Glasser_HCP.lbl.niml.dset');


#gl.meshload(pth+'std.141.lh.inflated.gii')
#gl.overlayload(pth+'std.141.FT_lh.niml.M2M')

#gl.overlayload(pth+'std.60.rh.aparc.a2009s.annot.niml.dset');
#gl.overlaycolorname(1,'Random')
#gl.overlayload(pth+'std.60.rh.curv.niml.dset')

#pth='/Users/chris/Downloads/MNI_Glasser_HCP_2019_03_26/surfs/'
#gl.meshload(pth+'HCP.R_Superior_Frontal_Language_Area.k1026.gii')
#gl.overlayload(pth+'HCP.R_Superior_Frontal_Language_Area.k1026.niml.dset')

#suma -i lh.std.141.Glasser.gii.dset   -load_dset lh.std.141.Glasser_HCP.lbl.niml.dset

