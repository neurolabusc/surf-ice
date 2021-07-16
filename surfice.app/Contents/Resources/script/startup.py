import gl
gl.resetdefaults()
pth = '/Users/chrisrorden/Neuro/spm12/toolbox/cat12'
mesh = pth + '/templates_surfaces/rh.inflated.freesurfer.gii'
atlas = pth+'/atlases_surfaces/rh.aparc_a2009s.freesurfer.annot'
outpth =  '/Users/chrisrorden/Neuro'
outmesh = outpth+'surficetemp.mz3';
gl.meshload(mesh)
gl.atlasstatmap(atlas, outmesh, (14,50, 26),(7.8,3.1,4))
gl.meshload(mesh)
gl.overlayload(outmesh)
gl.shaderxray(1.0, 1.0)
gl.overlayminmax(1,2,7)
gl.azimuthelevation(90, 15)