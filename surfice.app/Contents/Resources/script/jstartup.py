import gl
gl.resetdefaults()
gl.meshload('/Users/chris/afni/pial/lh.sphere')
gl.overlayload('/Users/chris/afni/pial/lh.HCP-MMP1.annot')
gl.atlas2node('/Users/chris/afni/pial/mynodes.node');
