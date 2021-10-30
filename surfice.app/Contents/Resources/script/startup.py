import gl
gl.resetdefaults()
gl.hidenodeswithoutedges(1)
gl.edgeload('LPBA40.edge')
gl.edgethresh(0.8,1.0)
gl.nodehemisphere(-1)

