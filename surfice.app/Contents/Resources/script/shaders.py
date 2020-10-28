import gl
gl.resetdefaults()
kframesperrotation = 180
gl.meshcolor(210, 148, 148)
gl.meshload('BrainMesh_ICBM152.lh.mz3')
gl.edgeload('LPBA40.edge')
gl.clipazimuthelevation(0.3, 0, 130)
gl.nodesize(6, 1)
gl.edgesize(3,1)
gl.nodehemisphere(-1)
gl.azimuthelevation(250, 35)
gl.edgecolor('actc',1)
gl.nodecolor('red',1)
gl.nodethresh(1.0,1.0)
gl.edgethresh(0.5,1.0)
for i in range(1, kframesperrotation * 5):
  s = 0.5 + (i+0.0)/72
  if (s <= 1):
    gl.cameradistance(s)
  if ((i % kframesperrotation) == 0):
    rot = (i / kframesperrotation)
    if rot == 1:
        gl.shadername('metal')
    elif rot == 2:
        gl.shadername('wireframe')
    elif rot == 3:
        gl.shadername('toon')
        gl.shaderambientocclusion(0)
    else :
        gl.shadername('wire')
        gl.shaderforbackgroundonly(1)
  gl.azimuth( int(round(360.0/kframesperrotation)))
  gl.wait(20)