import gl
ksteps = 100
gl.resetdefaults()
gl.meshload('stroke.mz3')
gl.trackload('stroke.trk.gz')
gl.trackprefs(15, 3, 0.5)
for i in range(1, ksteps):
  gl.clipazimuthelevation(( (0.8*i)/ksteps ), 0, 130)
  gl.wait(20)

