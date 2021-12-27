import gl
gl.resetdefaults()
gl.trackload('stroke.trk.gz')
gl.trackprefs(15, 3, 0)
ksteps = 90
for i in range(1, ksteps):
  gl.trackazimuthelevation(i,0,90)
  gl.wait(20)
for i in range(1, ksteps):
  gl.trackazimuthelevation(90-i,90,0)
  gl.wait(20)
for i in range(1, ksteps):
  gl.trackazimuthelevation(i,90,0)
  gl.wait(20)
for i in range(1, ksteps):
  gl.trackazimuthelevation(90-i,0,0)
  gl.wait(20)
for i in range(1, ksteps):
  gl.trackazimuthelevation(i,0,0)
  gl.wait(20)