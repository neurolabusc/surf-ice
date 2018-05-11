# Python scripting for Surfice

##### About

Surfice allows the user to run scripts. This is useful to illustrate features or automate laborious and repetitive tasks. The scripts can be entered in the graphical interface (the Advanced/Scripting menu item) or via the command line. These scripts can either be written in either the Python or Pascal languages. The default scripts that come with Surfice are written in Pascal. The advantage of Pascal is that the compiler is built into Surfice, so it will always work. On the other hand, Python requires your computer to have a Python compiler. On the other hand, Python has become a very popular language. By supporting both languages, users can choose their preferred language.

Scripting is described in the
[Surfice wiki](https://www.nitrc.org/plugins/mwiki/index.php/surfice:MainPage#Scripting). A full list of the available functions is listed on [Github](https://github.com/neurolabusc/surf-ice/blob/master/COMMANDS.md).

Specific considerations for Python in Surfice
 - Each Python script should include 'import gl' - this provides access to the MRIcroGL commands and also tells the software that this is a Python (rather than Pascal) program.
 - Python uses '#' for comments, while Pascal uses '//'
 - Python is case sensitive, and all the [Surfice](https://github.com/neurolabusc/surf-ice/blob/master/COMMANDS.md) functions are lower cased. Therefore "gl.resetdefaults" is valid but "gl.ResetDefaults" will not compile.
 - Python uses "=" for assignments, while Pascal uses ":=". Therefore, the Python line "i = 1" is the same as the Pascal "i := 1;"
 - Python uses "==" to test equality, while Pascal uses "=". Therefore, the Python line "if rot == 1:" is the same as the Pascal "if rot = 1 then"
 - PyArg_ParseTuple requires that boolean values are either "0" (false) or "1" (true). In contrast, Pascal uses true/false, so the Python command "gl.colorbarvisible(1)" is the same as the Pascal "colorbarvisible(true);"

 Below each of the Pascal scripts that are provided with MRIcroGL have been ported to Python.

##### basic_paint_surface
```python
import gl
gl.resetdefaults()
gl.azimuthelevation(70, 15)
gl.meshload('BrainMesh_ICBM152Right.mz3')
gl.overlayload('motor_4t95vol.nii.gz')
gl.overlayminmax(1,2,12)
gl.overlayload('motor_4t95vol.nii.gz')
gl.overlayminmax(2,-1,-2)
gl.colorbarvisible(1)
gl.overlaytransparencyonbackground(25)
gl.meshcurv()
```


##### create_atlas

**atlashide not yet compatible with Python**

##### fmri_mesh

```python
import gl
gl.resetdefaults()
gl.meshload('BrainMesh_ICBM152Right.mz3')
gl.overlayload('motor_4t95mesh.mz3')
gl.overlaycolorname(1, 'red')
gl.shaderxray(1.0, 0.3)
gl.azimuthelevation(110, 15)
gl.meshcurv()
```

##### frontal_atlas

**atlasstatmap not yet compatible with Python**


##### hide_curves

```python
import gl
gl.resetdefaults()
gl.meshload('BrainMesh_ICBM152Left_smoothed.mz3')
gl.meshcurv()
gl.shadername('hidecurves')
gl.overlayload('CIT168.mz3')
gl.shaderforbackgroundonly(1)
gl.shaderadjust('curvthreshhi', 0.44)
```

##### mesh

```python
import gl
gl.resetdefaults()
gl.meshload('mni152_2009.mz3')
gl.overlayload('motor_4t95mesh.mz3')
gl.clipazimuthelevation(0.37, 0, 140)
gl.shaderambientocclusion(0.5)
gl.azimuthelevation(90, 20)
gl.shaderxray(1.0, 0.3)
```

##### newer_2017

```python
import gl
gl.resetdefaults()
gl.meshload('brainmesh_icbm152right.mz3')
gl.meshcurv()
gl.overlayminmax(1,-1,1)
gl.overlaycolorname(1,'surface')
#gl.overlaycolorname(1,'bone')
gl.overlayinvert(1,1)
#overlaytranslucent(1, 1)
gl.overlayload('motor_4t95vol.nii.gz')
gl.overlaycolorname(2,'kelvin')
gl.overlayminmax(2,2,7)
gl.overlayload('scalp.mz3')
gl.overlaycolorname(3,'gold')
#gl.shadername('metal')
gl.shaderxray(1.0, 0.9)
gl.meshoverlayorder(1)
gl.colorbarvisible(0)
gl.shaderambientocclusion(0.05)
gl.azimuthelevation(90, 15)
gl.clipazimuthelevation(0.5, 0, 90)
```

##### node

```python
import gl
gl.resetdefaults()
gl.meshload('BrainMesh_ICBM152Left.mz3')
gl.edgeload('LPBA40.edge')
gl.clipazimuthelevation(0.3, 0, 130)
gl.nodesize(6, 1)
gl.edgesize(3,1)
gl.nodehemisphere(-1)
gl.azimuthelevation(250, 35)
gl.edgecolor('actc',1)
gl.nodecolor('blue',1)
gl.nodethresh(1.0,1.0)
gl.edgethresh(0.5,1.0)
gl.meshcurv()
gl.overlayminmax(1,-1,1)
gl.overlaycolorname(1,'surface')
gl.overlayinvert(1,1)
gl.overlaytranslucent(1, 1)
```

##### shaders

```python
import gl
gl.resetdefaults()
kframesperrotation = 180
gl.meshcolor(210, 148, 148)
gl.meshload('BrainMesh_ICBM152Left.mz3')
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
  gl.azimuth( round(360.0/kframesperrotation))
  gl.wait(20)
```

##### track

```python
import gl
ksteps = 100
gl.resetdefaults()
gl.meshload('stroke.mz3')
gl.trackload('stroke.trk.gz')
gl.trackprefs(15, 3, 0.5)
for i in range(1, ksteps):
  gl.clipazimuthelevation(( (0.8*i)/ksteps ), 0, 130)
  gl.wait(20)
```

##### fmri_mesh

```python
import gl

```

##### fmri_mesh

```python
import gl

```

##### fmri_mesh

```python
import gl

```
