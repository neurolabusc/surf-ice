# Surf Ice

##### About

Surf Ice is an open source surface render. It can be compiled for the Linux, Macintosh OSX, and Windows operating systems. For details and compiled versions visit the [NITRC wiki](https://www.nitrc.org/plugins/mwiki/index.php/surfice:MainPage). You can also get a compiled version from the [Github releases page](https://github.com/neurolabusc/surf-ice/releases).

Supported mesh formats include 3DO, 3DS, AC, BYU, CTM, DAE, DXF, FreeSurfer, GII (GIfTI), GTS, LWO2, MS3D, MZ3, NV (BrainNetViewer), OBJ, OFF, PLY, PLY2, STL, VTK, WFR. Online converters can convert meshes from other formats to one of these. Tractography formats include BFloat, PDB, TCK, TRK, and VTK. Supported volume formats include those supported by [i2nii](https://github.com/rordenlab/i2nii): AFNI Brik(.head), Analyze(.hdr), Bio-Rad PIC(.pic), Blender Voxel data(.bvox), BrainVoyager VMR(.vmr, .v16), DeltaVision(.dv), ECAT(.v), FreeSurfer MGH/MGZ Volume(.mgh/.mgz), Guys Image Processing Lab(.gipl), ICS Image Cytometry Standard(.ics), Interfile(.varies, limited support), ITK MHA/MHD(.mha/.mhd), MRTrix Volume(.mif/.mih; not all variants supported), NIfTI(.hdr/.nii/.nii.gz/.voi), NRRD(.nhdr/.nrrd), POV-Ray Density_File(.df3), Spectroscopic Imaging, Visualization and Computing (SIVIC)(.idf), Stimulate Sdt(.spr/.sdt), Vaa3D(.v3draw), VTK Legacy Voxel Format(.vtk). Tools like [dcm2niix](https://github.com/rordenlab/dcm2niix) can help convert voxel-based DICOM images to NIfTI.

![alt tag](https://github.com/neurolabusc/surf-ice/blob/master/Surfice.jpg)

##### Installing

Compiled releases are provided for Linux, MacOS and Windows. 

You can get MRIcroGL using three methods:

 - Download from [NITRC](https://www.nitrc.org/plugins/mwiki/index.php/surfice:MainPage).
 - Download from [Github](https://github.com/neurolabusc/surf-ice/releases).
 - Run the following command to get the latest version for Linux, Macintosh or Windows: 
   * `curl -fLO https://github.com/neurolabusc/surf-ice/releases/latest/download/surfice_linux.zip`
   * `curl -fLO https://github.com/neurolabusc/surf-ice/releases/latest/download/surfice_macOS.dmg`
   * `curl -fLO https://github.com/neurolabusc/surf-ice/releases/latest/download/surfice_windows.zip`

The Windows and Linux releases includes two versions of Surf Ice: "Surfice" requires OpenGL 3.3 or later, while "SurficeOld" runs on older computers (requiring OpenGL 2.1). A [wiki](https://www.nitrc.org/plugins/mwiki/index.php/surfice:MainPage) provides a full manual and troubleshooting advice.

##### Compiling

This project is built using the open source [FreePascal Lazarus](http://www.lazarus-ide.org/) compiler and integrated development environment. You will need to have the `LazOpenGLContext` and `PascalScript` packages installed. To do this, launch the Lazarus application and choose Packages/InstallPackages. You will want to select these two packages from the "Available for installation" list. Finally, click the "Save and rebuild" button. Once these are installed, you can load this package and compile it using the Run/Run menu option.

The latest version also allows [Python Scripting](https://github.com/neurolabusc/surf-ice/blob/master/PYTHON.md). To enable this you will need to download and install the [Python-for-Lazarus package](https://github.com/Alexey-T/Python-for-Lazarus). Once you download the package, you can install it in the same way as LazOpenGLContext and PascalScript (described above). Alternatively, if you do not want to enable Python, edit the `opts.inc` file by removing the line `{$DEFINE MYPY}`.

You can also edit your opts.inc file to build either of the [flavors of Surfice](https://www.nitrc.org/plugins/mwiki/index.php/surfice:MainPage#Versions). Uncomment the line `{$DEFINE COREGL}` for OpenGL 3.3 Core specification, or comment the line `//{$DEFINE COREGL}` for OpenGL 2.1.


You can also add the required packages from the command line:

```
lazbuild --verbose-pkgsearch lazopenglcontext --verbose-pkgsearch pascalscript
if [ $? -eq 0 ]
then
    echo "required packages already installed"
else
    echo "installing packages"
    lazbuild --add-package lazopenglcontext --add-package pascalscript --build-ide=
fi
if [[ "$OSTYPE" == "darwin"* ]]; then
	echo "macOS compiling for Cocoa, instead of default Carbon widgetset"
	lazbuild  -B --ws=cocoa ./simplelaz.lpr
else
	lazbuild -B ./simplelaz.lpi
fi
```

You can also build this project from the command line (assuming you have FPC Lazarus and the requried packages installed):

```
 lazbuild surfice.lpr
```

##### Versions

The [releases](https://github.com/neurolabusc/surf-ice/releases) page provides version notes.

##### License

This software includes a [BSD license](https://opensource.org/licenses/BSD-2-Clause)
