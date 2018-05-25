# Surf Ice

##### About

Surf Ice is an open source surface render. It can be compiled for the Linux, Macintosh OSX, and Windows operating systems. For details and compiled versions visit the [NITRC wiki](https://www.nitrc.org/plugins/mwiki/index.php/surfice:MainPage). You can also get a compiled version from the [Github releases page](https://github.com/neurolabusc/surf-ice/releases).

Supported mesh formats include 3DS, CTM, DXF, FreeSurfer, GII (GIfTI), GTS, LWO2, MS3D, MZ3, NV (BrainNetViewer), OBJ, OFF, PLY, STL, VTK. Online converters can convert meshes from other formats to one of these. Tractography formats include BFloat, PDB, TCK, TRK, and VTK. The software supports the NIfTI voxelwise format (tools like dcm2niix can help convert other voxel-based formats to NIfTI).

![alt tag](https://github.com/neurolabusc/surf-ice/blob/master/Surfice.jpg)

##### Compiling

This project is built using the open source [FreePascal Lazarus](http://www.lazarus-ide.org/) compiler and integrated development environment. You will need to have the LazOpenGLContext and PascalScript packages installed. To do this, launch the Lazarus application and choose Packages/InstallPackages. You will want to select these two packages from the "Available for installation" list. Finally, click the "Save and rebuild" button. Once these are installed, you can load this package and compile it using the Run/Run menu option.

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

##### Recent Versions


20-December-2017 (v1.0.20171220)
 - Support [PALM](https://github.com/andersonwinkler/PALM) format [DPV](https://brainder.org/2011/09/25/braindering-with-ascii-files/).

21-June-2017
 - [Smooth](https://github.com/neurolabusc/OpenGLCoreTutorials) numbers in colorbars.
 - Conversion of images from voxels to mesh now allows the user to specify a minimum cluster size.
 - Pan image by [shift-dragging](https://www.nitrc.org/forum/forum.php?thread_id=7857&forum_id=6713).
 - MacOS retina support (better but slower, use Preferences window to disable).

7-February-2017
 - Ability to individually adjust translucency of overlays (rather than having one value for all).
 - Looks better on Linux high-DPI screens.
 - New command Advanced/ComputeCurvature creates FreeSurfer Format CURV file.
 - Control-clicking overlay name in the overlay panel reverses the overlay colors (and the name is displayed in black instead of blue). Control-click again to switch to normal mode.
 - Support for several variations of the [CIfTI format](https://www.humanconnectome.org/software/get-connectome-workbench.html)(dscalar.nii, dlabel.nii, dtseries.nii).

30-September-2016
 - Add support for the TRK.GZ (DSI studio), DAT (MRI studio) formats.
 - Support for tracks with [scalars](https://github.com/johncolby/along-tract-stats) and [properties](http://trackvis.org/blog/tag/track_merge/).
 - "Faster" track viewing mode looks better (Lucas Nesi's imposter method).

20-April-2016
 - Add support for the 3DS, MS3D, LWO2, DXF and GTS formats.

##### License

This software includes a [BSD license](https://opensource.org/licenses/BSD-2-Clause)
