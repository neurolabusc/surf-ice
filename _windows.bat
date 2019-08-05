REM COMPILE Surf Ice
D:
cd D:\pas\surf-ice

REM NO LONGER REQUIRED: we now use glcorearb.pas for OpenGL core
REM  copy new version of GLEXT that supports geometry shaders
REM   copy /Y .\coregl\glext.pp .\glext.pp


REM create core version
rmdir /S /Q lib
copy /Y .\optsCore.inc .\opts.inc
d:\lazarus\lazbuild --cpu=x86_64 -B surfice.lpi
move /Y "D:\pas\surf-ice\surfice.exe" "D:\neuro\Surf_Ice\surfice.exe"


REM  create legacy version with geometry support
rmdir /S /Q lib
copy /Y .\optsCompat.inc .\opts.inc
d:\lazarus\lazbuild --cpu=x86_64 -B surfice.lpi
move /Y "D:\pas\surf-ice\surfice.exe" "D:\neuro\Surf_Ice\surficeOld.exe"


del d:\neuro\Surf_Ice\*.ini
c:\Progra~1\7-Zip\7z a -tzip d:\surfice_windows.zip d:\neuro\Surf_Ice

REM  return to default libraries
REM  del .\glext.pp
copy /Y .\optsCompat.inc .\opts.inc
rmdir /S /Q lib

c:\Progra~1\7-Zip\7z a -tzip d:\surfice_windows.zip d:\neuro\Surf_Ice


