REM COMPILE Surf Ice
cd c:\pas\surfice

rmdir /S /Q lib
copy /Y .\optsCore.inc .\opts.inc
c:\lazarus\lazbuild --cpu=x86_64 -B surfice.lpi
move /Y "C:\pas\surfice\surfice.exe" "c:\Surf_Ice\surfice.exe"

rmdir /S /Q lib
copy /Y .\optsCompat.inc .\opts.inc
c:\lazarus\lazbuild --cpu=x86_64 -B surfice.lpi
move /Y "C:\pas\surfice\surfice.exe" "c:\Surf_Ice\surficeOld.exe"


del c:\Surf_Ice\*.ini
c:\Progra~1\7-Zip\7z a -tzip c:\pas\surfice_windows.zip c:\Surf_Ice

rmdir /S /Q lib


