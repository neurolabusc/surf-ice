REM COMPILE Surf Ice
cd c:\pas\surfice
c:\lazarus\lazbuild --cpu=x86_64 -B surfice.lpi
move /Y "C:\pas\surfice\surfice.exe" "c:\Surf_Ice\surfice.exe"

del c:\Surf_Ice\*.ini
c:\Progra~1\7-Zip\7z a -tzip c:\pas\surfice_windows.zip c:\Surf_Ice


