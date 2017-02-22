cd ~/surf-ice

#1.) build OpenGL core version of surfice


#next lines obsolete: we use glcorearb.pas
# the copy of glext that comes with freepascal 3.0 does not support geometry shaders
# cp ./coregl/glext.pp ./glext.pp

cp ./optsCore.inc ./opts.inc
rm -rf lib
lazbuild -B surfice.lpr
cp surfice ~/Surf_Ice/surfice



#compile Surfice as 32-bit Carbon: OSX 10.6 OpenGL support (OpenGL2.1)
cp ./optsCompatGeom.inc ./opts.inc

rm -rf lib
lazbuild -B surfice.lpr
cp surfice ~/Surf_Ice/surficeOld

#clean up - remove modified glext so it does not interfere with other builds
rm -rf lib
cp ./optsCompat.inc ./opts.inc
rm ./glext.pp

cd ~
zip -FSr surfice_linux.zip Surf_Ice
