cd ~/surf-ice


#compile Surfice to OpenGL 3.3 CORE

#next lines obsolete: we use glcorearb.pas
# the copy of glext that comes with freepascal 3.0 does not support geometry shaders
# cp ./coregl/glext.pp ./glext.pp

cp ./optsCore.inc ./opts.inc
rm -rf lib
lazbuild -B --compiler=/home/chris/pas/fpcllvm86/lib/fpc/3.3.1/ppcx64 surfice.lpr
cp surfice ~/Surf_Ice/surfice


rm -rf lib
lazbuild -B --compiler=/home/chris/pas/fpcllvm86/lib/fpc/3.3.1/ppcx64 surfice.lpr --ws=qt5
cp surfice ~/Surf_Ice/surfice_qt5

#compile Surfice to OpenGL 2.1

#next line not used: we will not use the geometry shaders
# cp ./optsCompatGeom.inc ./opts.inc
cp ./optsCompat.inc ./opts.inc

rm -rf lib
lazbuild -B --compiler=/home/chris/pas/fpcllvm86/lib/fpc/3.3.1/ppcx64 surfice.lpr
cp surfice ~/Surf_Ice/surficeOld

rm -rf lib
lazbuild -B --compiler=/home/chris/pas/fpcllvm86/lib/fpc/3.3.1/ppcx64 surfice.lpr --ws=qt5
cp surfice ~/Surf_Ice/surficeOld_qt5


#clean up - remove modified glext so it does not interfere with other builds
rm -rf lib
#cp ./optsCompat.inc ./opts.inc
#rm ./glext.pp

cd ~
zip -FSr surfice_linux.zip Surf_Ice
