#!/bin/sh

find /Users/rorden/Documents/osx -name ‘*.DS_Store’ -type f -delete

cd ~/Documents/pas/surfice/

rm -rf lib

#1.) build OpenGL core version of surfice


#the copy of glext that comes with freepascal 3.0 does not support geometry shaders
cp ./coregl/glext.pp ./glext.pp

#compile Surfice as 64-bit Cocoa (OpenGL 4.1 Core)
cp ./optsCore.inc ./opts.inc
# /Developer/lazarus/lazbuild ./surfice.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/bin/ppcx64"
#Current FPC 3.0.0 can not compile on OSX 10.11 El Capitan, so use 3.1.1
#/Developer/lazarus/lazbuild ./surfice.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/lib/fpc/3.1.1/ppcx64"
#/Users/rorden/lazarus/lazbuild ./surfice.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/bin/ppcx64"

/Users/rorden/lazarus/lazbuild ./surfice.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/lib/fpc/3.1.1/ppcx64"
#/Developer/lazarus/lazbuild ./surfice.lpr --ws=cocoa
strip ./surfice
cp surfice /Users/rorden/Desktop/Surf_Ice/Surfice/surfice.app/Contents/MacOS/surfice

#2.) build OpenGL legacy version with geomtery shaders

#compile Surfice as 32-bit Carbon: OSX 10.6 OpenGL support (OpenGL2.1)
cp ./optsCompatGeom.inc ./opts.inc
/Developer/lazarus/lazbuild -B ./surfice.lpr
strip ./surfice
cp surfice /Users/rorden/Desktop/Surf_Ice/Surfice/surficeOld.app/Contents/MacOS/surfice

#return to default version
cp ./optsCompat.inc ./opts.inc
rm ./glext.pp


rm *.bak
rm surfice
rm -rf lib
rm -rf backup

cd /Users/rorden/Documents/pas/
zip -FSr /Users/rorden/Documents/pas/surf.zip surfice

