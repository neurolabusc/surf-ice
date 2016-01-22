#!/bin/sh

find /Users/rorden/Documents/osx -name ‘*.DS_Store’ -type f -delete

cd ~/Documents/pas/surfice/

rm -rf lib

#compile Surfice as 64-bit Cocoa (OpenGL 4.1 Core)
cp ./optsCore.inc ./opts.inc
/Developer/lazarus/lazbuild ./surfice.lpr --cpu=x86_64 --ws=cocoa --compiler="/usr/local/bin/ppcx64"
#/Developer/lazarus/lazbuild ./surfice.lpr --ws=cocoa
strip ./surfice
cp surfice /Users/rorden/Desktop/Surf_Ice/surfice.app/Contents/MacOS/surfice



#compile Surfice as 32-bit Carbon: OSX 10.6 OpenGL support (OpenGL2.1)
cp ./optsCompat.inc ./opts.inc
/Developer/lazarus/lazbuild ./surfice.lpr
strip ./surfice
cp surfice /Users/rorden/Desktop/Surf_Ice/surficeOld.app/Contents/MacOS/surfice



rm *.bak
rm surfice
rm -rf lib
rm -rf backup

cd /Users/rorden/Documents/pas/
zip -FSr /Users/rorden/Documents/pas/surf.zip surfice

