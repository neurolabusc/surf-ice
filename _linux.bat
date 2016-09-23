cd ~/surfice

#compile OpenGL3.3 CORE version
cp ./optsCore.inc ./opts.inc
rm -rf lib
lazbuild -B surfice.lpr
cp surfice ~/Surf_Ice/surfice



#compile OpenGL2.1 version
cp ./optsCompat.inc ./opts.inc
rm -rf lib
lazbuild -B surfice.lpr
cp surfice ~/Surf_Ice/surficeOld



cd ~
zip -FSr surf_lx.zip Surf_Ice
