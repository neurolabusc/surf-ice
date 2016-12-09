#compile command line tools traction and entrail
#  note: delete library to ensure units are compiled for terminal not GUI
rm -rf lib
lazbuild -B traction.lpr
lazbuild -B entrail.lpr
rm -rf lib
