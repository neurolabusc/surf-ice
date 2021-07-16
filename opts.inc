{$DEFINE MYPY}
//{$DEFINE DGL}  //for DGL library, otherwise gl/glext

{$DEFINE PY27}
//If PY27 is defined, you must have classic version of Python4Lazarus (that supports both Python 2.7 and 3.x
//  https://github.com/neurolabusc/Python27-for-Lazarus
//If this is not defined, default Python4Lazarus is required (which comes with Lazarus). Thos only supports Python 3.7
//  https://github.com/Alexey-T/Python-for-Lazarus

{$DEFINE FASTGZ}
//There are 3 modes
// DEFINED COREGL : uses "modern" OpenGL 3.3 for most efficient use of resources (most efficient)
// NOT COREGL, DEFINED LEGACY_INDEXING : uses indexed vertices and glDrawElements (medium efficiency)
// NOT COREGL, NOT LEGACY_INDEXING : glCallList with redundant vertices (least efficient)
{$DEFINE LHRH} //load both left and right hemisphere
{$DEFINE COREGL}
{$DEFINE LEGACY_INDEXING}
{$DEFINE GEOM_GLEXT}
{$DEFINE FOREIGNVOL}
//{$DEFINE HEMISSAO}  //for hemispherical SSAO - requires shaders that save surface normal - see hemiAO3 and hemiAO
{$DEFINE CTM} //support OpenCTM format, requires LZMA decoding
//{$DEFINE TUBES}
{$DEFINE MATCAP}
