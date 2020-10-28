{$DEFINE MYPY}
//{$DEFINE DGL}  //for DGL library, otherwise gl/glext
{$DEFINE FASTGZ}
//There are 3 modes
// DEFINED COREGL : uses "modern" OpenGL 3.3 for most efficient use of resources (most efficient)
// NOT COREGL, DEFINED LEGACY_INDEXING : uses indexed vertices and glDrawElements (medium efficiency)
// NOT COREGL, NOT LEGACY_INDEXING : glCallList with redundant vertices (least efficient)
{$DEFINE LHRH} //load both left and right hemisphere
//{$DEFINE COREGL}
{$DEFINE LEGACY_INDEXING}
{$DEFINE GEOM_GLEXT}
//{$DEFINE HEMISSAO}  //for hemispherical SSAO - requires shaders that save surface normal - see hemiAO3 and hemiAO
{$DEFINE CTM} //support OpenCTM format, requires LZMA decoding
//{$DEFINE TUBES}
{$DEFINE MATCAP}