{$DEFINE MYPY} //Allow Python scripts
//{$DEFINE DGL}  //for DGL library, otherwise gl/glext

//{$DEFINE FASTGZ} //Fast CloudFlare zlib, requires linking files from folders x86_64*


//There are 3 modes
// DEFINED COREGL : uses "modern" OpenGL 3.3 for most efficient use of resources (most efficient): required for GTK3
// NOT COREGL, DEFINED LEGACY_INDEXING : uses indexed vertices and glDrawElements (medium efficiency)
// NOT COREGL, NOT LEGACY_INDEXING : glCallList with redundant vertices (least efficient)
//{$DEFINE COREGL}
//{$DEFINE LEGACY_INDEXING}
//{$DEFINE GEOM_GLEXT} //legacy OpenGL optionally supports geometry shaders: using these extensions may fail with some drivers

{$DEFINE LHRH} //load both left and right hemisphere
//{$DEFINE HEMISSAO}  //for hemispherical SSAO - requires shaders that save surface normal - see hemiAO3 and hemiAO
{$DEFINE CTM} //support OpenCTM format, requires LZMA decoding
//{$DEFINE TUBES}
{$DEFINE MATCAP} //Matcap based shaders