# MZ3 format specification

## General Overview
The MZ3 format is the internal file format for Surfice. The goals are to be simple, fast and small.

 - MZ3 only stores triangular meshes. A quad face with four edges is stored as two triangles. This matches the expectations of modern graphics cards
 - A mesh is based on a series of vertices and  [indexed](http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-9-vbo-indexing/) faces. The faces are indexed from 0, so the face [0,1,2] defines a triangle which uses the first 3 vertices.
 - The [winding order](https://www.khronos.org/opengl/wiki/Face_Culling) of faces defines the direction the triangle is facing. So faces [0,1,2] and [0,2,1] are at the same location but the front-face of one is the back-face of the other.
 - Every face index is stored as a 32-bit integer. For example, consider  a cube with just 8 vertices: even though each index will only be in the range 0..7, each index is saved as a 32-bit integer. Note GZip compression will mitigate this redundancy.
 - An MZ3 file may represent raw binary data or it may be GZip compressed. One can detect whether the file is compressed by reading the first 2-bytes of the file. All GZip files begin with hex = 0x1F8B, all uncompressed MZ3 files begin with hex = 0x4D5A.
 - Data is always stored as little-endian. For example, Intel-based computers can read the raw data directly. On the other hand, PowerPC-based computers will need to byte-swap values when reading and writing.
 - The first 16-bytes provide a header that describes the file. This reports number of faces (NFACE), the number of vertices (NVERT), and the number of bytes to skip (NSKIP). It also includes a 16-bit attribute integer (ATTR) that stores the following boolean (true/false) variables: are face indices stored (isFACE +1), are vertices stored (isVERT +2), are colors stored (isRGBA +4) and whether scalar intensity value are stored (isSCALAR +8). Note that the maximum value for ATTR is 15 - any greater value describes a future version of the MZ3 format.
 - The total file size in bytes is `FILESIZE = 16 + NSKIP + isFACE * NFACE * 12 + isVERT * NVERT * 12 + isRGBA * NVERT * 4 + isSCALAR * NVERT * 4`.
 - NSKIP allows developers to store private data. As long as ATTR is 15 or less (i.e. a current version), one can insert up to 2Gb of data in each file: the data is inserted immediately after the header and the NSKIP value is used to report the size of this private data. The developer can use any format they want for the private data. Private data will be ignored by typical implementations. If one wants to store private data, one may wish to consider using a specific signature to identify your private data format from some other private format.
 - If isFACE: face indices are stored as 32-bit integers. Each face index lists the three vertices that define a triangle.
 - If isVERT: vertices are stored as 32-bit floats. Each vertex has three coordinates (X,Y,Z). These coordinates are those of MNI space: X ascends to the right, Y ascends anteriorly and Z ascends superiorly. Note this differs from many graphics formats where X ascends toward the right of the screen, Y ascends toward the bottom of the screen, and Z is the depth dimension into the screen.
 - If isRGBA: then the file stores a color value for every vertex. The color is saved as a 32-bit integer with 8-bytes each to red, green, blue and alpha (in that order).
 - If isSCALAR: the file stores one 32-bit float for every vertex. For example, this might represent Z-score at each vertex. A value must be stored for every vertex. If one wants to ignore a vertex (e.g. no test computed at this location or Z-score did not reach threshould) the vertex intensity should be set to [NaN](https://en.wikipedia.org/wiki/NaN).
 - If BOTH isSCALAR and isRGBA: both values are stored in the file. However, in this case the mesh is assumed to be a template. The RGBA colors refer to the color of the region, and the scalar intensity value refers to the region number. In this case, one expects the scalar values can be losslessly converted to integers. In other words, one expects the template to have regions `17` and `18`, but one does not expect a region `17.2`.
 - Files can store isSCALAR without having any other data. Since these files do not report vertex position or face indexing, they must be viewed with a corresponding mesh that includes these values. An example includes a [Gaussian curvature](https://en.wikipedia.org/wiki/Gaussian_curvature) measurement. Further, statistics might produce separate isSCALAR files for every contrast: for example consider a brain activation study where we produce statistical maps for both left hand movements relative to rest and right hand movements relative to rest. This would produce two statistical maps that are likely to contain some overlap (e.g. some brain regions are only used by one task, but others are involved with moving either hand).
 - The format does not store vertex normals. It is typically straightforward to compute these based on the mesh (though see next bullet point for potential issues with per-face colors).
 - The format only allows per-vertex colors. This is typical for shader-based per-pixel interpolation. However, if per-face colors are desired one will have to create replicated vertices. For example, a cube with per-vertex colors can be saved as 8 vertices. However, if one wants each face of the cube to have a single unique color, one must save 24 faces (four for each of the six faces). Be aware that using per-face colors may influence the surface normal estimation, depending on the visualization software. If the software does not remove the duplicated vertices when computing normals you will get [jagged per-facet normals rather than smooth per pixel normals](https://www.scratchapixel.com/lessons/3d-basic-rendering/introduction-to-shading/shading-normals). For an example of per-face colors see the mesh 7ColoredMeshPerFace.mz3 described below.
 - File consistency checks. The following rules help identify invalid mz3 files:
   - isFACE and isTRUE always have the same value. If both are true, the file contains mesh geometry (the vertices and face indices). If both are false, the file only contains vertex scalars/colors (e.g. it is a statistical map or curvature file designed to be overlayed on top of a mesh). In theory, one could store a point cloud in an mz3 file by storing vertices without any faces. However, in practice mz3 files are assumed to store triangular meshes.
   - If isFACE is true, then NFACE must be at least 1. The minimum mesh has one triangle.
   - Every MZ3 file has NVERT of at least 3. This is because if isVERT, isRGBA or isSCALAR is true, then the number of vertices must be encoded. The minimum mesh is one triangle composed of three different vertices.

## Formal Specification
```
Faces indexed from 0: a triangle of the first 3 vertices is  0,1,2
  Always LITTLE endian
  First two bytes are a signature for detecting format, next two bytes reveal features and version
  Optionally: may be GZ compressed (read first two bytes to detect: GZip = 0x1f8B, raw = 0x4D5A )
 HEADER: first 16 bytes
  bytes : type : notes
  0-1: UINT16 : MAGIC signature hex = 0x4D5A integer = 23117, ASCII = 'MZ'
  2-3: UINT16 : ATTR attributes bitfield reporting stored data (value larger than 15 indicates future version):
        bitand(ATTR,1) = 1 : isFACE File includes FACE indices
        bitand(ATTR,2) = 2 : isVERT File includes VERT vertices
        bitand(ATTR,4) = 4 : isRGBA File includes RGBA values (one per vertex)
        bitand(ATTR,8) = 8 : isSCALAR File includes SCALAR values (one per vertex)
  4-7: UINT32 : NFACE number of faces (one face per triangle)
  8-11: UINT32 : NVERT number of vertices
  12-15: UINT32 : NSKIP bytes to skip (0 for current version)
 The header is 16+NSKIP bytes long
 Note: for better compression integer data is transposed (interleaved)
  FACE DATA: if isFACE, next 12*NFACE bytes
   +0..3: INT32 : 1st index of 1st triangle
   +0..3: INT32 : 1st index of 2nd triangle
   +0..3: INT32 : 1st index of 3rd triangle
    ....
   ++     INT32 : 3rd index of NVERT triangle
  VERTEX DATA: if isVERT, next 12*NVERT bytes
   +0..3: FLOAT32 : X of first vertex
   +4..7: FLOAT32 : Y of first vertex
   +8..11: FLOAT32 : Z of first vertex
   +12..15: FLOAT32 : X of second vertex
    ....
   ++     INT32 : Z of NVERT triangle
  RGBA DATA: if isRGBA next 4*NVERT bytes
   +0: UINT8: red for first vertex
   +1: UINT8: green for first vertex
   +2: UINT8: blue for first vertex
   +3: UINT8: alpha for first vertex
   +4: UINT8: red for 2nd vertex
   ...
   ++     UINT8 : alpha for NVERT vertex
  SCALAR DATA: if isSCALAR next 4*NVERT bytes
   +0..3: FLOAT32: intensity for first vertex
   +4..7: FLOAT32: intensity for 2nd vertex
   +8..11: FLOAT32: intensity for 3rd vertex
   ...
   ++     FLOAT32: intensity for NVERT vertex
```

## Examples

The included Matlab script `demo_mz3.m` will generate a series of simple mz3 files to demonstrate the properties of the format. These meshes are inspired by these [ascii meshes](https://brainder.org/2011/09/25/braindering-with-ascii-files/).
 - `3Mesh.mz3` a mesh defined by vertices and faces, but no colors. ATTR: isFACE(+1), isVERT(+2).
 - `7ColoredMeshPerVertex.mz3` a mesh defined by vertices, faces, and vertex colors. ATTR: isFACE(+1), isVERT(+2), isRGBA(+4).
 - `7ColoredMeshPerFace.mz3` a mesh defined by vertices, faces, and vertex colors. This used per-face colors, requiring each vertex position to be duplicated. ATTR: isFACE(+1), isVERT(+2), isRGBA(+4).
 - `8Scalar.mz3` a file that stores scalar values only: requires a mesh to infer geometry. ATTR: isSCALAR(+8).
 - `15TemplateMesh.mz3` a mesh defined by vertices and faces, colors and scalars. As described in the specification, this is assumed to be a template mesh. ATTR: isFACE(+1), isVERT(+2), isRGBA(+4), isSCALAR(+8).

## Implementations

 - [MRIcroS](https://github.com/bonilhamusclab/MRIcroS) can read, write and visualize MZ3 meshes. The Matlab scripts [writeMz3.m](https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bmz3/writeMz3.m) and [readMz3.m](https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bmz3/readMz3.m) handle the MZ3 format.
 - [Surfice](https://www.nitrc.org/plugins/mwiki/index.php/surfice:MainPage) can read, write and visualize MZ3 meshes. The Pascal functions [loadMz3 and saveMz3](https://github.com/neurolabusc/surf-ice/blob/master/mesh.pas) handle the MZ3 format.
  - [Blender import/export](https://github.com/neurolabusc/surf-ice/tree/master/mz3/blender-mz3-addon).
  - This web page includes a simple [Python script](https://github.com/neurolabusc/surf-ice/tree/master/mz3/mz3.py) that reports the contents of a mz3 file. For example, running `python ./mz3.py 7ColoredMeshPerFace.mz3` will report the properties of that mesh. Note that all faces, vertices, colors and scalars are reported to the terminal window, so this tool is only practical for simple meshes.