unit ctm_loader;

{$mode objfpc}{$H+}
//CTM format loader
//  Format specification : http://openctm.sourceforge.net/?page=about
//  C source code (Marcus Geelnard) : http://openctm.sourceforge.net/?page=download
//  JavaScript code (Juan Mellado) https://github.com/jcmellado/js-openctm
//
//ported Pascal by Chris Rorden and retain original license
//  closely based on C source code, and retains same license
// Description: Implementation of the MG2 compression method.
//-----------------------------------------------------------------------------
// Copyright (c) 2009-2010 Marcus Geelnard
//
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
//     1. The origin of this software must not be misrepresented; you must not
//     claim that you wrote the original software. If you use this software
//     in a product, an acknowledgment in the product documentation would be
//     appreciated but is not required.
//
//     2. Altered source versions must be plainly marked as such, and must not
//     be misrepresented as being the original software.
//
//     3. This notice may not be removed or altered from any source
//     distribution.

interface

uses
  Classes, SysUtils, define_types, ULZMADecoder, dialogs;

function readCTM(const FileName: string; var Faces: TFaces;  var Verts: TVertices; var vertexRGBA : TVertexRGBA): boolean;

implementation

const
 kLZMApropBytes = 5; //LZMA specific props (five bytes, required by the LZMA decoder)
type
TMG2Header = packed record
 magic  : int32;
 vtxPrec, nPrec, LBx, LBy, LBz, HBx, HBy, HBz: single;
 divx, divy, divz: uint32;
end;

type
 CTMfloat3 = array[0..2] of single;
 CTMuint3 = array[0..2] of uint32;

procedure deByteInterleave(var F: TMemoryStream);
//see "1.3.2 Byte interleaving" of CTM specification
const
 kElementSize = 4; //LZMA specific props (five bytes, required by the LZMA decoder)
var
   iBytes, Bytes: array of byte;
   n, i,j, k, nElem: integer;
begin
     n := F.Size;
     setlength(iBytes, n);
     setlength(Bytes, n);
     F.Position := 0;
     F.Read(iBytes[0], n);
     nElem := n div kElementSize;
     k := 0;
     for i := 0 to nElem do  begin
         for j :=   (kElementSize-1) downto 0  do begin
             Bytes[k] := iBytes[(j * nElem)+ i];
             k := k + 1;
         end;
     end;
     F.Clear;
     F.Position:= 0;
     F.WriteBuffer(Bytes[0], n);
     F.Position:= 0;
end;


function LZMAdecompress(var inStream, outStream: TMemoryStream; count: Int64): Int64;
var
  decoder: TLZMADecoder;
  propArray : array of byte;
begin
  outStream.Clear;
  result := 0;
  setlength(propArray,kLZMApropBytes);
  inStream.Read(propArray[0],kLZMApropBytes);
  decoder := TLZMADecoder.Create;
  if not decoder.SetDecoderProperties(propArray) then begin
     showmessage('decode error');
     exit;

  end;
  decoder.OnProgress:= nil;
  if not decoder.Code(inStream, outStream, count) then exit;
  //decoder.CleanupInstance;
  decoder.free;
  result := outStream.Size;
  outStream.Position:= 0;
  deByteInterleave(OutStream);
end;

procedure restoreIndices(var inStream : TMemoryStream; var faces: TFaces; triangleCount: int32);
// see "3.2.1 Indices"  of CTM specification
//   compressMG2.c
var
  outSize, i, nTri, nTri2: integer;
  ints: array of longint;
begin
  outSize := triangleCount * sizeof(TPoint3i);
  if (inStream.Size <> outSize) or (triangleCount < 1) then exit;
  setLength(ints, triangleCount * 3);
  inStream.Read(ints[0], outSize);
  inStream.Clear;
  setlength(faces, triangleCount);
  nTri := triangleCount; //element interleaving stride for Y
  nTri2 := nTri * 2; //element interleaving stride for Z
  faces[0].X := ints[0];
  faces[0].Y := faces[0].X + ints[nTri];
  faces[0].Z := faces[0].X + ints[nTri2];
  if nTri < 2 then exit;
  for i := 1 to (nTri-1) do begin
      faces[i].X := faces[i-1].X + ints[i];
      if (faces[i].X = faces[i-1].X) then
         faces[i].Y := faces[i-1].Y + ints[i+nTri]
      else
          faces[i].Y := faces[i].X + ints[i+nTri];
      faces[i].Z := faces[i].X + ints[i+nTri2];
  end;
end;

procedure gridIdxToPoint(mDivision: CTMuint3; mSize, mMin: CTMfloat3; aIdx: uint32; out aPoint: CTMfloat3);
var
  zdiv, ydiv, i: uint32;
  gridIdx: CTMuint3;
begin
  zdiv := mDivision[0] * mDivision[1];
  ydiv := mDivision[0];

  gridIdx[2] :=  trunc(aIdx / zdiv);
  aIdx := aIdx - (gridIdx[2] * zdiv);
  gridIdx[1] :=  trunc(aIdx / ydiv);
  aIdx :=  aIdx - (gridIdx[1] * ydiv);
  gridIdx[0] := aIdx;
  for i := 0 to 2 do
    aPoint[i] := gridIdx[i] * mSize[i] + mMin[i];
end;

procedure restoreVertices(hdr: TMG2Header; var intVertices, gridIndices: TInts; var Verts: TVertices; mVertexCount: int32);
//3.3.3 Vertices for MG2 - see function ctmRestoreVertices of "compressMG2.c" Copyright (c) 2009-2010 Marcus Geelnard
var
  i,j: integer;
  gridOrigin, mMin, mMax, mSize: CTMfloat3;
  mDivision: CTMuint3;
  scale: single;
  gridIdx, prevGridIndex: uint32;
  deltaX, prevDeltaX: int32;
  intVerticesA: TInts;
begin
     if (mVertexCount < 3) or (length(gridIndices) <> mVertexCount) or (length(intVertices) <> (3 *mVertexCount)) then exit; //single triangle has 3 vertices
     //remove element interleaving
     intVerticesA := Copy(intVertices, Low(intVertices), Length(intVertices));
     j := 0;
     for i := 0 to (mVertexCount - 1) do begin
           intVertices[j] := intVerticesA[i];
           intVertices[j+1] := intVerticesA[i+mVertexCount];
           intVertices[j+2] := intVerticesA[i+mVertexCount+mVertexCount];
           j := j + 3;
     end;
     setlength(intVerticesA, 0);
     //decode vertices
     setlength(Verts,mVertexCount);
     mMin[0] := hdr.LBx; mMin[1] := hdr.LBy; mMin[2] := hdr.LBz;
     mMax[0] := hdr.HBx; mMax[1] := hdr.HBy; mMax[2] := hdr.HBz;
     mDivision[0] := hdr.divX;  mDivision[1] := hdr.divY; mDivision[2] := hdr.divZ;
     for i := 0 to 2 do
         mSize[i] := (mMax[i]- mMin[i]) / mDivision[i];
     for i := 1 to (mVertexCount -1) do // Restore grid indices (deltas)
         gridIndices[i] := gridIndices[i] + gridIndices[i-1]; //run length encoded, convert gi' -> gi, 3.3.4
     scale := hdr.vtxPrec;
     prevGridIndex := $7fffffff;
     prevDeltaX := 0;
     for i := 0 to (mVertexCount-1) do begin
         // Get grid box origin
         gridIdx := gridIndices[i];
         //ctmGridIdxToPoint(aGrid, gridIdx, gridOrigin);
         gridIdxToPoint(mDivision, mSize, mMin, gridIdx, gridOrigin);
         // Restore original point
         deltaX := intVertices[i * 3];
         if (gridIdx = prevGridIndex) then
            deltaX := deltaX + prevDeltaX;
         Verts[i].X := scale * deltaX + gridOrigin[0];
         Verts[i].Y := scale * intVertices[i * 3 + 1] + gridOrigin[1];
         Verts[i].Z := scale * intVertices[i * 3 + 2] + gridOrigin[2];
         prevGridIndex := gridIdx;
         prevDeltaX := deltaX;
     end;
end;

procedure RestoreAttribs(var inStream : TMemoryStream; var vertexRGBA: TVertexRGBA; vertexCount: int32; scale: single);
//decode MG2 ATTR, see 3.3.8 Attribute maps
var
  i,j: integer;
  aIntAttribs: TInts;
  value,prev: int32;
  b :byte;
begin
  if vertexCount < 1 then exit;
  setlength(aIntAttribs, vertexCount * 4);
  inStream.Read(aIntAttribs[0], vertexCount * 4 * sizeof(single));
  //adjust for Signed magnitude representation, see 1.3.3
  for i := 0 to ((4*vertexCount)-1) do begin
     if odd(aIntAttribs[i]) then
        aIntAttribs[i] := -(1+(aIntAttribs[i]shr 1))
     else
         aIntAttribs[i] := aIntAttribs[i]shr 1;
  end;
  setlength(vertexRGBA, vertexCount);
  for j := 0 to 3 do begin
     prev := 0;
     for i := 0 to (vertexCount-1) do begin
         value := aIntAttribs[i+(J * vertexCount)] + prev;
         b := round(255 * value * scale);
         case j of
              0: vertexRGBA[i].R := b;
              1: vertexRGBA[i].G := b;
              2: vertexRGBA[i].B := b;
              3: vertexRGBA[i].A := b;
         end;
         prev := value;
     end; //for i: each vertex
  end; //for j: RGBA
end;

function readCTM(const FileName: string; var Faces: TFaces;  var Verts: TVertices; var vertexRGBA : TVertexRGBA): boolean;
type
  TCTMFileHeader = record
   magic, fileFormat, compressionMethod,vertexCount,triangleCount,uvMapCount,attrMapCount, flags, commentBytes: int32;
  end;
label
  123, 666;
const
  kFileMagic = 1297367887; //"OCTM" as 32-bit little-endian integer
  kRAW = $00574152; //"RAW\0"
  kMG1 = $0031474d; //"MG1\0"
  kMG2 = $0032474d; //"MG2\0"
  kMG2Magic = $4832474d; //"MG2H"
  kVertMagic = $54524556; //"VERT"
  kGidxMagic = $58444947; //"GIDX"
  kIndxMagic = $58444e49; //"INDX"
  kNormMagic = $4d524f4e; //"NORM"
  kTexcMagic = $43584554; //"TEXC" UV texture map
  kAttrMagic = $52545441; //"ATTR"
var
  F: TMemoryStream;
  Bytes : TBytes;
  hdr: TCTMFileHeader;
  hdrMG2 : TMG2Header;
  id, sz, attr: int32;
  outSize:int64;
  outStream : TMemoryStream;
  intVertices, gridIndices: TInts;
  vertAttr: array of single;
  i,j, mx: integer;
  str: string;
  sAttr: single;
begin
  {$IFDEF ENDIAN_BIG} adjust code to bytewap values {$ENDIF}
  result := false;
  setlength(vertexRGBA,0);
  if not FileExists(FileName) then exit;
  //initialize values
  outStream :=TMemoryStream.Create;
  F := TMemoryStream.Create;
  F.LoadFromFile(FileName);
  //CTM Header
  F.Read(hdr, sizeof(hdr));
  if hdr.magic <> kFileMagic then goto 666; //signature does not match

  if (hdr.compressionMethod <> kRAW) and (hdr.compressionMethod <> kMG1) and (hdr.compressionMethod <> kMG2) then goto 666; //signature does not match
  if hdr.commentBytes > 0 then begin
     setlength(Bytes, hdr.commentBytes);
     F.Read(Bytes[0], hdr.commentBytes);
     //comment:= TEncoding.ASCII.GetString(Bytes);
  end;
  //raw format
  if (hdr.compressionMethod = kRAW)  then begin
     //read INDX
      F.Read(id, sizeof(int32));
      if (id <> kIndxMagic)  then goto 666;
      setlength(Faces, hdr.triangleCount);
      F.Read(Faces[0], hdr.triangleCount * sizeof(TPoint3i) );
      F.Read(id, sizeof(int32));
     //read VERT
      if (id <> kVertMagic)  then goto 666;
       setlength(Verts, hdr.vertexCount);
       F.Read(Verts[0], hdr.vertexCount  * 3 * sizeof(single));
       if hdr.attrMapCount < 1 then goto 123; //all done - no vertex color map
       attr := 0;
       while (attr < hdr.attrMapCount) and (F.Position < (F.Size-12)) do begin
             F.Read(id, sizeof(int32));
             if id = kNormMagic then // 3.1.3 Normals
                F.Seek(4*(1 + 3 * hdr.vertexCount), soFromCurrent); //skip this
             if id = kTexcMagic then //3.1.4 UV maps
                goto 123; //this file uses color texture, not vertex colors
             if id = kAttrMagic then begin
                F.Read(sz, sizeof(int32));
                setlength(Bytes, sz);
                F.Read(Bytes[0], sz);
                //For all versions of Lazarus
                SetString(str, PAnsiChar(@Bytes[0]), sz);
                //For newer versions of Lazarus
                // str := upcase(TEncoding.ASCII.GetString(Bytes));
                setlength(vertAttr, hdr.vertexCount * 4);
                F.Read(vertAttr[0], hdr.vertexCount * 4 * sizeof(single));
                if (sz = 5) and (upcase(str) = 'COLOR') then begin
                   setlength(vertexRGBA, hdr.vertexCount);
                   for i := 0 to (hdr.vertexCount - 1) do begin
                       j := i * 4;
                       vertexRGBA[i].R := round(255 * vertAttr[j]);
                       vertexRGBA[i].G := round(255 * vertAttr[j+1]);
                       vertexRGBA[i].B := round(255 * vertAttr[j+2]);
                       vertexRGBA[i].A := round(255 * vertAttr[j+3]);
                   end;
                   goto 123; //all done: vertex color map loaded
                end; //is COLOR
                attr := attr + 1;
             end; //is ATTR
       end; //while not EOF
       goto 123; //all done: no vertex color map found
  end;  //RAW
  if (hdr.compressionMethod = kMG1)  then begin
    //read INDX
    F.Read(id, sizeof(int32));
    F.Read(sz, sizeof(int32));
    if (id <> kIndxMagic) or (sz < 8) then goto 666;
    outSize := hdr.triangleCount * sizeof(TPoint3i);
    sz := LZMAdecompress(F,outStream,outSize);
    if sz <> outSize then goto 666;
    restoreIndices(outStream, Faces, hdr.triangleCount);
    //read VERT
    F.Read(id, sizeof(int32));
    F.Read(sz, sizeof(int32));
    if (id <> kVertMagic) or (sz < 8) then goto 666;
    outSize := hdr.vertexCount * 3 * sizeof(single);
    sz := LZMAdecompress(F,outStream,outSize);
    if sz <> outSize then goto 666;
    setlength(Verts, hdr.vertexCount);
    outStream.Read(Verts[0], outSize);
  end; //MG1
  if (hdr.compressionMethod = kMG2)  then begin
    //read MG2H
    F.Read(hdrMG2, sizeof(hdrMG2));

    if (hdrMG2.magic <> kMG2Magic)then goto 666;
    //read VERT
    F.Read(id, sizeof(int32));
    F.Read(sz, sizeof(int32));
    if (id <> kVertMagic) or (sz < 8) then goto 666;

    outSize := hdr.vertexCount * 3 * sizeof(int32);
    outStream.Clear;
    sz := LZMAdecompress(F,outStream,outSize);
    if sz <> outSize then
       showmessage(inttostr(sz)+'<>'+inttostr(outSize));
    if sz <> outSize then goto 666;
    setlength(intVertices, hdr.vertexCount * 3);
    outStream.Read(intVertices[0], outSize);
    //read GIDX
    F.Read(id, sizeof(int32));
    F.Read(sz, sizeof(int32));
    if (id <> kGidxMagic) or (sz < 8) then goto 666;

    outSize := hdr.vertexCount  * sizeof(int32); //one element per vertex
    sz := LZMAdecompress(F,outStream,outSize);
    if sz <> outSize then goto 666;
    setlength(gridIndices, hdr.vertexCount);
    outStream.Read(gridIndices[0], outSize);
    restoreVertices(hdrMG2, intVertices, gridIndices, Verts, hdr.vertexCount);
    //read INDX
    F.Read(id, sizeof(int32));
    F.Read(sz, sizeof(int32));
    if (id <> kIndxMagic) or (sz < 8) then goto 666;
    outSize := hdr.triangleCount * sizeof(TPoint3i);
    //showmessage(inttostr(F.Position)+' '+inttostr(outSize));
    sz := LZMAdecompress(F,outStream,outSize);
    if sz <> outSize then goto 666;
    restoreIndices(outStream, Faces, hdr.triangleCount);
  end; //MG2

  //read color for MG1 and MG2
  if hdr.attrMapCount < 1 then goto 123; //all done - no vertex color map
  attr := 0;
  while (attr < hdr.attrMapCount) and (F.Position < (F.Size-12)) do begin
        F.Read(id, sizeof(int32));
        F.Read(sz, sizeof(int32));
        if id = kNormMagic then // 3.1.3 Normals
           F.Seek(kLZMApropBytes + sz, soFromCurrent); //skip this, 5
        if id = kTexcMagic then //3.1.4 UV maps
           goto 123; //this file uses color texture, not vertex colors
        if id = kAttrMagic then begin
           setlength(Bytes, sz); //sz refers to size of string
           F.Read(Bytes[0], sz);
           //For all versions of Lazarus
           SetString(str, PAnsiChar(@Bytes[0]), sz);
           //For newer versions of Lazarus
           // str := upcase(TEncoding.ASCII.GetString(Bytes));
           if (hdr.compressionMethod = kMG2) then
              F.Read(sAttr, sizeof(single)); //see 3.3.8 : Attribute value precision, s.
           F.Read(sz, sizeof(int32)); //sz refers to packed bytes
           outSize := hdr.vertexCount * 4 * sizeof(single);
           outStream.Clear; outStream.position := 0;
           sz := LZMAdecompress(F,outStream,outSize);
           if (outSize = sz) and (length(str) = 5) and (upcase(str) = 'COLOR') then begin
             if (hdr.compressionMethod = kMG2) then
                RestoreAttribs(outStream, vertexRGBA, hdr.vertexCount, sAttr)
             else begin
                  setlength(vertAttr, hdr.vertexCount * 4);
                  outStream.Read(vertAttr[0], outSize);
                  setlength(vertexRGBA, hdr.vertexCount);
                  for i := 0 to (hdr.vertexCount - 1) do begin  //n.b. element interleaving
                      vertexRGBA[i].R := round(255 * vertAttr[i]);
                      vertexRGBA[i].G := round(255 * vertAttr[i+hdr.vertexCount]);
                      vertexRGBA[i].B := round(255 * vertAttr[i+(2*hdr.vertexCount)]);
                      vertexRGBA[i].A := round(255 * vertAttr[i+(3*hdr.vertexCount)]);
                  end; //for each vertex
             end; //if MG2 else MG1
             goto 123;
           end; //attr = COLOR
           attr := attr + 1;
        end; //is ATTR
  end; //not EOF
  123:
  result := true;
  if length(vertexRGBA) > 0 then begin //if alpha not assigned, make vertex colors opaque
     mx := 0;
     for i := 0 to (length(vertexRGBA)-1) do
         if vertexRGBA[i].A > mx then
            mx := vertexRGBA[i].A;
     if mx = 0 then
        for i := 0 to (length(vertexRGBA)-1) do
            vertexRGBA[i].A := 128;
  end;
  666:
  outStream.Free;
  F.Free;
  if not result then begin
     Showmessage('Unable to decode CTM file '+Filename);
     setlength(Faces,0);
     setlength(Verts,0);
  end;
end;

end.

