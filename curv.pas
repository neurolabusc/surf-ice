unit curv;
interface

uses define_types, matmath, sysutils;

procedure GenerateCurv(fnm: string; var faces: TFaces; vertices: TVertices);

implementation

function curvK(v0, v1, n0, n1: TPoint3f): single;
//http://computergraphics.stackexchange.com/questions/1718/what-is-the-simplest-way-to-compute-principal-curvature-for-a-mesh-triangle
//local curvature for an edge
// k = ((n1-n0)⋅(n1-p0))/len(p1,p0)
var
   len: single;
begin
  result := 0;
  len := vectorLength(v0,v1);
  if len <= 0 then exit; //avoid div/0
  result := vectorDot(vectorSubtractF(n1,n0), vectorSubtractF(v1,v0));
  result := result / len;
end;

procedure SaveCurv(fnm: string; k: TFloats; num_f: integer);
//simple format used by Freesurfer  BIG-ENDIAN
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bpial/readPial.m
// http://www.grahamwideman.com/gw/brain/fs/surfacefileformats.htm
var
   f: File;
   i: integer;
   num_vS, num_v, num_fS, ValsPerVertex : LongWord;
   b : byte = 255;
   kS: TFloats;
begin
  num_v := length(k);
  if (num_v < 3) or (num_f < 1) then exit;
  num_vS := num_v;
  num_fS := num_f;
  ValsPerVertex := 1;
  AssignFile(f, fnm);
  FileMode := fmOpenWrite;
  try
    Rewrite(f,1);
    blockwrite(f, b, 1 ); //since these files do not have a file extension, check first 3 bytes "0xFFFFFF"
    blockwrite(f, b, 1 ); //since these files do not have a file extension, check first 3 bytes "0xFFFFFF"
    blockwrite(f, b, 1 ); //since these files do not have a file extension, check first 3 bytes "0xFFFFFF"
    {$IFDEF ENDIAN_LITTLE}
    SwapLongWord(num_vS);
    SwapLongWord(num_fS);
    SwapLongWord(ValsPerVertex);
    {$ENDIF}
    blockwrite(f, num_vS, 4 ); //uint32
    blockwrite(f, num_fS, 4 ); //uint32
    blockwrite(f, ValsPerVertex, 4 ); //uint32
    {$IFDEF ENDIAN_LITTLE}
    setlength(kS, num_v);
    for i := 0 to (num_v-1) do begin
       kS[i] := k[i];
       SwapSingle(kS[i]);
    end;
    blockwrite(f,kS[0], 4 * num_v);
    setlength(kS, 0);
    {$ELSE}
    blockwrite(f,k[0], 4 * num_v);
    {$ENDIF}
    CloseFile(f);
  except
   // If there was an error the reason can be found here
   on E: EInOutError do
     writeln('Unable to create '+fnm+' Details: ', E.ClassName, '/', E.Message);
  end;
end;

(*procedure SmoothK (var vK : TFloats; var faces: TFaces);
//smooth curvature across neighbors
//vK : one curvature value k for each vertex, faces: triangle face indices
var
   tK: TFloats;
   nK : array of integer;
   i, num_v, x, y, z: integer;
begin
  num_v := length(vK);
  setlength(tK, num_v);
  setlength(nK, num_v);
  tK := Copy(vK, Low(vK), num_v);
  for i := 0 to (num_v-1) do begin
     vK[i] := 0;
     nK[i] := 0;
  end;
  for i := 0 to (length(faces)-1) do begin //compute the normal for each face
    X := faces[i].X;
    Y := faces[i].X;
    Z := faces[i].X;
    nK[X] := nK[X] + 1;
    nK[Y] := nK[Y] + 1;
    nK[Z] := nK[Z] + 1;
    vK[X] := vK[X] + tK[X];
    vK[Y] := vK[Y] + tK[Y];
    vK[Z] := vK[Z] + tK[Z];
  end;
  setlength(tK, 0);
  for i := 0 to (num_v-1) do
      if nK[i] > 1 then
         vK[i] := vK[i] / nK[i];
  setlength(nK, 0);
end;  *)

procedure GenerateCurv(fnm: string; var faces: TFaces; vertices: TVertices);
var
  vNorm : array of TPoint3f;
  vK : TFloats;
  vnK : array of integer;
  fNorm: TPoint3f;
  k, mx, scale: single;
  i, X, Y, Z: integer;
begin
  if (length(vertices) < 3) or (length(faces) < 1) then exit;
  //compute surface normals...
  setlength(vNorm, length(vertices));
  fNorm := ptf(0,0,0);
  for i := 0 to (length(vertices)-1) do
      vNorm[i] := fNorm;
  for i := 0 to (length(faces)-1) do begin //compute the normal for each face
      fNorm := getSurfaceNormal(vertices[faces[i].X], vertices[faces[i].Y], vertices[faces[i].Z]);
      vectorAdd(vNorm[faces[i].X] , fNorm);
      vectorAdd(vNorm[faces[i].Y] , fNorm);
      vectorAdd(vNorm[faces[i].Z] , fNorm);
  end;
  for i := 0 to (length(vertices)-1) do
      vectorNormalize(vNorm[i]);
  //compute curv
  setlength(vK, length(vertices));
  setlength(vnK, length(vertices));
  for i := 0 to (length(vertices)-1) do begin
      vK[i] := 0.0;
      vnK[i] := 0;
  end;
  for i := 0 to (length(faces)-1) do begin //compute the curvature for each edge
      X := faces[i].X;
      Y := faces[i].Y;
      Z := faces[i].Z;
      //we will add curvature of two edges to each vertex of triangle
      inc(vnK[X],2);
      inc(vnK[Y],2);
      inc(vnK[Z],2);
      //compute edge X-Y
      k := curvK(vertices[X], vertices[Y], vNorm[X], vNorm[Y]);
      vK[X] := vK[X]+ k;
      vK[Y] := vK[Y]+ k;
      //compute edge Y-Z
      k := curvK(vertices[Y], vertices[Z], vNorm[Y], vNorm[Z]);
      vK[Y] := vK[Y]+ k;
      vK[Z] := vK[Z]+ k;
      //compute edge Z-X
      k := curvK(vertices[Z], vertices[X], vNorm[Z], vNorm[X]);
      vK[Z] := vK[Z]+ k;
      vK[X] := vK[X]+ k;
  end;
  //compute mean curvature for each vertex
  for i := 0 to (length(vertices)-1) do begin
      if vnK[i] > 0 then
         vK[i] := vK[i]/vnK[i];
  end;
  //optional: smooth curves
  //SmoothK (vK, faces);
  //normalize curvature from
  mx := vK[0];
  for i := 0 to (length(vertices)-1) do
      if abs(vK[i]) > mx then
         mx := abs(vK[i]);
  if mx = 0.0 then exit; //no variability: a flat plane?
  scale := 1/mx; //-1..1
  for i := 0 to (length(vertices)-1) do
      vK[i] := (vK[i] * scale) * -1; //-1 to match freesurfer
  SaveCurv(fnm, vK, length(faces));
end;

end.
