unit matmath;
{$IFDEF FPC}
{$mode objfpc}{$ENDIF}{$H+}
interface

uses
  Classes, SysUtils, define_types;


 function ptf(x,y,z: single): TPoint3f; //create float vector
 function pti(x,y,z: integer): TPoint3i; //create integer vector
 function getSurfaceNormal(v1, v2, v3: TPoint3f): TPoint3f;
 function normalDirection(v1, v2: TPoint3f): TPoint3f;
 function crossProduct(v1, v2: TPoint3f): TPoint3f;
 procedure vectorNormalize(var v: TPoint3f);  inline;
 function vectorDot (A: TPoint3f; B: TPoint3f): single;
 procedure vectorAdd (var A: TPoint3f; B: TPoint3f);  inline; overload;
 function vectorAdd (var A: TPoint3i; B: integer): TPoint3i;  inline; overload;

 procedure vectorSubtract (var A: TPoint3f; B: TPoint3f);  inline;
 procedure minMax(var v, mn, mx: TPoint3f); overload;
 procedure minMax(var v: TPoint3i; var mn, mx: integer); overload;
 function vectorLength(pt1, pt2: TPoint3F): single;
 function matrixMult(a, b: TMat33): TMat33;
 function matrixSet(a,b,c, d,e,f, g,h,i: single): TMat33;
 function matrixInvert(a: TMat33): TMat33;
 procedure matrixNegate(var a: TMat33);
 procedure matrixTranspose(var a: TMat33);
 procedure vectorNegate(var v: TPoint3f);  inline;
 function AlignVector(alignmentVector: TPoint3f): TMat33;

 function vectorDistance(A,B: TPoint3f): single;
 function vectorScale(A: TPoint3f; Scale: single): TPoint3f;
 procedure MakeCylinder(radius: single; start, dest: TPoint3f; var faces: TFaces; var vertices: TVertices; slices: integer = 20); overload;
 procedure MakeCylinder( radius, len: single; var faces: TFaces; var vertices: TVertices; slices: integer = 20); overload;
 procedure makeCylinderEnd(radius: single; prevPos, Pos, nextPos: TPoint3f; var vertices: TVertices; slices: integer = 20);


implementation

function vectorScale(A: TPoint3f; Scale: single): TPoint3f;
begin
     result.X := A.X * Scale;
     result.Y := A.Y * Scale;
     result.Z := A.Z * Scale;
end;

function vectorAdd (var A: TPoint3i; B: integer): TPoint3i;  inline; overload;
begin
  result.X := A.X + B;
  result.Y := A.Y + B;
  result.Z := A.Z + B;
end;

function vectorDistance(A,B: TPoint3f): single;
begin
  result := sqrt(sqr(A.X-B.X)+ sqr(A.Y-B.Y) + sqr(A.Z-B.Z));
end;

function distance(A,B: TPoint3f): single;
begin
  result := sqrt(sqr(A.X-B.X)+ sqr(A.Y-B.Y) + sqr(A.Z-B.Z));
end;

function lengthCross (A,B: TPoint3f): single;
var
      v: TPoint3f;
begin
  v := crossProduct(A, B);
  result := sqrt(sqr(v.X)+ sqr(v.Y) + sqr(v.Z));
end;

function AlignVector(alignmentVector: TPoint3f): TMat33;
//http://math.stackexchange.com/questions/180418/calculate-rotation-matrix-to-align-vector-a-to-vector-b-in-3d
//much simpler and 20% faster than
// http://www.mathworks.com/matlabcentral/fileexchange/12285-3d-quiver-with-volumized-arrows/content/quiver3D_pub/arrow3D.m
var
  A, B, crossBA, vx: TPoint3f;
  GG, FFi, invFFi, UU: TMat33;
  dotAB: single;
begin
  A := ptf(-1, 0, 0);
  B := alignmentVector;
  GG := matrixSet( vectorDot(A,B), -lengthCross(A,B), 0,
                lengthCross(A,B), vectorDot(A,B),  0,
                0, 0, 1);
  crossBA := crossProduct(B,A);
  dotAB := vectorDot(A,B);
  vx.X :=B.X - A.X * dotAB;
  vx.Y :=B.Y - A.Y * dotAB;
  vx.Z :=B.Z - A.Z * dotAB;
  vectorNormalize(vx);
    FFi := matrixSet(A.X, vx.X, crossBA.X,
        A.Y, vx.Y, crossBA.Y,
        A.Z, vx.Z, crossBA.Z);
  invFFi := matrixInvert(FFi);
  UU := matrixMult(FFi, GG);
  result := matrixMult(UU,invFFi);
  matrixNegate(result);
  matrixTranspose(result);
end; //AlignVector



procedure MakeCylinder( radius, len: single; var faces: TFaces; var vertices: TVertices; slices : integer = 20); Overload;
//procedure MakeCylinder( radius, len: single; var faces: TFaces; var vertices: TVertices); Overload;
//make a triangular mesh cylinder of specified radius and length. length is in X-dimension, so center is from (0,0,0)..(len,0,0)
var
   i, num_v, num_f: integer;
   x,y: single;
begin
  num_v := 2 * slices;
  num_f := 2 * slices;
  setlength(vertices, num_v);
  setlength(faces, num_f);
  for i := 0 to (slices-1) do begin
     y :=  radius * cos(i/slices * 2 *PI);
     x :=  radius * sin(i/slices * 2 * PI);
     vertices[i] := ptf(0,x, y);
     vertices[i + slices] := ptf(len, x, y);
     if i < (slices-1) then begin
        faces[i * 2] := pti( i,  i + 1, i+slices);
        faces[(i * 2)+1] := pti( i+1,  i + slices + 1, i + slices);
     end else begin
        faces[i * 2] := pti( i,  0, i+slices);
        faces[i * 2 + 1] := pti( 0,  0 + slices, i + slices);
     end;
  end;
end; // MakeCylinder()

procedure makeCylinder(radius: single; start, dest: TPoint3f; var faces: TFaces; var vertices: TVertices; slices: integer = 20); overload;
var
   alignmentVector, v: TPoint3f;
   i: integer;
   m: TMat33;
   len: single;
begin
  len := vectorLength(start, dest);
  MakeCylinder(radius, len, faces, vertices, slices);
  //rotate
  if length(vertices) < 1 then exit;
  alignmentVector := dest;
  vectorSubtract(alignmentVector, start);
  vectorNormalize(alignmentVector);
  m := AlignVector(alignmentVector);
  for i := 0 to (length(vertices)-1) do begin
      v := vertices[i];
      vertices[i].X := v.X * m[1,1] + v.Y * m[2,1] + v.Z * m[3,1] + start.X;
      vertices[i].Y := v.X * m[1,2] + v.Y * m[2,2] + v.Z * m[3,2] + start.Y;
      vertices[i].Z := v.X * m[1,3] + v.Y * m[2,3] + v.Z * m[3,3] + start.Z;
  end;
end; // makeCylinder()

procedure makeCylinderEnd(radius: single; prevPos, Pos, nextPos: TPoint3f; var vertices: TVertices; slices: integer = 20);
//make an end cap disk located at Pos and oriented along the axis of the previous and next entry
var
   i: integer;
   x,y: single;
   alignmentVector, v: TPoint3f;
     m: TMat33;
begin
  setlength(vertices, slices);
  for i := 0 to (slices-1) do begin
     y :=  radius * cos(i/slices * 2 *PI);
     x :=  radius * sin(i/slices * 2 * PI);
     vertices[i] := ptf(0,x, y);
  end;
  alignmentVector := nextPos;
  vectorSubtract(alignmentVector, prevPos);
  vectorNormalize(alignmentVector);
  m := AlignVector(alignmentVector);
  for i := 0 to (slices-1) do begin
      v := vertices[i];
      vertices[i].X := v.X * m[1,1] + v.Y * m[2,1] + v.Z * m[3,1] + Pos.X;
      vertices[i].Y := v.X * m[1,2] + v.Y * m[2,2] + v.Z * m[3,2] + Pos.Y;
      vertices[i].Z := v.X * m[1,3] + v.Y * m[2,3] + v.Z * m[3,3] + Pos.Z;
  end;
end; // makeCylinderEnd()

procedure matrixTranspose(var a: TMat33);
var
   b: TMat33;
   i,j: integer;
begin
 b := a;
 for i := 1 to 3 do
         for j := 1 to 3 do
             a[i,j] := b[j,i];

end; // matrixTranspose()

procedure matrixNegate(var a: TMat33);
var
   i,j: integer;
begin
     for i := 1 to 3 do
         for j := 1 to 3 do
             a[i,j] := -a[i,j];

end; // matrixNegate()

function matrixInvert( a: TMat33): TMat33;
//Translated by Chris Rorden, from C function "nifti_mat33_inverse"
// Authors: Bob Cox, revised by Mark Jenkinson and Rick Reynolds
// License: public domain
// http://niftilib.sourceforge.net
//Note : For higher performance we could assume the matrix is orthonormal and simply Transpose
//Note : We could also compute Gauss-Jordan here
var
	r11,r12,r13,r21,r22,r23,r31,r32,r33, deti : double;
begin
 r11 := a[1,1]; r12 := a[1,2]; r13 := a[1,3];  // [ r11 r12 r13 ]
 r21 := a[2,1]; r22 := a[2,2]; r23 := a[2,3];  // [ r21 r22 r23 ]
 r31 := a[3,1]; r32 := a[3,2]; r33 := a[3,3];  // [ r31 r32 r33 ]
 deti := r11*r22*r33-r11*r32*r23-r21*r12*r33
 +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;
 if( deti <> 0.0 ) then
    deti := 1.0 / deti ;
 result[1,1] := deti*( r22*r33-r32*r23) ;
 result[1,2] := deti*(-r12*r33+r32*r13) ;
 result[1,3] := deti*( r12*r23-r22*r13) ;
 result[2,1] := deti*(-r21*r33+r31*r23) ;
 result[2,2] := deti*( r11*r33-r31*r13) ;
 result[2,3] := deti*(-r11*r23+r21*r13) ;
 result[3,1] := deti*( r21*r32-r31*r22) ;
 result[3,2] := deti*(-r11*r32+r31*r12) ;
 result[3,3] := deti*( r11*r22-r21*r12) ;
end; // matrixInvert()

function vectorDot (A: TPoint3f; B: TPoint3f): single;
begin  //dot product
     result := A.X*B.X + A.Y*B.Y + A.Z*B.Z;
end;  // vectorDot()

function matrixSet(a,b,c, d,e,f, g,h,i: single): TMat33;
begin
     result[1,1]:=a; result[1,2]:=b; result[1,3]:=c;
     result[2,1]:=d; result[2,2]:=e; result[2,3]:=f;
     result[3,1]:=g; result[3,2]:=h; result[3,3]:=i;
end; // matrixSet()

function matrixMult(a, b: TMat33): TMat33;
var i,j: integer;
begin
   for i := 1 to 3 do begin
       for j := 1 to 3 do begin
           result[i, j] := A[i, 1] * B[1,j]
           + A[i, 2] * B[2, j]
           + A[i, 3] * B[3, j];
       end;  //for j
   end; //for i
end; //multiplymatrices()

function ptf(x,y,z: single): TPoint3f; //create float vector
begin
  result.X := x;
  result.Y := y;
  result.Z := z;
end; // ptf()

function pti(x,y,z: integer): TPoint3i; //create integer vector
begin
  result.X := x;
  result.Y := y;
  result.Z := z;
end; // pti()

procedure minMax(var v, mn, mx: TPoint3f); overload;
begin
     if v.X > mx.X then
        mx.X := v.X
     else if v.X < mn.X then
        mn.X := v.X;
     if v.Y > mx.Y then
        mx.Y := v.Y
     else if v.Y < mn.Y then
        mn.Y := v.Y;
     if v.Z > mx.Z then
        mx.Z := v.Z
     else if v.Z < mn.Z then
        mn.Z := v.Z;
end; // minMax()

procedure minMax(var v: TPoint3i; var mn, mx: integer); overload;
begin
     if v.X > mx then
        mx := v.X
     else if v.X < mn then
        mn := v.X;
     if v.Y > mx then
        mx := v.Y
     else if v.Y < mn then
        mn := v.Y;
     if v.Z > mx then
        mx := v.Z
     else if v.Z < mn then
        mn := v.Z;
end; // minMax()

function crossProduct(v1, v2: TPoint3f): TPoint3f;
// http://openinggl.blogspot.com/2012/04/adding-lighting-normals.html
begin
     result := ptf(v1.y * v2.z - v1.z * v2.y, v1.z * v2.x - v1.x * v2.z,
     v1.x * v2.y - v1.y * v2.x);
end; // CrossProduct()

procedure vectorNormalize(var v: TPoint3f);  inline;
var
   len: single;
begin
     len := sqrt( (v.X*v.X) + (v.Y*v.Y) + (v.Z*v.Z) );
     if len <= 0 then exit;
     v.X := v.X / len;
     v.Y := v.Y / len;
     v.Z := v.Z / len;
end; // normalize()

function normalDirection(v1, v2: TPoint3f): TPoint3f;
begin
     result.X := v2.X - v1.X;
     result.Y := v2.Y - v1.Y;
     result.Z := v2.Z - v1.Z;
     vectorNormalize(result);
end; // normalDirection()

procedure vectorAdd (var A: TPoint3f; B: TPoint3f);  inline;
//sum two vectors
begin
     A.X := A.X + B.X;
     A.Y := A.Y + B.Y;
     A.Z := A.Z + B.Z;
end; // vectorAdd()

function vectorLength(pt1, pt2: TPoint3F): single;
begin
  result := sqrt (sqr(pt1.X - pt2.X)  + sqr(pt1.Y - pt2.Y)  + sqr(pt1.Z - pt2.Z)  );
end; // vectorLength()

procedure vectorNegate(var v: TPoint3f);  inline;
begin
     v.X := -v.X;
     v.Y := -v.Y;
     v.Z := -v.Z;
end; // vectorNegate()

procedure vectorSubtract (var A: TPoint3f; B: TPoint3f);  inline;
//sum two vectors
begin
     A.X := A.X - B.X;
     A.Y := A.Y - B.Y;
     A.Z := A.Z - B.Z;
end; // vectorSubtract()

function getSurfaceNormal(v1, v2, v3: TPoint3f): TPoint3f;
var
   polyVector1, polyVector2: TPoint3f;
begin
 polyVector1 := ptf(v2.x - v1.x, v2.y - v1.y, v2.z - v1.z);
 polyVector2 := ptf(v3.x - v1.x, v3.y - v1.y, v3.z - v1.z);
 result := crossProduct(polyVector1, polyVector2);
 //make sure to normalize(result) !
end; // getSurfaceNormal()

end.

