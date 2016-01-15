unit gl_core_matrix;
//emulates legacy opengl matrix stack that was removed from core opengl

{$mode objfpc}{$H+}
//http://glm.g-truc.net/0.9.7/index.html
interface

uses
  Classes, SysUtils, math;
type

  TnMode = (nGL_PROJECTION, nGL_MODELVIEW);
  TnMat44 = array [0..3, 0..3] of single;
  TnMat33 = array [0..2, 0..2] of single;
  TnGL = record
        mode: TnMode;
        mat: array[TnMode] of TnMat44;
  end;
  procedure nglMatrixMode (mode: TnMode);
  procedure nglLoadIdentity ();
  procedure nglScalef(x, y, z: single);
  procedure nglRotatef(angle, x, y, z: single);
  procedure nglTranslatef(x, y, z: single);
  procedure nglOrtho (left, right, bottom, top, zNear, zFar: single);
  procedure ngluPerspective (fovy, aspect, zNear, zFar: single);
  function ngl_ModelViewProjectionMatrix : TnMat44;
  function ngl_ModelViewMatrix : TnMat44;
  function ngl_NormalMatrix: TnMat33;
  function ngl_ProjectionMatrix : TnMat44;
var
   gnGL: TnGL;

implementation


function multMat(a, b: TnMat44): TnMat44;
var i,j: integer;
begin
   for i := 0 to 3 do begin
       for j := 0 to 3 do begin
           result[i, j] := A[i, 0] * B[0,j]
           + A[i, 1] * B[1, j]
           + A[i, 2] * B[2, j]
           + A[i, 3] * B[3, j];
       end;  //for j
   end; //for i
end; //multMat()

function transposeMat(a: TnMat44): TnMat44;
var i,j: integer;
begin
   for i := 0 to 3 do
       for j := 0 to 3 do
           result[i, j] := A[j,i]
end; //transposeMat()

function inverseMat (Rm: TnMat44): TnMat44;
//invert matrix see nifti_mat44_inverse( mat44 R )
// http://niftilib.sourceforge.net/c_api_html/nifti1__io_8h.html#a36
var
   r11,r12,r13,r21,r22,r23,r31,r32,r33,v1,v2,v3 , deti: double;
   Qm : TnMat44;
begin
  r11 := Rm[0,0]; r12 := Rm[0,1]; r13 := Rm[0,2];
  r21 := Rm[1,0]; r22 := Rm[1,1]; r23 := Rm[1,2];
  r31 := Rm[2,0]; r32 := Rm[2,1]; r33 := Rm[2,2];
  v1  := Rm[0,3]; v2  := Rm[1,3]; v3  := Rm[2,3];
  deti := r11*r22*r33-r11*r32*r23-r21*r12*r33
  +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;
  if( deti <> 0.0 ) then deti := 1.0 / deti ;
  Qm[0,0] := deti*( r22*r33-r32*r23) ;
  Qm[0,1] := deti*(-r12*r33+r32*r13) ;
  Qm[0,2] := deti*( r12*r23-r22*r13) ;
  Qm[0,3] := deti*(-r12*r23*v3+r12*v2*r33+r22*r13*v3
                    -r22*v1*r33-r32*r13*v2+r32*v1*r23) ;
  Qm[1,0] := deti*(-r21*r33+r31*r23) ;
  Qm[1,1] := deti*( r11*r33-r31*r13) ;
  Qm[1,2] := deti*(-r11*r23+r21*r13) ;
  Qm[1,3] := deti*( r11*r23*v3-r11*v2*r33-r21*r13*v3
                    +r21*v1*r33+r31*r13*v2-r31*v1*r23) ;
  Qm[2,0] := deti*( r21*r32-r31*r22) ;
  Qm[2,1] := deti*(-r11*r32+r31*r12) ;
  Qm[2,2] := deti*( r11*r22-r21*r12) ;
  Qm[2,3] := deti*(-r11*r22*v3+r11*r32*v2+r21*r12*v3
                    -r21*r32*v1-r31*r12*v2+r31*r22*v1) ;
  Qm[3,0] := 0;
  Qm[3,1] := 0;
  Qm[3,2] := 0;
  Qm[3,3] := 1;
  if (deti = 0.0) then Qm[3,3] := 0; // failure flag if deti = 0
  result := Qm ;
end;

function ngl_NormalMatrix: TnMat33;
//Returns normal matrix, in GLSL this would be
// mat4 NormalMatrix = transpose(inverse(ModelViewMatrix));
var
   m, q: TnMat44;
   i,j: integer;
begin
     q := inverseMat(gnGL.mat[nGL_MODELVIEW]);
     m := transposeMat(q);
     for i := 0 to 2 do //normal matrix is orthonormal, so only ignore final row/column
         for j := 0 to 2 do
             result[i,j] := m[i,j];
end;

procedure nglMatrixMode (mode: TnMode);
begin
     gnGL.mode := mode;
end;

function RSqrt(v: single): single;
begin
     result := 1/sqrt(v);
end;

function IdentityHmgMatrix: TnMat44;
var
   i,j: integer;
begin
     for i := 0 to 3 do
         for j := 0 to 3 do
             result[i,j] := 0;
     for i := 0 to 3 do
         result[i,i] := 1;
end;

function CreateGlRotateMatrix(angle, x, y, z: single) : TnMat44;
//http://www.gamedev.net/topic/600537-instead-of-glrotatef-build-a-matrix/
var
	c, s: single;
	invLen : Single;
begin
	angle:= degtorad(angle);
	invLen:= RSqrt(x * x + y * y + z * z);
	x:= x * invLen;
	y:= y * invLen;
	z:= z * invLen;
	result:= IdentityHmgMatrix;
	c:= cos(angle);
	s:= sin(angle);
	result[0,0] := (x*x) * (1-c)+c;
	result[1,0] := x*y * (1-c)-z*s;
	result[2,0] := x*z * (1-c)+y*s;
	result[0,1] := y*x * (1-c)+z*s;
	result[1,1] := (y*y) * (1-c)+c;
	result[2,1] := y*z * (1-c)-x*s;
	result[0,2] := x*z * (1-c)-y*s;
	result[1,2] := y*z * (1-c)+x*s;
	result[2,2] := (z*z) * (1-c)+c;
end;

function ngl_ModelViewProjectionMatrix : TnMat44;
 //gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * gl_Vertex;
begin
   //result := matrixMult(gnGL.mat[nGL_PROJECTION], gnGL.mat[nGL_MODELVIEW]);
   result := multMat( gnGL.mat[nGL_MODELVIEW], gnGL.mat[nGL_PROJECTION]);
end;

procedure nglRotatef(angle, x, y, z: single);
var
    m : TnMat44;
begin
     m := CreateGlRotateMatrix(angle, x, y, z);
    gnGL.mat[gnGL.mode] := multMat(m, gnGL.mat[gnGL.mode]);
end;

procedure nglTranslatef(x, y, z: single);
var
   m : TnMat44;
begin
     //m := gnGL.mat[gnGL.mode];
     m := IdentityHmgMatrix;
     m[3,0] := m[3,0] + x;
     m[3,1] := m[3,1] + y;
     m[3,2] := m[3,2] + z;
     //gnGL.mat[gnGL.mode] := m;
     gnGL.mat[gnGL.mode] := multMat(m, gnGL.mat[gnGL.mode]);
end;

procedure nglScalef(x, y, z: single);
var
   m : TnMat44;
begin
     m := IdentityHmgMatrix;
     m[0,0] :=  x;
     m[1,1] :=  y;
     m[2,2] :=  z;
     gnGL.mat[gnGL.mode] := multMat(m, gnGL.mat[gnGL.mode]);
end;

procedure nglLoadIdentity ();
begin
     gnGL.mat[gnGL.mode] := IdentityHmgMatrix
end;

procedure ngluPerspective (fovy, aspect, zNear, zFar: single);
//https://www.opengl.org/sdk/docs/man2/xhtml/gluPerspective.xml
var
  f: single;
  m : TnMat44;
begin
   m :=  IdentityHmgMatrix;
   f :=  cot(degtorad(fovy)/2);
   m[0,0] := f/aspect;
   m[1,1] := f;
   m[2,2] := (zFar+zNear)/(zNear-zFar) ;
   m[3,2] := (2*zFar*zNear)/(zNear-zFar);
   m[2,3] := -1;
   m[3,3] := 0;
   //raise an exception if zNear = 0??
   gnGL.mat[gnGL.mode] := multMat(m,gnGL.mat[gnGL.mode]);
end;

procedure nglOrtho (left, right, bottom, top, zNear, zFar: single);
//https://www.opengl.org/sdk/docs/man2/xhtml/glOrtho.xml
var
   m : TnMat44;
begin
        m :=  IdentityHmgMatrix;
	m[0,0] := 2 / (right - left);
	m[1,1] := 2 / (top - bottom);
	m[2,2] := - 2 / (zFar - zNear);
	m[3,0] := - (right + left) / (right - left);
	m[3,1] := - (top + bottom) / (top - bottom);
	m[3,2] := - (zFar + zNear) / (zFar - zNear);
	//gnGL.mat[gnGL.mode] := m;
        gnGL.mat[gnGL.mode] := multMat(m,gnGL.mat[gnGL.mode]);
end;

function ngl_ModelViewMatrix : TnMat44;
begin;
   result := gnGL.mat[nGL_MODELVIEW];
end;

function ngl_ProjectionMatrix : TnMat44;
begin
  result := gnGL.mat[nGL_PROJECTION];

end;

begin
  gnGL.mat[nGL_PROJECTION] := IdentityHmgMatrix;
  gnGL.mat[nGL_MODELVIEW] := IdentityHmgMatrix;
end.

