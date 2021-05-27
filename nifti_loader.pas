unit nifti_loader;

{$mode objfpc}{$H+}
{$Include opts.inc} //FOREIGNVOL
interface
uses
  nifti_foreign, {$ifndef isTerminalApp}  dialogs, {$endif}
  Classes, SysUtils, nifti_types, define_types,  zstream;

const               //(0/1/2 none/smoothMasked/smooth)
  kNiftiSmoothNone = 0; //no smoothing: raw values
  kNiftiSmoothMaskZero = 1; //smoothing but ignoring zeros (avoid erosion due to brain mask)
  kNiftiSmooth = 2; //conventional smoothing


type
 TMatrix = array[1..4, 1..4] of single;
TImgRaw = array of byte;
TImgScaled= array of single;
TNIFTI = class
    hdr : TNIFTIhdr;
    mat, invMat: TMatrix;
    maxInten, minInten: single;
    isZeroMasked, isLoad4D: boolean;
    img: TImgScaled;//array of single;
  private
    function ImgRawToSingle(imgBytes: TImgRaw; isSwap: boolean): boolean;
    function  readImg(const FileName: string; isSwap: boolean; gzFlag: int64): boolean;
    procedure setMatrix;
    procedure SetDescriptives;
  public
    isBinary: boolean;
    function mm2intensity( Xmm, Ymm, Zmm: single; isInterpolate: boolean): single;
    function mm2vox0(Xmm, Ymm, Zmm: single): TPoint3i; //rounded, voxels indexed from 0! e.g. if dim[0]=50 then output will range 0..49
    function validVox0(vox: TPoint3i): boolean; //returns true if voxel [indexed from 0] is inside volume, e.g. if dim[0]=50 then vox.X must be in range 0..49
    constructor Create;
    function LoadFromFile(const FileName: string; smoothMethod: integer): boolean; //smoothMethod is one of kNiftiSmooth...  options
    procedure SmoothMaskZero;
    procedure Smooth;
    procedure Close;
    Destructor  Destroy; override;
end;
function readVoxHeader (var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean; var xDim64: int64): boolean;

implementation

procedure nifti_quatern_to_mat44( var lR :TMatrix;
                             var qb, qc, qd,
                             qx, qy, qz,
                             dx, dy, dz, qfac : single);
var
   a,b,c,d,xd,yd,zd: double;
begin
   //a := qb;
   b := qb;
   c := qc;
   d := qd;
   //* last row is always [ 0 0 0 1 ] */
   lR[4,1] := 0;
   lR[4,2] := 0;
   lR[4,3] := 0;
   lR[4,4] := 1;
   //* compute a parameter from b,c,d */
   a := 1.0 - (b*b + c*c + d*d) ;
   if( a < 1.e-7 ) then begin//* special case */
     a := 1.0 / sqrt(b*b+c*c+d*d) ;
     b := b*a ; c := c*a ; d := d*a ;//* normalize (b,c,d) vector */
     a := 0.0 ;//* a = 0 ==> 180 degree rotation */
   end else begin
     a := sqrt(a) ; //* angle = 2*arccos(a) */
   end;
   //* load rotation matrix, including scaling factors for voxel sizes */
   if dx > 0 then
      xd := dx
   else
       xd := 1;
   if dy > 0 then
      yd := dy
   else
       yd := 1;
   if dz > 0 then
      zd := dz
   else
       zd := 1;
   if( qfac < 0.0 ) then zd := -zd ;//* left handedness? */
   lR[1,1]:=        (a*a+b*b-c*c-d*d) * xd ;
   lR[1,2]:= 2.0 * (b*c-a*d        ) * yd ;
   lR[1,3]:= 2.0 * (b*d+a*c        ) * zd ;
   lR[2,1]:=  2.0 * (b*c+a*d        ) * xd ;
   lR[2,2]:=        (a*a+c*c-b*b-d*d) * yd ;
   lR[2,3]:=  2.0 * (c*d-a*b        ) * zd ;
   lR[3,1]:= 2.0 * (b*d-a*c        ) * xd ;
   lR[3,2]:=  2.0 * (c*d+a*b        ) * yd ;
   lR[3,3]:=         (a*a+d*d-c*c-b*b) * zd ;
   //* load offsets */
   lR[1,4]:= qx ;
   lR[2,4]:= qy ;
   lR[3,4]:= qz ;
end;

function invertMatrixF(a: TMatrix): TMatrix;
//Translated by Chris Rorden, from C function "nifti_mat44_inverse"
// Authors: Bob Cox, revised by Mark Jenkinson and Rick Reynolds
// License: public domain
// http://niftilib.sourceforge.net
//Note : For higher performance we could assume the matrix is orthonormal and simply Transpose
//Note : We could also compute Gauss-Jordan here
var
	r11,r12,r13,r21,r22,r23,r31,r32,r33,v1,v2,v3 , deti : double;
begin
   r11 := a[1,1]; r12 := a[1,2]; r13 := a[1,3];  //* [ r11 r12 r13 v1 ] */
   r21 := a[2,1]; r22 := a[2,2]; r23 := a[2,3];  //* [ r21 r22 r23 v2 ] */
   r31 := a[3,1]; r32 := a[3,2]; r33 := a[3,3];  //* [ r31 r32 r33 v3 ] */
   v1  := a[1,4]; v2  := a[2,4]; v3  := a[3,4];  //* [  0   0   0   1 ] */
   deti := r11*r22*r33-r11*r32*r23-r21*r12*r33
		 +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;
   if( deti <> 0.0 ) then
	deti := 1.0 / deti ;
   result[1,1] := deti*( r22*r33-r32*r23) ;
   result[1,2] := deti*(-r12*r33+r32*r13) ;
   result[1,3] := deti*( r12*r23-r22*r13) ;
   result[1,4] := deti*(-r12*r23*v3+r12*v2*r33+r22*r13*v3
                      -r22*v1*r33-r32*r13*v2+r32*v1*r23) ;
   result[2,1] := deti*(-r21*r33+r31*r23) ;
   result[2,2] := deti*( r11*r33-r31*r13) ;
   result[2,3] := deti*(-r11*r23+r21*r13) ;
   result[2,4] := deti*( r11*r23*v3-r11*v2*r33-r21*r13*v3
                      +r21*v1*r33+r31*r13*v2-r31*v1*r23) ;
   result[3,1] := deti*( r21*r32-r31*r22) ;
   result[3,2] := deti*(-r11*r32+r31*r12) ;
   result[3,3] := deti*( r11*r22-r21*r12) ;
   result[3,4] := deti*(-r11*r22*v3+r11*r32*v2+r21*r12*v3
                      -r21*r32*v1-r31*r12*v2+r31*r22*v1) ;
   result[4,1] := 0; result[4,2] := 0; result[4,3] := 0.0 ;
   if (deti = 0.0) then
        result[4,4] := 0
   else
       result[4,4] := 1;//  failure flag if deti == 0
end;

function notZero(v: single): single;
//binary result
begin
   if v = 0 then
        result := 0
   else
       result := 1;
end;

function TNIfTI.validVox0(vox: TPoint3i): boolean; //returns true if voxel [indexed from 0] is inside volume, e.g. if dim[0]=50 then vox.X must be in range 0..49
begin
   result := false;
   if (vox.X < 0) or (vox.Y < 0) or (vox.Z < 0) then exit;
   if (vox.X >= hdr.dim[1]) or (vox.Y >= hdr.dim[2]) or (vox.Z >= hdr.dim[3]) then exit;
   result := true;
end;

function TNIfTI.mm2vox0(Xmm, Ymm, Zmm: single): TPoint3i; //voxels indexed from 0!
begin
   result.X := round(Xmm*invMat[1,1] + Xmm*invMat[1,2] + Xmm*invMat[1,3] + invMat[1,4]);
   result.Y := round(Ymm*invMat[2,1] + Ymm*invMat[2,2] + Ymm*invMat[2,3] + invMat[2,4]);
   result.Z := round(Zmm*invMat[3,1] + Zmm*invMat[3,2] + Zmm*invMat[3,3] + invMat[3,4]);
end;

function TNIfTI.mm2intensity( Xmm, Ymm, Zmm: single; isInterpolate: boolean): single;
var
   Xvox, Yvox, Zvox: single; //voxel coordinates indexed from 0
   Xfrac1, Yfrac1, Zfrac1, Xfrac0, Yfrac0, Zfrac0, Weight : single;
   vx, sliceVx: integer;
begin
   if length(img) < 1 then exit(0);
   result := 0;
   Xvox := Xmm*invMat[1,1] + Xmm*invMat[1,2] + Xmm*invMat[1,3] + invMat[1,4];
   Yvox := Ymm*invMat[2,1] + Ymm*invMat[2,2] + Ymm*invMat[2,3] + invMat[2,4];
   Zvox := Zmm*invMat[3,1] + Zmm*invMat[3,2] + Zmm*invMat[3,3] + invMat[3,4];
   if (Xvox < 0) or (Yvox < 0) or (Zvox < 0) then exit;
   if (Xvox >= (hdr.dim[1]-1)) or (Yvox >= (hdr.dim[2]-1)) or (Zvox >= (hdr.dim[3]-1)) then exit;
   sliceVx := hdr.dim[1] * hdr.dim[2]; //voxels per slice
   if not isInterpolate then begin
      vx := round(Xvox) + round(Yvox) * hdr.dim[1] + round(Zvox) * sliceVx;
      result := img[vx];
      exit;
   end;
   Xfrac1 := frac(Xvox);  Yfrac1 := frac(Yvox);  Zfrac1 := frac(Zvox);
   Xfrac0 := 1 - Xfrac1;  Yfrac0 := 1 - Yfrac1;  Zfrac0 := 1 - Zfrac1;

   vx := trunc(Xvox) + trunc(Yvox) * hdr.dim[1] + trunc(Zvox) * sliceVx;

   weight :=  Xfrac0 * Yfrac0 * Zfrac0  * notZero(img[vx]) +
              Xfrac1 * Yfrac0 * Zfrac0  * notZero(img[vx+1]) +
              Xfrac0 * Yfrac1 * Zfrac0  * notZero(img[vx+hdr.dim[1]]) +
              Xfrac1 * Yfrac1 * Zfrac0  * notZero(img[vx+1+hdr.dim[1]]) +
              Xfrac0 * Yfrac0 * Zfrac1  * notZero(img[vx+sliceVx]) +
              Xfrac1 * Yfrac0 * Zfrac1  * notZero(img[vx+1+sliceVx]) +
              Xfrac0 * Yfrac1 * Zfrac1  * notZero(img[vx+hdr.dim[1]+sliceVx]) +
              Xfrac1 * Yfrac1 * Zfrac1  * notZero(img[vx+1+hdr.dim[1]+sliceVx]);
   if weight = 0 then begin  //all zeros
      result := 0;
      exit;
   end;
   result :=  Xfrac0 * Yfrac0 * Zfrac0  * img[vx] +
              Xfrac1 * Yfrac0 * Zfrac0  * img[vx+1] +
              Xfrac0 * Yfrac1 * Zfrac0  * img[vx+hdr.dim[1]] +
              Xfrac1 * Yfrac1 * Zfrac0  * img[vx+1+hdr.dim[1]] +
              Xfrac0 * Yfrac0 * Zfrac1  * img[vx+sliceVx] +
              Xfrac1 * Yfrac0 * Zfrac1  * img[vx+1+sliceVx] +
              Xfrac0 * Yfrac1 * Zfrac1  * img[vx+hdr.dim[1]+sliceVx] +
              Xfrac1 * Yfrac1 * Zfrac1  * img[vx+1+hdr.dim[1]+sliceVx];
   result := result / weight; //exclude influence of zero values (e.g. NaN voxels)

end;

procedure TNIfTI.setMatrix;
begin
  mat[1,1] := Hdr.srow_x[0];
  mat[1,2] := hdr.srow_x[1];
  mat[1,3] := hdr.srow_x[2];
  mat[1,4] := hdr.srow_x[3];
  mat[2,1] := hdr.srow_y[0];
  mat[2,2] := hdr.srow_y[1];
  mat[2,3] := hdr.srow_y[2];
  mat[2,4] := hdr.srow_y[3];
  mat[3,1] := hdr.srow_z[0];
  mat[3,2] := hdr.srow_z[1];
  mat[3,3] := hdr.srow_z[2];
  mat[3,4] := hdr.srow_z[3];
  mat[4,1] := 0;
  mat[4,2] := 0;
  mat[4,3] := 0;
  mat[4,4] := 1;
   if (Hdr.sform_code <= kNIFTI_XFORM_UNKNOWN) or (Hdr.sform_code > kNIFTI_XFORM_MNI_152) then begin //use quaternion
    if (Hdr.qform_code > kNIFTI_XFORM_UNKNOWN) and (Hdr.qform_code <= kNIFTI_XFORM_MNI_152) then begin
       nifti_quatern_to_mat44(mat, Hdr.quatern_b,Hdr.quatern_c,Hdr.quatern_d,
       Hdr.qoffset_x,Hdr.qoffset_y,Hdr.qoffset_z,
       Hdr.pixdim[1],Hdr.pixdim[2],Hdr.pixdim[3],
       Hdr.pixdim[0]);
    end;
  end;
  invMat := invertMatrixF(mat);
end;

procedure Xswap4r ( var s:single);
type
  swaptype = packed record
	case byte of
	  0:(Word1,Word2 : word); //word is 16 bit
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  inguy^.Word1 := outguy.Word1;
  inguy^.Word2 := outguy.Word2;
end;

procedure swap4(var s : LongInt);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongInt);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  s:=outguy.Long;
end;

function swapDouble(s : double):double;
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2,Word3,Word4 : word); //word is 16 bit
      1:(float:double);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word4);
  outguy.Word2 := swap(inguy^.Word3);
  outguy.Word3 := swap(inguy^.Word2);
  outguy.Word4 := swap(inguy^.Word1);
  try
    result:=outguy.float;
  except
        result := 0;
        exit;
  end;
end; //func swap8r

constructor  TNIFTI.Create;
begin
  isZeroMasked := false;
  isBinary := false;
  isLoad4D := false;
  setlength(img,0);
     //
end; // Create()

function Swap2(s : SmallInt): smallint;
type
  swaptype = packed record
    case byte of
      0:(Word1 : word); //word is 16 bit
      1:(Small1: SmallInt);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word1);
  result :=outguy.Small1;
end;

procedure NIFTIhdr_SwapBytes (var lAHdr: TNIFTIhdr); //Swap Byte order for the Analyze type
var
   lInc: integer;
begin
    with lAHdr do begin
         swap4(hdrsz);
         swap4(extents);
         session_error := swap2(session_error);
         for lInc := 0 to 7 do
             dim[lInc] := swap2(dim[lInc]);//666
         Xswap4r(intent_p1);
         Xswap4r(intent_p2);
         Xswap4r(intent_p3);
         intent_code:= swap2(intent_code);
         datatype:= swap2(datatype);
         bitpix := swap2(bitpix);
         slice_start:= swap2(slice_start);
         for lInc := 0 to 7 do
             Xswap4r(pixdim[linc]);
         Xswap4r(vox_offset);
{roi scale = 1}
         Xswap4r(scl_slope);
         Xswap4r(scl_inter);
         slice_end := swap2(slice_end);
         Xswap4r(cal_max);
         Xswap4r(cal_min);
         Xswap4r(slice_duration);
         Xswap4r(toffset);
         swap4(glmax);
         swap4(glmin);
         qform_code := swap2(qform_code);
         sform_code:= swap2(sform_code);
         Xswap4r(quatern_b);
         Xswap4r(quatern_c);
         Xswap4r(quatern_d);
         Xswap4r(qoffset_x);
         Xswap4r(qoffset_y);
         Xswap4r(qoffset_z);
		 for lInc := 0 to 3 do //alpha
			 Xswap4r(srow_x[lInc]);
		 for lInc := 0 to 3 do //alpha
			 Xswap4r(srow_y[lInc]);
		 for lInc := 0 to 3 do //alpha
             Xswap4r(srow_z[lInc]);
    end; //with NIFTIhdr
end; //proc NIFTIhdr_SwapBytes

function ReadHdrGz(const FileName: string): TNIfTIhdr;
var
   decomp: TGZFileStream;
begin
     decomp := TGZFileStream.create(FileName, gzopenread);
     decomp.Read(result, sizeof(TNIfTIhdr));
     decomp.free;
end;

function ReadHdr(const FileName: string): TNIfTIhdr;
var
   f: File;
begin
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  blockread(f, result, sizeof(TNIfTIhdr) ); //since these files do not have a file extension, check first 8 bytes "0xFFFFFE creat"
  CloseFile(f);
end;

function Nifti2to1(h2 : TNIFTI2hdr): TNIFTIhdr;
type
  tmagic = packed record
    case byte of
      0:(b1,b2,b3,b4 : ansichar); //word is 16 bit
      1:(l: longint);
  end;
var
  h1 : TNIFTIhdr;
  i: integer;
  magic: tmagic;
begin
  NII_Clear(h1);
  magic.b1 := h2.magic[1];
  magic.b2 := h2.magic[2];
  magic.b3 := h2.magic[3];
  magic.b4 := h2.magic[4];
  h1.magic := magic.l;
  h1.dim_info := h2.dim_info; //MRI slice order
  for i := 0 to 7 do
   h1.dim[i] := h2.dim[i];
  h1.intent_p1 := h2.intent_p1;
  h1.intent_p2 := h2.intent_p2;
  h1.intent_p3 := h2.intent_p3;
  h1.intent_code := h2.intent_code;
  if (h2.intent_code >= 3000) and (h2.intent_code <= 3012) then begin //https://www.nitrc.org/forum/attachment.php?attachid=342&group_id=454&forum_id=1955
     showmessage('NIfTI2 image has CIfTI intent code ('+inttostr(h2.intent_code)+'): open as an overlay with Surfice');
  end;
  h1.datatype := h2.datatype;
  h1.bitpix := h2.bitpix;
  h1.slice_start := h2.slice_start;
  for i := 0 to 7 do
   h1.pixdim[i] := h2.pixdim[i];
  h1.vox_offset := h2.vox_offset;
  h1.scl_slope := h2.scl_slope;
  h1.slice_end := h2.slice_end;
  h1.slice_code := h2.slice_code; //e.g. ascending
  h1.cal_min:= h2.cal_min;
  h1.cal_max:= h2.cal_max;
  h1.xyzt_units := h2.xyzt_units; //e.g. mm and sec
  h1.slice_duration := h2.slice_duration; //time for one slice
  h1.toffset := h2.toffset; //time axis to shift
  for i := 1 to 80 do
   h1.descrip[i] := h2.descrip[i];
  for i := 1 to 24 do
   h1.aux_file[i] := h2.aux_file[i];
  h1.qform_code := h2.qform_code;
  h1.sform_code := h2.sform_code;
  h1.quatern_b := h2.quatern_b;
  h1.quatern_c := h2.quatern_c;
  h1.quatern_d := h2.quatern_d;
  h1.qoffset_x := h2.qoffset_x;
  h1.qoffset_y := h2.qoffset_y;
  h1.qoffset_z := h2.qoffset_z;
  for i := 0 to 3 do begin
     h1.srow_x[i] := h2.srow_x[i];
     h1.srow_y[i] := h2.srow_y[i];
     h1.srow_z[i] := h2.srow_z[i];
  end;
  for i := 1 to 16 do
     h1.intent_name[i] := h2.intent_name[i];
  h1.HdrSz := 348;
  result := h1;
end;

function ReadHdr2Gz(const FileName: string; var xDim64: int64): TNIfTIhdr;
var
   decomp: TGZFileStream;
   h2: TNIFTI2hdr;
begin
     decomp := TGZFileStream.create(FileName, gzopenread);
     decomp.Read(h2, sizeof(TNIFTI2hdr));
     decomp.free;
     xDim64 := h2.Dim[1];
     result := Nifti2to1(h2);
end;

function ReadHdr2(const FileName: string; var xDim64: int64): TNIfTIhdr;
var
   f: File;
   h2: TNIFTI2hdr;
begin
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  blockread(f, h2, sizeof(TNIFTI2hdr) ); //since these files do not have a file extension, check first 8 bytes "0xFFFFFE creat"
  CloseFile(f);
  xDim64 := h2.Dim[1];
  result := Nifti2to1(h2);
end;


function FixDataType (var lHdr: TNIFTIhdr): boolean;
var
  ldatatypebpp: integer;
begin
  result := true;
  //lbitpix := lHdr.bitpix;
  case lHdr.datatype of
    kDT_BINARY : ldatatypebpp := 1;
    kDT_UNSIGNED_CHAR  : ldatatypebpp := 8;     // unsigned char (8 bits/voxel)
    kDT_SIGNED_SHORT  : ldatatypebpp := 16;      // signed short (16 bits/voxel)
    kDT_SIGNED_INT : ldatatypebpp := 32;      // signed int (32 bits/voxel)
    kDT_FLOAT : ldatatypebpp := 32;      // float (32 bits/voxel)
    kDT_COMPLEX : ldatatypebpp := 64;      // complex (64 bits/voxel)
    kDT_DOUBLE  : ldatatypebpp := 64;      // double (64 bits/voxel)
    kDT_RGB : ldatatypebpp := 24;      // RGB triple (24 bits/voxel)
    kDT_INT8 : ldatatypebpp := 8;     // signed char (8 bits)
    kDT_UINT16 : ldatatypebpp := 16;      // unsigned short (16 bits)
    kDT_UINT32 : ldatatypebpp := 32;     // unsigned int (32 bits)
    kDT_INT64 : ldatatypebpp := 64;     // long long (64 bits)
    kDT_UINT64 : ldatatypebpp := 64;     // unsigned long long (64 bits)
    kDT_FLOAT128 : ldatatypebpp := 128;     // long double (128 bits)
    kDT_COMPLEX128 : ldatatypebpp := 128;   // double pair (128 bits)
    kDT_COMPLEX256 : ldatatypebpp := 256;     // long double pair (256 bits)
    else
      ldatatypebpp := 0;
  end;
  if (ldatatypebpp = lHdr.bitpix) and (ldatatypebpp <> 0) then
    exit; //all OK
  //showmessage(inttostr(ldatatypebpp));
  if (ldatatypebpp <> 0) then begin //use bitpix from datatype...
    lHdr.bitpix := ldatatypebpp;
    exit;
  end;
  showmessage('Corrupt NIfTI header');
  result := false;
end;

Type
  WordP = array of Word;
  SingleP = array of Single;
  DoubleP = array of Double;

function TNIFTI.ImgRawToSingle(imgBytes: TImgRaw; isSwap: boolean): boolean;
var
  i, nVox: integer;
  l16Buf : WordP;
  l32Buf : singleP;
  l64Buf : doubleP;
begin
     result := false;
     nVox:= hdr.dim[1] * hdr.dim[2] * hdr.dim[3] * hdr.dim[4];
     setlength(img, nVox);
     if hdr.bitpix = 8 then begin
        for i := 0 to (nVox -1) do
            img[i] := imgBytes[i]
     end else if hdr.bitpix = 16 then begin
        l16Buf := WordP(imgBytes );
        if isSwap then
          for i := 0 to (nVox -1) do
            l16Buf[i] := swap(l16Buf[i]);
         if hdr.datatype = kDT_UINT16 then begin
           for i := 0 to (nVox -1) do
             img[i] := l16Buf[i];
         end else begin
             for i := 0 to (nVox -1) do
                 img[i] := smallint(l16Buf[i]);
         end;
     end else if hdr.bitpix = 32 then begin
         l32Buf := SingleP(imgBytes );
        if isSwap then
          for i := 0 to (nVox -1) do
            Xswap4r (l32Buf[i]);
        if hdr.datatype = kDT_INT32 then begin
          for i := 0 to (nVox -1) do
              img[i] := longint(l32Buf[i]);
        end else begin //assume kDT_FLOAT
             for i := 0 to (nVox -1) do
               img[i] := l32Buf[i];
        end;
     end else if hdr.bitpix = 64 then begin
         l64Buf := DoubleP(imgBytes );
         if isSwap then
           for i := 0 to (nVox -1) do
               l64Buf[i] := SwapDouble(l64Buf[i]);
           for i := 0 to (nVox -1) do
               img[i] := l64Buf[i];
     end else
         exit;//Showmessage('Unsupported NIfTI datatype '+inttostr(hdr.bitpix)+'bpp');
     for i := 0 to (nVox -1) do //remove NaN
       if SpecialSingle(img[i]) then
          img[i] := 0;
     if (hdr.scl_slope = 0) or SpecialSingle(hdr.scl_slope) then
       hdr.scl_slope := 1;
     for i := 0 to (nVox -1) do //remove NaN
         img[i] := (img[i] * hdr.scl_slope) + hdr.scl_inter;
     result := true;
end;

//readForeignHeader (var lFilename: string; var lHdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean): boolean;
// K_gzBytes_headerAndImageCompressed = -2;
// K_gzBytes_onlyImageCompressed= -1;
// K_gzBytes_headerAndImageUncompressed= 0;

function TNIFTI.readImg(const FileName: string; isSwap: boolean; gzFlag: int64): boolean; //read first volume
const
  k0 : single = 0.0;
  k1 : single = 1.0;
var
   f: File;
   i,nVol, nVox, nByte: integer;
   decomp: TGZFileStream;
   imgBytes: array of byte;
begin
  result := false;
  nVol := 1;
  if isLoad4D then begin
     for i := 4 to 7 do
       if hdr.dim[i] > 1 then
         nVol := nVol * hdr.dim[i];
  end;
  hdr.dim[4] := nVol;
  nVox := hdr.dim[1] * hdr.dim[2] * hdr.dim[3] * hdr.dim[4];
  if nVox < 1 then exit;
  nByte := nVox * (hdr.bitpix div 8);
  if gzFlag = K_gzBytes_headerAndImageCompressed then begin
     decomp := TGZFileStream.create(FileName, gzopenread);
     setlength(imgBytes, round(hdr.vox_offset));
     decomp.Read(imgBytes[0], round(hdr.vox_offset));
     setlength(imgBytes, nByte);
     decomp.Read(imgBytes[0], nByte);
     decomp.free;
  end else if gzFlag = K_gzBytes_headerAndImageUncompressed then begin
    setlength(imgBytes, nByte);
    AssignFile(f, FileName);
    FileMode := fmOpenRead;
    Reset(f,1);
    Seek(f,round(hdr.vox_offset));
    BlockRead(f, imgBytes[0],nByte);
    CloseFile(f);
  end else begin
       showmessage('Unable to read compressed images with uncompressed headers!');
       exit;
  end;
  if not ImgRawToSingle(imgBytes, isSwap) then exit;
  isBinary := true;
  //nVox:= hdr.dim[1] * hdr.dim[2] * hdr.dim[3] * hdr.dim[4];
  i := 0;
  while (i < nVox) and (isBinary) do begin
        if (img[i] <> k0) and (img[i] <> k1) then
          isBinary := false;
        i := i + 1;
  end;
  result := true;
end;

procedure TNIFTI.SetDescriptives;
var
   i, numZero: integer;
begin
     isZeroMasked:= false;
     numZero := 0;
     if length(img) < 1 then exit;
     maxInten := img[0];
     minInten := maxInten;
     for i := 0 to (length(img) - 1) do begin
         if img[i] > maxInten then maxInten := img[i];
         if img[i] < minInten then minInten := img[i];
         if img[i] = 0 then inc(numZero);
     end;
     //showmessage(floattostr(numZero/length(img)) );
     isZeroMasked := (numZero/length(img)) > 0.75;
end; // SetDescriptives()

procedure SmoothFWHM2Vox (var lImg: TImgScaled; lXi,lYi,lZi: integer);
const
  k0=0.45;//weight of center voxel
  k1=0.225;//weight of nearest neighbors
  k2=0.05;//weight of subsequent neighbors
  kWid = 2; //we will look +/- 2 voxels from center
var
  lyPos,lPos,lX,lY,lZ,lXi2,lXY,lXY2: integer;
  lTemp: TImgScaled;
begin
   if (lXi < 5) or (lYi < 5) or (lZi < 5) then exit;
   lXY := lXi*lYi; //offset one slice
   lXY2 := lXY * 2; //offset two slices
   lXi2 := lXi*2;//offset to voxel two lines above or below
   setlength(lTemp,lXi*lYi*lZi);
   lTemp := Copy(lImg, Low(lImg), Length(lImg));
   //smooth horizontally
   for lZ := 0 to (lZi-1) do begin
     for lY := (0) to (lYi-1) do begin
       lyPos := (lY*lXi) + (lZ*lXY) ;
       for lX := (kWid) to (lXi-kWid-1) do begin
           lPos := lyPos + lX;
           lTemp[lPos] := lImg[lPos-2]*k2+lImg[lPos-1]*k1
                 +lImg[lPos]*k0
                 +lImg[lPos+1]*k1+lImg[lPos+2]*k2;
       end; {lX}
     end; {lY}
   end; //lZi
   //smooth vertically
   lImg := Copy(lTemp, Low(lTemp), Length(lTemp));
   for lZ := 0 to (lZi-1) do begin
     for lX := 0 to (lXi-1) do begin
       for lY := (kWid) to (lYi-kWid-1) do begin
           lPos := (lY*lXi) + lX + (lZ*lXY) ;
           lImg[lPos]  := lTemp[lPos-lXi2]*k2+lTemp[lPos-lXi]*k1
                 +lTemp[lPos]*k0
                 +lTemp[lPos+lXi]*k1+lTemp[lPos+lXi2]*k2;
       end; {lX}
     end; //lY
   end; //lZ
   //if between slices...
   lTemp := Copy(lImg, Low(lImg), Length(lImg));
     for lZ := (kWid) to (lZi-kWid-1) do begin
       for lY := 0 to (lYi-1) do begin
         lyPos := (lY*lXi) + (lZ*lXY) ;
         for lX := 0 to (lXi-1) do begin
             lPos := lyPos + lX;
             lTemp[lPos] := lImg[lPos-lXY2]*k2+lImg[lPos-lXY]*k1
                   +lImg[lPos]*k0
                   +lImg[lPos+lXY]*k1+lImg[lPos+lXY2]*k2;
         end; {lX}
       end; {lY}
     end; //lZi
     lImg := Copy(lTemp, Low(lTemp), Length(lTemp));
   SetLength(lTemp,0);
end;

procedure SmoothFWHM2VoxIgnoreZeros (var lImg: TImgScaled; lXi,lYi,lZi: integer);
//blur data, but do not allow zeros to influence values (e.g. brain mask)
var
  lTemp01: TImgScaled;
  i: integer;
begin
   setlength(lTemp01, Length(lImg));
   for i := 0 to (length(lImg)-1) do begin
     if lImg[i] = 0 then
       lTemp01[i] := 0
     else
         lTemp01[i] := 1;
   end;
   SmoothFWHM2Vox(lTemp01,  lXi,lYi,lZi);
   SmoothFWHM2Vox(lImg,  lXi,lYi,lZi);
   for i := 0 to (length(lImg)-1) do
     if lTemp01[i] <> 0 then
       lImg[i] := lImg[i]/lTemp01[i];
end;

//function readVoxHeader (var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean; var xDim64: int64): boolean;
function readVoxHeader (var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean; var xDim64: int64): boolean;
var
   ext, hdrName, imgName: string;
   isDimPermute2341: boolean = false;
begin
    result := false;
    swapEndian := false; //assume native endian
    gzBytes := K_gzBytes_headerAndImageUncompressed;
     if not FileExists(fname) then exit;
     ext := UpperCase(ExtractFileExt(fname));
     imgName := fname;
     hdrName := fname;
     if (ext = '.GZ') then begin
        nhdr := ReadHdrGz(fname);
        xDim64 := nhdr.Dim[1];
        gzBytes := K_gzBytes_headerAndImageCompressed;
     end else if ((ext = '.NII') or (ext = '.HDR') or (ext = '.IMG')) then begin
        if (ext = '.IMG') then
           hdrName := ChangeFileExt(fname, '.hdr')
        else if ext = '.HDR' then
            imgName := ChangeFileExt(fname, '.img');
        nhdr := ReadHdr(hdrName);
        xDim64 := nhdr.Dim[1];
     end else begin
       if not readForeignHeader (imgName, nhdr, gzBytes, swapEndian, isDimPermute2341, xDim64) then exit;
     end;
     //NIfTI2
     if (nhdr.HdrSz = SizeOf (TNIFTI2hdr)) then begin
        if (ext = '.GZ') then
           nhdr := ReadHdr2Gz(fname, xDim64)
        else
            nhdr := ReadHdr2(fname, xDim64);
     end;
     if sizeof(TNIfTIhdr) <> nhdr.HdrSz then begin
       NIFTIhdr_SwapBytes(nhdr);
       if (nhdr.HdrSz = SizeOf (TNIFTI2hdr)) then begin
          {$IFDEF ENDIAN_LITTLE}
          showmessage('Unable to read big-endian NIfTI 2 headers.');
          {$ELSE}
          showmessage('Unable to read little-endian NIfTI 2 headers.');
          {$ENDIF}
       end;
       swapEndian := true;
       xDim64 := nhdr.Dim[1];
     end;
     if sizeof(TNIfTIhdr) <> nhdr.HdrSz then
        exit; //not a valid nifti header
     if not FixDataType (nhdr) then exit;
     result := true;
end;

procedure printf(s: string);
begin
     {$IFDEF UNIX}writeln(s);{$ENDIF}
end;

function TNIFTI.LoadFromFile(const FileName: string; smoothMethod: integer): boolean; //smoothMethod is one of kNiftiSmooth...  options
var
   ext, hdrName, imgName: string;
   gzFlag, xDim64 : int64;
   isSwap: boolean;
begin
    (*result := false;
    isSwap := false; //assume native endian
    gzFlag := K_gzBytes_headerAndImageUncompressed;
     if not FileExists(FileName) then exit;
     ext := UpperCase(ExtractFileExt(Filename));
     imgName := Filename;
     hdrName := Filename;
     if (ext = '.GZ') then begin
        hdr := ReadHdrGz(FileName);
        gzFlag := K_gzBytes_headerAndImageCompressed;
     end else if ((ext = '.NII') or (ext = '.HDR') or (ext = '.IMG')) then begin
          if (ext = '.IMG') then
             hdrName := ChangeFileExt(FileName, '.hdr')
          else if ext = '.HDR' then
              imgName := ChangeFileExt(FileName, '.img');
          hdr := ReadHdr(hdrName);
     end else begin
       {$IFDEF FOREIGNVOL}
         if not readForeignHeader (imgName, hdr, gzFlag, isSwap) then exit;
       {$ELSE}
         exit;
       {$ENDIF}
     end;
     if (hdr.HdrSz = SizeOf (TNIFTI2hdr)) then begin
        if (ext = '.GZ') then
           hdr := ReadHdr2Gz(FileName, xDim64)
        else
            hdr := ReadHdr2(FileName, xDim64);

     end;
     if sizeof(TNIfTIhdr) <> hdr.HdrSz then begin
        NIFTIhdr_SwapBytes(hdr);
        if (hdr.HdrSz = SizeOf (TNIFTI2hdr)) then begin
           {$IFDEF ENDIAN_LITTLE}
           showmessage('Unable to read big-endian NIfTI 2 headers.');
           {$ELSE}
           showmessage('Unable to read little-endian NIfTI 2 headers.');
           {$ENDIF}
        end;
        isSwap := true;
     end;

     if sizeof(TNIfTIhdr) <> hdr.HdrSz then
        exit; //not a valid nifti header
     if not FixDataType (hdr) then exit;
     *)
     imgName := FileName;
     if not readVoxHeader (imgName, hdr, gzFlag, isSwap, xDim64) then begin
       printf(format('Unable to load as voxelwise data: %s', [FileName]));
       exit(false);
     end;
     printf(format('voxel dimensions %d %d %d', [hdr.dim[1],hdr.dim[2],hdr.dim[3]]));
     if (hdr.dim[1] < 2) or (hdr.dim[2] < 2) or (hdr.dim[3] < 2) then begin
       printf(format('Not a 3D voxel-based image: %s', [FileName]));
       exit(false);
     end;
     if not ReadImg(imgName, isSwap, gzFlag) then exit(false);
     setMatrix;
     if smoothMethod = kNiftiSmoothMaskZero  then
        SmoothFWHM2VoxIgnoreZeros (img,  hdr.dim[1],hdr.dim[2],hdr.dim[3])
     else if smoothMethod = kNiftiSmooth  then
        SmoothFWHM2Vox (img,  hdr.dim[1],hdr.dim[2],hdr.dim[3]);
     //else kNiftiSmoothNone <- no smoothing
     SetDescriptives;
     result := true;
end;

procedure TNIFTI.SmoothMaskZero;
begin
     if (length(img) < 9) or (hdr.dim[1] < 3) or (hdr.dim[2] < 3) or (hdr.dim[3] < 3) then exit;
     SmoothFWHM2VoxIgnoreZeros (img,  hdr.dim[1],hdr.dim[2],hdr.dim[3]) ;
end;

procedure TNIFTI.Smooth;
begin
  if (length(img) < 9) or (hdr.dim[1] < 3) or (hdr.dim[2] < 3) or (hdr.dim[3] < 3) then exit;
  SmoothFWHM2Vox (img,  hdr.dim[1],hdr.dim[2],hdr.dim[3]);
end;

procedure TNIFTI.Close;
begin
  setlength(img,0);
end;

destructor TNIFTI.Destroy;
begin
  Close;
  inherited;
end;

end.

