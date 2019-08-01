unit nifti_foreign;

interface
{$mode objfpc}{$H+}

uses
{$ifndef isTerminalApp}  dialogs, {$endif}
 nifti_types, define_types, sysutils, classes, StrUtils, zstream;

function readForeignHeader (var lFilename: string; var lHdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean; var xDim64: int64): boolean;
procedure NII_Clear (var lHdr: TNIFTIHdr);
function readMGHHeader (var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean; var xDim64: int64): boolean;
procedure NII_SetIdentityMatrix (var lHdr: TNIFTIHdr); //create neutral rotation matrix

implementation

Type
  mat44 = array [0..3, 0..3] of Single;
  vect4 = array [0..3] of Single;
  mat33 = array [0..2, 0..2] of Single;
  vect3 = array [0..2] of Single;
  ivect3 = array [0..2] of integer;
  ByteRA = array [1..1] of byte;
  Bytep = ^ByteRA;

procedure ShowMsg(s: string);
begin
     Showmessage(s);
end;

procedure fromMatrix (m: mat44; var r11,r12,r13,r21,r22,r23,r31,r32,r33: double);
begin
  r11 := m[0,0];
  r12 := m[0,1];
  r13 := m[0,2];
  r21 := m[1,0];
  r22 := m[1,1];
  r23 := m[1,2];
  r31 := m[2,0];
  r32 := m[2,1];
  r33 := m[2,2];
end;

function Matrix2D (r11,r12,r13,r21,r22,r23,r31,r32,r33: double): mat33;
begin
  result[0,0] := r11;
  result[0,1] := r12;
  result[0,2] := r13;
  result[1,0] := r21;
  result[1,1] := r22;
  result[1,2] := r23;
  result[2,0] := r31;
  result[2,1] := r32;
  result[2,2] := r33;
end;

function nifti_mat33_determ( R: mat33 ):double;   //* determinant of 3x3 matrix */
begin
  result := r[0,0]*r[1,1]*r[2,2]
           -r[0,0]*r[2,1]*r[1,2]
           -r[1,0]*r[0,1]*r[2,2]
           +r[1,0]*r[2,1]*r[0,2]
           +r[2,0]*r[0,1]*r[1,2]
           -r[2,0]*r[1,1]*r[0,2] ;
end;

function nifti_mat33_rownorm( A: mat33 ): single;  // max row norm of 3x3 matrix
var
   r1,r2,r3: single ;
begin
   r1 := abs(A[0,0])+abs(A[0,1])+abs(A[0,2]);
   r2 := abs(A[1,0])+abs(A[1,1])+abs(A[1,2]);
   r3 := abs(A[2,0])+abs(A[2,1])+abs(A[2,2]);
   if( r1 < r2 ) then r1 := r2 ;
   if( r1 < r3 ) then r1 := r3 ;
   result := r1 ;
end;

procedure fromMatrix33 (m: mat33; var r11,r12,r13,r21,r22,r23,r31,r32,r33: double);
begin
  r11 := m[0,0];
  r12 := m[0,1];
  r13 := m[0,2];
  r21 := m[1,0];
  r22 := m[1,1];
  r23 := m[1,2];
  r31 := m[2,0];
  r32 := m[2,1];
  r33 := m[2,2];
end;

function nifti_mat33_inverse( R: mat33 ): mat33;   //* inverse of 3x3 matrix */
var
   r11,r12,r13,r21,r22,r23,r31,r32,r33 , deti: double ;
begin
   FromMatrix33(R,r11,r12,r13,r21,r22,r23,r31,r32,r33);
   deti := r11*r22*r33-r11*r32*r23-r21*r12*r33
         +r21*r32*r13+r31*r12*r23-r31*r22*r13 ;
   if( deti <> 0.0 ) then deti := 1.0 / deti ;
   result[0,0] := deti*( r22*r33-r32*r23) ;
   result[0,1] := deti*(-r12*r33+r32*r13) ;
   result[0,2] := deti*( r12*r23-r22*r13) ;
   result[1,0] := deti*(-r21*r33+r31*r23) ;
   result[1,1] := deti*( r11*r33-r31*r13) ;
   result[1,2] := deti*(-r11*r23+r21*r13) ;
   result[2,0] := deti*( r21*r32-r31*r22) ;
   result[2,1] := deti*(-r11*r32+r31*r12) ;
   result[2,2] := deti*( r11*r22-r21*r12) ;
end;

function nifti_mat33_colnorm( A: mat33 ): single;  //* max column norm of 3x3 matrix */
var
   r1,r2,r3: single ;
begin
   r1 := abs(A[0,0])+abs(A[1,0])+abs(A[2,0]) ;
   r2 := abs(A[0,1])+abs(A[1,1])+abs(A[2,1]) ;
   r3 := abs(A[0,2])+abs(A[1,2])+abs(A[2,2]) ;
   if( r1 < r2 ) then r1 := r2 ;
   if( r1 < r3 ) then r1 := r3 ;
   result := r1 ;
end;

function nifti_mat33_polar( A: mat33 ): mat33;
var
   k:integer;
   X , Y , Z: mat33 ;
   dif,alp,bet,gam,gmi : single;
begin
  dif := 1;
  k := 0;
   X := A ;
   gam := nifti_mat33_determ(X) ;
   while( gam = 0.0 )do begin        //perturb matrix
     gam := 0.00001 * ( 0.001 + nifti_mat33_rownorm(X) ) ;
     X[0,0] := X[0,0]+gam ;
     X[1,1] := X[1,1]+gam ;
     X[2,2] := X[2,2] +gam ;
     gam := nifti_mat33_determ(X) ;
   end;
   while true do begin
     Y := nifti_mat33_inverse(X) ;
     if( dif > 0.3 )then begin     // far from convergence
       alp := sqrt( nifti_mat33_rownorm(X) * nifti_mat33_colnorm(X) ) ;
       bet := sqrt( nifti_mat33_rownorm(Y) * nifti_mat33_colnorm(Y) ) ;
       gam := sqrt( bet / alp ) ;
       gmi := 1.0 / gam ;
     end else begin
       gam := 1.0;
       gmi := 1.0 ;  //close to convergence
     end;
     Z[0,0] := 0.5 * ( gam*X[0,0] + gmi*Y[0,0] ) ;
     Z[0,1] := 0.5 * ( gam*X[0,1] + gmi*Y[1,0] ) ;
     Z[0,2] := 0.5 * ( gam*X[0,2] + gmi*Y[2,0] ) ;
     Z[1,0] := 0.5 * ( gam*X[1,0] + gmi*Y[0,1] ) ;
     Z[1,1] := 0.5 * ( gam*X[1,1] + gmi*Y[1,1] ) ;
     Z[1,2] := 0.5 * ( gam*X[1,2] + gmi*Y[2,1] ) ;
     Z[2,0] := 0.5 * ( gam*X[2,0] + gmi*Y[0,2] ) ;
     Z[2,1] := 0.5 * ( gam*X[2,1] + gmi*Y[1,2] ) ;
     Z[2,2] := 0.5 * ( gam*X[2,2] + gmi*Y[2,2] ) ;
     dif := abs(Z[0,0]-X[0,0])+abs(Z[0,1]-X[0,1])+abs(Z[0,2]-X[0,2])
           +abs(Z[1,0]-X[1,0])+abs(Z[1,1]-X[1,1])+abs(Z[1,2]-X[1,2])
           +abs(Z[2,0]-X[2,0])+abs(Z[2,1]-X[2,1])+abs(Z[2,2]-X[2,2]);
     k := k+1 ;
     if( k > 100) or (dif < 3.e-6 ) then begin
         result := Z;
         break ; //convergence or exhaustion
     end;
     X := Z ;
   end;
   result := Z ;
end;

procedure nifti_mat44_to_quatern( lR :mat44;
                             var qb, qc, qd,
                             qx, qy, qz,
                             dx, dy, dz, qfac : single);
var
   r11,r12,r13 , r21,r22,r23 , r31,r32,r33, xd,yd,zd , a,b,c,d : double;
   P,Q: mat33;  //3x3
begin
   // offset outputs are read write out of input matrix
   qx := lR[0,3];
   qy := lR[1,3];
   qz := lR[2,3];
   //load 3x3 matrix into local variables
   fromMatrix(lR,r11,r12,r13,r21,r22,r23,r31,r32,r33);
   //compute lengths of each column; these determine grid spacings
   xd := sqrt( r11*r11 + r21*r21 + r31*r31 ) ;
   yd := sqrt( r12*r12 + r22*r22 + r32*r32 ) ;
   zd := sqrt( r13*r13 + r23*r23 + r33*r33 ) ;
   //if a column length is zero, patch the trouble
   if( xd = 0.0 )then begin r11 := 1.0 ; r21 := 0; r31 := 0.0 ; xd := 1.0 ; end;
   if( yd = 0.0 )then begin r22 := 1.0 ; r12 := 0; r32 := 0.0 ; yd := 1.0 ; end;
   if( zd = 0.0 )then begin r33 := 1.0 ; r13 := 0; r23 := 0.0 ; zd := 1.0 ; end;
   //assign the output lengths
   dx := xd;
   dy := yd;
   dz := zd;
   //normalize the columns
   r11 := r11/xd ; r21 := r21/xd ; r31 := r31/xd ;
   r12 := r12/yd ; r22 := r22/yd ; r32 := r32/yd ;
   r13 := r13/zd ; r23 := r23/zd ; r33 := r33/zd ;
   { At this point, the matrix has normal columns, but we have to allow
      for the fact that the hideous user may not have given us a matrix
      with orthogonal columns. So, now find the orthogonal matrix closest
      to the current matrix.
      One reason for using the polar decomposition to get this
      orthogonal matrix, rather than just directly orthogonalizing
      the columns, is so that inputting the inverse matrix to R
      will result in the inverse orthogonal matrix at this point.
      If we just orthogonalized the columns, this wouldn't necessarily hold.}
   Q :=  Matrix2D (r11,r12,r13,          // 2D "graphics" matrix
                           r21,r22,r23,
                           r31,r32,r33);
   P := nifti_mat33_polar(Q) ; //P is orthog matrix closest to Q
   FromMatrix33(P,r11,r12,r13,r21,r22,r23,r31,r32,r33);
{                           [ r11 r12 r13 ]
 at this point, the matrix  [ r21 r22 r23 ] is orthogonal
                            [ r31 r32 r33 ]
 compute the determinant to determine if it is proper}

   zd := r11*r22*r33-r11*r32*r23-r21*r12*r33
       +r21*r32*r13+r31*r12*r23-r31*r22*r13 ; //should be -1 or 1

   if( zd > 0 )then begin // proper
     qfac  := 1.0 ;
   end else begin //improper ==> flip 3rd column
     qfac := -1.0 ;
     r13 := -r13 ; r23 := -r23 ; r33 := -r33 ;
   end;
   // now, compute quaternion parameters
   a := r11 + r22 + r33 + 1.0;
   if( a > 0.5 ) then begin  //simplest case
     a := 0.5 * sqrt(a) ;
     b := 0.25 * (r32-r23) / a ;
     c := 0.25 * (r13-r31) / a ;
     d := 0.25 * (r21-r12) / a ;
   end else begin  //trickier case
     xd := 1.0 + r11 - (r22+r33) ;// 4*b*b
     yd := 1.0 + r22 - (r11+r33) ;// 4*c*c
     zd := 1.0 + r33 - (r11+r22) ;// 4*d*d
     if( xd > 1.0 ) then begin
       b := 0.5 * sqrt(xd) ;
       c := 0.25* (r12+r21) / b ;
       d := 0.25* (r13+r31) / b ;
       a := 0.25* (r32-r23) / b ;
     end else if( yd > 1.0 ) then begin
       c := 0.5 * sqrt(yd) ;
       b := 0.25* (r12+r21) / c ;
       d := 0.25* (r23+r32) / c ;
       a := 0.25* (r13-r31) / c ;
     end else begin
       d := 0.5 * sqrt(zd) ;
       b := 0.25* (r13+r31) / d ;
       c := 0.25* (r23+r32) / d ;
       a := 0.25* (r21-r12) / d ;
     end;
     if( a < 0.0 )then begin b:=-b ; c:=-c ; d:=-d; {a:=-a; this is not used} end;
   end;
   qb := b ;
   qc := c ;
   qd := d ;
end;


procedure NII_SetIdentityMatrix (var lHdr: TNIFTIHdr); //create neutral rotation matrix
var lInc: integer;
begin
	with lHdr do begin
		 for lInc := 0 to 3 do
			 srow_x[lInc] := 0;
		 for lInc := 0 to 3 do
             srow_y[lInc] := 0;
         for lInc := 0 to 3 do
             srow_z[lInc] := 0;
         for lInc := 1 to 16 do
             intent_name[lInc] := chr(0);
         //next: create identity matrix: if code is switched on there will not be a problem
		 srow_x[0] := 1;
         srow_y[1] := 1;
         srow_z[2] := 1;
    end;
end; //proc NIFTIhdr_IdentityMatrix

procedure NII_Clear (var lHdr: TNIFTIHdr);
var
 lInc: integer;
begin
  with lHdr do begin
    HdrSz := sizeof(TNIFTIhdr);
    for lInc := 1 to 10 do
       Data_Type[lInc] := chr(0);
    for lInc := 1 to 18 do
       db_name[lInc] := chr(0);
    extents:=0;
    session_error:= 0;
    regular:='r'{chr(0)};
    dim_info:=(0);
    dim[0] := 4;
    for lInc := 1 to 7 do
       dim[lInc] := 0;
    intent_p1 := 0;
    intent_p2 := 0;
    intent_p3 := 0;
    intent_code:=0;
    datatype:=0 ;
    bitpix:=0;
    slice_start:=0;
    for lInc := 1 to 7 do
       pixdim[linc]:= 1.0;
    vox_offset:= 0.0;
    scl_slope := 1.0;
    scl_inter:= 0.0;
    slice_end:= 0;
    slice_code := 0;
    xyzt_units := 10;
    cal_max:= 0.0;
    cal_min:= 0.0;
    slice_duration:=0;
    toffset:= 0;
    glmax:= 0;
    glmin:= 0;
    for lInc := 1 to 80 do
      descrip[lInc] := chr(0);{80 spaces}
    for lInc := 1 to 24 do
      aux_file[lInc] := chr(0);{80 spaces}
    {below are standard settings which are not 0}
    bitpix := 16;//vc16; {8bits per pixel, e.g. unsigned char 136}
    DataType := 4;//vc4;{2=unsigned char, 4=16bit int 136}
    Dim[0] := 3;
    Dim[1] := 256;
    Dim[2] := 256;
    Dim[3] := 128;
    Dim[4] := 1; {n vols}
    Dim[5] := 1;
    Dim[6] := 1;
    Dim[7] := 1;
    glMin := 0;
    glMax := 255;
    qform_code := kNIFTI_XFORM_UNKNOWN;
    sform_code:= kNIFTI_XFORM_UNKNOWN;
    quatern_b := 0;
    quatern_c := 0;
    quatern_d := 0;
    qoffset_x := 0;
    qoffset_y := 0;
    qoffset_z := 0;
    NII_SetIdentityMatrix(lHdr);
    magic := kNIFTI_MAGIC_SEPARATE_HDR;
  end; //with the NIfTI header...
end;


procedure ZERO_MAT44(var m: mat44); //note sets m[3,3] to one
var
  i,j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      m[i,j] := 0.0;
  m[3,3] := 1;
end;

procedure LOAD_MAT33(var m: mat33; m00,m01,m02, m10,m11,m12, m20,m21,m22: single);
begin
  m[0,0] := m00;
  m[0,1] := m01;
  m[0,2] := m02;
  m[1,0] := m10;
  m[1,1] := m11;
  m[1,2] := m12;
  m[2,0] := m20;
  m[2,1] := m21;
  m[2,2] := m22;
end;

function nifti_mat33_mul( A,B: mat33): mat33;
var
  i,j: integer;
begin
	for i:=0 to 3 do
    	for j:=0 to 3 do
        result[i,j] :=  A[i,0] * B[0,j]
            + A[i,1] * B[1,j]
            + A[i,2] * B[2,j] ;
end;


procedure LOAD_MAT44(var m: mat44; m00,m01,m02,m03, m10,m11,m12,m13, m20,m21,m22,m23: single);
begin
  m[0,0] := m00;
  m[0,1] := m01;
  m[0,2] := m02;
  m[0,3] := m03;
  m[1,0] := m10;
  m[1,1] := m11;
  m[1,2] := m12;
  m[1,3] := m13;
  m[2,0] := m20;
  m[2,1] := m21;
  m[2,2] := m22;
  m[2,3] := m23;
  m[3,0] := 0.0;
  m[3,1] := 0.0;
  m[3,2] := 0.0;
  m[3,3] := 1.0;
end;

function validMatrix(var m: mat44): boolean;
var
  i: integer;
begin
     result := false;
     for i := 0 to 2 do begin
         if (m[0,i] = 0.0) and (m[1,i] = 0.0) and (m[2,i] = 0.0) then exit;
         if (m[i,0] = 0.0) and (m[i,1] = 0.0) and (m[i,2] = 0.0) then exit;
     end;
     result := true;
end;

procedure convertForeignToNifti(var nhdr: TNIFTIhdr);
var
  i,nonSpatialMult: integer;
  qto_xyz: mat44;
   //dumqx, dumqy, dumqz,
     dumdx, dumdy, dumdz: single;
begin
  nhdr.HdrSz := 348; //used to signify header does not need to be byte-swapped
	nhdr.magic:=kNIFTI_MAGIC_EMBEDDED_HDR;
	if (nhdr.dim[3] = 0) then nhdr.dim[3] := 1; //for 2D images the 3rd dim is not specified and set to zero
	nhdr.dim[0] := 3; //for 2D images the 3rd dim is not specified and set to zero
  nonSpatialMult := 1;
  for i := 4 to 7 do
    if nhdr.dim[i] > 0 then
      nonSpatialMult := nonSpatialMult * nhdr.dim[i];
	if (nonSpatialMult > 1) then begin
    nhdr.dim[0] := 4;
    nhdr.dim[4] := nonSpatialMult;
    for i := 5 to 7 do
      nhdr.dim[i] := 0;
  end;
  nhdr.bitpix := 8;
  if (nhdr.datatype = 4) or (nhdr.datatype = 512) then nhdr.bitpix := 16;
  if (nhdr.datatype = 8) or (nhdr.datatype = 16) or (nhdr.datatype = 768) then nhdr.bitpix := 32;
  if (nhdr.datatype = 32) or (nhdr.datatype = 64) or (nhdr.datatype = 1024) or (nhdr.datatype = 1280) then nhdr.bitpix := 64;
  LOAD_MAT44(qto_xyz, nhdr.srow_x[0], nhdr.srow_x[1], nhdr.srow_x[2], nhdr.srow_x[3],
              nhdr.srow_y[0], nhdr.srow_y[1], nhdr.srow_y[2], nhdr.srow_y[3],
              nhdr.srow_z[0], nhdr.srow_z[1], nhdr.srow_z[2], nhdr.srow_z[3]);
  if not validMatrix(qto_xyz) then begin
     nhdr.sform_code := 0;
     nhdr.qform_code :=  0;
     for i := 0 to 3 do begin
         nhdr.srow_x[i] := 0;
         nhdr.srow_y[i] := 0;
         nhdr.srow_z[i] := 0;
     end;
     nhdr.srow_x[0] := 1;
     nhdr.srow_y[1] := 1;
     nhdr.srow_z[2] := 1;
     exit;
  end;
  nhdr.sform_code := 1;
  nifti_mat44_to_quatern( qto_xyz , nhdr.quatern_b, nhdr.quatern_c, nhdr.quatern_d,nhdr.qoffset_x,nhdr.qoffset_y,nhdr.qoffset_z, dumdx, dumdy, dumdz,nhdr.pixdim[0]) ;
  nhdr.qform_code := kNIFTI_XFORM_SCANNER_ANAT;
end;

procedure NSLog( str: string);
begin
  showmsg(str);
end;

function parsePicString(s: string): single;
//given "AXIS_4 001 0.000000e+00 4.000000e-01 microns"  return 0.4
var
  sList : TStringList;
begin
  result := 0.0;
  DecimalSeparator := '.';
  sList := TStringList.Create;
  sList.Delimiter := ' ';        // Each list item will be blank separated
  sList.DelimitedText := s;
  if sList.Count > 4 then begin
     //ShowMessage(sList[3]);
     try
        result := StrToFloat(sList[3]);    // Middle blanks are not supported
     except
           //ShowMessage(Exception.Message);
     end;
  end;
  sList.Free;
end;

function nii_readpic (var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean; var xDim64: int64): boolean;
const
     kBIORAD_HEADER_SIZE  = 76;
     kBIORAD_NOTE_HEADER_SIZE = 16;
     kBIORAD_NOTE_SIZE = 80;
Type
  Tbiorad_header = packed record //Next: PIC Format Header structure
        nx, ny : word;    //  0   2*2     image width and height in pixels
        npic: SmallInt;               //  4   2       number of images in file
        ramp1_min: SmallInt;          //  6   2*2     LUT1 ramp min. and max.
        ramp1_max: SmallInt;
        notes: LongInt;                // 10   4       no notes=0; has notes=non zero
        byte_format: SmallInt;        // 14   2       bytes=TRUE(1); words=FALSE(0)
        n : word;         // 16   2       image number within file
        name: array [1..32] of char;            // 18   32      file name
        merged: SmallInt;             // 50   2       merged format
        color1 : word;    // 52   2       LUT1 color status
        file_id : word;   // 54   2       valid .PIC file=12345
        ramp2_min: SmallInt;          // 56   2*2     LUT2 ramp min. and max.
        ramp2_max: SmallInt;
        color2: word;    // 60   2       LUT2 color status
        edited: SmallInt;             // 62   2       image has been edited=TRUE(1)
        lens: SmallInt;               // 64   2       Integer part of lens magnification
        mag_factor: single;         // 66   4       4 byte real mag. factor (old ver.)
        dummy1, dummy2, dummy3: word;  // 70   6       NOT USED (old ver.=real lens mag.)
     end; // biorad_header;
    Tbiorad_note_header = packed record
      blank: SmallInt;		// 0	2
      note_flag: LongInt;		// 2	4
      blank2: LongInt;			// 6	4
      note_type: SmallInt;	// 10	2
      blank3: LongInt;			// 12	4
      note: array[1..kBIORAD_NOTE_SIZE] of char;
    end;//biorad_note_header;
var
   bhdr : Tbiorad_header;
   nh: Tbiorad_note_header;
   lHdrFile: file;
   //s: string;
   i, bytesHdrImg, nNotes: integer;
begin
  result := false;
  FileMode := fmOpenRead;
  {$I-}
  AssignFile(lHdrFile, fname);
  FileMode := 0;  //Set file access to read only
  Reset(lHdrFile, 1);
  {$I+}
  if ioresult <> 0 then begin
        NSLog('Error in reading BioRad PIC header.'+inttostr(IOResult));
        FileMode := 2;
        exit;
  end;
  BlockRead(lHdrFile, bhdr, sizeof(Tbiorad_header));
  if (bhdr.file_id <> 12345) then begin //signature not found!
    CloseFile(lHdrFile);
    NSLog('Error in reading BioRad PIC header file ID not 12345.');
    exit;
  end;
  nhdr.dim[0]:=3;//3D
  nhdr.dim[1]:=bhdr.nx;
  xDim64:=bhdr.nx;
  nhdr.dim[2]:=bhdr.ny;
  nhdr.dim[3]:=bhdr.npic;
  nhdr.dim[4]:=1;
  nhdr.pixdim[1]:=1.0;
  nhdr.pixdim[2]:=1.0;
  nhdr.pixdim[3]:=1.0;
  if (bhdr.byte_format = 1) then
      nhdr.datatype := kDT_UINT8 // 2
  else
      nhdr.datatype := kDT_UINT16;
  nhdr.vox_offset := kBIORAD_HEADER_SIZE;
  bytesHdrImg := sizeof(Tbiorad_header)+bhdr.nx*bhdr.ny*bhdr.npic*bhdr.byte_format;
  nNotes := (Filesize(lHdrFile) - bytesHdrImg) div (kBIORAD_NOTE_HEADER_SIZE+kBIORAD_NOTE_SIZE);
  if (nNotes > 0) then begin
     seek(lHdrFile, bytesHdrImg);
     for i := 1 to nNotes do begin
         BlockRead(lHdrFile, nh, sizeof(Tbiorad_note_header));
         if(nh.note_type=1) then continue; // These are not interesting notes
         if AnsiStartsStr('AXIS_2 ', nh.note) then
             nhdr.pixdim[1]  := parsePicString(nh.note);
         if AnsiStartsStr('AXIS_3 ', nh.note) then
             nhdr.pixdim[2]  := parsePicString(nh.note);
         if AnsiStartsStr('AXIS_4 ', nh.note) then
             nhdr.pixdim[3]  := parsePicString(nh.note);
     end;
  end;
  CloseFile(lHdrFile);
  nhdr.sform_code := 1;
  nhdr.srow_x[0]:=nhdr.pixdim[1];nhdr.srow_x[1]:=0.0;nhdr.srow_x[2]:=0.0;nhdr.srow_x[3]:=0.0;
  nhdr.srow_y[0]:=0.0;nhdr.srow_y[1]:=nhdr.pixdim[2];nhdr.srow_y[2]:=0.0;nhdr.srow_y[3]:=0.0;
  nhdr.srow_z[0]:=0.0;nhdr.srow_z[1]:=0.0;nhdr.srow_z[2]:=-nhdr.pixdim[3];nhdr.srow_z[3]:=0.0;
  convertForeignToNifti(nhdr);
  result := true;
end;

//UnGZip(fname,lBuff,0,sizeof(Tmgh));
procedure UnGZip(const FileName: string; buffer: bytep; offset, sz: integer);
var
   decomp: TGZFileStream;
   skip: array of byte;
begin
     decomp := TGZFileStream.create(FileName, gzopenread);
     if offset > 0 then begin
        setlength(skip, offset);
        decomp.Read(skip[0], offset);
     end;
     decomp.Read(buffer[0], sz); //must have mode set correctly
     decomp.free;
end;

function readMGHHeader (var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean; var xDim64: int64): boolean;
Type
  Tmgh = packed record //Next: MGH Format Header structure
   version, width,height,depth,nframes,mtype,dof : longint;
   goodRASFlag: smallint;
   spacingX,spacingY,spacingZ,xr,xa,xs,yr,ya,ys,zr,za,zs,cr,ca,cs: single;
  end;
var
  mgh: Tmgh;
  lBuff: Bytep;
  lExt: string;
  lHdrFile: file;
  PxyzOffset, Pcrs: vect4;
  i,j: integer;
  base: single;
  m: mat44;
begin
  result := false;
  FileMode := fmOpenRead;
  lExt := ExtractFileExtGzUpper(fname);
  if (lExt = '.MGZ') then begin
          lBuff := @mgh;
	  UnGZip(fname,lBuff,0,sizeof(Tmgh)); //1388
    gzBytes := K_gzBytes_headerAndImageCompressed;
  end else begin //if MGZ, else assume uncompressed MGH
     gzBytes := 0;
	   {$I-}
	   AssignFile(lHdrFile, fname);
	   FileMode := 0;  //Set file access to read only
	   Reset(lHdrFile, 1);
	   {$I+}
	   if ioresult <> 0 then begin
		  NSLog('Error in reading MGH header.'+inttostr(IOResult));
		  FileMode := 2;
		  exit;
	   end;
	   BlockRead(lHdrFile, mgh, sizeof(Tmgh));
	   CloseFile(lHdrFile);
  end;
  {$IFDEF ENDIAN_BIG} //data always stored big endian
    swapEndian := false;
  {$ELSE}
    swapEndian := true;
    SwapLongInt(mgh.version);
    SwapLongInt(mgh.width);
    SwapLongInt(mgh.height);
    SwapLongInt(mgh.depth);
    SwapLongInt(mgh.nframes);
    SwapLongInt(mgh.mtype);
    SwapLongInt(mgh.dof);
    mgh.goodRASFlag := swap(mgh.goodRASFlag);
    SwapSingle(mgh.spacingX);
    SwapSingle(mgh.spacingY);
    SwapSingle(mgh.spacingZ);
    SwapSingle(mgh.xr);
    SwapSingle(mgh.xa);
    SwapSingle(mgh.xs);
    SwapSingle(mgh.yr);
    SwapSingle(mgh.ya);
    SwapSingle(mgh.ys);
    SwapSingle(mgh.zr);
    SwapSingle(mgh.za);
    SwapSingle(mgh.zs);
    SwapSingle(mgh.cr);
    SwapSingle(mgh.ca);
    SwapSingle(mgh.cs);
  {$ENDIF}
  if ((mgh.version <> 1) or (mgh.mtype < 0) or (mgh.mtype > 4)) then begin
        NSLog(format('Error: first value in a MGH header should be 1 (got %d) and data type should be in the range 1..4. (got %d)', [mgh.version, mgh.mtype] ));
        exit;
  end;
  if (mgh.mtype = 0) then
        nhdr.datatype := kDT_UINT8
  else if (mgh.mtype = 4)  then
        nhdr.datatype := kDT_INT16
  else if (mgh.mtype = 1)  then
        nhdr.datatype := kDT_INT32
  else if (mgh.mtype = 3)  then
        nhdr.datatype := kDT_FLOAT32;
  xDim64 := mgh.Width;
  nhdr.dim[1]:=mgh.width;
  nhdr.dim[2]:=mgh.height;
  nhdr.dim[3]:=mgh.depth;
	nhdr.dim[4]:=mgh.nframes;
	nhdr.pixdim[1]:=mgh.spacingX;
	nhdr.pixdim[2]:=mgh.spacingY;
	nhdr.pixdim[3]:=mgh.spacingZ;
	nhdr.vox_offset := 284;
	nhdr.sform_code := 1;
	//convert MGH to NIfTI transform see Bruce Fischl mri.c MRIxfmCRS2XYZ https://github.com/neurodebian/freesurfer/blob/master/utils/mri.c
	LOAD_MAT44(m,mgh.xr*nhdr.pixdim[1],mgh.yr*nhdr.pixdim[2],mgh.zr*nhdr.pixdim[3],0,
               mgh.xa*nhdr.pixdim[1],mgh.ya*nhdr.pixdim[2],mgh.za*nhdr.pixdim[3],0,
			         mgh.xs*nhdr.pixdim[1],mgh.ys*nhdr.pixdim[2],mgh.zs*nhdr.pixdim[3],0);
  base := 0.0; //0 or 1: are voxels indexed from 0 or 1?
	Pcrs[0] := (nhdr.dim[1]/2.0)+base;
	Pcrs[1] := (nhdr.dim[2]/2.0)+base;
	Pcrs[2] := (nhdr.dim[3]/2.0)+base;
	Pcrs[3] := 1;
	for i:=0 to 3 do begin //multiply Pcrs * m
		PxyzOffset[i] := 0;
		for j := 0 to 3 do
			PxyzOffset[i] := PxyzOffset[i]+ (m[i,j]*Pcrs[j]);
	end;
  nhdr.srow_x[0]:=m[0,0]; nhdr.srow_x[1]:=m[0,1]; nhdr.srow_x[2]:=m[0,2]; nhdr.srow_x[3]:=mgh.cr - PxyzOffset[0];
	nhdr.srow_y[0]:=m[1,0]; nhdr.srow_y[1]:=m[1,1]; nhdr.srow_y[2]:=m[1,2]; nhdr.srow_y[3]:=mgh.ca - PxyzOffset[1];
	nhdr.srow_z[0]:=m[2,0]; nhdr.srow_z[1]:=m[2,1]; nhdr.srow_z[2]:=m[2,2]; nhdr.srow_z[3]:=mgh.cs - PxyzOffset[2];
	convertForeignToNifti(nhdr);
  result := true;
end;

procedure splitStr(delimiter: char; str: string; mArray: TStrings);
begin
  mArray.Clear;
  mArray.Delimiter := delimiter;
  mArray.DelimitedText := str;
end;

procedure splitStrStrict(delimiter: char; S: string; sl: TStrings);
begin
  sl.Clear;
  sl.Delimiter := delimiter;
  sl.DelimitedText := '"' + StringReplace(S, sl.Delimiter, '"' + sl.Delimiter + '"', [rfReplaceAll]) + '"';
end;

function cleanStr (S:string): string; // "(12.31)" ->"12.31"
begin
  result := StringReplace(S, '(', '', [rfReplaceAll]);
  result := StringReplace(result, ')', '', [rfReplaceAll]);
end;

(*function FSize (lFName: String): Int64;
var SearchRec: TSearchRec;
begin
  result := 0;
  if not fileexistsex(lFName) then exit;
  FindFirst(lFName, faAnyFile, SearchRec);
  result := SearchRec.size;
  FindClose(SearchRec);
end; *)

(*procedure report_mat(m: mat33);
begin
     showmsg('mat = ['+floattostr(m[0,0])+' ' +floattostr(m[0,1])  +' ' +floattostr(m[0,2]) +'; '
                  +floattostr(m[1,0])+' ' +floattostr(m[1,1])  +' ' +floattostr(m[1,2]) +'; '
                  +floattostr(m[2,0])+' ' +floattostr(m[2,1])  +' ' +floattostr(m[2,2]) +'] ');
end;*)

type TFByte =  File of Byte;
  function ReadLnBin(var f: TFByte; var s: string; skipEmptyLines: boolean = false): boolean;
  const
    kEOLN = $0A;
    kCR = $0D;
  var
     bt : Byte;
  begin
       s := '';
       if EOF(f) then exit(false);
       while (not  EOF(f)) do begin
             Read(f,bt);
             if (bt = kCR) then continue;
             if (bt = kEOLN) and ((not skipEmptyLines) or (s <> '' )) then exit(true);
             if (bt = kEOLN) then continue;
             s := s + Chr(bt);
       end;
       exit(true);
  end;

(*function readVTKHeader (var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean; var xDim64: int64): boolean;
//VTK Simple Legacy Formats : STRUCTURED_POINTS : BINARY
// http://daac.hpc.mil/gettingStarted/VTK_DataFormats.html
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bvtk/readVtk.m
// http://www.ifb.ethz.ch/education/statisticalphysics/file-formats.pdf
// ftp://ftp.tuwien.ac.at/visual/vtk/www/FileFormats.pdf
//  "The VTK data files described here are written in big endian form"
label
   666;
var
   f: TFByte;//TextFile;
   strlst: TStringList;
   str: string;
   i, num_vox: integer;
   ok: boolean;
begin
  gzBytes := 0;
  {$IFDEF ENDIAN_BIG}
  swapEndian := false;
  {$ELSE}
  swapEndian := true;
  {$ENDIF}
  result := false;
  strlst:=TStringList.Create;
  FileMode := fmOpenRead;
  AssignFile(f, fname);
  {$IFDEF FPC} Reset(f,1); {$ELSE} Reset(f); {$ENDIF}
  ReadLnBin(f, str); //signature: '# vtk DataFile'
  if pos('VTK', UpperCase(str)) <> 3 then begin
    showmessage('Not a VTK file');
    goto 666;
  end;
  ReadLnBin(f, str); //comment: 'Comment: created with MRIcroS'
  ReadLnBin(f, str, true); //kind: 'BINARY' or 'ASCII'
  if pos('BINARY', UpperCase(str)) < 1 then begin  // '# vtk DataFile'
     showmessage('Only able to read binary VTK file: "'+str+'"');
     goto 666;
  end;
  ReadLnBin(f, str, true); // kind, e.g. "DATASET POLYDATA" or "DATASET STRUCTURED_ POINTS"
  if pos('STRUCTURED_POINTS', UpperCase(str)) = 0 then begin
    showmessage('Only able to read VTK images saved as STRUCTURED_POINTS, not '+ str);
    goto 666;
  end;
  ok := true;
  while (ok) and (pos('POINT_DATA', UpperCase(str)) = 0) do begin
    ok := ReadLnBin(f, str, true);
    strlst.DelimitedText := str;
    if pos('DIMENSIONS', UpperCase(str)) <> 0 then begin //e.g. "DIMENSIONS 128 128 128"
       nhdr.dim[1] := StrToIntDef(strlst[1],1);
       xDim64 := StrToIntDef(strlst[1],1);
       nhdr.dim[2] := StrToIntDef(strlst[2],1);
       nhdr.dim[3] := StrToIntDef(strlst[3],1);
    end; //dimensions
    if (pos('ASPECT_RATIO', UpperCase(str)) <> 0) or (pos('SPACING', UpperCase(str)) <> 0) then begin //e.g. "ASPECT_RATIO 1.886 1.886 1.913"
      nhdr.pixdim[1] := StrToFloatDef(strlst[1],1);
      nhdr.pixdim[2] := StrToFloatDef(strlst[2],1);
      nhdr.pixdim[3] := StrToFloatDef(strlst[3],1);
      //showmessage(format('%g %g %g',[nhdr.pixdim[1], nhdr.pixdim[2], nhdr.pixdim[3] ]));
    end; //aspect ratio
    if (pos('ORIGIN', UpperCase(str)) <> 0) then begin //e.g. "ASPECT_RATIO 1.886 1.886 1.913"
      nhdr.srow_x[3] := -StrToFloatDef(strlst[1],1);
      nhdr.srow_y[3] := -StrToFloatDef(strlst[2],1);
      nhdr.srow_z[3] := -StrToFloatDef(strlst[3],1);
      //showmessage(format('%g %g %g',[nhdr.pixdim[1], nhdr.pixdim[2], nhdr.pixdim[3] ]));
    end; //aspect ratio
  end; //not POINT_DATA
  if pos('POINT_DATA', UpperCase(str)) = 0 then goto 666;
  num_vox :=  StrToIntDef(strlst[1],0);
  if num_vox <> (nhdr.dim[1] * nhdr.dim[2] * nhdr.dim[3]) then begin
     showmessage(format('Expected POINT_DATA to equal %dx%dx%d',[nhdr.dim[1], nhdr.dim[2], nhdr.dim[3] ]));
     goto 666;
  end;
  ReadLnBin(f, str, true);
  if pos('SCALARS', UpperCase(str)) = 0 then goto 666; //"SCALARS scalars unsigned_char"
  strlst.DelimitedText := str;
  str := UpperCase(strlst[2]);
  //dataType is one of the types bit, unsigned_char, char, unsigned_short, short, unsigned_int, int, unsigned_long, long, float, or double
  if pos('UNSIGNED_CHAR', str) <> 0 then
      nhdr.datatype := kDT_UINT8 //
  else if pos('SHORT', str) <> 0 then
       nhdr.datatype := kDT_INT16 //
  else if pos('UNSIGNED_SHORT', str) <> 0 then
       nhdr.datatype := kDT_UINT16 //
  else if pos('INT', str) <> 0 then
       nhdr.datatype := kDT_INT32 //
  else  if pos('FLOAT', str) <> 0 then
      nhdr.datatype := kDT_FLOAT
  else  if pos('DOUBLE', str) <> 0 then
      nhdr.datatype := kDT_DOUBLE
  else begin
        showmessage('Unknown VTK scalars type '+str);
        goto 666;
  end;
  convertForeignToNifti(nhdr);
  //showmessage(inttostr(nhdr.datatype));
  ReadLnBin(f, str, true);
  if pos('LOOKUP_TABLE', UpperCase(str)) = 0 then goto 666; //"LOOKUP_TABLE default"
  nhdr.vox_offset := filepos(f);
  //fill matrix
  for i := 0 to 2 do begin
    nhdr.srow_x[i] := 0;
    nhdr.srow_y[i] := 0;
    nhdr.srow_z[i] := 0;
  end;
  nhdr.srow_x[0] := nhdr.pixdim[1];
  nhdr.srow_y[1] := nhdr.pixdim[2];
  nhdr.srow_z[2] := nhdr.pixdim[3];
  //showmessage('xx' +inttostr( filepos(f) ));
  result := true;
  666:
  closefile(f);
  strlst.Free;
end;*)
function readVTKHeader (var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean; var xDim64: int64): boolean;
//VTK Simple Legacy Formats : STRUCTURED_POINTS : BINARY
// http://daac.hpc.mil/gettingStarted/VTK_DataFormats.html
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bvtk/readVtk.m
// http://www.ifb.ethz.ch/education/statisticalphysics/file-formats.pdf
// ftp://ftp.tuwien.ac.at/visual/vtk/www/FileFormats.pdf
//  "The VTK data files described here are written in big endian form"
//Some VTK datasets seem to insert empty lines (e.g. 0x0A0A instead of 0x0A: "ironProt.vtk"
//   https://www.aliza-dicom-viewer.com/download/datasets
//   https://vtk.org/vtk-textbook-examples-and-data/
label
   666;
var
   f: TFByte;//TextFile;
   strlst: TStringList;
   str: string;
   i, num_vox: integer;
   ok: boolean;
begin
  gzBytes := 0;
  xDim64 := 0;
  {$IFDEF ENDIAN_BIG}
  swapEndian := false;
  {$ELSE}
  swapEndian := true;
  {$ENDIF}
  result := false;
  strlst:=TStringList.Create;
  AssignFile(f, fname);
  FileMode := fmOpenRead;
  {$IFDEF FPC} Reset(f,1); {$ELSE} Reset(f); {$ENDIF}
  ReadLnBin(f, str); //signature: '# vtk DataFile'
  if pos('VTK', UpperCase(str)) <> 3 then begin
    showmessage('Not a VTK file');
    goto 666;
  end;
  ReadLnBin(f, str); //comment: 'Comment: created with MRIcroS'
  showmessage(str);
  ReadLnBin(f, str, true); //kind: 'BINARY' or 'ASCII'
  showmessage(str);
  if pos('BINARY', UpperCase(str)) < 1 then begin  // '# vtk DataFile'
     showmessage('Only able to read binary VTK files, not "'+str+'"');
     goto 666;
  end;
  ReadLnBin(f, str, true); // kind, e.g. "DATASET POLYDATA" or "DATASET STRUCTURED_ POINTS"
  if pos('STRUCTURED_POINTS', UpperCase(str)) = 0 then begin
    showmessage('Only able to read VTK images saved as STRUCTURED_POINTS, not '+ str);
    goto 666;
  end;
  //while (str <> '') and (pos('POINT_DATA', UpperCase(str)) = 0) do begin
  ok := true;
  while (ok) and (pos('POINT_DATA', UpperCase(str)) = 0) do begin
    ok := ReadLnBin(f, str, true);
    strlst.DelimitedText := str;
    if pos('DIMENSIONS', UpperCase(str)) <> 0 then begin //e.g. "DIMENSIONS 128 128 128"
       nhdr.dim[1] := StrToIntDef(strlst[1],1);
       xDim64 := StrToIntDef(strlst[1],1);
       nhdr.dim[2] := StrToIntDef(strlst[2],1);
       nhdr.dim[3] := StrToIntDef(strlst[3],1);
    end; //dimensions
    if (pos('ASPECT_RATIO', UpperCase(str)) <> 0) or (pos('SPACING', UpperCase(str)) <> 0) then begin //e.g. "ASPECT_RATIO 1.886 1.886 1.913"
      nhdr.pixdim[1] := StrToFloatDef(strlst[1],1);
      nhdr.pixdim[2] := StrToFloatDef(strlst[2],1);
      nhdr.pixdim[3] := StrToFloatDef(strlst[3],1);
      //showmessage(format('%g %g %g',[nhdr.pixdim[1], nhdr.pixdim[2], nhdr.pixdim[3] ]));
    end; //aspect ratio
    if (pos('ORIGIN', UpperCase(str)) <> 0) then begin //e.g. "ASPECT_RATIO 1.886 1.886 1.913"
      nhdr.srow_x[3] := -StrToFloatDef(strlst[1],1);
      nhdr.srow_y[3] := -StrToFloatDef(strlst[2],1);
      nhdr.srow_z[3] := -StrToFloatDef(strlst[3],1);
      //showmessage(format('%g %g %g',[nhdr.pixdim[1], nhdr.pixdim[2], nhdr.pixdim[3] ]));
    end; //aspect ratio
  end; //not POINT_DATA
  if pos('POINT_DATA', UpperCase(str)) = 0 then goto 666;
  num_vox :=  StrToIntDef(strlst[1],0);
  if num_vox <> (nhdr.dim[1] * nhdr.dim[2] * nhdr.dim[3]) then begin
     showmessage(format('Expected POINT_DATA to equal %dx%dx%d',[nhdr.dim[1], nhdr.dim[2], nhdr.dim[3] ]));
     goto 666;
  end;
  ReadLnBin(f, str, true);
  if pos('SCALARS', UpperCase(str)) = 0 then goto 666; //"SCALARS scalars unsigned_char"
  strlst.DelimitedText := str;
  str := UpperCase(strlst[2]);
  //dataType is one of the types bit, unsigned_char, char, unsigned_short, short, unsigned_int, int, unsigned_long, long, float, or double
  if pos('UNSIGNED_CHAR', str) <> 0 then
      nhdr.datatype := kDT_UINT8 //
  else if pos('SHORT', str) <> 0 then
       nhdr.datatype := kDT_INT16 //
  else if pos('UNSIGNED_SHORT', str) <> 0 then
       nhdr.datatype := kDT_UINT16 //
  else if pos('INT', str) <> 0 then
       nhdr.datatype := kDT_INT32 //
  else  if pos('FLOAT', str) <> 0 then
      nhdr.datatype := kDT_FLOAT
  else  if pos('DOUBLE', str) <> 0 then
      nhdr.datatype := kDT_DOUBLE
  else begin
        showmessage('Unknown VTK scalars type '+str);
        goto 666;
  end;
  convertForeignToNifti(nhdr);
  //showmessage(inttostr(nhdr.datatype));
  ReadLnBin(f, str);
  if pos('LOOKUP_TABLE', UpperCase(str)) = 0 then goto 666; //"LOOKUP_TABLE default"
  nhdr.vox_offset := filepos(f);
  //fill matrix
  for i := 0 to 2 do begin
    nhdr.srow_x[i] := 0;
    nhdr.srow_y[i] := 0;
    nhdr.srow_z[i] := 0;
  end;
  nhdr.srow_x[0] := nhdr.pixdim[1];
  nhdr.srow_y[1] := nhdr.pixdim[2];
  nhdr.srow_z[2] := nhdr.pixdim[3];
  //showmessage('xx' +inttostr( filepos(f) ));
  result := true;
  666:
  closefile(f);
  strlst.Free;
end;

function readMHAHeader (var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean; var xDim64: int64): boolean;
//Read VTK "MetaIO" format image
//http://www.itk.org/Wiki/ITK/MetaIO/Documentation#Reading_a_Brick-of-Bytes_.28an_N-Dimensional_volume_in_a_single_file.29
//https://www.assembla.com/spaces/plus/wiki/Sequence_metafile_format
//http://itk-insight-users.2283740.n2.nabble.com/MHA-MHD-File-Format-td7585031.html
var
  FP: TextFile;
  str, tagName, elementNames, pth, nam, ext: string;
  ch: char;
  isLocal,compressedData: boolean;
  matOrient, mat, d, t: mat33;
  //compressedDataSize,
  nPosition, nOffset, matElements, matElementsOrient,  headerSize, nItems, nBytes, i, channels, fileposBytes: longint;
  offset,position, elementSize: array [0..3] of single;
  transformMatrix: array [0..11] of single;
  mArray: TStringList;
begin
  result := false;
  if not FileExists(fname) then exit;
    {$IFDEF FPC}
  DefaultFormatSettings.DecimalSeparator := '.' ;
   // DecimalSeparator := '.';
  {$ELSE}
  DecimalSeparator := '.';
  {$ENDIF}
  for i := 0 to 3 do begin
      position[i] := 0;
      offset[i] := 0;
      elementSize[i] := 1;
  end;
  nPosition := 0;
  nOffset := 0;
  gzBytes := 0;
  fileposBytes := 0;
  //compressedDataSize := 0;
  swapEndian := false;
  isLocal := true; //image and header embedded in same file, if false detached image
  headerSize := 0;
  matElements := 0;
  matElementsOrient := 0;
  compressedData := false;
  mArray := TStringList.Create;
  Filemode := fmOpenRead;
  AssignFile(fp,fname);
  reset(fp);
  while not EOF(fp) do begin
    str := '';
    while not EOF(fp) do begin
      read(fp,ch);
      inc(fileposBytes);
      if (ch = chr($0D)) or (ch = chr($0A)) then break;
      str := str+ch;
    end;
    if (length(str) < 1) or (str[1]='#') then continue;
    splitstrStrict('=',str,mArray);
    if (mArray.count < 2) then continue;
    tagName := cleanStr(mArray[0]);
    elementNames := mArray[1];
    splitstr(',',elementNames,mArray);
    nItems :=mArray.count;
    if (nItems < 1) then continue;
    for i := 0 to (nItems-1) do
      mArray[i] := cleanStr(mArray[i]); //remove '(' and ')',
    if AnsiContainsText(tagName, 'ObjectType') and (not AnsiContainsText(mArray.Strings[0], 'Image')) then begin
        NSLog('Expecting file with tag "ObjectType = Image" instead of "ObjectType = '+mArray.Strings[0]+'"');

    end {else if AnsiContainsText(tagName, 'NDims') then begin
            nDims := strtoint(mArray[0]);
            if (nDims > 4) then begin
                NSLog('Warning: only reading first 4 dimensions');
                nDims := 4;
            end;
    end} else if AnsiContainsText(tagName, 'BinaryDataByteOrderMSB') then begin
            {$IFDEF ENDIAN_BIG} //data always stored big endian
            if not AnsiContainsText(mArray[0], 'True') then swapEndian := true;
            {$ELSE}
            if AnsiContainsText(mArray[0], 'True') then swapEndian := true;
            {$ENDIF}
    end {else if AnsiContainsText(tagName, 'BinaryData') then begin
            if AnsiContainsText(mArray[0], 'True') then binaryData := true;
    end else if AnsiContainsText(tagName, 'CompressedDataSize') then begin
            compressedDataSize := strtoint(mArray[0]);
        end} else if AnsiContainsText(tagName, 'CompressedData') then begin
            if AnsiContainsText(mArray[0], 'True') then
                compressedData := true;
        end  else if AnsiContainsText(tagName, 'Orientation') and (not AnsiContainsText(tagName, 'Anatomical') ) then begin
            if (nItems > 12) then nItems := 12;
            matElementsOrient := nItems;
            for i := 0 to (nItems-1) do
              transformMatrix[i] :=  strtofloat(mArray[i]);


            if (matElementsOrient >= 12) then
                LOAD_MAT33(matOrient, transformMatrix[0],transformMatrix[1],transformMatrix[2],
                           transformMatrix[4],transformMatrix[5],transformMatrix[6],
                           transformMatrix[8],transformMatrix[9],transformMatrix[10])
            else if (matElementsOrient >= 9) then
                LOAD_MAT33(matOrient, transformMatrix[0],transformMatrix[1],transformMatrix[2],
                           transformMatrix[3],transformMatrix[4],transformMatrix[5],
                           transformMatrix[6],transformMatrix[7],transformMatrix[8]);

        end else if AnsiContainsText(tagName, 'TransformMatrix') then begin
            if (nItems > 12) then nItems := 12;
            matElements := nItems;
            for i := 0 to (nItems-1) do
              transformMatrix[i] :=  strtofloat(mArray[i]);
            if (matElements >= 12) then
                LOAD_MAT33(mat, transformMatrix[0],transformMatrix[1],transformMatrix[2],
                           transformMatrix[4],transformMatrix[5],transformMatrix[6],
                           transformMatrix[8],transformMatrix[9],transformMatrix[10])
            else if (matElements >= 9) then
                LOAD_MAT33(mat, transformMatrix[0],transformMatrix[1],transformMatrix[2],
                           transformMatrix[3],transformMatrix[4],transformMatrix[5],
                           transformMatrix[6],transformMatrix[7],transformMatrix[8]);
        end else if AnsiContainsText(tagName, 'Position') then begin
            if (nItems > 3) then nItems := 3;
            nPosition := nItems;
            for i := 0 to (nItems-1) do
              position[i] :=  strtofloat(mArray[i]);
        end else if AnsiContainsText(tagName, 'Offset') then begin
            if (nItems > 3) then nItems := 3;
            nOffset := nItems;
            for i := 0 to (nItems-1) do
              offset[i] :=  strtofloat(mArray[i]);
        end else if AnsiContainsText(tagName, 'AnatomicalOrientation') then begin
            //e.g. RAI
        end else if AnsiContainsText(tagName, 'ElementSpacing') then begin
            if (nItems > 4) then nItems := 4;
            for i := 0 to (nItems-1) do
                nhdr.pixdim[i+1] := strtofloat(mArray[i]);
        end else if AnsiContainsText(tagName, 'DimSize') then begin
            if (nItems > 4) then nItems := 4;
            xDim64 := strtoint(mArray[0]);
            for i := 0 to (nItems-1) do
                nhdr.dim[i+1] :=  strtoint(mArray[i]);
        end else if AnsiContainsText(tagName, 'HeaderSize') then begin
            headerSize := strtoint(mArray[0]);
        end else if AnsiContainsText(tagName, 'ElementSize') then begin
            if (nItems > 4) then nItems := 4;
            for i := 0 to (nItems-1) do
                elementSize[i] := strtofloat(mArray[i]);
        end else if AnsiContainsText(tagName, 'ElementNumberOfChannels') then begin
            channels := strtoint(mArray[0]);
            if (channels > 1) then NSLog('Unable to read MHA/MHD files with multiple channels ');
        end else if AnsiContainsText(tagName, 'ElementByteOrderMSB') then begin
            {$IFDEF ENDIAN_BIG} //data always stored big endian
            if not AnsiContainsText(mArray[0], 'True') then swapEndian := true;
            {$ELSE}
            if AnsiContainsText(mArray[0], 'True') then swapEndian := true;
            {$ENDIF}
        end else if AnsiContainsText(tagName, 'ElementType') then begin

            //convert metaImage format to NIfTI http://portal.nersc.gov/svn/visit/tags/2.2.1/vendor_branches/vtk/src/IO/vtkMetaImageWriter.cxx
            //set NIfTI datatype http://nifti.nimh.nih.gov/pub/dist/src/niftilib/nifti1.h
            if AnsiContainsText(mArray[0], 'MET_UCHAR') then
                nhdr.datatype := kDT_UINT8 //
            else if AnsiContainsText(mArray[0], 'MET_CHAR') then
                nhdr.dataType := kDT_INT8 //
            else if AnsiContainsText(mArray[0], 'MET_SHORT') then
                nhdr.dataType := kDT_INT16 //
            else if AnsiContainsText(mArray[0], 'MET_USHORT') then
                nhdr.dataType := kDT_UINT16 //
            else if AnsiContainsText(mArray[0], 'MET_INT') then
                nhdr.dataType := kDT_INT32 //DT_INT32
            else if AnsiContainsText(mArray[0], 'MET_UINT') then
                nhdr.dataType := kDT_UINT32 //DT_UINT32
            else if AnsiContainsText(mArray[0], 'MET_ULONG') then
                nhdr.dataType := kDT_UINT64 //DT_UINT64
            else if AnsiContainsText(mArray[0], 'MET_LONG') then
                nhdr.dataType := kDT_INT64 //DT_INT64
            else if AnsiContainsText(mArray[0], 'MET_FLOAT') then
                nhdr.dataType := kDT_FLOAT32 //DT_FLOAT32
            else if AnsiContainsText(mArray[0], 'MET_DOUBLE') then
                nhdr.dataType := kDT_DOUBLE; //DT_FLOAT64
        end else if AnsiContainsText(tagName, 'ElementDataFile') then begin
            if not AnsiContainsText(mArray[0], 'local') then begin
                str := mArray.Strings[0];
                if fileexists(str) then
                  fname := str
                else begin
                   FilenameParts(fname, pth, nam, ext);
                  fname := pth+str;
                end;
                isLocal := false;
            end;
            break;
        end;
  end; //while reading
  if (headerSize = 0) and (isLocal) then headerSize :=fileposBytes; //!CRAP 2015
  nhdr.vox_offset := headerSize;
  CloseFile(FP);
  Filemode := 2;
  mArray.free;
  //convert transform
  if (matElements >= 9) or (matElementsOrient >= 9) then begin
    //report_Mat(matOrient);
    LOAD_MAT33(d,  nhdr.pixdim[1],0,0,
                 0, nhdr.pixdim[2],0,
                 0,0, nhdr.pixdim[3]);
      if (matElements >= 9) then
         t := nifti_mat33_mul( d, mat)
      else
          t := nifti_mat33_mul( d, matOrient) ;
      if nPosition > nOffset then begin
          offset[0] := position[0];
          offset[1] := position[1];
          offset[2] := position[2];

      end;
      nhdr.srow_x[0] := -t[0,0];
      nhdr.srow_x[1] := -t[1,0];
      nhdr.srow_x[2] := -t[2,0];
      nhdr.srow_x[3] := -offset[0];
      nhdr.srow_y[0] := -t[0,1];
      nhdr.srow_y[1] := -t[1,1];
      nhdr.srow_y[2] := -t[2,1];
      nhdr.srow_y[3] := -offset[1];
      nhdr.srow_z[0] := t[0,2];
      nhdr.srow_z[1] := t[1,2];
      nhdr.srow_z[2] := t[2,2];
      nhdr.srow_z[3] := offset[2];
  end else begin
      //NSLog('Warning: unable to determine image orientation (unable to decode metaIO "TransformMatrix" tag)')};
      nhdr.sform_code:=0;
      nhdr.srow_x[0] := 0;
      nhdr.srow_x[1] := 0;
      nhdr.srow_x[2] := 0;
  end;
  //end transform
  convertForeignToNifti(nhdr);
  if (compressedData) then
      gzBytes := K_gzBytes_onlyImageCompressed;
  if (nhdr.vox_offset < 0) then begin
      nBytes := (nhdr.bitpix div 8);
      for i := 1 to 7 do begin
          if nhdr.dim[i] > 0 then
              nBytes := nBytes * nhdr.dim[i];
      end;
      nhdr.vox_offset := FSize(fname) - nBytes;
      if (nhdr.vox_offset < 0) then nhdr.vox_offset := -1;
  end;
  result := true;
end;//MHA

function readNRRDHeader (var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean; var xDim64: int64): boolean;
//http://www.sci.utah.edu/~gk/DTI-data/
//http://teem.sourceforge.net/nrrd/format.html
label
  666;
var
  FP: TextFile;
  ch: char;
  mArray: TStringList;
  str,tagName,elementNames, pth, nam, ext: string;
  i,s,nItems,headerSize,matElements,fileposBytes: integer;
  mat: mat33;
  isDetachedFile,isFirstLine: boolean;
  offset: array[0..3] of single;
  vSqr: single;
  transformMatrix: array [0..11] of single;
begin
  {$IFDEF FPC}
  DefaultFormatSettings.DecimalSeparator := '.' ;
  //DecimalSeparator := '.';
  {$ELSE}
  DecimalSeparator := '.';
  {$ENDIF}
  result := false;
  gzBytes :=0;
  fileposBytes := 0;
  swapEndian :=false;
  //nDims := 0;
  headerSize :=0;
  isDetachedFile :=false;
  matElements :=0;
  mArray := TStringList.Create;
  Filemode := 0;
  isFirstLine := true;
  FileMode := fmOpenRead;
  AssignFile(fp,fname);
  reset(fp);
  while (not EOF(fp))  do begin
    str := '';
    while not EOF(fp) do begin
      read(fp,ch);
      fileposBytes := fileposBytes + 1;
      if (ch = chr($0D)) or (ch = chr($0A)) then break;
      str := str+ch;
    end;
    if str = '' then continue;
    if (isFirstLine) then begin
      if (length(str) <4) or (str[1]<>'N') or (str[2]<>'R') or (str[3]<>'R') or (str[4]<>'D') then
        goto 666;
      isFirstLine := false;
    end;
    //showmessage(str+'->'+inttostr(fileposBytes));
    if (length(str) < 1) or (str[1]='#') then continue;
    splitstrStrict(':',str,mArray);
    if (mArray.count < 2) then continue;
    tagName := mArray[0];
    elementNames := mArray[1];
    splitstr(',',elementNames,mArray);
    nItems :=mArray.count;
    if (nItems < 1) then continue;
    for i := 0 to (nItems-1) do
      mArray.Strings[i] := cleanStr(mArray.Strings[i]); //remove '(' and ')'
    (*if AnsiContainsText(tagName, 'dimension') then
      nDims := strtoint(mArray.Strings[0])
    else*) if AnsiContainsText(tagName, 'spacings') then begin
      if (nItems > 6) then nItems :=6;
      for i:=0 to (nItems-1) do
        nhdr.pixdim[i+1] :=strtofloat(mArray.Strings[i]);
    end else if AnsiContainsText(tagName, 'sizes') then begin
      if (nItems > 6) then nItems :=6;
      xDim64 := strtoint(mArray.Strings[0]);
      for i:=0 to (nItems-1) do
          nhdr.dim[i+1] := strtoint(mArray.Strings[i]);
    end else if AnsiContainsText(tagName, 'space directions') then begin
      if (nItems > 12) then nItems :=12;
      matElements :=nItems;
      for i:=0 to (nItems-1) do
          transformMatrix[i] :=strtofloat(mArray.Strings[i]);
      if (matElements >= 12) then
          LOAD_MAT33(mat, transformMatrix[0],transformMatrix[1],transformMatrix[2],
                     transformMatrix[4],transformMatrix[5],transformMatrix[6],
                     transformMatrix[8],transformMatrix[9],transformMatrix[10])
      else if (matElements >= 9) then
          LOAD_MAT33(mat, transformMatrix[0],transformMatrix[1],transformMatrix[2],
                     transformMatrix[3],transformMatrix[4],transformMatrix[5],
                     transformMatrix[6],transformMatrix[7],transformMatrix[8]);
    end else if AnsiContainsText(tagName, 'type') then begin
      if AnsiContainsText(mArray.Strings[0], 'uchar') or
          AnsiContainsText(mArray.Strings[0], 'uint8') or
          AnsiContainsText(mArray.Strings[0], 'uint8_t')  then
          nhdr.datatype := KDT_UINT8 //DT_UINT8 DT_UNSIGNED_CHAR
      else if AnsiContainsText(mArray.Strings[0], 'short') or //specific so
               AnsiContainsText(mArray.Strings[0], 'int16') or
               AnsiContainsText(mArray.Strings[0], 'int16_t') then
          nhdr.datatype :=kDT_INT16 //DT_INT16
      else if AnsiContainsText(mArray.Strings[0], 'float') then
          nhdr.datatype := kDT_FLOAT32 //DT_FLOAT32
      else if AnsiContainsText(mArray.Strings[0], 'unsigned')
               and (nItems > 1) and AnsiContainsText(mArray.Strings[1], 'char') then
          nhdr.datatype := kDT_UINT8 //DT_UINT8
      else if AnsiContainsText(mArray.Strings[0], 'unsigned') and
               (nItems > 1) and AnsiContainsText(mArray.Strings[1], 'int') then
          nhdr.datatype := kDT_INT32 //
      else if AnsiContainsText(mArray.Strings[0], 'signed') and
               (nItems > 1) and AnsiContainsText(mArray.Strings[1], 'char') then
          nhdr.datatype := kDT_INT8 //do UNSIGNED first, as "isigned" includes string "unsigned"
      else if AnsiContainsText(mArray.Strings[0], 'signed') and
               (nItems > 1) and AnsiContainsText(mArray.Strings[1], 'short') then
          nhdr.datatype := kDT_INT16 //do UNSIGNED first, as "isigned" includes string "unsigned"
      else if AnsiContainsText(mArray.Strings[0], 'double') then
          nhdr.datatype := kDT_DOUBLE //DT_DOUBLE
      else if AnsiContainsText(mArray.Strings[0], 'int') then //do this last and "uint" includes "int"
          nhdr.datatype := kDT_UINT32
      else begin
          NSLog('Unsupported NRRD datatype'+mArray.Strings[0]);
      end
    end else if AnsiContainsText(tagName, 'endian') then begin
      {$IFDEF ENDIAN_BIG} //data always stored big endian
      if AnsiContainsText(mArray.Strings[0], 'little') then swapEndian :=true;
      {$ELSE}
      if AnsiContainsText(mArray.Strings[0], 'big') then swapEndian :=true;
      {$ENDIF}
    end else if AnsiContainsText(tagName, 'encoding') then begin
      if AnsiContainsText(mArray.Strings[0], 'raw') then
          gzBytes :=0
      else if AnsiContainsText(mArray.Strings[0], 'gz') or AnsiContainsText(mArray.Strings[0], 'gzip') then
          gzBytes := K_gzBytes_headerAndImageCompressed//K_gzBytes_headeruncompressed
      else
          NSLog('Unknown encoding format '+mArray.Strings[0]);
    end  else if AnsiContainsText(tagName, 'space origin') then begin
      if (nItems > 3) then nItems :=3;
      for i:=0 to (nItems-1) do
          offset[i] := strtofloat(mArray.Strings[i]);
    end else if AnsiContainsText(tagName, 'data file') then begin
      str := mArray.Strings[0];
      if (pos('LIST', UpperCase(str)) = 1) and (length(str) = 4) then begin  //e.g. "data file: LIST"
         readln(fp,str);
      end;
      if (pos('%', UpperCase(str)) > 0) and (nItems  > 1) then begin  //e.g. "data file: ./r_sphere_%02d.raw.gz 1 4 1"
         str := format(str,[strtoint(mArray.Strings[1])]);
      end;
      if fileexists(str) then
        fname := str
      else begin
         if (length(str) > 0) and (str[1] = '.') then  // "./r_sphere_01.raw.gz"
           str := copy(str, 2, length(str)-1 );
         if (length(str) > 0) and (str[1] = pathdelim) then  // "./r_sphere_01.raw.gz"
           str := copy(str, 2, length(str)-1 );  // "/r_sphere_01.raw.gz"
         FilenameParts(fname, pth, nam, ext);
         fname := pth+str;
      end;
      isDetachedFile :=true;
      //break;
    end; //for ...else tag names
  end;
  if ((headerSize = 0) and ( not isDetachedFile)) then begin
    if gzBytes = K_gzBytes_headerAndImageCompressed then
      gzBytes := K_gzBytes_onlyImageCompressed; //raw text file followed by GZ image
    headerSize :=fileposBytes;
  end;
  result := true;
666:
  CloseFile(FP);
  Filemode := 2;
  mArray.free;
  if not result then exit;
  nhdr.vox_offset :=headerSize;
  if (matElements >= 9) then begin
        nhdr.srow_x[0] :=-mat[0,0];
        nhdr.srow_x[1] :=-mat[1,0];
        nhdr.srow_x[2] :=-mat[2,0];
        nhdr.srow_x[3] :=-offset[0];
        nhdr.srow_y[0] :=-mat[0,1];
        nhdr.srow_y[1] :=-mat[1,1];
        nhdr.srow_y[2] :=-mat[2,1];
        nhdr.srow_y[3] :=-offset[1];
        nhdr.srow_z[0] :=mat[0,2];
        nhdr.srow_z[1] :=mat[1,2];
        nhdr.srow_z[2] :=mat[2,2];
        nhdr.srow_z[3] :=offset[2];
        //next: ITK does not generate a "spacings" tag - get this from the matrix...
        for s :=0 to 2 do begin
            vSqr :=0.0;
            for i :=0 to 2 do
                vSqr := vSqr+ ( mat[s,i]*mat[s,i]);
            nhdr.pixdim[s+1] :=sqrt(vSqr);
        end //for each dimension
  end;// else
      //  NSLog('Warning: unable to determine image orientation (unable to decode metaIO "TransformMatrix" tag)'+inttostr(matElements));
  convertForeignToNifti(nhdr);
end;


procedure THD_daxes_to_NIFTI (var nhdr: TNIFTIhdr; xyzDelta, xyzOrigin: vect3; orientSpecific: ivect3);
//see http://afni.nimh.nih.gov/pub/dist/src/thd_matdaxes.c
const
  ORIENT_xyz1 = 'xxyyzzg'; //note Pascal strings indexed from 1, not 0!
  ORIENT_sign1 = '+--++-';  //note Pascal strings indexed from 1, not 0!
var
  axnum: array[0..2] of integer;
  axcode,axsign: array[0..2] of char;
  axstart,axstep: array[0..2] of single;
  ii, nif_x_axnum, nif_y_axnum, nif_z_axnum: integer;
  qto_xyz: mat44;

begin
    nif_x_axnum := -1;
    nif_y_axnum := -1;
    nif_z_axnum := -1;
    axnum[0] := nhdr.dim[1];
    axnum[1] := nhdr.dim[2];
    axnum[2] := nhdr.dim[3];
    axcode[0] := ORIENT_xyz1[1+ orientSpecific[0] ] ;
    axcode[1] := ORIENT_xyz1[1+ orientSpecific[1] ] ;
    axcode[2] := ORIENT_xyz1[1+ orientSpecific[2] ] ;
    axsign[0] := ORIENT_sign1[1+ orientSpecific[0] ] ;
    axsign[1] := ORIENT_sign1[1+ orientSpecific[1] ] ;
    axsign[2] := ORIENT_sign1[1+ orientSpecific[2] ] ;
    axstep[0] := xyzDelta[0] ;
    axstep[1] := xyzDelta[1]  ;
    axstep[2] := xyzDelta[2]  ;
    axstart[0] := xyzOrigin[0] ;
    axstart[1] := xyzOrigin[1] ;
    axstart[2] := xyzOrigin[2] ;
    for ii := 0 to 2 do begin
        if (axcode[ii] = 'x') then
            nif_x_axnum := ii
        else if (axcode[ii] = 'y') then
            nif_y_axnum := ii
        else
          nif_z_axnum := ii ;
    end;
    if (nif_x_axnum < 0) or (nif_y_axnum < 0) or (nif_z_axnum < 0) then exit; //not assigned
    if (nif_x_axnum  = nif_y_axnum) or (nif_x_axnum  = nif_z_axnum) or (nif_y_axnum  = nif_z_axnum) then exit; //not assigned
    ZERO_MAT44(qto_xyz);
    //-- set voxel and time deltas and units --
    nhdr.pixdim[1] := abs ( axstep[0] ) ;
    nhdr.pixdim[2] := abs ( axstep[1] ) ;
    nhdr.pixdim[3] := abs ( axstep[2] ) ;
    qto_xyz[0,nif_x_axnum] := - axstep[nif_x_axnum];
    qto_xyz[1,nif_y_axnum] := - axstep[nif_y_axnum];
    qto_xyz[2,nif_z_axnum] :=   axstep[nif_z_axnum];
    nhdr.qoffset_x :=  -axstart[nif_x_axnum] ;
    nhdr.qoffset_y :=  -axstart[nif_y_axnum];
    nhdr.qoffset_z :=  axstart[nif_z_axnum];
    qto_xyz[0,3] := nhdr.qoffset_x ;
    qto_xyz[1,3] := nhdr.qoffset_y ;
    qto_xyz[2,3] := nhdr.qoffset_z ;
    //nifti_mat44_to_quatern( qto_xyz , nhdr.quatern_b, nhdr.quatern_c, nhdr.quatern_d,dumqx, dumqy, dumqz, dumdx, dumdy, dumdz,nhdr.pixdim[0]) ;
    //nhdr.qform_code := kNIFTI_XFORM_SCANNER_ANAT;
    nhdr.srow_x[0] :=qto_xyz[0,0]; nhdr.srow_x[1] :=qto_xyz[0,1]; nhdr.srow_x[2] :=qto_xyz[0,2]; nhdr.srow_x[3] :=qto_xyz[0,3];
    nhdr.srow_y[0] :=qto_xyz[1,0]; nhdr.srow_y[1] :=qto_xyz[1,1]; nhdr.srow_y[2] :=qto_xyz[1,2]; nhdr.srow_y[3] :=qto_xyz[1,3];
    nhdr.srow_z[0] :=qto_xyz[2,0]; nhdr.srow_z[1] :=qto_xyz[2,1]; nhdr.srow_z[2] :=qto_xyz[2,2]; nhdr.srow_z[3] :=qto_xyz[2,3];
    nhdr.sform_code := kNIFTI_XFORM_SCANNER_ANAT;
end;

function readAFNIHeader (var fname: string; var nhdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean; var xDim64: int64): boolean;
label
  666;
var
  sl, mArray: TStringList;
  typeStr,nameStr, valStr: string;
  lineNum, itemCount,i, vInt, nVols: integer;
  isAllVolumesSame, isProbMap, isStringAttribute: boolean;
  valArray  : Array of double;
  orientSpecific: ivect3;
  xyzOrigin, xyzDelta: vect3;
begin
 {$IFDEF FPC}
  DefaultFormatSettings.DecimalSeparator := '.' ;
 //DecimalSeparator := '.';
  {$ELSE}
  DecimalSeparator := '.';
  {$ENDIF}
  nVols := 1;
  result := false;
  isProbMap := false;
  gzBytes := 0;
  swapEndian := false;
  sl := TStringList.Create;
  mArray := TStringList.Create;
  sl.LoadFromFile(fname);
  if(sl.count) < 4 then goto 666;
  lineNum := -1;
  repeat
    //read type string
    lineNum := lineNum + 1;
    if length(sl[lineNum]) < 1 then continue;
    splitstr('=',sl[lineNum],mArray);
    if mArray.Count < 2 then continue;
    if not AnsiContainsText(cleanStr(mArray[0]), 'type') then continue;
    typeStr := cleanStr(mArray[1]);
    isStringAttribute :=  AnsiContainsText(typeStr, 'string-attribute');
    //next: read name string
    lineNum := lineNum + 1;
    if (lineNum >= (sl.count-1)) then continue;
    splitstr('=',sl[lineNum],mArray);
    if mArray.Count < 2 then continue;
    if not AnsiContainsText(cleanStr(mArray[0]), 'name') then continue;
    nameStr := cleanStr(mArray[1]);
    //if AnsiContainsText(nameStr,'BYTEORDER_STRING') and isStringAttribute then showmessage('txt');
    //next: read count string
    lineNum := lineNum + 1;
    if (lineNum >= (sl.count-1)) then continue;
    splitstr('=',sl[lineNum],mArray);
    if mArray.Count < 2 then continue;
    if not AnsiContainsText(cleanStr(mArray[0]), 'count') then continue;
    itemCount := strtoint(cleanStr(mArray[1]));
    if itemCount < 1 then exit;
    //next read values
    lineNum := lineNum + 1;
    if (lineNum > (sl.count-1)) then continue;
    valStr := sl[lineNum];
    while ((lineNum+1) <= (sl.count-1)) and (length(sl[lineNum+1]) > 0) do begin
      lineNum := lineNum + 1;  //AFNI wraps some arrays across multiple lines
      valStr := valStr + ' '+ sl[lineNum];
    end;
    splitstr(' ',valStr,mArray);
    if (mArray.Count < itemCount) then itemCount := mArray.Count; // <- only if corrupt
    if itemCount < 1 then continue; // <- only if corrupt data
    if isStringAttribute then begin
        if AnsiContainsText(nameStr,'BYTEORDER_STRING') then begin
              {$IFDEF ENDIAN_BIG}
              if AnsiContainsText(mArray[0],'LSB_FIRST') then swapEndian := true;
              {$ELSE}
              if AnsiContainsText(mArray[0],'MSB_FIRST') then swapEndian := true;
              {$ENDIF}
        end
    end else begin //if numeric attributes...
      setlength(valArray,itemCount);
      for i := 0 to (itemCount-1) do
        valArray[i] := strtofloat(cleanStr(mArray[i]) );
      //next - harvest data from important names
      if AnsiContainsText(nameStr,'BRICK_TYPES') then begin
              vInt := round(valArray[0]);
              if (vInt = 0) then begin
                  nhdr.datatype := kDT_UINT8;
              end else if (vInt = 1) then begin
                  nhdr.datatype := kDT_INT16; //16 bit signed int
              end else if (vInt = 3) then begin
                  nhdr.datatype := kDT_FLOAT32;//32-bit float
              end else begin
                  NSLog('Unsupported BRICK_TYPES '+inttostr(vInt));
                  goto 666;
              end;
              if (itemCount > 1) then begin //check that all volumes are of the same datatype
                  nVols := itemCount;
                  isAllVolumesSame := true;
                  for i := 1 to (itemCount-1) do
                      if (valArray[0] <> valArray[i]) then isAllVolumesSame := false;
                  if (not isAllVolumesSame) then begin
                      NSLog('Unsupported BRICK_TYPES feature: datatype varies between sub-bricks');
                      goto 666;
                  end;
              end; //if acount > 0
              //NSLog('HEAD datatype is '+inttostr(nhdr.datatype) );
          end else if AnsiContainsText(nameStr,'BRICK_FLOAT_FACS') then begin
              nhdr.scl_slope := valArray[0];
              if (itemCount > 1) then begin //check that all volumes are of the same datatype
                  isAllVolumesSame := true;
                  for i := 1 to (itemCount-1) do
                      if (valArray[0] <> valArray[i]) then isAllVolumesSame := false;
                  if (not isAllVolumesSame) then begin
                      NSLog('Unsupported BRICK_FLOAT_FACS feature: intensity scale between sub-bricks');
                  end;
              end; //if acount > 0
          end else if AnsiContainsText(nameStr,'DATASET_DIMENSIONS') then begin
              if itemCount > 3 then itemCount := 3;
              xDim64:= round(valArray[0]);
              for i := 0 to (itemCount-1) do
                  nhdr.dim[i+1] := round(valArray[i]);
          end else if AnsiContainsText(nameStr,'ORIENT_SPECIFIC') then begin
              if itemCount > 3 then itemCount := 3;
              for i := 0 to (itemCount-1) do
                  orientSpecific[i] := round(valArray[i]);;
              //NSLog(@"HEAD orient specific %d %d %d",orientSpecific.v[0],orientSpecific.v[1],orientSpecific.v[2]);
          end else if AnsiContainsText(nameStr,'ORIGIN') then begin
              if itemCount > 3 then itemCount := 3;
              for i := 0 to (itemCount-1) do
                  xyzOrigin[i] := valArray[i];
              //NSLog(@"HEAD origin %g %g %g",xyzOrigin.v[0],xyzOrigin.v[1],xyzOrigin.v[2]);
          end else if AnsiContainsText(nameStr,'ATLAS_PROB_MAP') then begin
              if (round(valArray[0]) = 1) then isProbMap := true;
          end else if AnsiContainsText(nameStr,'ATLAS_LABEL_TABLE') then begin
              nhdr.intent_code := kNIFTI_INTENT_LABEL;
          end else if AnsiContainsText(nameStr,'DELTA') then begin
              if itemCount > 3 then itemCount := 3;
              for i := 0 to (itemCount-1) do
                  xyzDelta[i] := valArray[i];
              //NSLog(@"HEAD delta %g %g %g",xyzDelta.v[0],xyzDelta.v[1],xyzDelta.v[2]);
          end else if AnsiContainsText(nameStr,'TAXIS_FLOATS') then begin
              if (itemCount > 1) then nhdr.pixdim[4] := valArray[1]; //second item is TR
          end;
      end;// if isStringAttribute else numeric inputs...
  until (lineNum >= (sl.count-1));
  result := true;
666:
  valArray := nil; //release dynamic array
  Filemode := 2;
  sl.free;
  mArray.free;
  if not result then exit; //error - code jumped to 666 without setting result to true
  if (nVols > 1) then nhdr.dim[4] := nVols;
  if (isProbMap) and (nhdr.intent_code = kNIFTI_INTENT_LABEL)  then nhdr.intent_code := kNIFTI_INTENT_NONE;
  THD_daxes_to_NIFTI(nhdr, xyzDelta, xyzOrigin, orientSpecific );
  nhdr.vox_offset := 0;
  convertForeignToNifti(nhdr);
  fname := ChangeFileExtX(fname, '.BRIK');
  if (not FileExists(fname)) then begin
    fname := fname+'.gz';
    gzBytes := K_gzBytes_headerAndImageCompressed;
  end;
end;

function readForeignHeader (var lFilename: string; var lHdr: TNIFTIhdr; var gzBytes: int64; var swapEndian: boolean; var xDim64: int64): boolean;
var
  lExt: string;
begin
  NII_Clear (lHdr);
  result := false;
  lExt := ExtractFileExtGzUpper(lFilename);
  if (lExt = '.PIC') then
    result := nii_readpic(lFilename, lHdr, gzBytes, swapEndian, xDim64)
  else if (lExt = '.VTK') then
    result := readVTKHeader(lFilename, lHdr, gzBytes, swapEndian, xDim64)
  else if (lExt = '.MGH') or (lExt = '.MGZ') then
    result := readMGHHeader(lFilename, lHdr, gzBytes, swapEndian, xDim64)
  else if (lExt = '.MHD') or (lExt = '.MHA') then
    result := readMHAHeader(lFilename, lHdr, gzBytes, swapEndian, xDim64)
  else if (lExt = '.NRRD') or (lExt = '.NHDR') then
    result := readNRRDHeader(lFilename, lHdr, gzBytes, swapEndian, xDim64)
  else if (lExt = '.HEAD') then
    result := readAFNIHeader(lFilename, lHdr, gzBytes, swapEndian, xDim64);
end;

end.

