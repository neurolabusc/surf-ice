unit define_types;
interface
uses graphics;
const
  NaN : double = 1/0;
  kTab = chr(9);
  kCR = chr (13);
  kDel = #127 ; // Delete
  kBS = #8 ; // Backspace
  UNIXeoln = chr(10);

type
   TRGBA = packed record //Next: analyze Format Header structure
    R,G,B,A : byte;
  end;
  TPoint4f = packed record
     X: single;
     Y: single;
     Z: single;
     W: single
   end;
  TPoint3f = packed record
    X: single;
    Y: single;
    Z: single
  end;
 TPoint3i = packed record
    X: longint; //ensure 32-bit for simple GIfTI writing
    Y: longint;
    Z: longint;
  end;
 TFaces = array of TPoint3i;
 TVertices = array of TPoint3f;
 TVertexRGBA = array of TRGBA;
 TInts = array of integer;
 TFloats = array of single;
 TMat33 = array [1..3, 1..3] of single;
  TMat44 = array [1..4, 1..4] of single;
  TFByte =  File of Byte;
  TStrRA = Array of String;
  TUnitRect = record
     L,T,R,B: single;
  end;

procedure FilenameParts (lInName: string; var lPath,lName,lExt: string);
procedure SensibleUnitRect (var U: TUnitRect);
procedure SortSingle(var lLo,lHi: single);
function RealToStr(lR: double; lDec: integer): string;
function RGBA(lR,lG,lB,lA: byte): TRGBA;
function CreateUnitRect (L,T,R,B: single) : TUnitRect;
procedure IntBound (var lVal: integer; lMin, lMax: integer);
function UnitBound (lS: single): single;
procedure ReadLnBin(var f: TFByte; var s: string);
procedure SwapSingle(var s : single);
procedure SwapLongInt(var s : LongInt);
procedure SwapLongWord(var s : LongWord);
function asSingle(i : longint): single; overload;
function asSingle(b0,b1,b2,b3: byte): single; overload;
function asInt(s : single): longint;
function specialsingle (var s:single): boolean; //isFinite
function asRGBA(clr: TColor): TRGBA;
function ExtractFileExtGzUpper(FileName: string): string;

implementation

uses sysutils, dialogs;

function ExtractFileExtGzUpper(FileName: string): string;
//the file 'img.nii.gz' returns '.NII.GZ', not just '.gz'
var
  lPath,lName,lExt: string;
begin
  //result := UpperCase(ExtractFileExt(FileName));

  FilenameParts (FileName, lPath,lName,lExt);
  result := UpperCase(lExt);
end;

procedure IntBound (var lVal: integer; lMin, lMax: integer);
begin
    if lVal < lMin then lVal := lMin;
    if lVal > lMax then lVal := lMax;
end;

function asRGBA(clr: TColor): TRGBA;
begin
  result.R := red(clr);
  result.G := green(clr);
  result.B := blue(clr);
  result.A := 255;
end;

function specialsingle (var s:single): boolean;
//returns true if s is Infinity, NAN or Indeterminate
//4byte IEEE: msb[31] = signbit, bits[23-30] exponent, bits[0..22] mantissa
//exponent of all 1s =   Infinity, NAN or Indeterminate
const kSpecialExponent = 255 shl 23;
var Overlay: LongInt absolute s;
begin
  if ((Overlay and kSpecialExponent) = kSpecialExponent) then
     RESULT := true
  else
      RESULT := false;
end;

function asSingle(i : longint): single;  overload;
type
  swaptype = packed record
    case byte of
      0:(Lng : longint);
      1:(Sngl : single);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
begin
  inguy := @i; //assign address of s to inguy
  result := inguy^.Sngl;
end; // asSingle()

function asSingle(b0,b1,b2,b3: byte): single; overload;
type
  swaptype = packed record
    case byte of
      0:(b0,b1,b2,b3 : byte);
      1:(Sngl : single);
  end;
  //swaptypep = ^swaptype;
var
  //inguy:swaptypep;
  outguy:swaptype;
begin //should work with both little and big endian, as order is same
  outguy.b0 := b0;
  outguy.b1 := b1;
  outguy.b2 := b2;
  outguy.b3 := b3;
  result := outguy.Sngl;
end; // asSingle()


function asInt(s : single): longint;
type
  swaptype = packed record
    case byte of
      0:(Lng : longint);
      1:(Sngl : single);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
begin
  inguy := @s; //assign address of s to inguy
  result := inguy^.Lng;
end; // asInt()

procedure SwapSingle(var s : single);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word);
      1:(Sngl : single);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  s := outguy.Sngl;
end; // SwapSingle()

procedure SwapLongInt(var s : LongInt);
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
  s := outguy.Long;
end; // SwapLongInt()

procedure SwapLongWord(var s : LongWord);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongWord);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  s := outguy.Long;
end; // SwapLongWord()

procedure ReadLnBin(var f: TFByte; var s: string);
const
  kEOLN = $0A;
var
   bt : Byte;
begin
     s := '';
     while (not  EOF(f)) do begin
           Read(f,bt);
           if bt = kEOLN then exit;
           s := s + Chr(bt);
     end;
end;

function RGBA(lR,lG,lB,lA: byte): TRGBA;
//set red,green,blue and alpha of a Quad
begin
  result.r := lR;
  result.g := lG;
  result.b := lB;
  result.a := lA;
end;

function RealToStr(lR: double; lDec: integer): string;
begin
     result := FloatToStrF(lR, ffFixed,7,lDec);
end;

procedure FilenameParts (lInName: string; var lPath,lName,lExt: string);
var
   lX: string;
begin
  lPath := ExtractFilePath(lInName);
  lName := ExtractFileName(lInName);
  lExt := ExtractFileExt(lInName);
  if lExt = '' then exit;
  Delete(lName, length(lName)-length(lExt)+1, length(lExt)); //nam.ext -> nam
  lX := lExt;
  if UpperCase(lX) <> '.GZ' then exit;
  lExt := ExtractFileExt(lName);
  Delete(lName, length(lName)-length(lExt)+1, length(lExt)); //nam.ext -> nam
  lExt := lExt + lX;
  //showmessage(lName+':'+lExt);
end;

(*function FilenameParts (lInName: string; var lPath,lName,lExt: string): boolean;
var
   lLen,lPos,lExtPos,lPathPos: integer;
begin
    result := false;
    lPath := '';
    lName := '';
    lExt := '';
    lLen := length(lInName);
    if lLen < 1 then exit;
    //next find final pathdelim
    lPathPos := lLen;
    while (lPathPos > 0) and (lInName[lPathPos] <> '\') and (lInName[lPathPos] <> '/') do
          dec(lPathPos);
    if (lInName[lPathPos] = '\') or (lInName[lPathPos] = '/') then begin
       for lPos := 1 to lPathPos do
           lPath := lPath + lInName[lPos];
    end;
    // else
    //    dec(lPathPos);
    inc(lPathPos);
    //next find first ext
    lExtPos := 1;
    while (lExtPos <= lLen) and (lInName[lExtPos] <> '.') do
          inc(lExtPos);
    if (lInName[lExtPos] = '.')  then begin
       for lPos := lExtPos to lLen do
           lExt := lExt + lInName[lPos];
    end;
    // else
    //    inc(lExtPos);
    dec(lExtPos);
    //next extract filename
    //fx(lPathPos,lExtPos);

    if (lPathPos <= lExtPos) then
       for lPos := lPathPos to lExtPos do
           lName := lName + lInName[lPos];
    result := true;
end; *)

procedure SortSingle(var lLo,lHi: single);
var lSwap: single;
begin
	if lLo > lHi then begin
		lSwap := lLo;
		lLo := lHi;
		lHi := lSwap;
	end; //if Lo>Hi
end; //proc SortSingle

function UnitBound (lS: single): single;
begin
  if lS < 0 then
    result := 0
  else if lS > 1 then
    result := 1
  else
    result := lS;
end;

procedure SensibleUnitRect (var U: TUnitRect);
begin
  U.L := UnitBound(U.L);
  U.T := UnitBound(U.T);
  U.R := UnitBound(U.R);
  U.B := UnitBound(U.B);
  //left should be lower value than right
  SortSingle(U.L,U.R);
  if U.L = U.R then begin
    if U.R < 0.1 then
      U.R := 0.1
    else
      U.L := U.R -0.1;
  end;
  //bottom should lower value than top
  SortSingle(U.B,U.T);
  if U.B = U.T then begin
    if U.T < 0.1 then
      U.T := 0.1
    else
      U.B := U.T -0.1;
  end;
end;

function CreateUnitRect (L,T,R,B: single) : TUnitRect;
begin
  result.L := UnitBound(L);
  result.T := UnitBound(T);
  result.R := UnitBound(R);
  result.B := UnitBound(B);
end;

end.
