unit define_types;
interface
{$ifndef isTerminalApp}
uses graphics;
{$endif}
const
  kVers = 'v1.0.20180202';
  NaN : double = 1/0;
  kTab = chr(9);
  kCR = chr (13);
  kDel = #127 ; // Delete
  kBS = #8 ; // Backspace
  kUNIXeoln = chr(10);
  {$IFDEF UNIX} //end of line
  kEOLN = kUNIXeoln; //Windows CRLF   ;
  {$ELSE}
   kEOLN = #13#10; //Windows CRLF
  {$ENDIF}

type
   TRGBA = packed record //Next: analyze Format Header structure
    R,G,B,A : byte;
  end;
       {$ifdef isTerminalApp}
  TColor = -$7FFFFFFF-1..$7FFFFFFF;
  TLUT = array [0..255] of TRGBA;
  {$endif}
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
 TBools = array of boolean;
 TInts = array of integer;
 TFloats = array of single;
 TMat33 = array [1..3, 1..3] of single;
  TMat44 = array [1..4, 1..4] of single;
  TFByte =  File of Byte;
  TStrRA = Array of String;
  TUnitRect = record
     L,T,R,B: single;
  end;
  function ParseFileName(Filename: string): string;
procedure FilenameParts (lInName: string; out lPath,lName,lExt: string);
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
function asPt4f(x,y,z,w: single): TPoint4f;
function asSingle(i : longint): single; overload;
function asSingle(b0,b1,b2,b3: byte): single; overload;
function asInt(s : single): longint;
function specialsingle (var s:single): boolean; //isFinite
function ExtractFileExtGzUpper(FileName: string): string;
function FileExistsF(fnm: string): boolean; //returns false if file exists but is directory
function FSize (lFName: String): longint;
function ChangeFileExtX( lFilename: string; lExt: string): string;
function ReadNumBin(var f: TFByte): string; //read next ASCII number in binary file
function float2str(Avalue:double; ADigits:integer):string; //e.g x:single=2.6; floattostrf(x,8,4);
function DefaultToHomeDir(FileName: string): string; //set path to home if not provided
{$ifdef isTerminalApp}
function RGBToColor(R, G, B: Byte): TColor;
procedure ShowMessage(msg: string);
function Red(rgb: TColor): BYTE;
function Green(rgb: TColor): BYTE;
function Blue(rgb: TColor): BYTE;
{$else}
function asRGBA(clr: TColor): TRGBA;
{$endif}

implementation

uses
  {$IFDEF UNIX} BaseUnix, {$ELSE} windows, {$ENDIF}
  sysutils, math;

function FileExistsF(fnm: string): boolean; //returns false if file exists but is directory
begin
     result := FileExists(fnm);
     if result = false then exit;
     result := not DirectoryExists(fnm);
     {$IFDEF UNIX}
     if result = false then exit;
     //showmessage(fnm + inttostr( fpAccess (fnm,R_OK)));
     if fpAccess(fnm,R_OK) < 0 then
        result := false;
     {$ENDIF}
end;

function DefaultToHomeDir(FileName: string): string; //set path to home if not provided
{$IFDEF UNIX}
var
   p,n,x: string;
begin
   result := FileName;
   FilenameParts (Filename, p,n,x);
   if (p <> '') and (DirectoryExists(p)) then exit;
   p := expandfilename('~/');
   result := p+n+x;
end;
{$ELSE}
var
   p,n,x: string;
var
    PIDL : PItemIDList;
    Folder : array[0..MAX_PATH] of Char;
    const CSIDL_PERSONAL = $0005;
begin
   result := FileName;
   FilenameParts (Filename, p,n,x);
   if (p <> '') and (DirectoryExists(p)) then exit;
   SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, PIDL);
   SHGetPathFromIDList(PIDL, Folder);
   p := Folder+pathdelim;
   result := p+n+x;
end;
{$ENDIF}

{$IFDEF oldFloat2Str}
function float2str(Avalue:double; ADigits:integer):string; //e.g x:single=2.6; floattostrf(x,8,4);
begin
     result := FloatToStrF(Avalue, ffFixed,7,ADigits);
end;
{$ELSE}
function float2str(Avalue:double; ADigits:integer):string; //e.g x:single=2.6; floattostrf(x,8,4);
//http://stackoverflow.com/questions/5650051/how-to-keep-2-decimal-places-in-delphi
var v:double; p:integer; e:string;
begin
if abs(Avalue)<1 then
begin
  result:=floatTostr(Avalue);
  p:=pos('E',result);
  if p>0 then
  begin
    e:=copy(result,p,length(result));
    setlength(result,p-1);
    v:=RoundTo(StrToFloat(result),-Adigits);
    result:=FloatToStr(v)+e;
  end else
    result:=FloatToStr(RoundTo(Avalue,-Adigits));
end
else
  result:=FloatToStr(RoundTo(Avalue,-Adigits));
end;
{$ENDIF}

{$ifdef isTerminalApp}
function Blue(rgb: TColor): BYTE;
begin
  Result := (rgb shr 16) and $000000ff;
end;

function Green(rgb: TColor): BYTE;
begin
  Result := (rgb shr 8) and $000000ff;
end;

function Red(rgb: TColor): BYTE;
begin
  Result := rgb and $000000ff;
end;
function RGBToColor(R, G, B: Byte): TColor;
begin
Result := (B shl 16) or (G shl 8) or R;
end;

procedure ShowMessage(msg: string);
begin
  writeln(msg);
end;
{$else}
function asRGBA(clr: TColor): TRGBA;
begin
  result.R := red(clr);
  result.G := green(clr);
  result.B := blue(clr);
  result.A := 255;
end;
{$endif}

function ReadNumBin(var f: TFByte): string; //read next ASCII number in binary file
var
   bt : Byte;
   ch : Char;
begin
     result := '';
     while (not  EOF(f)) do begin
           Read(f,bt);
           ch := Chr(bt);
           if ch in ['-','.','E','e','0'..'9'] then
              result := result + ch
           else if length(result) > 0 then
              exit;
     end;
end;

function asPt4f(x,y,z,w: single): TPoint4f;
begin
     result.x := x;
     result.y := y;
     result.z := z;
     result.w := w;
end;

function ChangeFileExtX( lFilename: string; lExt: string): string;
//sees .nii.gz as single extension
var
   lPath,lName,lOrigExt: string;
begin
     FilenameParts (lFilename, lPath,lName,lOrigExt);
     result := lPath+lName+lExt;
end;

function FSize (lFName: String): longint;
var F : File Of byte;
begin
  result := 0;
  if not fileexists(lFName) then exit;
  Assign (F, lFName);
  Reset (F);
  result := FileSize(F);
  Close (F);
end;

function ParseFileName(Filename: string): string;
var
  lPath,lName,lExt: string;
begin
  FilenameParts (FileName, lPath,lName,lExt);
  result := (lName);
end;

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

procedure FilenameParts (lInName: string; out lPath,lName,lExt: string);
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
