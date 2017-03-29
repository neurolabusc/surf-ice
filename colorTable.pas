unit colorTable;

{$ifdef fpc}{$mode delphi}{$endif}

interface
uses
  Classes, SysUtils, dialogs, userdir, prefs, IniFiles, define_types;

const  //maximum number of control points for color schemes...
  maxNodes = 32;

type

  TLUT = array [0..255] of TRGBA;
  TLUTnodes = record
	isFreeSurfer: boolean; //make items brighter or darker than range transparent
	rgba: array [0..maxNodes] of TRGBA;
        intensity: array [0..maxNodes] of integer;
	end;

function UpdateTransferFunction (var lIndex: integer; isInvert: boolean): TLUT;//change color table
function CLUTDir: string;
function blendRGBA(c1, c2: TRGBA ): TRGBA;
function maxRGBA(c1, c2: TRGBA ): TRGBA;
function inten2rgb(intensity, mn, mx: single; lut: TLUT): TRGBA;
function inten2rgb1(intensity, mn, mx: single; lut: TLUT): TRGBA; //use 1st not 0th LUT color (0 is transparent)
function desaturateRGBA ( rgba: TRGBA; frac: single; alpha: byte): TRGBA;
function isFreeSurferLUT(lIndex: integer): boolean;

implementation

uses mainunit;

function isFreeSurferLUT(lIndex: integer): boolean;
begin
     result := (lIndex >= 15) and (lIndex <= 18);
end;

function blendRGBA(c1, c2: TRGBA ): TRGBA;
var
  frac1, frac2: single;
begin
  result := c1;
  if c2.A = 0 then exit;
  result := c2;
  if c1.A = 0 then exit;
  frac1 := c1.A / (c1.A+c2.A);
  frac2 := 1 - frac1;
  result.R := round(c1.R*frac1 + c2.R*frac2) ;
  result.G := round(c1.G*frac1 + c2.G*frac2);
  result.B := round(c1.B*frac1 + c2.B*frac2);
  if frac1 >= 0.5 then //result.a = max(c1.a, c2.a)
     result.A := c1.A
  else
      result.A := c2.A;
end;

function maxRGBA(c1, c2: TRGBA ): TRGBA;
begin
  result := c1;
  if c2.A = 0 then exit;
  if (c2.R > result.R) then
     result.R := c2.R;
  if (c2.G > result.G) then
     result.G := c2.G;
  if (c2.B > result.B) then
     result.B := c2.B;
  if (c2.A > result.A) then
     result.A := c2.A;
end;

function inten2rgb(intensity, mn, mx: single; lut: TLUT): TRGBA;
begin
  if  (mn < 0) and (mx < 0) then begin
    if intensity >= mx then begin
       result := lut[0];
    end else if intensity <= mn then
       result := lut[255]
    else
         result := lut[round(255* (1.0-   (intensity-mn)/(mx-mn)))];
  end else begin
     if (intensity <= mn) and (intensity <> 0) and (LUT[0].A <> 0) then
        result := lut[1]
     else if intensity <= mn then
        result := lut[0]
     else if intensity >= mx then
        result := lut[255]
     else
          result := lut[round(255*(intensity-mn)/(mx-mn))];
  end;
end;

function inten2rgb1(intensity, mn, mx: single; lut: TLUT): TRGBA; //use 1st not 0th LUT color (0 is transparent)
var i :integer;
begin
  if  (mn < 0) and (mx < 0) then begin
    if intensity >= mx then
       i := 0
    else if intensity <= mn then
       i := 255
    else
         i := round(255* (1.0-   (intensity-mn)/(mx-mn)));
  end else begin
     if intensity <= mn then
        i := 0
     else if intensity >= mx then
        i := 255
     else
          i := round(255*(intensity-mn)/(mx-mn));
  end;
  if (i < 1) then i := 1;
  result := lut[i];
end;

function desaturateRGBA ( rgba: TRGBA; frac: single; alpha: byte): TRGBA;
var
  y: single;
begin
  //convert RGB->YUV http://en.wikipedia.org/wiki/YUV
    y := 0.299 * rgba.r + 0.587 * rgba.g + 0.114 * rgba.b;
    result.r := round(y * (1-frac) + rgba.r * frac);
    result.g := round(y * (1-frac) + rgba.g * frac);
    result.b := round(y * (1-frac) + rgba.b * frac);
    result.a := alpha;
end;

function CLUTDir: string;
begin
  //result := extractfilepath(paramstr(0))+'lut';
  result := AppDir+'lut';
  {$IFDEF UNIX}
  if fileexists(result) then exit;
  result := '/usr/share/surfice/lut';
  if fileexists(result) then exit;
  result := AppDir+'lut';
  {$ENDIF}
end;

procedure setNode (r,g,b,a,i, node: integer; var  lLUTnodes : TLUTnodes);
begin
  lLUTnodes.rgba[node].R := r;
  lLUTnodes.rgba[node].G := g;
  lLUTnodes.rgba[node].B := b;
  lLUTnodes.rgba[node].A := a;
  lLUTnodes.intensity[node] := i;
end;

function loadCustomLUT(var lIndex: integer): TLUTnodes;
var
 lFilename: string;
 lIniFile: TIniFile;
 numnodes, i: integer;
 inten: byte;
 rgba: TRGBA;
begin
 setNode(0,0,0,0,0, 0, result);
 setNode(255,255,255,255,255, 1, result);
 result.isFreeSurfer:= false;
 lFilename := CLUTdir+pathdelim+GLForm1.LUTdrop.Items[lIndex]+'.clut';
 if not fileexists(lFilename) then begin
    lIndex := 0;
    exit;
 end;
 lIniFile := TIniFile.Create(lFilename);
 IniInt(true,lIniFile, 'numnodes', numnodes);
 if (numnodes < 1) or (numnodes > maxNodes) then begin
   if (numnodes > maxNodes) then
      showmessage(format('Too many nodes (%d, maximum %d)', [numnodes, maxNodes]));
     lIniFile.Free;
    lIndex := 0;
    exit;
 end;
   for i := 0 to (numnodes-1) do begin
     IniByte(true,lIniFile, 'nodeintensity'+inttostr(i),inten);
     IniRGBA(true,lIniFile, 'nodergba'+inttostr(i),rgba);
     setNode(rgba.r, rgba.g, rgba.b, rgba.a, inten, i, result);
   end;
 lIniFile.Free;
end;

function makeLUT(var lIndex: integer): TLUTnodes;
begin
  //generate default grayscale color table
  //result.numnodes := 2; //number of nodes implicit: final node has intensity=255
  setNode(0,0,0,0,0, 0, result);
  setNode(255,255,255,255,255, 1, result);
  result.isFreeSurfer:= false;
  case lIndex of //generate alternative color table if specified
       0: exit; //default grayscale
       1: begin //Red-Yellow
         setNode(192,0,0,0,0, 0, result);
         setNode(255,255,0,255,255, 1, result);
         end;
       2: begin //Blue-Green
           setNode(0,0,192,0,0, 0, result);
          setNode(0,255,128,255,255, 1, result);
         end;
       3: begin //Red
          setNode(255,0,0,255,255, 1, result);
       end;
       4: begin //Green
          setNode(0,255,0,255,255, 1, result);
       end;
       5: begin //Blue
          setNode(0,0,255,255,255, 1, result);
       end;
       6: begin //Violet R+B
          setNode(255,0,255,255,255, 1, result);
       end;
       7: begin //Yellow R+G
          setNode(255,255,0,255,255, 1, result);
       end;
       8: begin //Cyan B+G
          setNode(0,255,255,255,255, 1, result);
       end;
       9: begin //HotLut
          //result.numnodes:=4;
          setNode(3,0,0,0,0, 0, result);
          setNode(255,0,0,48,95, 1, result);
          setNode(255,255,0,96,191, 2, result);
          setNode(255,255,255,128,255, 3, result);
       end;
       10: begin //bone
          //result.numnodes:=3;
          setNode(0,0,0,0,0, 0, result);
          setNode(103,126,165,76,153, 1, result);
          setNode(255,255,255,128,255, 2, result);
       end;
       11: begin //WinterLut
          //result.numnodes:=3;
          setNode(0,0,255,0,0, 0, result);
          setNode(0,128,196,64,128, 1, result);
          setNode(0,255,128,128,255, 2, result);
       end;
       12: begin //GE-Color
          //result.numnodes:=5;
          setNode(0,0,0,0,0, 0, result);
          setNode(0,128,125,32,63, 1, result);
          setNode(128,0,255,64,128, 2, result);
          setNode(255,128,0,96,192, 3, result);
          setNode(255,255,255,128,255, 4, result);
       end;
       13: begin //ACTC
          //result.numnodes:=5;
          setNode(0,0,0,0,0, 0, result);
          setNode(0,0,136,32,64, 1, result);
          setNode(24,177,0,64,128, 2, result);
          setNode(248,254,0,78,156, 3, result);
          setNode(255,0,0,128,255, 4, result);
       end;
       14: begin //X-rain
          //result.numnodes:=7;
          setNode(0,0,0,0,0, 0, result);
          setNode(64,0,128,8,32, 1, result);
          setNode(0,0,255,16,64, 2, result);
          setNode(0,255,0,24,96, 3, result);
          setNode(255,255,0,32,160, 4, result);
          setNode(255,192,0,52,192, 5, result);
          setNode(255,3,0,80,255, 6, result);
       end;
       15: begin //FreeSurferCurve - center dark
          result.isFreeSurfer:= true;
         setNode(0,0,0,0,0, 0, result);
         setNode(0,0,0,128,40,1, result);
         setNode(0,0,0,128,215,2, result);
         setNode(0,0,0,0,255, 3, result);
       end;
       16: begin //FreeSurferCurve  - center transparent
         result.isFreeSurfer:= true;
         setNode(0,0,0,128,0, 0, result);
         setNode(0,0,0,0, 40,1, result);
         setNode(0,0,0,0, 215,2, result);
         setNode(0,0,0,128,255, 3, result);
       end;
       17: begin //FreeSurferCurve - center dark
         result.isFreeSurfer:= true;
         setNode(0,0,0,0,0, 0, result);
         setNode(0,0,0,148,128,1, result);
         setNode(0,0,0,0,255, 2, result);
       end;
       18: begin //FreeSurferCurve  - center transparent
         result.isFreeSurfer:= true;
         setNode(0,0,0,148,0, 0, result);
         setNode(0,0,0,0,128,1, result);
         setNode(0,0,0,148,255, 2, result);
       end;
       else begin
           result := loadCustomLUT(lIndex);  //index unknown!!!
       end;
  end; //case: alternative LUTs
end; //makeLUT()

function lerpRGBA (p1,p2: TRGBA; frac: single): TRGBA;
//linear interpolation
begin
  result.R := round(p1.R + frac * (p2.R - p1.R));
  result.G := round(p1.G + frac * (p2.G - p1.G));
  result.B := round(p1.B + frac * (p2.B - p1.B));
  result.A := round(p1.A + frac * (p2.A - p1.A));
end;//lerpRGBA()

function UpdateTransferFunction (var lIndex: integer; isInvert: boolean): TLUT;//change color table
var
 lLUTnodes :TLUTnodes;
 lInc,lNodeLo: integer;
 frac, f: single;
 rev: TLUT;
begin
 lLUTNodes := makeLUT(lIndex);
 lNodeLo := 0;
 result[0] := lerpRGBA(lLUTNodes.rgba[0],lLUTNodes.rgba[0],1);
 for lInc := 1 to 255 do begin
   f := lInc;
   if (f < 0) then f := 0;
   if (f > 255) then f := 255;
   //if ((lNodeLo+1) < lLUTNodes.numnodes) and ( f > lLUTNodes.intensity[lNodeLo + 1] ) then
   if ( f > lLUTNodes.intensity[lNodeLo + 1] ) then
      lNodeLo := lNodeLo + 1;
   frac := (f-lLUTNodes.Intensity[lNodeLo])/(lLUTNodes.Intensity[lNodeLo+1]-lLUTNodes.Intensity[lNodeLo]);
   if (frac < 0) then frac := 0;
   if frac > 1 then frac := 1;
   result[lInc] := lerpRGBA(lLUTNodes.rgba[lNodeLo],lLUTNodes.rgba[lNodeLo+1],frac);
 end;

 if isInvert then begin
    rev := result;
    for lInc := 0 to 255 do
        result[lInc] := rev[255-lInc];
    result[0].A := rev[0].A;
    result[255].A := rev[255].A;
 end;
 if lLUTNodes.isFreeSurfer then exit; //not for freesurfer
 //result[0].A := 0; //see LUT[0].A <> 0
 for lInc := 1 to 255 do
     result[lInc].A := 255;
end;//LoadLUT()

end.

