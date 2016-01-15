unit colorTable;

{$ifdef fpc}{$mode delphi}{$endif}

interface
uses
  Classes, SysUtils, dialogs, userdir, prefs, IniFiles, define_types;

const  //maximum number of control points for color schemes...
  maxNodes = 6;

type

  TLUT = array [0..255] of TRGBA;
  TLUTnodes = record
	isFreeSurfer: boolean; //make items brighter or darker than range transparent
	rgba: array [0..maxNodes] of TRGBA;
        intensity: array [0..maxNodes] of integer;
	end;

function UpdateTransferFunction (var lIndex: integer): TLUT;//change color table
function CLUTDir: string;

implementation

uses mainunit;

function CLUTDir: string;
begin
  //result := extractfilepath(paramstr(0))+'lut';
  result := AppDir+'lut'
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
 if (numnodes < 1) or (numnodes > 256) then begin
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

function UpdateTransferFunction (var lIndex: integer): TLUT;//change color table
var
 lLUTnodes :TLUTnodes;
 lInc,lNodeLo: integer;
 frac, f: single;
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

 if lLUTNodes.isFreeSurfer then exit; //not for freesurfer
 result[0].A := 0;
 for lInc := 1 to 255 do
     result[lInc].A := 255;
end;//LoadLUT()

end.

