unit colorTable;

{$ifdef fpc}{$mode delphi}{$endif}

interface
uses
  Classes, SysUtils, dialogs, userdir, prefs, IniFiles, define_types;

const  //maximum number of control points for color schemes...
  maxNodes = 100;
     kPaintHideDefaultBehavior = -1;
     kPaintHideDarkHideBright = 0;
     kPaintHideDarkShowBright = 1;
     kPaintShowDarkHideBright = 2;
     kPaintShowDarkShowBright = 3;

type

  TLUT = array [0..255] of TRGBA;
  TLUTnodes = record
	isFreeSurfer: boolean; //make items brighter or darker than range transparent
	rgba: array [0..maxNodes] of TRGBA;
        intensity: array [0..maxNodes] of integer;
	end;

function UpdateTransferFunction(lLUTnodes :TLUTnodes; isInvert: boolean): TLUT; overload;
function UpdateTransferFunction (lo, hi: TRGBA; isInvert: boolean): TLUT; overload;
//function UpdateTransferFunction (var lIndex: integer; fnm: string; isInvert: boolean): TLUT; overload;
function UpdateTransferFunction (fnm: string; isInvert: boolean): TLUT; overload;
function UpdateTransferFunction (var lIndex: integer; isInvert: boolean): TLUT; overload;//change color table
function CLUTDir: string;
function blendRGBA(c1, c2: TRGBA ): TRGBA;
function blendRGBAover(ca, cb: TRGBA ): TRGBA;
function maxRGBA(c1, c2: TRGBA ): TRGBA;
//function inten2rgb(intensity, mn, mx: single; lut: TLUT): TRGBA;
function inten2rgb(intensity, mn, mx: single; lut: TLUT; mode: integer): TRGBA; overload;
//function inten2rgb(intensity, mn, mx: single; lut: TLUT): TRGBA; overload;
function inten2rgb1(intensity, mn, mx: single; lut: TLUT): TRGBA; //use 1st not 0th LUT color (0 is transparent)
function desaturateRGBA ( rgba: TRGBA; frac: single; alpha: byte): TRGBA;
function isFreeSurferLUT(lIndex: integer): boolean;

implementation

uses mainunit;

function isFreeSurferLUT(lIndex: integer): boolean;
begin
     result := (lIndex >= 15) and (lIndex <= 18);
end;

function blendRGBAover(ca, cb: TRGBA ): TRGBA;
//https://en.wikipedia.org/wiki/Alpha_compositing#Analytical_derivation_of_the_over_operator
var
  aa, ab, ao: single;
begin
  if cb.A = 0 then exit(ca);
  if ca.A = 0 then exit(cb);
  aa := ca.A / 255;
  ab := cb.A / 255;
  ao := 1.0 - (1.0-aa)*(1.0-ab);
  result.R := round(((aa*ca.r)+(1-aa)*ab*cb.r)/ao) ;
  result.G := round(((aa*ca.g)+(1-aa)*ab*cb.g)/ao) ;
  result.B := round(((aa*ca.b)+(1-aa)*ab*cb.b)/ao) ;
  result.A := round(ao * 255.0);
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

function inten2rgb(intensity, mn, mx: single; lut: TLUT; mode: integer): TRGBA; overload;
var
 i: byte;
 //isInvert : boolean;
begin
  if  (mn < 0) and (mx < 0) and (mode = kPaintHideDefaultBehavior) then begin
    if intensity >= mx then
      exit( lut[0])
    else if intensity <= mn then
      exit( lut[255])
    else
       exit( lut[round(255* (1.0-   (intensity-mn)/(mx-mn)))]);
  end;
  if intensity > mx then begin
     i := 255;
     if (mode = kPaintHideDarkHideBright) or (mode = kPaintShowDarkHideBright) then //hide bright
            i := 0;
  end else if intensity < mn then begin
     i := 0;
     if (mode = kPaintShowDarkHideBright) or (mode = kPaintShowDarkShowBright) then //hide dark
        i := 1;
  end else begin
      i := round(255*(intensity-mn)/(mx-mn));
      if (i = 0) and ((mode = kPaintHideDefaultBehavior) or (mode = kPaintShowDarkHideBright) or (mode = kPaintShowDarkShowBright)) then //hide dark
           i := 1;
  end;
  result := lut[i];
end;

function inten2rgb(intensity, mn, mx: single; lut: TLUT): TRGBA; overload;
begin
  //result := inten2rgb(intensity, mn, mx, lut, kPaintHideDefaultBehavior);
  result := inten2rgb(intensity, mn, mx, lut,kPaintHideDarkHideBright);
end;

(*function inten2rgb(intensity, mn, mx: single; lut: TLUT): TRGBA;
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
     else if intensity < mn then
        result := lut[0]
     else if intensity = mn then begin
        result := lut[0];
        result.A := lut[1].A;
     end else if intensity >= mx then
        result := lut[255]
     else
          result := lut[round(255*(intensity-mn)/(mx-mn))];
  end;
end; *)

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

procedure printf(s: string);
begin
{$ifdef Unix}
writeln(s);
{$endif}
end;

function CLUTDir: string;
begin
  //result := extractfilepath(paramstr(0))+'lut';
  result := ResourceDir+pathdelim+'lut';
  if DirectoryExists(result) then exit;
  result := AppDir+'lut';
  {$IFDEF UNIX}
  if DirectoryExists(result) then exit;
  result := '/usr/share/surfice/lut';
  if DirectoryExists(result) then exit;
  result := AppDir+'lut';
  {$ENDIF}
  printf('Unable to find "lut" resource folder');
end;

procedure setNode (r,g,b,a,i, node: integer; var  lLUTnodes : TLUTnodes);
begin
  lLUTnodes.rgba[node].R := r;
  lLUTnodes.rgba[node].G := g;
  lLUTnodes.rgba[node].B := b;
  lLUTnodes.rgba[node].A := a;
  lLUTnodes.intensity[node] := i;
end;

function loadCustomLUT(lFilename: string): TLUTnodes; overload;
var
 lIniFile: TIniFile;
 numnodes, i: integer;
 inten: byte;
 rgba: TRGBA;
begin
 setNode(0,0,0,0,0, 0, result);
 setNode(255,255,255,255,255, 1, result);
 result.isFreeSurfer:= false;
 lIniFile := TIniFile.Create(lFilename);
 IniInt(true,lIniFile, 'numnodes', numnodes);
 if (numnodes < 1) or (numnodes > maxNodes) then begin
   if (numnodes > maxNodes) then
      showmessage(format('Too many nodes (%d, maximum %d)', [numnodes, maxNodes]));
     lIniFile.Free;
    //lIndex := 0;
    exit;
 end;
   for i := 0 to (numnodes-1) do begin
     IniByte(true,lIniFile, 'nodeintensity'+inttostr(i),inten);
     IniRGBA(true,lIniFile, 'nodergba'+inttostr(i),rgba);
     setNode(rgba.r, rgba.g, rgba.b, rgba.a, inten, i, result);
   end;
 lIniFile.Free;
end;


function loadCustomLUT(var lIndex: integer): TLUTnodes; overload;
var
 lFilename: string;
 (*lIniFile: TIniFile;
 numnodes, i: integer;
 inten: byte;
 rgba: TRGBA; *)
begin
 setNode(0,0,0,0,0, 0, result);
 setNode(255,255,255,255,255, 1, result);
 result.isFreeSurfer:= false;
 if lIndex >= GLForm1.LayerColorDrop.Items.Count then
    lIndex := GLForm1.LayerColorDrop.Items.Count -1;
 if lIndex < 0 then
    lIndex :=  0;
 lFilename := CLUTdir+pathdelim+GLForm1.LayerColorDrop.Items[lIndex]+'.clut';
 if not fileexists(lFilename) then begin
    lIndex := 0;
    exit;
 end;
 result := loadCustomLUT(lFilename);
(* lIniFile := TIniFile.Create(lFilename);
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
 lIniFile.Free; *)
end;

function defaultLabelLut: TLUT;
function makeRGB (r,b,g: byte): TRGBA;
//linear interpolation
begin
  result.R := r;
  result.G := g;
  result.B := b;
end;//lerpRGBA()
var
   lut: TLUT;
   i: integer;
begin
    //lut[0] := makeRGB(fBackColor.r,fBackColor.g, fBackColor.b);
    lut[0].A := 0;
    lut[1] := makeRGB(71,46,154);
    lut[2] := makeRGB(33,78,43);
    lut[3] := makeRGB(192,199,10);
    lut[4] := makeRGB(32,79,207);
    lut[5] := makeRGB(195,89,204);
    lut[6] := makeRGB(208,41,164);
    lut[7] := makeRGB(173,208,231);
    lut[8] := makeRGB(233,135,136);
    lut[9] := makeRGB(202,20,58);
    lut[10] := makeRGB(25,154,239);
    lut[11] := makeRGB(210,35,30);
    lut[12] := makeRGB(145,21,147);
    lut[13] := makeRGB(89,43,230);
    lut[14] := makeRGB(87,230,101);
    lut[15] := makeRGB(245,113,111);
    lut[16] := makeRGB(246,191,150);
    lut[17] := makeRGB(38,147,35);
    lut[18] := makeRGB(3,208,128);
    lut[19] := makeRGB(25,37,57);
    lut[20] := makeRGB(57,28,252);
    lut[21] := makeRGB(167,27,79);
    lut[22] := makeRGB(245,86,173);
    lut[23] := makeRGB(86,203,120);
    lut[24] := makeRGB(227,25,25);
    lut[25] := makeRGB(208,209,126);
    lut[26] := makeRGB(81,148,81);
    lut[27] := makeRGB(64,187,85);
    lut[28] := makeRGB(90,139,8);
    lut[29] := makeRGB(199,111,7);
    lut[30] := makeRGB(140,48,122);
    lut[31] := makeRGB(48,102,237);
    lut[32] := makeRGB(212,76,190);
    lut[33] := makeRGB(180,110,152);
    lut[34] := makeRGB(70,106,246);
    lut[35] := makeRGB(120,130,182);
    lut[36] := makeRGB(9,37,130);
    lut[37] := makeRGB(192,160,219);
    lut[38] := makeRGB(245,34,67);
    lut[39] := makeRGB(177,222,76);
    lut[40] := makeRGB(65,90,167);
    lut[41] := makeRGB(157,165,178);
    lut[42] := makeRGB(9,245,235);
    lut[43] := makeRGB(193,222,250);
    lut[44] := makeRGB(100,102,28);
    lut[45] := makeRGB(181,47,61);
    lut[46] := makeRGB(125,19,186);
    lut[47] := makeRGB(145,130,250);
    lut[48] := makeRGB(62,4,199);
    lut[49] := makeRGB(8,232,67);
    lut[50] := makeRGB(108,137,58);
    lut[51] := makeRGB(36,211,50);
    lut[52] := makeRGB(140,240,86);
    lut[53] := makeRGB(237,11,182);
    lut[54] := makeRGB(242,140,108);
    lut[55] := makeRGB(248,21,77);
    lut[56] := makeRGB(161,42,89);
    lut[57] := makeRGB(189,22,112);
    lut[58] := makeRGB(41,241,59);
    lut[59] := makeRGB(114,61,125);
    lut[60] := makeRGB(65,99,226);
    lut[61] := makeRGB(121,115,50);
    lut[62] := makeRGB(97,199,205);
    lut[63] := makeRGB(50,166,227);
    lut[64] := makeRGB(238,114,125);
    lut[65] := makeRGB(149,190,128);
    lut[66] := makeRGB(44,204,104);
    lut[67] := makeRGB(214,60,27);
    lut[68] := makeRGB(124,233,59);
    lut[69] := makeRGB(167,66,66);
    lut[70] := makeRGB(40,115,53);
    lut[71] := makeRGB(167,230,133);
    lut[72] := makeRGB(127,125,159);
    lut[73] := makeRGB(178,103,203);
    lut[74] := makeRGB(231,203,97);
    lut[75] := makeRGB(30,125,125);
    lut[76] := makeRGB(173,13,139);
    lut[77] := makeRGB(244,176,159);
    lut[78] := makeRGB(193,94,158);
    lut[79] := makeRGB(203,131,7);
    lut[80] := makeRGB(204,39,215);
    lut[81] := makeRGB(238,198,47);
    lut[82] := makeRGB(139,167,140);
    lut[83] := makeRGB(135,124,226);
    lut[84] := makeRGB(71,67,223);
    lut[85] := makeRGB(234,175,231);
    lut[86] := makeRGB(234,254,44);
    lut[87] := makeRGB(217,1,110);
    lut[88] := makeRGB(66,15,184);
    lut[89] := makeRGB(14,198,61);
    lut[90] := makeRGB(129,62,233);
    lut[91] := makeRGB(19,237,47);
    lut[92] := makeRGB(97,159,67);
    lut[93] := makeRGB(165,31,148);
    lut[94] := makeRGB(112,218,22);
    lut[95] := makeRGB(244,58,120);
    lut[96] := makeRGB(35,244,173);
    lut[97] := makeRGB(73,47,156);
    lut[98] := makeRGB(192,61,117);
    lut[99] := makeRGB(12,67,181);
    lut[100] := makeRGB(149,94,94);
    for i := 1 to 100 do
        lut[i+100] := lut[i]; //fill 101..200
    for i := 1 to 55 do
        lut[i+200] := lut[i]; //fill 201..255
    result := lut;
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
       15: begin //FreeSurferCurve - valleys dark
          result.isFreeSurfer:= true;
          setNode(  0,  0,  0,  0,  0, 0, result);
          setNode(  0,  0,  0,  0,136, 1, result);
          setNode(  0,  0,  0,140,155, 2, result);
          setNode(  0,  0,  0,200,255, 3, result);
       end;
       16: begin //FreeSurferCurve  - curves (valleys and ridges) dark
         result.isFreeSurfer:= true;
            setNode(0,0,0,255,0, 0, result);
          setNode(0,0,0,0,100, 1, result);
          setNode(0,0,0,0,156, 2, result);
          setNode(0,0,0,255,255, 3, result);
       end;
       17: begin //FreeSurferCurve - flat surfaces darkened
         result.isFreeSurfer:= true;
         setNode(0,0,0,0,0, 0, result);
       setNode(0,0,0,0,100, 1, result);
       setNode(0,0,0,255,128, 2, result);
       setNode(0,0,0,0,156, 3, result);
       setNode(0,0,0,0,255, 4, result);
       end;
       18: begin //FreeSurferCurve  - ridges dark
         result.isFreeSurfer:= true;
         setNode(0,0,0,255,0, 0, result);
         setNode(0,0,0,0,100,1, result);
         setNode(0,0,0,0,255, 2, result);
       end;
       19: begin //Random Label
         result.isFreeSurfer:= false;
         setNode(7,7,7,255,255, 0, result);
         setNode(0,0,0,0,100,1, result);
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

function UpdateTransferFunction (fnm: string; isInvert: boolean): TLUT; overload;//change color table
var
   lLUTNodes :TLUTnodes;
begin
     lLUTNodes := loadCustomLUT(fnm);
	 result := UpdateTransferFunction(lLUTNodes, isInvert);
end;

function UpdateTransferFunction (var lIndex: integer; isInvert: boolean): TLUT; overload;//change color table
var
   lLUTNodes :TLUTnodes;
begin
     lLUTNodes := makeLUT(lIndex);
	 result := UpdateTransferFunction(lLUTNodes, isInvert);
end;

function UpdateTransferFunction (lo, hi: TRGBA; isInvert: boolean): TLUT; overload;
var
   lLUTNodes :TLUTnodes;
   i: integer = 0;
begin
  lLUTNodes := makeLUT(i);
  setNode(lo.r,lo.g,lo.b,0,0, 0, lLUTNodes);
  setNode(hi.r,hi.g,hi.b,255,255, 1, lLUTNodes);
  //lLUTNodes.isFreeSurfer:= false;
  result := UpdateTransferFunction(lLUTnodes, isInvert);
end;

function UpdateTransferFunction(lLUTnodes :TLUTnodes; isInvert: boolean): TLUT; overload;
label
     123;
var
 lInc,lNodeLo: integer;
 frac, f: single;
 rev: TLUT;
begin
 (*if fnm <> '' then
    lLUTNodes := loadCustomLUT(fnm)
 else
 	 lLUTNodes := makeLUT(lIndex);*)
 if (lLUTNodes.rgba[0].R = 7) and  (lLUTNodes.rgba[0].G = 7) and  (lLUTNodes.rgba[0].B = 7) and  (lLUTNodes.rgba[0].A = 255) and (lLUTNodes.intensity[0] = 255) then begin
    result := defaultLabelLut;
    goto 123;
 end;
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
123:
 if isInvert then begin
    rev := result;
    for lInc := 0 to 255 do
        result[lInc] := rev[255-lInc];
    result[0].A := rev[0].A;
    result[255].A := rev[255].A;
 end;
 if lLUTNodes.isFreeSurfer then begin
   exit; //not for freesurfer
 end;
 //result[0].A := 0; //see LUT[0].A <> 0
 for lInc := 1 to 255 do
     result[lInc].A := 255;
end;

(*function UpdateTransferFunction (var lIndex: integer; fnm: string; isInvert: boolean): TLUT; overload;
label
     123;
var
 lLUTnodes :TLUTnodes;
 lInc,lNodeLo: integer;
 frac, f: single;
 rev: TLUT;
begin
 if fnm <> '' then
    lLUTNodes := loadCustomLUT(fnm)
 else
 	 lLUTNodes := makeLUT(lIndex);
 if (lLUTNodes.rgba[0].R = 7) and  (lLUTNodes.rgba[0].G = 7) and  (lLUTNodes.rgba[0].B = 7) and  (lLUTNodes.rgba[0].A = 255) and (lLUTNodes.intensity[0] = 255) then begin
    result := defaultLabelLut;
    goto 123;
 end;
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
123:
 if isInvert then begin
    rev := result;
    for lInc := 0 to 255 do
        result[lInc] := rev[255-lInc];
    result[0].A := rev[0].A;
    result[255].A := rev[255].A;
 end;
 if lLUTNodes.isFreeSurfer then begin
   exit; //not for freesurfer
 end;
 //result[0].A := 0; //see LUT[0].A <> 0
 for lInc := 1 to 255 do
     result[lInc].A := 255;
end;//LoadLUT() *)

end.

