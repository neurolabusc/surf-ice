unit prefs;
{$D-,O+,Q-,R-,S-}   //Delphi L-,Y-
{$H+}

interface
uses IniFiles,SysUtils,define_types,graphics,Dialogs,Classes;
const
  knMRU = 10;
  kRenderPoor = 0;
  kRenderBetter = 1;
  //kRenderBest = 2;
  kSaveAsObj = 0;
  kSaveAsGii = 1;
  kSaveAsMz3 = 2;

type
  TMRU =  array [1..knMRU] of string;
  TPrefs = record
    SupportBetterRenderQuality, AdditiveOverlay,Perspective, OrientCube,
     TracksAreTubes,Colorbar, ScreenCaptureTransparentBackground,
     ZDimIsUp, SmoothVoxelwiseData, ShaderForBackgroundOnly: boolean;
    window_width, window_height, RenderQuality, SaveAsFormat: integer;
    ObjColor,BackColor: TColor;
    PrevFilename: TMRU;
    PrevTrackname, PrevNodename, PrevOverlayname : string;
    TextColor,TextBorder,GridAndBorder: TRGBA;
    ColorBarPos: TUnitRect;
  end;
function IniFile(lRead: boolean; lFilename: string; var lPrefs: TPrefs): boolean;
procedure IniByte(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: byte);
procedure IniInt(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: integer);
procedure IniFloat(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: single);
procedure IniColor(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: TColor);
procedure SetDefaultPrefs (var lPrefs: TPrefs; lEverything: boolean);
procedure IniRGBA(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: TRGBA);

implementation

procedure IniFloat(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: single);
//read or write an integer value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('FLT',lIdent,FloattoStr(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('FLT',lIdent, '');
	if length(lStr) > 0 then
		lValue := StrToFloat(lStr);
end; //IniFloat

procedure IniByte(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: byte);
//read or write an integer value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('BYT',lIdent,InttoStr(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('BYT',lIdent, '');
	if length(lStr) > 0 then
		lValue := StrToInt(lStr);
end; //IniFloat

procedure SetDefaultPrefs (var lPrefs: TPrefs; lEverything: boolean);
begin
  if lEverything then begin  //These values are typically not changed...
       with lPrefs do begin
            //CrossHairs := true;
            ObjColor := RGBToColor(212, 168, 168);
            BackColor := RGBToColor(0, 0, 0);
            TextColor := RGBA(255,255,255,255);
            TextBorder := RGBA(92,92,132,255);
            GridAndBorder := RGBA(106,106,142,222);
            ColorBarPos:= CreateUnitRect (0.1,0.1,0.9,0.14);
       end;
  end;
  with lPrefs do begin
    RenderQuality := kRenderBetter;
    SupportBetterRenderQuality := false;
    Colorbar := false;
    ZDimIsUp := true;
    ShaderForBackgroundOnly := false;
    TracksAreTubes := true;
    SaveAsFormat := kSaveAsObj;
    //MultiPassRendering := true;
    OrientCube := true;
    Perspective := false;
    AdditiveOverlay := false;
    ScreenCaptureTransparentBackground := true;
    SmoothVoxelwiseData := true;
    PrevTrackname := '';
    PrevOverlayname := '';
    PrevNodename := '';
  end;//with lPrefs
end; //Proc SetDefaultPrefs

procedure SetDefaultPrefsMRU (var lPrefs: TPrefs);
var
  lI: integer;
begin
    SetDefaultPrefs(lPrefs,true);
    for lI := 1 to knMRU do begin
      lPrefs.PrevFilename[lI] := '';
    end;
end;

procedure IniInt(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: integer);
//read or write an integer value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('INT',lIdent,IntToStr(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('INT',lIdent, '');
	if length(lStr) > 0 then
		lValue := StrToInt(lStr);
end; //IniInt

function Bool2Char (lBool: boolean): char;
begin
	if lBool then
		result := '1'
	else
		result := '0';
end;

function Char2Bool (lChar: char): boolean;
begin
	if lChar = '1' then
		result := true
	else
		result := false;
end;

procedure IniBool(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: boolean);
//read or write a boolean value to the initialization file
var
	lStr: string;
begin
        if not lRead then begin
           lIniFile.WriteString('BOOL',lIdent,Bool2Char(lValue));
           exit;
        end;
	lStr := lIniFile.ReadString('BOOL',lIdent, '');
	if length(lStr) > 0 then
		lValue := Char2Bool(lStr[1]);
end; //IniBool

procedure IniStr(lRead: boolean; lIniFile: TIniFile; lIdent: string; var lValue: string);
//read or write a string value to the initialization file
begin
  if not lRead then begin
    lIniFile.WriteString('STR',lIdent,lValue);
    exit;
  end;
	lValue := lIniFile.ReadString('STR',lIdent, '');
end; //IniStr

const
  kStrSep = '|';

function RGBToStr (lU: TColor) : string;
begin
  result := Inttostr(red(lU))+ kStrSep+Inttostr(green(lU))+ kStrSep+Inttostr(blue(lU));
end;

function StrToRGB(lS: string; var lU: TColor): boolean;
var
 strlst:TStringList;
begin
  result := false;
  strlst:=TStringList.Create;
  strlst.Delimiter:=kStrSep;
  strlst.DelimitedText := lS;
  if strlst.Count > 2 then begin
     lU := RGBToColor( strtoint(strlst[0]), strtoint(strlst[1]), strtoint(strlst[2]))
  end;
  strlst.free;
end;

procedure IniColor(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: TColor);
var
	lStr: string;
begin
  if not lRead then begin
    lIniFile.WriteString('RGB255',lIdent,RGBToStr(lValue));
    exit;
  end;
  lStr := lIniFile.ReadString('RGB255',lIdent, '');
  StrToRGB(lStr,lValue);
end; //IniRGBA

procedure IniMRU(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lMRU: TMRU);
var
	lI,lOK: integer;
function Novel: boolean;
var
  lX: integer;
begin
  if lI < 2 then begin
    result := true;
    exit;
  end;
   result := false;
   for lX := 1 to (lI-1) do
    if lMRU[lX] = lMRU[lI] then
      exit;
   result := true;
end;
begin
  if lRead then begin //compress files so lowest values are OK
    lOK := 0;
    for lI := 1 to knMRU do begin
      IniStr(lRead,lIniFile,lIdent+inttostr(lI),lMRU[lI]);
	    if (length(lMRU[lI]) > 0) and (fileexists(lMRU[lI])) and (Novel) then begin
		    inc(lOK);
		    lMRU[lOK] := lMRU[lI];
      end else
        lMRU[lI] := '';
	  end; //for each MRU
  end else
	  for lI := 1 to knMRU do
      IniStr(lRead,lIniFile,lIdent+inttostr(lI),lMRU[lI]); //write values
end;

function RGBAToStr (lU: tRGBA) : string;
//floatrect values 0..1 convert to byte 0..1
begin
  result := Inttostr(lU.r)+ kStrSep+Inttostr(lU.g)+ kStrSep+Inttostr(lU.b)+ kStrSep+Inttostr(lU.a);
end;

function StrToRGBA(lS: string; var lU: TRGBA): boolean;
var
 strlst:TStringList;
begin
  result := false;
  strlst:=TStringList.Create;
  strlst.Delimiter:=kStrSep;
  strlst.DelimitedText := lS;
  if strlst.Count > 3 then begin
     lU := RGBA( strtoint(strlst[0]), strtoint(strlst[1]), strtoint(strlst[2]), strtoint(strlst[3])) ;
     result := true;
  end;
  strlst.free;
end;

procedure IniRGBA(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: TRGBA);
//read or write an integer value to the initialization file
var
	lStr: string;
begin
  if not lRead then begin
    //lI64 := lValue.rgbred + lValue.rgbGreen shl 8 + lValue.rgbBlue shl 16 + lValue.rgbReserved shl 24;
    //lIniFile.WriteString('RGBA',lIdent,InttoStr(lI64));
    lIniFile.WriteString('RGBA255',lIdent,RGBAToStr(lValue));
    exit;
  end;
	lStr := lIniFile.ReadString('RGBA255',lIdent, '');
  StrToRGBA(lStr,lValue);
end; //IniRGBA

function UnitToByteStr (lS: single): string;
begin
  result := inttostr(round(255 * UnitBound(lS)));
end;

function UnitRectToStr (lU: TUnitRect) : string;
//floatrect values 0..1 convert to byte 0..1
begin
  result := UnitToByteStr(lU.L)+ kStrSep+UnitToByteStr(lU.T)+ kStrSep+UnitToByteStr(lU.R)+ kStrSep+UnitToByteStr(lU.B);
end;

function StrToUnitRect (lS: string; var lU: TUnitRect): boolean;
var
  lQ : TRGBA;
begin
  result := false;
  if not StrToRGBA(lS,lQ) then
    exit;
  lU.L := lQ.r / 255;
  lU.T := lQ.g / 255;
  lU.R := lQ.b / 255;
  lU.B := lQ.a / 255;
  result := true;
end;

procedure IniUnitRect(lRead: boolean; lIniFile: TIniFile; lIdent: string; var lValue: TUnitRect);
var
  lS: string;
  lU: TUnitRect;
begin
  if not lRead then begin
    lIniFile.WriteString('STR',lIdent,UnitRectToStr (lValue));
    exit;
  end;
  lS := lIniFile.ReadString('STR',lIdent, '');
  if StrToUnitRect (lS, lU) then
    lValue := lU;
end; //IniUnitRect

function IniFile(lRead: boolean; lFilename: string; var lPrefs: TPrefs): boolean;
//Read or write initialization variables to disk
var
  lIniFile: TIniFile;
begin
  result := false;
  if (lRead) then
    SetDefaultPrefsMRU (lPrefs);
  if (lRead) and (not Fileexists(lFilename)) then begin
        exit;
  end;
  lIniFile := TIniFile.Create(lFilename);
  IniBool(lRead,lIniFile, 'OrientCube',lPrefs.OrientCube);
  IniBool(lRead,lIniFile, 'Perspective',lPrefs.Perspective);
  IniBool(lRead,lIniFile, 'AdditiveOverlay',lPrefs.AdditiveOverlay);
  //IniBool(lRead,lIniFile, 'MultiPassRendering',lPrefs.MultiPassRendering);
  //IniBool(lRead,lIniFile, 'SaveAsObj',lPrefs.SaveAsObj);
  IniBool(lRead,lIniFile, 'TracksAreTubes',lPrefs.TracksAreTubes);
  IniBool(lRead,lIniFile, 'ZDimIsUp',lPrefs.ZDimIsUp);
  IniBool(lRead,lIniFile, 'ShaderForBackgroundOnly',lPrefs.ShaderForBackgroundOnly);
  IniBool(lRead,lIniFile, 'Colorbar',lPrefs.Colorbar);
  IniBool(lRead,lIniFile, 'SmoothVoxelwiseData',lPrefs.SmoothVoxelwiseData);
  IniBool(lRead,lIniFile, 'ScreenCaptureTransparentBackground',lPrefs.ScreenCaptureTransparentBackground);
  IniColor(lRead,lIniFile, 'ObjColor',lPrefs.ObjColor);
  IniColor(lRead,lIniFile, 'BackColor',lPrefs.BackColor);
  IniStr(lRead, lIniFile, 'PrevTrackname', lPrefs.PrevTrackname);
  IniStr(lRead, lIniFile, 'PrevNodename', lPrefs.PrevNodename);
  IniStr(lRead, lIniFile, 'PrevOverlayname', lPrefs.PrevOverlayname);
  IniMRU(lRead,lIniFile,'PrevFilename',lPrefs.PrevFilename);
  IniRGBA(lRead,lIniFile, 'TextColor',lPrefs.TextColor);
  IniRGBA(lRead,lIniFile, 'TextBorder',lPrefs.TextBorder);
  IniRGBA(lRead,lIniFile, 'GridAndBorder',lPrefs.GridAndBorder);
  IniUnitRect(lRead,lIniFile, 'ColorBarPos',lPrefs.ColorBarPos);
  IniInt(lRead,lIniFile,'RenderQuality',lPrefs.RenderQuality);
  IniInt(lRead,lIniFile,'SaveAsFormat',lPrefs.SaveAsFormat);
  if (lPrefs.RenderQuality < kRenderPoor) then lPrefs.RenderQuality:= kRenderPoor;
  if (lPrefs.RenderQuality > kRenderBetter) then lPrefs.RenderQuality:= kRenderBetter;
  lIniFile.Free;
end;

end.
