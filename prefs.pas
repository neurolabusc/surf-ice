unit prefs;
{$D-,O+,Q-,R-,S-}   //Delphi L-,Y-
{$H+}

interface
uses {$ifndef isTerminalApp}graphics,Dialogs,{$endif}IniFiles,SysUtils,define_types,Classes;  //,
const
  knMRU = 10;
  kRenderPoor = 0;
  kRenderBetter = 1;
  //kRenderBest = 2;
  kSaveAsObj = 0;
  kSaveAsGii = 1;
  kSaveAsMz3 = 2;
  kSaveAsTrackVtk = 0;
  kSaveAsTrackBfloat = 1;
  kSaveAsTrackTrk = 2;


type
  TMRU =  array [1..knMRU] of string;
  TPrefs = record
    SmoothVoxelwiseData,
    OverlayClip, StartupScript, SupportBetterRenderQuality, AdditiveOverlay,Perspective, OrientCube, MultiSample,
     TracksAreTubes,Colorbar, ScreenCaptureTransparentBackground,LoadTrackOnLaunch,ColorBarPrecedenceTracksNotOverlays,
     ZDimIsUp,  ShaderForBackgroundOnly, CoreTrackDisableDepth, SkipPrefWriting, isFlipMeshOverlay : boolean;
    TrackTubeSlices, ScreenCaptureZoom,
    window_width, window_height, RenderQuality, SaveAsFormat,SaveAsFormatTrack, OcclusionAmount: integer;
    ObjColor,BackColor: TColor;
    PrevFilename, PrevScriptName: TMRU;
    //{$IFDEF Darwin}BaseDirname,  {$ENDIF} //OSX Sierra puts application in random directory, so 'Sample' 'BrainNet' folders will be in unknown locations
    PrevTrackname, PrevNodename, PrevOverlayname,PrevScript, InitScript : string;
    TextColor,TextBorder,GridAndBorder: TRGBA;
    ColorBarPos: TUnitRect;
    ScreenPan: TPoint3f;
  end;
function IniFile(lRead: boolean; lFilename: string; var lPrefs: TPrefs): boolean;
procedure IniByte(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: byte);
procedure IniInt(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: integer);
procedure IniFloat(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: single);
procedure IniColor(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: TColor);
procedure SetDefaultPrefs (var lPrefs: TPrefs; lEverything: boolean);
procedure IniRGBA(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lValue: TRGBA);
procedure FillMRU (var lMRU: TMRU; lSearchPath,lSearchExt: string; lForce: boolean);
procedure Add2MRU (var lMRU: TMRU;  lNewFilename: string); //add new file to most-recent list

implementation

uses userdir;

function IsNovel (lName: string; var lMRU: TMRU; lnOK: integer):boolean;
var lI,lN: integer;
begin
  result := false;
  lN := lNOK;
  if lnOK > knMRU then
    lN := knMRU;
  if lN < 1 then begin
    result := true;
    exit;
  end;
  for lI := 1 to lN do
    if lMRU[lI] = lName then
      exit;
  result := true;
end;

procedure Add2MRU (var lMRU: TMRU;  lNewFilename: string); //add new file to most-recent list
var
  lNewM,lOldM,lStr: string;
  lPos,lN : integer;
begin
  lNewM := extractfilename(lNewFilename);
  //first, increase position of all old MRUs
  lN := 0; //Number of MRU files
  for lPos := 1 to (knMRU) do begin//first, eliminate duplicates
	  lStr := lMRU[lPos];
          lOldM := extractfilename(lStr);
          if (lStr <> '') {and (lStr <> lNewFileName)} and (lNewM <> lOldM) then begin
             inc(lN);
	     lMRU[lN] := lStr;
	  end; //keep in MRU list
  end; //for each MRU
  //next, increment positions
  if lN >= knMRU then
	 lN := knMRU - 1;
  for lPos := lN downto 1 do
	  lMRU[lPos+1] := lMRU[lPos];
  if (lN+2) < (knMRU) then //+1 as we have added a file
	 for lPos := (lN+2) to knMRU do
	   lMRU[lPos] := '';
  lMRU[1] := lNewFilename;
end;//Add2MRU

procedure FillMRU (var lMRU: TMRU; lSearchPath,lSearchExt: string; lForce: boolean);
//e.g. SearchPath  includes final pathdelim, e.g. c:\filedir\
var
	lSearchRec: TSearchRec;
  lI,lMax,lOK: integer;
  lS: TStringList;
begin
  lOK := 0;
  if (not lForce) and (lMRU[1] <> '') then begin
      //exit; //only fill empty MRUs...
      for lI := 1 to knMRU do begin
        if (lMRU[lI] <> '') and (fileexists(lMRU[lI])) and (IsNovel (lMRU[lI], lMRU, lOK)) then begin
          inc(lOK);
          lMRU[lOK] := lMRU[lI];
        end; //if file exists
      end; //for each MRU
      if lOK = knMRU then
        exit; //all slots filled;
      for lI := (lOK+1) to knMRU do
        lMRU[lI] :=  '';//empty slot
  end; //check exisiting MRUs
  lS := TStringList.Create;
  if FindFirst(lSearchPath+'*'+lSearchExt, faAnyFile, lSearchRec) = 0 then
     repeat
      if (lSearchRec.Name <> '') and (lSearchRec.Name[1] <> '.') and IsNovel (lSearchPath+lSearchRec.Name, lMRU, lOK) then
         lS.Add(lSearchPath+lSearchRec.Name) ;
     until (FindNext(lSearchRec) <> 0);
  FindClose(lSearchRec);
  lMax := lS.count;

  if lMax > 0 then begin
    lS.sort;
    if lMax > knMRU then
      lMax := knMRU;
    for lI := (lOK+1) to lMax do
      lMRu[lI] := lS[lI-1];
  end;
  Freeandnil(lS);
end;//UpdateLUT

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

const
  kNumDefaultMesh = 6;
function DefaultMeshName (indx: integer): string;
  var
    lPath,lName {$IFDEF Darwin}, lExt {$ENDIF}: string;
  begin
   lPath := ExtractFileDir(AppDir2());
   lPath := lPath + PathDelim;
   case indx of
        2: lName := lPath  +  'BrainNet'+ PathDelim + 'BrainMesh_ICBM152_smoothed.mz3';
        3: lName := lPath  + 'BrainNet'+ PathDelim + 'BrainMesh_ICBM152Left_smoothed.mz3';
        4: lName := lPath  +  'BrainNet'+ PathDelim + 'BrainMesh_ICBM152Right_smoothed.mz3';
        5:  lName := lPath  +  'fs'+ PathDelim + 'lh.inflated';
        6:  lName := lPath  +  'fs'+ PathDelim + 'lh.pial';
    else
          lName := lPath  + 'sample'+ PathDelim + 'mni152_2009.mz3';
   end;
   if fileexists(lName) then
     result := lName
   else
       result := '';
  end;

(* below: basic outline for OSX Sierra - unused since we now code sign a DMG
function DefaultMeshName (indx: integer; var lPrefs: TPrefs): string;
var
  lPath,lName {$IFDEF Darwin}, lExt {$ENDIF}: string;
  {$IFDEF Darwin}
  dirDlg : TSelectDirectoryDialog;
  {$ENDIF}
begin
 lPath := ExtractFileDir(AppDir());
 {$IFDEF Darwin} //OSX Sierra: application in randomized location on your drive https://9to5mac.com/2016/06/15/macos-sierra-gatekeeper-changes/
 if (length(lPrefs.BaseDirname) < 1) or (not fileexists(lPrefs.BaseDirname)) then begin

    FilenameParts (lPath, lPath,lName,lExt) ;
    lName := lPath  + 'sample';
    showmessage(lName);
    if not fileexists(lName) then begin
      dirDlg := TSelectDirectoryDialog.Create(nil);
      dirDlg.Title:='Please select "sample" folder';
      if dirDlg.Execute then begin
         //lName := dirDlg.FileName;
         FilenameParts (dirDlg.FileName, lPath,lName,lExt) ;
         lName := lPath;
      end;
      dirDlg.Free;
    end;
    lPrefs.BaseDirname := lName;
 end else
     lPath := lPrefs.BaseDirname;
 {$ELSE}
 lPath := lPath + PathDelim;
 {$ENDIF}
 case indx of
      2: lName := lPath  +  'BrainNet'+ PathDelim + 'BrainMesh_ICBM152_smoothed.mz3';
      3: lName := lPath  + 'BrainNet'+ PathDelim + 'BrainMesh_ICBM152Left_smoothed.mz3';
      4: lName := lPath  +  'BrainNet'+ PathDelim + 'BrainMesh_ICBM152Right_smoothed.mz3';
      5:  lName := lPath  +  'fs'+ PathDelim + 'lh.inflated';
      6:  lName := lPath  +  'fs'+ PathDelim + 'lh.pial';

      else
        lName := lPath  + 'sample'+ PathDelim + 'mni152_2009.mz3';
 end;
 if fileexists(lName) then
   result := lName
 else
     result := '';
end; *)

procedure SetDefaultPrefs (var lPrefs: TPrefs; lEverything: boolean);
var
   i: integer;
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
            ScreenPan.X := 0; ScreenPan.Y := 0; ScreenPan.Z := 0;
            for i := 1 to knMRU do  begin
              PrevFilename[i] := '';
              PrevScriptName[i] := '';
            end;
            //lPrefs.BaseDirName := '';
            for i := 1 to kNumDefaultMesh do
                PrevFilename[i] := DefaultMeshName(i);
       end;
  end;
  with lPrefs do begin
    RenderQuality := kRenderBetter;
    ColorBarPrecedenceTracksNotOverlays := false;
    SupportBetterRenderQuality := false;
    Colorbar := false;
    ZDimIsUp := true;
    ShaderForBackgroundOnly := false;
    CoreTrackDisableDepth := false;
    LoadTrackOnLaunch := false;
    TracksAreTubes := true;
    TrackTubeSlices := 5;
    ScreenCaptureZoom := 3;
    SaveAsFormat := kSaveAsObj;
    SaveAsFormatTrack := kSaveAsTrackVtk;
    OcclusionAmount := 25;
    //MultiPassRendering := true;
    MultiSample := true;
    OrientCube := true;
    Perspective := false;
    AdditiveOverlay := false;
    SkipPrefWriting := false;
    isFlipMeshOverlay := false;
    OverlayClip := false;
    StartupScript := false;
    ScreenCaptureTransparentBackground := true;
    SmoothVoxelwiseData := true;
    PrevTrackname := '';
    PrevOverlayname := '';
    PrevScript := '';
    PrevNodename := '';
  end;//with lPrefs
end; //Proc SetDefaultPrefs

(*procedure SetDefaultPrefsMRU (var lPrefs: TPrefs);
var
  lI: integer;
begin
    SetDefaultPrefs(lPrefs,true);
    for lI := 1 to knMRU do begin
      lPrefs.PrevFilename[lI] := '';
    end;
end; *)

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

procedure IniMRU(lRead: boolean; lIniFile: TIniFile; lIdent: string;  var lMRU: TMRU; var lPrefs: TPrefs);
var
	lI,lOK: integer;
        lStr: string;
function Novel: boolean;
var
  lX: integer;
begin
   result  := true;
   if (lOK < 1) or (lOK > knMRU) then exit;
   result := false;
   for lX := 1 to lOK do
    if lMRU[lX] = lStr then
      exit;
   result := true;
end;
begin
  if lRead then begin //compress files so lowest values are OK
    lOK := 0;
    for lI := 1 to knMRU do begin
      IniStr(lRead,lIniFile,lIdent+inttostr(lI),lStr);
      if (length(lStr) > 0) and (fileexists(lStr)) and (Novel) then begin
		    inc(lOK);
		    lMRU[lOK] := lStr;
      end else
        lMRU[lI] := '';
      end; //for each MRU
      //file empty slots
      lI := 0;
      while (lOK < knMRU) and (lI < kNumDefaultMesh) do begin
        lI := lI + 1;
        lStr := DefaultMeshName(lI);
        if (length(lStr) > 0) and (fileexists(lStr)) and (Novel) then begin
		    inc(lOK);
		    lMRU[lOK] := lStr;
        end;
      end;
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
    SetDefaultPrefs (lPrefs, true);
  if (not lRead) and (lPrefs.SkipPrefWriting) then exit;
  if (lRead) and (not Fileexists(lFilename)) then
        exit;
  lIniFile := TIniFile.Create(lFilename);
  IniBool(lRead,lIniFile, 'MultiSample',lPrefs.MultiSample);
  IniBool(lRead,lIniFile, 'OrientCube',lPrefs.OrientCube);
  IniBool(lRead,lIniFile, 'Perspective',lPrefs.Perspective);
  IniBool(lRead,lIniFile, 'AdditiveOverlay',lPrefs.AdditiveOverlay);
   IniBool(lRead,lIniFile, 'StartupScript',lPrefs.StartupScript);
  //IniBool(lRead,lIniFile, 'MultiPassRendering',lPrefs.MultiPassRendering);
  //IniBool(lRead,lIniFile, 'SaveAsObj',lPrefs.SaveAsObj);
  IniBool(lRead,lIniFile, 'TracksAreTubes',lPrefs.TracksAreTubes);
  IniBool(lRead,lIniFile, 'ZDimIsUp',lPrefs.ZDimIsUp);
  //IniBool(lRead,lIniFile, 'ShaderForBackgroundOnly',lPrefs.ShaderForBackgroundOnly);
  IniBool(lRead,lIniFile, 'CoreTrackDisableDepth',lPrefs.CoreTrackDisableDepth);
  IniBool(lRead,lIniFile, 'LoadTrackOnLaunch',lPrefs.LoadTrackOnLaunch);
  IniBool(lRead,lIniFile, 'Colorbar',lPrefs.Colorbar);
  IniBool(lRead,lIniFile, 'SmoothVoxelwiseData',lPrefs.SmoothVoxelwiseData);
  IniBool(lRead,lIniFile, 'ScreenCaptureTransparentBackground',lPrefs.ScreenCaptureTransparentBackground);
  IniColor(lRead,lIniFile, 'ObjColor',lPrefs.ObjColor);
  IniColor(lRead,lIniFile, 'BackColor',lPrefs.BackColor);
  IniStr(lRead, lIniFile, 'PrevTrackname', lPrefs.PrevTrackname);
  IniStr(lRead, lIniFile, 'PrevNodename', lPrefs.PrevNodename);
  IniStr(lRead, lIniFile, 'PrevOverlayname', lPrefs.PrevOverlayname);
  IniStr(lRead,lIniFile,'PrevScript',lPrefs.PrevScript);
  IniMRU(lRead,lIniFile,'PrevFilename',lPrefs.PrevFilename, lPrefs);
  //IniMRU(lRead,lIniFile,'PrevScriptName',lPrefs.PrevScriptName);
  IniRGBA(lRead,lIniFile, 'TextColor',lPrefs.TextColor);
  IniRGBA(lRead,lIniFile, 'TextBorder',lPrefs.TextBorder);
  IniRGBA(lRead,lIniFile, 'GridAndBorder',lPrefs.GridAndBorder);
  IniUnitRect(lRead,lIniFile, 'ColorBarPos',lPrefs.ColorBarPos);
  IniInt(lRead,lIniFile,'TrackTubeSlices',lPrefs.TrackTubeSlices);
  IntBound(lPrefs.TrackTubeSlices, 3,13);
  IniInt(lRead,lIniFile,'ScreenCaptureZoom',lPrefs.ScreenCaptureZoom);
  IntBound(lPrefs.ScreenCaptureZoom, 1,7);
  IniInt(lRead,lIniFile,'RenderQuality',lPrefs.RenderQuality);
  IniInt(lRead,lIniFile,'SaveAsFormat',lPrefs.SaveAsFormat);
  IniInt(lRead,lIniFile,'SaveAsFormatTrack',lPrefs.SaveAsFormatTrack);
  IniInt(lRead,lIniFile,'OcclusionAmount',lPrefs.OcclusionAmount);
  if (lPrefs.RenderQuality < kRenderPoor) then lPrefs.RenderQuality:= kRenderPoor;
  if (lPrefs.RenderQuality > kRenderBetter) then lPrefs.RenderQuality:= kRenderBetter;
  lIniFile.Free;
end;

end.
