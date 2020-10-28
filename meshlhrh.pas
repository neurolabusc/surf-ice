unit meshlhrh;
{$Include opts.inc}
{$mode objfpc}{$H+}
interface
//{$DEFINE TIMER}   //defined in opts.inc
{$DEFINE TREFOIL} //use Trefoil Knot as default object (instead of pyramid)
uses
  //nifti_foreign,
  {$IFDEF DGL} dglOpenGL, {$ELSE DGL} {$IFDEF COREGL}glcorearb, {$ELSE} gl, glext,{$ENDIF}  {$ENDIF DGL}
  {$IFDEF CTM} ctm_loader, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, strutils, DateUtils,
  base64, zstream, LcLIntf, nifti_loader, colorTable, matmath, math,
  define_types, nifti_types, fileutil, mesh;

type TMeshLHRH = class(TMesh)
  RH: TMesh;
  private
    procedure CreateDummyOverlays;
    procedure SynchronizeOverlays;
  public
    isShowLH: boolean;
    isShowRH: boolean;
    function LoadFromFile(const FileName: string; LoadBilateralLHRH: boolean): boolean;
    function LoadOverlay(const FileName: string; lLoadSmooth: boolean): boolean;
    procedure DrawGL (Clr: TRGBA; clipPlane: TPoint4f; isFlipMeshOverlay: boolean);
    procedure ReverseFaces;
    procedure SwapYZ;
    procedure SwapZY;
    function Contours(OverlayIndex: integer): boolean;
    function Mesh2Node(const FileName: string; isAppend: boolean = true):boolean;
    function Atlas2Node(const FileName: string; isAppend: boolean = true):boolean;
    procedure CloseOverlays;
    procedure Close;
    constructor Create;
    destructor  Destroy; override;
end;
  procedure LHRHNameChange(FileName: string; out FileNameLH, FileNameRH: string);
implementation

destructor TMeshLHRH.Destroy;
begin
  RH.Destroy;
  inherited;
end; // Destroy()

procedure TMeshLHRH.Close;
begin
     RH.Close;
	 inherited;
end;

constructor TMeshLHRH.Create;
begin
	 RH := TMesh.Create;
     isShowRH := true;
     isShowLH := true;
	 inherited;
end;

procedure printf(s: string);
begin
     {$IFDEF UNIX}writeln(s);{$ENDIF}
end;

procedure LHRHNameChange(FileName: string; out FileNameLH, FileNameRH: string);
//  ".LH", ".RH"  std.60.rh.white -> std.60.lh.white
var
   pos: integer;
   pth, fnm: string;
   ch: char;
begin
  fnm := extractfilename(FileName);
  pth := extractfilepath(Filename);
  FileNameLH := '';
  FileNameRH := '';
  pos  := PosEx('LH.', upcase(fnm));
  if pos > 0 then begin //LH left is input
  	 if (fnm[pos] = 'l') then
     	ch := 'r'
     else
         ch := 'R';
     FileNameLH := FileName;
     fnm[pos] := ch;
     FileNameRH := pth+fnm;
  end else begin
    pos  := PosEx('RH.', upcase(fnm));
    if pos > 0 then begin //RH left is input
  	   if (fnm[pos] = 'r') then
     	  ch := 'l'
       else
           ch := 'L';
       FileNameRH := FileName;
       fnm[pos] := ch;
       FileNameLH := pth+fnm;
    end;
  end;
  (*if  (not fileexists(FileNameRH)) or (not fileexists(FileNameLH)) then begin
     FileNameRH := '';
     FileNameLH := '';
  end;*)
end;

procedure TMeshLHRH.ReverseFaces;
begin
	 Inherited;
     if length(rh.Faces) > 0 then
     	rh.ReverseFaces;
end;

procedure TMeshLHRH.SwapYZ;
begin
    Inherited;
    if length(rh.Faces) > 0 then
    	rh.SwapYZ;
end;

procedure TMeshLHRH.SwapZY;
begin
    Inherited;
    if length(rh.Faces) > 0 then
    	rh.SwapZY;
end;

procedure TMeshLHRH.CloseOverlays;
begin
    Inherited;
    if length(rh.Faces) > 0 then
    	rh.CloseOverlays;
end;

function TMeshLHRH.Mesh2Node(const FileName: string; isAppend: boolean = true):boolean;
begin
    Inherited;
    if length(rh.Faces) > 0 then
    	rh.Mesh2Node(FileName, isAppend);
end;

function TMeshLHRH.Atlas2Node(const FileName: string; isAppend: boolean = true):boolean;
begin
    Inherited;
    if length(rh.Faces) > 0 then
    	rh.Atlas2Node(FileName, isAppend);
end;

function TMeshLHRH.Contours(OverlayIndex: integer): boolean;
begin
    Inherited;
    SynchronizeOverlays();
    if length(rh.Faces) > 0 then
    	rh.Contours(OverlayIndex);
    CreateDummyOverlays();
end;


function TMeshLHRH.LoadFromFile(const FileName: string; LoadBilateralLHRH: boolean): boolean;
var
   filenameLH, filenameRH: string;
begin
	 self.isBilateral := false;
     RH.Close;
	 if (not LoadBilateralLHRH) then begin
       result := Inherited LoadFromFile(FileName);
       exit;
  	 end;
     LHRHNameChange(FileName, filenameLH,  filenameRH);
     //printf(inttostr(random(888))+'<'+filenameLH+'->>>>'+ filenameRH);
     if (filenameLH = '') or (filenameRH = '')  then begin
        result := Inherited LoadFromFile(FileName);
        exit;
     end;
     self.isBilateral := true;
     result := Inherited LoadFromFile(filenameLH);
     if not result then begin
        result := Inherited LoadFromFile(FileName);
        exit;
     end;
     RH.LoadFromFile( filenameRH);
	 //inherited;
end;

procedure TMeshLHRH.CreateDummyOverlays;
//Left and right hemisphere should have the same number of overlays
// if one hemisphere has more, clone
var
   i: integer;
begin
	 if OpenOverlays = RH.OpenOverlays then exit;
     if (RH.OpenOverlays < OpenOverlays) then begin
	 	for i := (RH.OpenOverlays+1) to OpenOverlays do
            RH.LoadDummyOverlay(i, overlay[i]);
        RH.OpenOverlays := OpenOverlays
     end;
     if (RH.OpenOverlays > OpenOverlays) then begin
	 	for i := (OpenOverlays+1) to RH.OpenOverlays do
            LoadDummyOverlay(i, RH.overlay[i]);
        OpenOverlays := RH.OpenOverlays
     end;
end;

function TMeshLHRH.LoadOverlay(const FileName: string; lLoadSmooth: boolean): boolean;
var
   filenameLH, filenameRH: string;
   resultRH: boolean = false;
begin
     if length(rh.faces) < 1 then begin
     	result := Inherited LoadOverlay(FileName, lLoadSmooth);
        exit;
     end;
     LHRHNameChange(FileName, filenameLH,  filenameRH);
     //printf(format('%s -> LH %s RH %s', [FileName, filenameLH, filenameRH] ));
     if (filenameLH = '') and (filenameRH = '') then begin
	 	result := Inherited LoadOverlay(FileName, lLoadSmooth);
        resultRH := RH.LoadOverlay(filename, lLoadSmooth);
        result := result or resultRH;
        exit;
     end;
     result := false;
     if filenameLH <> '' then
        result := Inherited LoadOverlay(filenameLH, lLoadSmooth);
      if filenameRH <> '' then
         resultRH := RH.LoadOverlay(filenameRH, lLoadSmooth);
      CreateDummyOverlays();
      result := result or resultRH;
end;

procedure TMeshLHRH.SynchronizeOverlays;
var
   i: integer;
begin
  if (length(RH.faces) < 1) or (not isRebuildList) then exit;
  RH.vertexRgbaAlpha := self.vertexRgbaAlpha;
  RH.vertexRgbaSaturation := self.vertexRgbaSaturation;
  RH.isRebuildList:= true;
  if (self.OpenOverlays >= kMinOverlayIndex) then
    for i := kMinOverlayIndex to self.OpenOverlays do begin
        RH.overlay[i].OpacityPercent := Self.overlay[i].OpacityPercent;
        RH.overlay[i].WindowScaledMin := Self.overlay[i].WindowScaledMin;
        RH.overlay[i].WindowScaledMax := Self.overlay[i].WindowScaledMax;
        //printf(format('--->>>>> %g %g',[Self.overlay[i].WindowScaledMin, Self.overlay[i].WindowScaledMax]));
        RH.overlay[i].LUT := Self.overlay[i].LUT;
        RH.overlay[i].LUTinvert := Self.overlay[i].LUTinvert;
        RH.overlay[i].LUTindex:=Self.overlay[i].LUTindex;
        RH.overlay[i].aoMap := Self.overlay[i].aoMap;
    end;
end;

procedure TMeshLHRH.DrawGL (Clr: TRGBA; clipPlane: TPoint4f; isFlipMeshOverlay: boolean);
begin
     SynchronizeOverlays();
	 Inherited DrawGL(Clr, clipPlane, isFlipMeshOverlay);
end;


end.

