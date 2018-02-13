unit commandsu;

interface

function EXISTS(lFilename: string): boolean; //function
procedure ATLASSTATMAP(ATLASNAME, STATNAME: string; const Indices: array of integer; const Intensities: array of single);
procedure ATLASSATURATIONALPHA(lSaturation, lTransparency: single);
procedure ATLASFILTER(const Filt: array of integer);
procedure AZIMUTH (DEG: integer);
procedure AZIMUTHELEVATION (AZI, ELEV: integer);
procedure BACKCOLOR (R,G,B: byte);
procedure BMPZOOM(Z: byte);
procedure CAMERADISTANCE(DISTANCE: single);
procedure CAMERAPAN(X, Y: single);
procedure CLIP (DEPTH: single);
procedure CLIPAZIMUTHELEVATION (DEPTH,AZI,ELEV: single);
procedure COLORBARPOSITION(P: integer);
procedure COLORBARVISIBLE (VISIBLE: boolean);
procedure EDGECOLOR(name: string; varies: boolean);
procedure EDGELOAD(lFilename: string);
procedure EDGESIZE(size: single; varies: boolean);
procedure EDGETHRESH (LO, HI: single);
procedure ELEVATION (DEG: integer);
procedure FONTNAME(name: string);
procedure MESHLOAD(lFilename: string);
procedure MESHCURV;
procedure MESHOVERLAYORDER (FLIP: boolean);
procedure NODELOAD(lFilename: string);
procedure MODALMESSAGE(STR: string);
procedure MODELESSMESSAGE(STR: string);
procedure NODECOLOR(name: string; varies: boolean);
procedure NODEHEMISPHERE(VAL: integer);
procedure NODEPOLARITY(VAL: integer);
procedure NODESIZE(size: single; varies: boolean);
procedure NODETHRESH (LO, HI: single);
procedure MESHCOLOR (R,G,B: byte);
procedure ORIENTCUBEVISIBLE (VISIBLE: boolean);
procedure OVERLAYADDITIVE (ADD: boolean);
procedure OVERLAYCLOSEALL;
procedure OVERLAYCOLORNAME(lOverlay: integer; lFilename: string);
procedure OVERLAYLOAD(lFilename: string);
procedure OVERLAYMINMAX (lOverlay: integer; lMin,lMax: single);
procedure OVERLAYTRANSPARENCYONBACKGROUND(lPct: integer);
procedure OVERLAYVISIBLE(lOverlay: integer; VISIBLE: boolean);
procedure OVERLAYTRANSLUCENT(lOverlay: integer; TRANSLUCENT: boolean);
procedure OVERLAYINVERT(lOverlay: integer; INVERT: boolean);
procedure OVERLAYSMOOTHVOXELWISEDATA (SMOOTH: boolean);
procedure QUIT;
procedure RESETDEFAULTS;
procedure SAVEBMP(lFilename: string);
procedure SHADERADJUST(lProperty: string; lVal: single);
procedure SHADERAMBIENTOCCLUSION(lVal: single);
procedure SHADERFORBACKGROUNDONLY(BGONLY: boolean);
procedure SHADERLIGHTAZIMUTHELEVATION (AZI, ELEV: integer);
procedure SHADERNAME(lFilename: string);
procedure SHADERXRAY(lObject, lOverlay: single);
procedure TRACKLOAD(lFilename: string);
procedure TRACKPREFS(lLength,lWidth,lDither: single);
procedure VIEWAXIAL (STD: boolean);
procedure VIEWCORONAL (STD: boolean);
procedure VIEWSAGITTAL (STD: boolean);
procedure WAIT (MSEC: integer);

type
  TScriptRec =  RECORD //peristimulus plot
    Ptr: Pointer;
    Decl,Vars: string[255];
  end;
const
     knFunc = 1;
     kFuncRA : array [1..knFunc] of TScriptRec =( (
               Ptr:@EXISTS;Decl:'EXISTS';Vars:'(lFilename: string): boolean')
             );

knProc = 57;
  kProcRA : array [1..knProc] of TScriptRec = (

  (Ptr:@ATLASSTATMAP;Decl:'ATLASSTATMAP';Vars:'(ATLASNAME, STATNAME: string; const Intensities: array of integer; const Intensities: array of single)'),
  (Ptr:@ATLASSATURATIONALPHA;Decl:'ATLASSATURATIONALPHA';Vars:'(lSaturation, lTransparency: single)'),
  (Ptr:@ATLASFILTER;Decl:'ATLASFILTER';Vars:'(const Filt: array of integer)'),
   (Ptr:@AZIMUTH;Decl:'AZIMUTH';Vars:'(DEG: integer)'),
   (Ptr:@AZIMUTHELEVATION;Decl:'AZIMUTHELEVATION';Vars:'(AZI, ELEV: integer)'),
   (Ptr:@BMPZOOM;Decl:'BMPZOOM';Vars:'(Z: byte)'),
   (Ptr:@BACKCOLOR;Decl:'BACKCOLOR';Vars:'(R, G, B: byte)'),
   (Ptr:@CAMERADISTANCE;Decl:'CAMERADISTANCE';Vars:'(DISTANCE: single)'),
   (Ptr:@CAMERAPAN;Decl:'CAMERAPAN';Vars:'(X, Y: single)'),
   (Ptr:@CLIP;Decl:'CLIP';Vars:'(DEPTH: single)'),
   (Ptr:@CLIPAZIMUTHELEVATION;Decl:'CLIPAZIMUTHELEVATION';Vars:'(DEPTH,AZI,ELEV: single)'),
   (Ptr:@COLORBARPOSITION;Decl:'COLORBARPOSITION';Vars:'(P: integer)'),
   (Ptr:@COLORBARVISIBLE;Decl:'COLORBARVISIBLE';Vars:'(VISIBLE: boolean)'),
   (Ptr:@EDGECOLOR;Decl:'EDGECOLOR';Vars:'(name: string; varies: boolean)'),
   (Ptr:@EDGESIZE;Decl:'EDGESIZE';Vars:'(size: single; varies: boolean)'),
   (Ptr:@EDGETHRESH;Decl:'EDGETHRESH';Vars:'(LO, HI: single)'),
   (Ptr:@ELEVATION;Decl:'ELEVATION';Vars:'(DEG: integer)'),
   (Ptr:@EDGELOAD;Decl:'EDGELOAD';Vars:'(lFilename: string)'),
   (Ptr:@FONTNAME;Decl:'FONTNAME';Vars:'(name: string)'),
   (Ptr:@MESHCURV;Decl:'MESHCURV';Vars:''),
   (Ptr:@MESHLOAD;Decl:'MESHLOAD';Vars:'(lFilename: string)'),
   (Ptr:@MESHOVERLAYORDER;Decl:'MESHOVERLAYORDER';Vars:'(FLIP: boolean)'),
   (Ptr:@NODELOAD;Decl:'NODELOAD';Vars:'(lFilename: string)'),
   (Ptr:@MODALMESSAGE;Decl:'MODALMESSAGE';Vars:'(STR: string)'),
   (Ptr:@MODELESSMESSAGE;Decl:'MODELESSMESSAGE';Vars:'(STR: string)'),
   (Ptr:@NODECOLOR;Decl:'NODECOLOR';Vars:'(name: string; varies: boolean)'),
   (Ptr:@NODEHEMISPHERE;Decl:'NODEHEMISPHERE';Vars:'(VAL: integer)'),
   (Ptr:@NODEPOLARITY;Decl:'NODEPOLARITY';Vars:'(VAL: integer)'),
   (Ptr:@NODESIZE;Decl:'NODESIZE';Vars:'(size: single; varies: boolean)'),
   (Ptr:@NODETHRESH;Decl:'NODETHRESH';Vars:'(LO, HI: single)'),
   (Ptr:@MESHCOLOR;Decl:'MESHCOLOR';Vars:'(R, G, B: byte)'),
   (Ptr:@ORIENTCUBEVISIBLE;Decl:'ORIENTCUBEVISIBLE';Vars:'(VISIBLE: boolean)'),
   (Ptr:@OVERLAYADDITIVE;Decl:'OVERLAYADDITIVE';Vars:'(ADD: boolean)'),
   (Ptr:@OVERLAYCLOSEALL;Decl:'OVERLAYCLOSEALL';Vars:''),
   (Ptr:@OVERLAYCOLORNAME;Decl:'OVERLAYCOLORNAME';Vars:'(lOverlay: integer; lFilename: string)'),
   (Ptr:@OVERLAYLOAD;Decl:'OVERLAYLOAD';Vars:'(lFilename: string)'),
   (Ptr:@OVERLAYMINMAX;Decl:'OVERLAYMINMAX';Vars:'(lOverlay: integer; lMin,lMax: single)'),
   (Ptr:@OVERLAYTRANSPARENCYONBACKGROUND;Decl:'OVERLAYTRANSPARENCYONBACKGROUND';Vars:'(lPct: integer)'),
   (Ptr:@OVERLAYVISIBLE;Decl:'OVERLAYVISIBLE';Vars:'(lOverlay: integer; VISIBLE: boolean)'),
   (Ptr:@OVERLAYTRANSLUCENT;Decl:'OVERLAYTRANSLUCENT';Vars:'(lOverlay: integer; TRANSLUCENT: boolean)'),
   (Ptr:@OVERLAYINVERT;Decl:'OVERLAYINVERT';Vars:'(lOverlay: integer; INVERT: boolean)'),
   (Ptr:@OVERLAYSMOOTHVOXELWISEDATA;Decl:'OVERLAYSMOOTHVOXELWISEDATA';Vars:'(SMOOTH: boolean)'),
   (Ptr:@QUIT;Decl:'QUIT';Vars:''),
   (Ptr:@RESETDEFAULTS;Decl:'RESETDEFAULTS';Vars:''),
   (Ptr:@SAVEBMP;Decl:'SAVEBMP';Vars:'(lFilename: string)'),
   (Ptr:@SHADERADJUST;Decl:'SHADERADJUST';Vars:'(lProperty: string; lVal: single)'),
   (Ptr:@SHADERAMBIENTOCCLUSION;Decl:'SHADERAMBIENTOCCLUSION';Vars:'(lVal: single)'),
   (Ptr:@SHADERFORBACKGROUNDONLY;Decl:'SHADERFORBACKGROUNDONLY';Vars:'(BGONLY: boolean)'),
   (Ptr:@SHADERNAME;Decl:'SHADERNAME';Vars:'(lFilename: string)'),
   (Ptr:@SHADERLIGHTAZIMUTHELEVATION;Decl:'SHADERLIGHTAZIMUTHELEVATION';Vars:'(AZI, ELEV: integer)'),
   (Ptr:@SHADERXRAY;Decl:'SHADERXRAY';Vars:'(lObject, lOverlay: single)'),
   (Ptr:@TRACKLOAD;Decl:'TRACKLOAD';Vars:'(lFilename: string)'),
   (Ptr:@TRACKPREFS;Decl:'TRACKPREFS';Vars:'(lLength,lWidth,lDither: single)'),
   (Ptr:@VIEWAXIAL;Decl:'VIEWAXIAL';Vars:'(STD: boolean)'),
   (Ptr:@VIEWCORONAL;Decl:'VIEWCORONAL';Vars:'(STD: boolean)'),
   (Ptr:@VIEWSAGITTAL;Decl:'VIEWSAGITTAL';Vars:'(STD: boolean)'),
   (Ptr:@WAIT;Decl:'WAIT';Vars:'(MSEC: integer)')
    );

implementation
uses
    mainunit, define_types, shaderui, graphics, LCLintf, Forms, SysUtils, Dialogs, scriptengine, mesh;

procedure BMPZOOM(Z: byte);
begin
  if (Z > 10) or (Z < 1) then
     Z := 1;
  gPrefs.ScreenCaptureZoom := Z;
end;

procedure ATLASSTATMAP(ATLASNAME, STATNAME: string; const Indices: array of integer; const Intensities: array of single);
label
  123;
const
  kUndefined : single = 1/0; //NaN - used to indicate unused region - faces connected to these vertices will be discarded
var
  i, idx, num_idx, num_inten, maxROI: integer;
  ok: boolean;
  err: string;
begin
  err := '';
  num_idx := length(Indices);
  num_inten := length(Intensities);
  if (num_inten < 1) then begin
     err := 'No intensities specified';
     goto 123;
  end;
  if (num_idx > 1) and (num_idx <> num_inten) then begin
     err := 'Number of indices ('+inttostr(num_idx)+') does not match number of intensities ('+inttostr(num_inten)+')';
     goto 123;
  end;
  //ignore: preserve previous filter setlength(gMesh.AtlasFilter, 0); //show all regions - we might need some for parsing
  if not GLForm1.OpenMesh(ATLASNAME) then  begin
     err := 'Unable to load mesh named "'+ATLASNAME+'"';
     goto 123;
  end;
  maxROI := gMesh.MaxAtlasRegion;
  if maxROI < 1 then  begin
     err := 'This mesh not an Atlas "'+ATLASNAME+'"';
     goto 123;
  end;
  if (num_idx = 0) and (maxROI <> num_inten) then begin
    err := 'Expected precisely '+inttostr(maxROI)+' intensities, not '+inttostr(num_inten);
    goto 123;
  end;
  //create temporary data - no need to save a file
  setlength(gMesh.tempIntensityLUT, maxROI+1);
  for i := 0 to maxROI do
      gMesh.tempIntensityLUT[i] := kUndefined;
  if (length(Indices) < 1) then begin //no indices - precise mapping
     for i := 1 to maxROI do
         gMesh.tempIntensityLUT[i] := Intensities[i-1]; //e.g. first intensity is indexed from 0
  end else begin //indexed mapping - only specified regions are used
    for i := 0 to  (length(Indices) - 1) do begin
      idx := Indices[i];
      if (idx < 0) or (idx > maxROI) then continue;
      gMesh.tempIntensityLUT[idx] := Intensities[i];
    end;
  end;
  //load temporary file
  ok := gMesh.LoadOverlay('',false);
  err := gMesh.errorString;
  if not ok then goto 123;
  if STATNAME <> '' then
    gMesh.SaveOverlay(STATNAME, gMesh.OpenOverlays);
  //err := gMesh.AtlasStatMapCore(AtlasName, StatName, idxs, intens);
  123:
  if err <> '' then //report error
     ScriptForm.Memo2.Lines.Add('ATLASSTATMAP: '+err);
  GLForm1.GLboxRequestUpdate(nil);
  GLForm1.UpdateToolbar;
  GLForm1.StringGrid1.RowCount := gMesh.OpenOverlays+1;
  GLForm1.UpdateOverlaySpread;

end;

procedure RESETDEFAULTS;
begin
  GLForm1.CloseMenuClick(nil);
  GLForm1.ResetMenuClick(nil);
end;

function EXISTS(lFilename: string): boolean;
begin
  result := FileExists(lFilename);
end;

procedure CAMERAPAN(X, Y: single);
begin
  if (X > 1) then X := 1;
  if (X < -1) then X := -1;
  if (Y > 1) then Y := 1;
  if (Y < -1) then Y := -1;
  gPrefs.ScreenPan.X := X;
  gPrefs.ScreenPan.Y := Y;
  //GLBox.Invalidate;
end;

procedure CAMERADISTANCE(DISTANCE: single);
begin
     GLForm1.SetDistance(DISTANCE);
end;

procedure ATLASFILTER(const Filt: array of integer);
// http://rvelthuis.de/articles/articles-openarr.html
// ATLASFILTER([]);
// ATLASFILTER([17, 22, 32]);
var
  i: integer;
begin
  setlength(gMesh.AtlasFilter, length(Filt));
  if length(Filt) < 1 then begin //release filter
    GLForm1.GLboxRequestUpdate(nil);
    exit;
  end;
  for i := Low(Filt) to High(Filt) do
     gMesh.AtlasFilter[i] := Filt[i];//ScriptForm.Memo2.Lines.Add('F= '+inttostr(Filt[i]));
  GLForm1.GLboxRequestUpdate(nil);
end;

procedure AZIMUTH (DEG: integer);
begin
     gAzimuth := gAzimuth + DEG;
     while gAzimuth < 0 do
      gAzimuth := gAzimuth + 360;
     while gAzimuth > 359 do
      gAzimuth := gAzimuth - 360;
     GLForm1.GLboxRequestUpdate(nil);
end;

procedure ELEVATION (DEG: integer);
begin
     gElevation := gElevation -Deg;
     IntBound(gElevation,-90,90);
     GLForm1.GLboxRequestUpdate(nil);
end;

procedure AZIMUTHELEVATION (AZI, ELEV: integer);
begin
     gElevation := ELEV;
     IntBound(gElevation,-90,90);
     gAzimuth := AZI;
     while gAzimuth < 0 do
      gAzimuth := gAzimuth + 360;
     while gAzimuth > 359 do
      gAzimuth := gAzimuth - 360;
     GLForm1.GLboxRequestUpdate(nil);
end;

procedure VIEWAXIAL (STD: boolean);
begin
  if STD then
    AZIMUTHELEVATION(0,90)
  else
    AZIMUTHELEVATION(180,-90);
end;

procedure VIEWCORONAL (STD: boolean);
begin
  if STD then
    AZIMUTHELEVATION(0,0)
  else
    AZIMUTHELEVATION(180,0);
end;

procedure FONTNAME(name: string);
begin
     gPrefs.FontName:= name;
     GLForm1.UpdateFont(false);
end;

procedure VIEWSAGITTAL (STD: boolean);
begin
  if STD then
    AZIMUTHELEVATION(90,0)
  else
    AZIMUTHELEVATION(270,0);
end;

procedure MESHCURV;
begin
     GLForm1.CurvMenuTemp.Click;
end;

procedure MESHLOAD(lFilename: string);
begin
  if not GLForm1.OpenMesh(lFilename) then begin
     ScriptForm.Memo2.Lines.Add('Unable to load mesh named "'+lFilename+'"');
   end;
end;

procedure MESHOVERLAYORDER (FLIP: boolean);
begin
     gPrefs.isFlipMeshOverlay:= FLIP;
end;

procedure OVERLAYLOAD(lFilename: string);
begin
   if not GLForm1.OpenOverlay(lFilename)then
      ScriptForm.Memo2.Lines.Add('Unable to load overlay named "'+lFilename+'"');
end;


procedure TRACKLOAD(lFilename: string);
begin
      if not GLForm1.OpenTrack(lFilename) then
        ScriptForm.Memo2.Lines.Add('Unable to load track named "'+lFilename+'"');
end;

procedure NODELOAD(lFilename: string);
begin
  if not GLForm1.OpenNode(lFilename) then
      ScriptForm.Memo2.Lines.Add('Unable to load node named "'+lFilename+'"');
end;

procedure EDGELOAD(lFilename: string);
begin
  if not GLForm1.OpenEdge(lFilename)then
      ScriptForm.Memo2.Lines.Add('Unable to load edge named "'+lFilename+'"');
end;

procedure SHADERNAME(lFilename: string);
begin
   SetShaderAndDrop(lFilename);
end;

procedure SHADERFORBACKGROUNDONLY(BGONLY: boolean);
begin
  gPrefs.ShaderForBackgroundOnly:= BGONLY;
  GLForm1.GLBoxRequestUpdate(nil);
end;

procedure SHADERAMBIENTOCCLUSION( lVal: single);
begin
     GLForm1.occlusionTrack.Position := round(lVal * GLForm1.occlusionTrack.Max);
end;


procedure SHADERLIGHTAZIMUTHELEVATION (AZI, ELEV: integer);
begin
   GLForm1.LightElevTrack.Position := Elev;
   GLForm1.LightAziTrack.Position := Azi;
   GLForm1.SurfaceAppearanceChange(nil);
end;

procedure ATLASSATURATIONALPHA(lSaturation, lTransparency: single);
begin
  GLForm1.meshTransparencyTrack.Position := round(lTransparency * GLForm1.meshAlphaTrack.Max);
  GLForm1.MeshSaturationTrack.Position := round(lSaturation * GLForm1.MeshSaturationTrack.Max);
  //gMesh.isRebuildList:= true;
  //GLBoxRequestUpdate(nil);

end;

procedure SHADERXRAY(lObject, lOverlay: single);
begin
  GLForm1.meshAlphaTrack.Position := round(lObject * GLForm1.meshAlphaTrack.Max);
  GLForm1.meshBlendTrack.Position := round(lOverlay * GLForm1.meshBlendTrack.Max);
end;

procedure SHADERADJUST(lProperty: string; lVal: single);
begin
  SetShaderAdjust(lProperty,lVal);
end;

procedure SAVEBMP(lFilename: string);
begin
   GLForm1.SaveBitmap(lFilename);
end;

procedure TRACKPREFS(lLength,lWidth,lDither: single);
begin
     GLForm1.TrackLengthTrack.Position := round(lLength);
     GLForm1.TrackWidthTrack.Position := round(lWidth);
     GLForm1.TrackDitherTrack.Position := round(lDither * GLForm1.TrackDitherTrack.Max);
end;

procedure BACKCOLOR (R,G,B: byte);
begin
  gPrefs.BackColor := RGBToColor(R,G,B);
  GLForm1.GLBoxRequestUpdate(nil);
end;

procedure EDGECOLOR(name: string; varies: boolean);
begin
  GLForm1.LUTdropEdge.ItemIndex :=  GLForm1.ComboBoxName2Index(GLForm1.LUTdropEdge, name);
  GLForm1.EdgeColorVariesCheck.Checked := varies;
end;

procedure EDGETHRESH (LO, HI: single);
begin
     GLForm1.EdgeMinEdit.Value := LO;
     GLForm1.EdgeMaxEdit.Value := HI;
     GLForm1.NodePrefChange(nil);
end;

procedure MODALMESSAGE(STR: string);
begin
  showmessage(STR);
end;

procedure MODELESSMESSAGE(STR: string);
begin
   ScriptForm.Memo2.Lines.Add(STR);
   ScriptForm.Refresh;
end;

procedure NODECOLOR(name: string; varies: boolean);
begin
  GLForm1.LUTdropNode.ItemIndex :=  GLForm1.ComboBoxName2Index(GLForm1.LUTdropNode, name);
  GLForm1.NodeColorVariesCheck.Checked := varies;
end;

procedure NODETHRESH (LO, HI: single);
begin
  GLForm1.NodeMinEdit.Value := LO;
  GLForm1.NodeMaxEdit.Value := HI;
  GLForm1.NodePrefChange(nil);
end;

procedure NODEHEMISPHERE(VAL: integer);
begin
     if VAL < 0 then
         GLForm1.RestrictLeftMenu.Click
     else if VAL > 0 then
          GLForm1.RestrictRightMenu.Click
     else
       GLForm1.RestrictNoMenu.Click;
end;

procedure NODEPOLARITY(VAL: integer);
begin
     if VAL < 0 then
         GLForm1.RestrictNegEdgeMenu.Click
     else if VAL > 0 then
          GLForm1.RestrictPosEdgeMenu.Click
     else
       GLForm1.RestrictAnyEdgeMenu.Click;
end;

procedure EDGESIZE(size: single; varies: boolean);
begin
  GLForm1.edgeScaleTrack.Position := round(size * 10);
  GLForm1.EdgeSizeVariesCheck.checked := varies;
  GLForm1.NodePrefChange(nil);
end;

procedure NODESIZE(size: single; varies: boolean);
begin
     GLForm1.nodeScaleTrack.Position := round(size * 10);
     GLForm1.NodeSizeVariesCheck.checked := varies;
     GLForm1.NodePrefChange(nil);
end;

procedure MESHCOLOR (R,G,B: byte);
begin
  gPrefs.ObjColor := RGBToColor(R,G,B);
  GLForm1.GLBoxRequestUpdate(nil);
end;

procedure CLIP (DEPTH: single);
begin
  GLForm1.ClipTrack.position := round(Depth * GLForm1.ClipTrack.Max);
end;

procedure CLIPAZIMUTHELEVATION (DEPTH,AZI,ELEV: single);
begin
     GLForm1.ClipTrack.position := round(Depth * GLForm1.ClipTrack.Max);
     GLForm1.ClipAziTrack.position := round(Azi);
     GLForm1.ClipElevTrack.position := round(Elev);
end;


procedure COLORBARPOSITION(P: integer);
begin
  gPrefs.ColorBarPosition:= P;
  GLForm1.SetColorBarPosition;
  GLForm1.GLBoxRequestUpdate(nil);
end;

procedure COLORBARVISIBLE (VISIBLE: boolean);
begin
  gPrefs.Colorbar := VISIBLE;
  GLForm1.ColorBarVisibleMenu.Checked := VISIBLE;
  GLForm1.GLBoxRequestUpdate(nil);
end;

procedure OVERLAYADDITIVE (ADD: boolean);
begin
     if ADD <> GLForm1.AdditiveOverlayMenu.Checked then
          GLForm1.AdditiveOverlayMenu.Click;
end;

procedure OVERLAYMINMAX (lOverlay: integer; lMin,lMax: single);
begin
     GLForm1.OVERLAYMINMAX(lOverlay, lMin,lMax);
end;

procedure OVERLAYTRANSPARENCYONBACKGROUND(lPct: integer);
begin
     if lPct < 12 then
          GLForm1.Transparency0.Click
     else if lPct < 37 then
          GLForm1.Transparency25.Click
     else if lPct < 62 then
          GLForm1.Transparency50.Click
     else
       GLForm1.Transparency75.Click;
end;

procedure OVERLAYINVERT(lOverlay: integer; INVERT: boolean);
begin
     GLForm1.OverlayInvert(lOverlay, INVERT);
end;

procedure OVERLAYSMOOTHVOXELWISEDATA (SMOOTH: boolean);
begin
     gPrefs.SmoothVoxelwiseData:= SMOOTH;
end;

procedure OVERLAYTRANSLUCENT(lOverlay: integer; TRANSLUCENT: boolean);
begin
     if TRANSLUCENT then
        GLForm1.OverlayVisible(lOverlay, kLUTtranslucent)
     else
         GLForm1.OverlayVisible(lOverlay, kLUTopaque);
end;
procedure OVERLAYVISIBLE(lOverlay: integer; VISIBLE: boolean);
begin
     if VISIBLE then
        GLForm1.OverlayVisible(lOverlay, kLUTopaque)
     else
         GLForm1.OverlayVisible(lOverlay, kLUTinvisible);
end;

procedure OVERLAYCOLORNAME(lOverlay: integer; lFilename: string);
begin
  GLForm1.OVERLAYCOLORNAME(lOverlay, lFilename);
end;

procedure OVERLAYCLOSEALL;
begin
     GLForm1.CloseOverlaysMenuClick(nil);
end;

procedure ORIENTCUBEVISIBLE (VISIBLE: boolean);
begin
     if GLForm1.OrientCubeMenu.Checked <> VISIBLE then
        GLForm1.OrientCubeMenu.Click;

end;

procedure TRACKCLOSE;
begin
     GLForm1.CloseTracksMenuClick(nil);
end;

procedure NODECLOSE;
begin
  GLForm1.CloseNodesMenuClick(nil);
end;

procedure CLOSEALL;
begin
  GLForm1.CloseMenuClick(nil);
end;

procedure QUIT;
begin
  GLForm1.Close;
end;

(*procedure HaltScript;
begin
  ScriptForm.Memo2.Lines.Add('Script stopped due to errors.');
  ScriptForm.Stop1Click(nil);
end; *)

procedure WAIT (MSEC: integer);
var
  {$IFDEF FPC} lEND: QWord; {$ELSE}lEND : DWord;{$ENDIF}
 // var MemoryStatus: TMemoryStatus;
begin
//MemoryStatus.dwLength := SizeOf(MemoryStatus) ;
//  GlobalMemoryStatus(MemoryStatus) ;
//GLForm1.Caption := IntToStr(MemoryStatus.dwMemoryLoad) +' Available bytes in paging file';
  if MSEC < 0 then exit;
  {$IFDEF FPC} lEND := GetTickCount64+DWord(MSEC);{$ELSE}lEND := GetTickCount+DWord(MSEC);{$ENDIF}
  //FinishRender;//June 09
  if MSEC <= 0 then exit;
  repeat
    //Application.HandleMessage;
    Application.ProcessMessages;//HandleMessage
  {$IFDEF FPC}until (GetTickCount64 >= lEnd); {$ELSE}until (GetTickCount >= lEnd);{$ENDIF}
end;


end.
