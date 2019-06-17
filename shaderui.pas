unit shaderui;
{$Include opts.inc}
{$D-,O+,Q-,R-,S-} //Delphi only L- Y-
{$IFDEF FPC}
{$mode delphi} {$H+}
{$ENDIF}
interface
{$include opts.inc}
 uses
  {$IFDEF DGL} dglOpenGL, {$ELSE DGL} {$IFDEF COREGL}glcorearb, {$ELSE} gl, glext, {$ENDIF}  {$ENDIF DGL}
   {$IFDEF COREGL} define_types, {$ENDIF} //UnitBound
    {$IFDEF MATCAP} FPImage, IntfGraphics, LCLType,{$ENDIF}
  {$IFDEF FPC} FileUtil, GraphType, LCLProc,  LCLIntf,LResources,OpenGLContext,{$ELSE}Windows,glpanel, {$ENDIF}
  Graphics,Classes, SysUtils, Forms,  Buttons,userdir,
  Dialogs, ComCtrls, Menus, Controls,
  ExtCtrls, StdCtrls, shaderu;

procedure SetShader(lFilename: string);
{$IFDEF MATCAP}procedure SetMatCap(lFilename: string);{$ENDIF}
function ShaderPanelHeight: integer;
function ShaderDir: string;
function MatCapDir: string;
procedure FormCreateShaders;
procedure ReportUniformChange(Sender: TObject);
procedure SetShaderAndDrop(lFilename: string);
procedure SetShaderAdjust(lProperty: string; lVal: single);

implementation
uses mainunit;

var
   sLabel: array [1..kMaxUniform] of integer; //control count for Labels
   sTrack: array [1..kMaxUniform] of integer; //control count for TrackBars
   gUpdateGLSL: boolean = false;

function Val2Percent (min,val,max: single): integer;
var
  S: single;
begin
  if max = min then
    S := 0
  else if max < min then
    S := 100* ((val-max)/(min-max))
  else
    S := 100* ((val-min)/(max-min));
  if S < 0 then
    S := 0;
  if S > 100 then
    S := 100;
  result := round(S);
end;

function ShaderPanelHeight: integer;
begin
  result := 1 + GLForm1.LightElevTrack.top+GLForm1.LightElevTrack.height;
  if (gShader.nUniform < 1) or (gShader.nUniform > kMaxUniform) or (sTrack[gShader.nUniform] = 0) then exit;
  result := 1 + (GLForm1.ShaderBox.Controls[sTrack[gShader.nUniform]] as TTrackBar).top + (GLForm1.ShaderBox.Controls[sTrack[gShader.nUniform]] as TTrackBar).height;
end;

procedure CreateAllControls;
 var
   i, t: integer;
 begin
   for t := 1 to kMaxUniform do begin //assume we can not find control
     sLabel[t] := 0;
     sTrack[t] := 0;
   end;
   for i := 0 to GLForm1.ShaderBox.ControlCount - 1 do begin
       t := GLForm1.ShaderBox.Controls[i].tag;
       if (t < 1) or (t > kMaxUniform) then continue;
       if (GLForm1.ShaderBox.Controls[i] is TLabel) then
          sLabel[t] := i;
       if (GLForm1.ShaderBox.Controls[i] is TTrackBar) then
          sTrack[t] := i;
   end;
 end;

procedure ShowUniform(N: integer; U: TUniform);
var
  aLabel: TLabel;
  aTrack: TTrackBar;
begin
 if (n > kMaxUniform) or (n < 1) then
   exit;
 aLabel := (GLForm1.ShaderBox.Controls[sLabel[n]] as TLabel);
 aTrack := (GLForm1.ShaderBox.Controls[sTrack[n]] as TTrackBar);
 aLabel.Caption := U.Name;
 aLabel.Visible := true;
 if U.Widget = kBool then begin
   aTrack.Visible := true;
   aTrack.Min:=0;
   aTrack.Max:=1;
   if (U.Bool) then
     aTrack.Position := 1
   else
       aTrack.Position := 0;
   //aTrack.Position:=;
   //aTrack.Visible := true;
 end else if (U.Widget = kInt) or (U.Widget = kFloat) then begin
   aTrack.Visible := true;
   aTrack.Max := 100;
   aTrack.Min := 0;

   aTrack.position := Val2Percent(U.Min, U.DefaultV,U.Max);
 end else
   aTrack.visible := false;
end;


procedure SetShaderAdjust(lProperty: string; lVal: single);
var
  UpperName: string;
  i: integer;
   aLabel: TLabel;
   aTrack: TTrackBar;
begin
  if gShader.nUniform < 1  then
    exit;
  UpperName := UpperCase(lProperty);
  for i := 1 to gShader.nUniform do begin
    aLabel := (GLForm1.ShaderBox.Controls[sLabel[i]] as TLabel);
    aTrack := (GLForm1.ShaderBox.Controls[sTrack[i]] as TTrackBar);
    if UpperName = upperCase(aLabel.Caption) then begin
      aTrack.position := Val2Percent(gShader.Uniform[i].Min, lVal,gShader.Uniform[i].Max);
      GLForm1.UniformChange(nil);
    end;//if property matches shader's caption
  end; //for each uniform
end;

function MatCapDir: string;
var
   s: string;
begin
  s := 'matcap';
  result := AppDir+s;
  {$IFDEF UNIX}
  if fileexists(result) then exit;
  result := '/usr/share/surfice/'+s;
  if fileexists(result) then exit;
  result := AppDir+s;
  {$ENDIF}
end;

function ShaderDir: string;
var
   s: string;
begin
  {$IFDEF COREGL}   //OpenGL 3 Core shaders not compatible with OpenGL2
    {$IFDEF HEMISSAO}
    s := 'shader';
    {$ELSE}
    s := 'shaders';
    {$ENDIF}
  {$ELSE}
  s := 'shadersOld';
  {$ENDIF}
  result := AppDir+s;
  {$IFDEF UNIX}
  if fileexists(result) then exit;
  result := '/usr/share/surfice/'+s;
  if fileexists(result) then exit;
  result := AppDir+s;
  {$ENDIF}
end;

{$IFDEF COREGL}
procedure UpdateTrackUniforms;
var
   i: integer;
begin
  for i := 1 to gShader.nUniform do begin
    if gShader.Uniform[i].Widget = kFloat then begin
          if AnsiCompareText(gShader.Uniform[i].name, 'Ambient') = 0 then gShader.TrackAmbient:= UnitBound(gShader.Uniform[i].defaultV);
          if AnsiCompareText(gShader.Uniform[i].name, 'Diffuse') = 0 then gShader.TrackDiffuse:= UnitBound(gShader.Uniform[i].defaultV);
          if AnsiCompareText(gShader.Uniform[i].name, 'Specular') = 0 then gShader.TrackSpecular:= UnitBound(gShader.Uniform[i].defaultV);
    end; //if kFloat
  end; //for i: each uniform
end; //UpdateTrackUniforms()
{$ENDIF}

{$IFDEF MATCAP}

{$IFDEF WINDOWS}
procedure FlipVertical (var px: TPicture);
var
  p: array of byte;
  i, half, b: integer;
  LoPtr, HiPtr: PInteger;
begin
    if px.Height < 3 then exit;
    half := (px.Height div 2);
    b := px.Bitmap.RawImage.Description.BytesPerLine;
    LoPtr := PInteger(px.Bitmap.RawImage.Data);
    HiPtr := PInteger(px.Bitmap.RawImage.Data+ ((px.Height -1) * b));
    setlength(p, b);
    for i := 1 to half do begin
          System.Move(LoPtr^,p[0],b); //(src, dst,sz)
          System.Move(HiPtr^,LoPtr^,b); //(src, dst,sz)
          System.Move(p[0],HiPtr^,b); //(src, dst,sz)
          Inc(PByte(LoPtr), b );
          Dec(PByte(HiPtr), b);
    end;
end; //FlipVertical()
{$ENDIF}

function LoadMatCap(fnm: string; var texID: GLuint): boolean;
var
  px: TPicture;
  {$IFNDEF WINDOWS}
  AImage: TLazIntfImage;
  lRawImage: TRawImage;
  {$ENDIF}
begin
  result := false;
  if not fileexists(fnm) then begin
     GLForm1.ShowmessageError(format('LoadTex: unable to find "%s"',[fnm]));
     exit;
  end;
  px := TPicture.Create;
    try
       {$IFDEF WINDOWS}
       px.LoadFromFile(fnm);
       FlipVertical(px);
       {$ELSE}
       //ensure order is GL_RGBA8 - it is with many PNG files, but not JPEG
       lRawImage.Init;
       lRawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(0,0);
       lRawImage.Description.LineOrder := riloBottomToTop; // openGL uses cartesian coordinates
       lRawImage.CreateData(false);
       AImage := TLazIntfImage.Create(0,0);
       try
         AImage.SetRawImage(lRawImage);
         AImage.LoadFromFile(fnm);
         px.Bitmap.LoadFromIntfImage(AImage);
       finally
         AImage.Free;
       end;
       {$ENDIF}
    except
      px.Bitmap.Width:=-1;
    end;
  if ((px.Bitmap.PixelFormat <> pf24bit ) and  (px.Bitmap.PixelFormat <> pf32bit )) or (px.Bitmap.Width < 1) or (px.Bitmap.Height < 1) then begin
     GLForm1.ShowmessageError(format('LoadTex: unsupported pixel format bpp (%d) or size (%dx%d)',[PIXELFORMAT_BPP[px.Bitmap.PixelFormat], px.Bitmap.Width, px.Bitmap.Height]));
     exit;
  end;
  px.Bitmap.Height;
  px.Bitmap.Width;
  if texID <> 0 then
     glDeleteTextures(1,@texID);
  glGenTextures(1, @texID);
  glBindTexture(GL_TEXTURE_2D,  texID);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
  {$IFDEF WINDOWS}
  glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, px.Width, px.Height, 0, GL_BGRA, GL_UNSIGNED_BYTE, PInteger(px.Bitmap.RawImage.Data));
  {$ELSE}
  glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, px.Width, px.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, PInteger(px.Bitmap.RawImage.Data));
  {$ENDIF}
  px.Free;
  result := true;
end;

procedure SetMatCap(lFilename: string);
begin
  gUpdateGLSL := true;
  LoadMatCap(lFilename, gShader.matcap);
  gUpdateGLSL := false;
end;
{$ENDIF}


procedure SetShader(lFilename: string);
var
  i : integer;
begin
  gUpdateGLSL := true;
  LoadShader(lFilename, gShader);
  if gShader.nUniform > 0  then
    for i := 1 to gShader.nUniform do
      ShowUniform(i, gShader.Uniform[i]);
  if gShader.nUniform < kMaxUniform then begin
     for i := (gShader.nUniform+1) to kMaxUniform do begin
      (GLForm1.ShaderBox.Controls[sLabel[i]] as TLabel).Visible := false;
      (GLForm1.ShaderBox.Controls[sTrack[i]] as TTrackBar).Visible := false;
    end;//for all unused
  end; //not max uniforms
  GLForm1.ShaderBoxResize(nil);
  GLForm1.Memo1.Lines.Clear;
  GLForm1.Memo1.Lines.Add(gShader.note);
  {$IFDEF MATCAP}
  GLForm1.MatCapDrop.visible :=  gShader.isMatCap;
  GLForm1.LightAziTrack.visible := not gShader.isMatCap;
  GLForm1.LightElevTrack.visible := not gShader.isMatCap;
  if (gShader.isMatCap) and (gShader.matcap = 0) then
     GLForm1.MatCapDropChange(nil);
  {$ENDIF}
  {$IFDEF COREGL} UpdateTrackUniforms; {$ENDIF}
  gUpdateGLSL := false;
  //GLForm1.UpdateTimer.enabled := true;
end;

procedure SetShaderAndDrop(lFilename: string);
//precedence: if filename is in ShadersDrop list, set ShaderDrop, else load directly
var
  lName,lNameExt,lItem: string;
  i: integer;
begin
  lName := UPPERCASE(ExtractFileName(lFilename));
  lNameExt := lName +'.TXT';
  //showmessage(inttostr(ShaderDrop.Items.Count) + lFilename) ;
  if GLForm1.ShaderDrop.Items.Count > 1 then begin
    for i := 0 to (GLForm1.ShaderDrop.Items.Count-1) do begin
      //Showmessage(lName + ' <> '+  ShaderDrop.Items[i]);
      lItem := UPPERCASE (GLForm1.ShaderDrop.Items[i]);
      if (lItem = lName) or (lItem = lNameExt) then begin
        GLForm1.ShaderDrop.ItemIndex := i;
        GLForm1.ShaderDropChange(nil);
        exit;
      end;

    end;//for each shader
  end; //if at least one shader
  //only get here if filename not in shaderdrop - load directly from disk
  if fileexists(lFilename) then
    SetShader(lFilename);
end;

procedure UpdateMatCapDrop (var LUTdrop: TComboBox);
var
  lSearchRec: TSearchRec;
  lF: ansistring;
  lS: TStringList;
begin
  LUTdrop.Items.Clear;
  lS := TStringList.Create;
  if FindFirst(MatCapDir+pathdelim+'*.jpg', faAnyFile, lSearchRec) = 0 then
    repeat
      lF :=ExtractFileName(lSearchRec.Name);
      if (length(lF) > 1) and (lF[1] <> '.') then  //OSX can create hidden files
        lS.Add(ChangeFileExt(ExtractFileName(lSearchRec.Name),'')) ;
    until (FindNext(lSearchRec) <> 0);
  FindClose(lSearchRec);
  if lS.Count < 1 then begin;
     showmessage('Error: unable to find any MatCaps in '+MatCapDir+pathdelim+'*.jpg' );
     //LUTdrop.Items.Add('No MatCaps found');
     Freeandnil(lS);
     exit;
  end;
  lS.sort;
  LUTdrop.Items.AddStrings(lS);
  Freeandnil(lS);
end;//UpdateColorSchemes

procedure UpdateShaderDrop (var LUTdrop: TComboBox);
var
  lSearchRec: TSearchRec;
  lF: ansistring;
  lS: TStringList;
begin
  LUTdrop.Items.Clear;
  lS := TStringList.Create;
  if FindFirst(ShaderDir+pathdelim+'*.txt', faAnyFile, lSearchRec) = 0 then
    repeat
      lF :=ExtractFileName(lSearchRec.Name);
      if (length(lF) > 1) and (lF[1] <> '.') then  //OSX can create hidden files
        lS.Add(ChangeFileExt(ExtractFileName(lSearchRec.Name),'')) ;
        //lS.Add(ChangeFileExt(ExtractFileName(lSearchRec.Name,'')  ;
        //lS.Add((ExtractFileName(lSearchRec.Name)))
    until (FindNext(lSearchRec) <> 0);
  FindClose(lSearchRec);
  if lS.Count < 1 then begin;
     showmessage('Error: unable to find any shaders in '+ShaderDir+pathdelim+'*.txt' );
     LUTdrop.Items.Add('No shaders found');
     Freeandnil(lS);
     exit;
  end;
  lS.sort;
  LUTdrop.Items.AddStrings(lS);
  Freeandnil(lS);
end;//UpdateColorSchemes

procedure FormCreateShaders;
begin
  CreateAllControls;
  gShader.nUniform := 0;
  CreateAllControls;
  UpdateShaderDrop(GLForm1.ShaderDrop);
  UpdateMatCapDrop(GLForm1.MatCapDrop);
  GLForm1.ShaderDrop.ItemIndex := 0;
  //gShader := LoadShader(ShaderDir+pathdelim+GLForm1.ShaderDrop.Items[GLForm1.ShaderDrop.ItemIndex]);
end;

function boolstr(b: boolean): string;
begin
  if b then
    result := 'true'
  else
    result := 'false';
end;

function Track2S(Pct,Min,Max: single): single;
begin
  if Max > Min then
    result := Min + (Pct/100)*(Max-Min)
  else
    result := Min;
end;

function Track2I(Pct,Min,Max: single): integer;
begin
   result := round(Track2S(Pct,Min,Max));
end;

procedure ReportUniformChange(Sender: TObject);
var
  i: integer;
  aTrack: TTrackBar;
begin
  if gUpdateGLSL then exit;
  //GLForm1.updatetimer.enabled := true;
  if gShader.nUniform > 0  then
    for i := 1 to gShader.nUniform do begin
      aTrack := (GLForm1.ShaderBox.Controls[sTrack[i]] as TTrackBar);
      case gShader.Uniform[i].Widget of
        kBool: begin

          if aTrack.visible then
            gShader.Uniform[i].Bool:= (aTrack.Position >0) ;
          GLForm1.memo1.lines.add('Bool '+ gShader.Uniform[i].name+' '+ inttostr(aTrack.Position ));

          end;
        kInt:begin
          if aTrack.visible then
            gShader.Uniform[i].DefaultV := Track2I(aTrack.Position, gShader.Uniform[i].Min,gShader.Uniform[i].Max) ;
          GLForm1.memo1.lines.add('Int '+ gShader.Uniform[i].name+' '+ inttostr(round(gShader.Uniform[i].defaultV)) );
          end;
        kFloat:
        begin
          if aTrack.visible then
            gShader.Uniform[i].DefaultV := Track2S(aTrack.Position, gShader.Uniform[i].Min,gShader.Uniform[i].Max) ;
          GLForm1.memo1.lines.add('Float '+ gShader.Uniform[i].name+' '+ floattostrf(gShader.Uniform[i].defaultV,ffGeneral,4,4) );
          //GLForm1.memo1.lines.add('FloatZ '+ floattostrf(gShader.Uniform[i].Min,ffGeneral,4,4))+' '+floattostrf(gShader.Uniform[i].Max,ffGeneral,4,4)) );
          (*{$IFDEF COREGL}
              if AnsiCompareText(gShader.Uniform[i].name, 'Ambient') = 0 then gShader.TrackAmbient:= UnitBound(gShader.Uniform[i].defaultV);
              if AnsiCompareText(gShader.Uniform[i].name, 'Diffuse') = 0 then gShader.TrackDiffuse:= UnitBound(gShader.Uniform[i].defaultV);
              if AnsiCompareText(gShader.Uniform[i].name, 'Specular') = 0 then gShader.TrackSpecular:= UnitBound(gShader.Uniform[i].defaultV);
              {$ENDIF}  *)
          end;
      end;//case
    end;//cor each item
  {$IFDEF COREGL} UpdateTrackUniforms; {$ENDIF}
end;

end.


