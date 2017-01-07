unit shaderui;
{$Include opts.inc} //compile for either dglOpenGL or glext
{$D-,O+,Q-,R-,S-} //Delphi only L- Y-
{$IFDEF FPC}
{$mode delphi} {$H+}
{$ENDIF}
interface
{$include opts.inc}
 uses
   {$IFDEF DGL} dglOpenGL, {$ELSE} gl, {$ENDIF}
   {$IFDEF COREGL} define_types, {$ENDIF} //UnitBound
  {$IFDEF FPC} FileUtil, GraphType, LCLProc,  LCLIntf,LResources,OpenGLContext,{$ELSE}Windows,glpanel, {$ENDIF}
  Graphics,Classes, SysUtils, Forms,  Buttons,userdir,
  Dialogs, ComCtrls, Menus, Controls,
  ExtCtrls, StdCtrls, shaderu;

procedure SetShader(lFilename: string);
function ShaderPanelHeight: integer;
function ShaderDir: string;
procedure FormCreateShaders;
procedure ReportUniformChange(Sender: TObject);
procedure SetShaderAndDrop(lFilename: string);
procedure SetShaderAdjust(lProperty: string; lVal: single);

implementation
uses mainunit;

var
   sLabel: array [1..kMaxUniform] of integer; //control count for Labels
   sCheck: array [1..kMaxUniform] of integer; //control count for CheckBoxes
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
     sCheck[t] := 0;
     sLabel[t] := 0;
     sTrack[t] := 0;
   end;
   for i := 0 to GLForm1.ShaderBox.ControlCount - 1 do begin
       t := GLForm1.ShaderBox.Controls[i].tag;
       if (t < 1) or (t > kMaxUniform) then continue;
       if (GLForm1.ShaderBox.Controls[i] is TCheckBox) then
          sCheck[t] := i;
       if (GLForm1.ShaderBox.Controls[i] is TLabel) then
          sLabel[t] := i;
       if (GLForm1.ShaderBox.Controls[i] is TTrackBar) then
          sTrack[t] := i;
   end;
 end;

procedure ShowUniform(N: integer; U: TUniform);
var
  aCheck: TCheckBox;
  aLabel: TLabel;
  aTrack: TTrackBar;
begin
 if (n > kMaxUniform) or (n < 1) then
   exit;
 aCheck := (GLForm1.ShaderBox.Controls[sCheck[n]] as TCheckBox);
 aLabel := (GLForm1.ShaderBox.Controls[sLabel[n]] as TLabel);
 aTrack := (GLForm1.ShaderBox.Controls[sTrack[n]] as TTrackBar);
 aLabel.Caption := U.Name;
 aLabel.Visible := true;
 if U.Widget = kBool then begin
   aCheck.Visible := true;
   aCheck.Checked := U.Bool;
 end else
   aCheck.visible := false;
 if (U.Widget = kInt) or (U.Widget = kFloat) then begin
   aTrack.Visible := true;
   aTrack.position := Val2Percent(U.Min, U.DefaultV,U.Max);
 end else
   aTrack.visible := false;
end;

 procedure SetShaderAdjust(lProperty: string; lVal: single);
var
  UpperName: string;
  i: integer;
   aCheck: TCheckBox;
   aLabel: TLabel;
   aTrack: TTrackBar;
begin
  if gShader.nUniform < 1  then
    exit;
  UpperName := UpperCase(lProperty);
  for i := 1 to gShader.nUniform do begin
    aCheck := (GLForm1.ShaderBox.Controls[sCheck[i]] as TCheckBox);
    aLabel := (GLForm1.ShaderBox.Controls[sLabel[i]] as TLabel);
    aTrack := (GLForm1.ShaderBox.Controls[sTrack[i]] as TTrackBar);
    if UpperName = upperCase(aLabel.Caption) then begin
      if aCheck.visible then
        aCheck.Checked := not (lVal = 0.0)
      else
          aTrack.position := Val2Percent(gShader.Uniform[i].Min, lVal,gShader.Uniform[i].Max);
      GLForm1.UniformChange(nil);
    end;//if property matches shader's caption
  end; //for each uniform
end;

function ShaderDir: string;
begin
  {$IFDEF COREGL}   //OpenGL 3 Core shaders not compatible with OpenGL2
    {$IFDEF HEMISSAO}
    result := AppDir+'shader'
    {$ELSE}
    result := AppDir+'shaders'
    {$ENDIF}
  {$ELSE}
  result := AppDir+'shadersOld'
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
    for i :=   (gShader.nUniform+1) to kMaxUniform do begin
      (GLForm1.ShaderBox.Controls[sCheck[i]] as TCheckBox).Visible := false;
      (GLForm1.ShaderBox.Controls[sLabel[i]] as TLabel).Visible := false;
      (GLForm1.ShaderBox.Controls[sTrack[i]] as TTrackBar).Visible := false;
    end;//for all unused
  end; //not max uniforms
  GLForm1.ShaderBoxResize(nil);
  GLForm1.Memo1.Lines.Clear;
  GLForm1.Memo1.Lines.Add(gShader.note);
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
  aCheck: TCheckBox;
  aTrack: TTrackBar;
begin
  if gUpdateGLSL then exit;
  //GLForm1.updatetimer.enabled := true;
  if gShader.nUniform > 0  then
    for i := 1 to gShader.nUniform do begin
      aCheck := (GLForm1.ShaderBox.Controls[sCheck[i]] as TCheckBox);
      aTrack := (GLForm1.ShaderBox.Controls[sTrack[i]] as TTrackBar);
      case gShader.Uniform[i].Widget of
        kBool: begin
          if ACheck.visible then
            gShader.Uniform[i].Bool := ACheck.checked;
          GLForm1.memo1.lines.add('Bool '+ gShader.Uniform[i].name+' '+boolstr(gShader.Uniform[i].Bool) );

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


