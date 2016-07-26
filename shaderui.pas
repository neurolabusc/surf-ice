unit shaderui;
{$Include opts.inc} //compile for either dglOpenGL or glext
{$D-,O+,Q-,R-,S-} //Delphi only L- Y-
{$IFDEF FPC}
{$mode delphi} {$H+}
{$ENDIF}
interface
//{$include options.inc}
 uses
   {$IFDEF DGL} dglOpenGL, {$ELSE} gl, glext, {$ENDIF}
   {$IFDEF FPC}FileUtil, GraphType, LCLProc,  LCLIntf,LResources,OpenGLContext,{$ELSE}Windows,glpanel, {$ENDIF}
  Graphics,Classes, SysUtils, Forms,  Buttons,userdir, define_types,

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
 aCheck: array of TCheckbox;
 aLabel: array of TLabel;
 aTrack: array of TTrackbar;
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

{$IFDEF FPC}
const
  {$IFDEF WINDOWS}
  //kTrackHt = 30;
  kH = 30;//height
  {$ELSE}
  kH = 30;//height
  {$ENDIF}
{$ELSE}
const
  kH = 30;//height
{$ENDIF}

function ControlTop(N: integer): integer;
var
  kT: integer;
begin
 {$IFDEF FPC}
 {$IFDEF WINDOWS}
   kT := GLForm1.LightElevTrack.top+4; //Check with Windows * and 150% scaling!
 {$ELSE}
   {$IFDEF Darwin}
    kT := GLForm1.LightElevTrack.top; //do this dynamically - if user adjusts text size in Windows7, the position of static controls changes!

   {$ELSE} //Linux
   kT := GLForm1.LightElevTrack.top+4; //do this dynamically - if user adjusts text size in Windows7, the position of static controls changes!
   {$ENDIF}
 {$ENDIF}
 {$ELSE}
   kT := GLForm1.LightElevTrack.top+1; //do this dynamically - if user adjusts text size in Windows7, the position of static controls changes!
{$ENDIF}
  result := kT+ (kH* N);
end;

function ShaderPanelHeight: integer;
begin
{$IFDEF FPC}
 {$IFDEF WINDOWS}
   result := 1 + GLForm1.LightElevTrack.top+round((GLForm1.LightElevTrack.height+1)* (gShader.nUniform+2));
   //result := controlTop(gShader.nUniform+1);
 {$ELSE}
   {$IFDEF LCLCocoa}
      result := controlTop(gShader.nUniform+1);
   {$ELSE}
     result := controlTop(gShader.nUniform+1)+(kH div 2);
   {$ENDIF}
 {$ENDIF}
{$ELSE}
    result := controlTop(gShader.nUniform+1)+(kH div 2)-12;
{$ENDIF}
end;

procedure CreateControl(N: integer; var aLabel: TLabel; var aCheck: TCheckbox; var aTrack: TTrackbar);
const
  //kH = 30;//height
  kL1 = 6;
  kL2 = 118;
var
  lT: integer;
  //kT: integer;
begin
(*{$IFDEF FPC}
   kT := GLForm1.ShaderDrop.top+4{+GLForm1.ShaderDrop.height}; //do this dynamically - if user adjusts text size in Windows7, the position of static controls changes!
{$ELSE}
   kT := GLForm1.ShaderDrop.top+1{+GLForm1.ShaderDrop.height}; //do this dynamically - if user adjusts text size in Windows7, the position of static controls changes!
{$ENDIF} *)
lT := ControlTop(N);
   aLabel := TLabel.Create(GLForm1);
   aLabel.Parent := GLForm1.ShaderBox;
   aLabel.Visible := false;
   aLabel.Caption := inttostr(N);//U.Name;
   aLabel.Top := lT;//kT+ (kH* N);
   aLabel.Left := kL1;
    aCheck := TCheckbox.Create(GLForm1);
    aCheck.Parent := GLForm1.ShaderBox;
    aCheck.Visible := false;
    aCheck.Top := lT;//kT+ (kH* N);
    aCheck.Tag := N;
    aCheck.Left := kL2;
    {$IFDEF FPC}
     aCheck.OnClick := GLForm1.UniformChange; //2015   aCheck.OnClick := @GLForm1.UniformChange;
    {$ELSE}
    aCheck.OnClick := GLForm1.UniformChange;
    {$ENDIF}
    aTrack := TTrackbar.Create(GLForm1);
    aTrack.Parent := GLForm1.ShaderBox;
    aTrack.TickStyle := tsNone;
    aTrack.Visible := false;
    {$IFDEF LINUX}
    aTrack.Top := lT-10;
    {$ELSE}
    aTrack.Top := lT;
    {$ENDIF}
    aTrack.Tag := N;
    aTrack.Left := kL2;
    aTrack.Min := 0;
    aTrack.Max := 100;
    aTrack.Width := 140;
    {$IFDEF FPC}{$IFDEF WINDOWS}
    aTrack.Height := 30;
    {$ENDIF} {$ENDIF}
    {$IFDEF FPC}
    aTrack.OnChange := GLForm1.UniformChange; //aTrack.OnChange := @GLForm1.UniformChange;
    {$ELSE}
    aTrack.OnChange := GLForm1.UniformChange;
    {$ENDIF}
    {$IFNDEF FPC}
    aTrack.Height := kH; //Delphi7 uses a crazy default height
    {$ENDIF}
 end;

 procedure CreateAllControls;
 var
  i: integer;
 begin
  gUpdateGLSL := true;
  setlength(aLabel,kMaxUniform+1);
  setlength(aCheck,kMaxUniform+1);
  setlength(aTrack,kMaxUniform+1);
  for i := 1 to kMaxUniform do
    CreateControl(i, aLabel[i], aCheck[i], aTrack[i]);
  gUpdateGLSL := false;
 end;

 procedure ShowUniform(N: integer; U: TUniform);
 begin
  if (n > kMaxUniform) or (n < 1) then
    exit;
  aLabel[n].Caption := U.Name;
  aLabel[n].Visible := true;
  if U.Widget = kBool then begin
    aCheck[n].Visible := true;
    aCheck[n].Checked := U.Bool;
  end else
    aCheck[n].visible := false;
  if (U.Widget = kInt) or (U.Widget = kFloat) then begin
    aTrack[n].Visible := true;
    aTrack[n].position := Val2Percent(U.Min, U.DefaultV,U.Max);
  end else
    aTrack[n].visible := false;
 end;


 procedure SetShaderAdjust(lProperty: string; lVal: single);
var
  UpperName: string;
  i: integer;
begin
  if gShader.nUniform < 1  then
    exit;
  UpperName := UpperCase(lProperty);
  for i := 1 to gShader.nUniform do begin
    if UpperName = upperCase(aLabel[i].Caption) then begin
      if aCheck[i].visible then
        aCheck[i].Checked := not (lVal = 0.0)
      else
          aTrack[i].position := Val2Percent(gShader.Uniform[i].Min, lVal,gShader.Uniform[i].Max);
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
  //gUpdateGLSL := false; exit;
  //if length(aLabel) <  kMaxUniform then
  //  exit;
  if gShader.nUniform > 0  then
    for i := 1 to gShader.nUniform do
      ShowUniform(i, gShader.Uniform[i]);
  if gShader.nUniform < kMaxUniform then begin
    for i :=   (gShader.nUniform+1) to kMaxUniform do begin
      aLabel[i].Visible := false;
      aCheck[i].Visible := false;
      aTrack[i].Visible := false;
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
begin
  if gUpdateGLSL then exit;
  //GLForm1.updatetimer.enabled := true;
  if gShader.nUniform > 0  then
    for i := 1 to gShader.nUniform do begin
      case gShader.Uniform[i].Widget of
        kBool: begin
          if ACheck[i].visible then
            gShader.Uniform[i].Bool := ACheck[i].checked;
          GLForm1.memo1.lines.add('Bool '+ gShader.Uniform[i].name+' '+boolstr(gShader.Uniform[i].Bool) );

          end;
        kInt:begin
          if aTrack[i].visible then
            gShader.Uniform[i].DefaultV := Track2I(aTrack[i].Position, gShader.Uniform[i].Min,gShader.Uniform[i].Max) ;
          GLForm1.memo1.lines.add('Int '+ gShader.Uniform[i].name+' '+ inttostr(round(gShader.Uniform[i].defaultV)) );
          end;
        kFloat:
        begin
          if aTrack[i].visible then
            gShader.Uniform[i].DefaultV := Track2S(aTrack[i].Position, gShader.Uniform[i].Min,gShader.Uniform[i].Max) ;
          GLForm1.memo1.lines.add('Float '+ gShader.Uniform[i].name+' '+ floattostrf(gShader.Uniform[i].defaultV,ffGeneral,4,4) );
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

