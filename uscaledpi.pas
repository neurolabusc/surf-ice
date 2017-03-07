unit uscaledpi;
 //http://wiki.lazarus.freepascal.org/High_DPI
{$IFDEF FPC}{$mode delphi}  {$H+}{$ENDIF}

interface

uses
   {$IFDEF Linux} StrUtils, FileUtil, Process, Classes,SysUtils, {$ENDIF}
   Forms, Graphics, Controls, ComCtrls, Grids;

procedure HighDPI(FromDPI: integer);
procedure ScaleDPIX(Control: TControl; FromDPI: integer);

implementation

procedure ScaleDPI(Control: TControl; FromDPI: integer);
var
  i: integer;
  WinControl: TWinControl;
begin
  with Control do
  begin
    Left := ScaleX(Left, FromDPI);

    {$IFDEF LINUX} //strange minimum size and height on Lazarus 1.6.2
    if (Control is TTrackBar) then begin
      //i := 22;
      Height := ScaleY(Height, FromDPI);
      i := (Height) div 3;
      Top := ScaleY(Top, FromDPI) - i ;

    end else begin
       Top :=ScaleY(Top, FromDPI);
       Height := ScaleY(Height, FromDPI);
    end;
    {$ELSE}
       Top :=ScaleY(Top, FromDPI);
       Height := ScaleY(Height, FromDPI);
    {$ENDIF}
    Width := ScaleX(Width, FromDPI);
    {$IFNDEF WINDOWS}
    if (Control is TStringGrid) then begin
       (Control as TStringGrid).DefaultColWidth := ScaleY((Control as TStringGrid).DefaultColWidth, FromDPI);
      (Control as TStringGrid).DefaultRowHeight := ScaleY((Control as TStringGrid).DefaultRowHeight, FromDPI);
    end;
    {$ENDIF}
  end;
  if Control is TWinControl then
  begin
    WinControl := TWinControl(Control);
    if WinControl.ControlCount = 0 then
      exit;
    for i := 0 to WinControl.ControlCount - 1 do
      ScaleDPI(WinControl.Controls[i], FromDPI);
  end;
end;

{$IFDEF LINUX}
function getFontScale: single;
var
  AProcess: TProcess;
  Exe, Str: String;
  AStringList: TStringList;
begin
  //result := 1.5; exit;
  result := 1.0;
  if (Screen.PixelsPerInch > 48) then
     result := Screen.PixelsPerInch / 96;
  Exe := FindDefaultExecutablePath('gsettings');
  if length(Exe) < 1 then exit;
  if not FileExists(Exe) then exit;
  result := 1;
  AProcess := TProcess.Create(nil);
  AProcess.Executable:=Exe;
  //get scaling factor - this is an uint32, e.g. 1,2,3
  AProcess.Parameters.Add('get');
  AProcess.Parameters.Add('org.gnome.desktop.interface');
  AProcess.Parameters.Add('scaling-factor');
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  AProcess.Execute;
  if (AProcess.ExitCode = 0) then begin
     AStringList := TStringList.Create;
     AStringList.LoadFromStream(AProcess.Output);
     if AStringList.Count > 0 then begin  //"uint32 2"
        Str := ExtractDelimited(2, AStringList.Strings[0],[' ']); //remove "uint32 "
        result := strtofloatdef(Str, 1.0);
        if result <= 0 then result := 1; //some machines report "0" for 1
     end;
     AStringList.Free;
  end;
  //get fractional text-scaling-factor, range 1..1.9999, e.g. "1.5" - total zoom is scaling-factor*text-scaling-factor
  AProcess.Parameters.Clear;
  AProcess.Parameters.Add('get');
  AProcess.Parameters.Add('org.gnome.desktop.interface');
  AProcess.Parameters.Add('text-scaling-factor');
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  AProcess.Execute;
  if (AProcess.ExitCode = 0) then begin
     AStringList := TStringList.Create;
     AStringList.LoadFromStream(AProcess.Output);
     if AStringList.Count > 0 then
        result := result * strtofloatdef(AStringList.Strings[0], 1.0);
     if result <= 0 then result := 1; //some machines report "0" for 1
     AStringList.Free;
  end;
  AProcess.Free;
end;

function LinuxEffectiveDPI: integer;
var
  i, FromDPI: integer;
  scale: single;
begin
  result := 96;
  scale := getFontScale;
  if (scale = 1) or (scale = 0) then exit;
  result := round( 96/scale);

end;

procedure HighDPI(FromDPI: integer);
var
  vDPI, i: integer;
begin
  vDPI := LinuxEffectiveDPI;
  if (vDPI = FromDPI) then exit;
  for i := 0 to Screen.FormCount - 1 do
    ScaleDPI(Screen.Forms[i], vDPI);
end;

procedure ScaleDPIX(Control: TControl; FromDPI: integer);
var
  vDPI: integer;
begin
  vDPI := LinuxEffectiveDPI;
  if (vDPI = FromDPI) then exit;
  ScaleDPI(Control, vDPI);
end;

{$ELSE}
procedure HighDPI(FromDPI: integer);
var
  i: integer;
begin
  if Screen.PixelsPerInch = FromDPI then
    exit;
  for i := 0 to Screen.FormCount - 1 do
    ScaleDPI(Screen.Forms[i], FromDPI);
end;

procedure ScaleDPIX(Control: TControl; FromDPI: integer);
begin
     ScaleDPI(Control, FromDPI);
end;
{$ENDIF}

end.

