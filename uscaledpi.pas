unit uscaledpi;
 //http://wiki.lazarus.freepascal.org/High_DPI
{$mode delphi}  {$H+}

interface

uses
   Forms, Graphics, Controls;

procedure HighDPI(FromDPI: integer);
procedure ScaleDPI(Control: TControl; FromDPI: integer);
procedure HighDPIfont(fontSize: integer);

implementation

procedure ScaleDPI(Control: TControl; FromDPI: integer);
var
  i: integer;
  WinControl: TWinControl;
begin
  with Control do
  begin
    Left := ScaleX(Left, FromDPI);
    Top := ScaleY(Top, FromDPI);
    Width := ScaleX(Width, FromDPI);
    Height := ScaleY(Height, FromDPI);
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

procedure HighDPIfont(fontSize: integer);
var
  i, FromDPI: integer;
begin
  if (fontSize <= 13) then exit;
  FromDPI :=  round(14/fontSize * 96);
  for i := 0 to Screen.FormCount - 1 do
    ScaleDPI(Screen.Forms[i], FromDPI);
end;

procedure HighDPI(FromDPI: integer);
var
  i: integer;
begin
  if Screen.PixelsPerInch = FromDPI then
    exit;
  for i := 0 to Screen.FormCount - 1 do
    ScaleDPI(Screen.Forms[i], FromDPI);
end;

end.

