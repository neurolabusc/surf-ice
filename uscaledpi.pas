unit uscaledpi;
 //http://wiki.lazarus.freepascal.org/High_DPI
{$mode delphi}  {$H+}

interface

uses
   Forms, Graphics, Controls, SysUtils,{$IFDEF Windows} Windows {$ELSE} Types{$ENDIF} ;

procedure HighDPI(FromDPI: integer);
procedure ScaleDPI(Control: TControl; FromDPI: integer);
procedure HighDPIfont(fontSize: integer);

implementation


procedure ScaleForm(Control: TControl; FromDPI: integer);
var
{$IFDEF Windows}
   h: THandle;
   r: TRect;
   TB, LR: integer;
{$ENDIF}
  tray : TRect;
begin
  if not (Control is TForm) then exit;
  tray.Top := 0; tray.Bottom := 0; tray.Left := 0; tray.Right := 0;
  {$IFDEF Windows}
  h := FindWindow('Shell_traywnd','');
  GetWindowRect(h,r);
  //Control.caption := format('%dx%d %dx%d  ',[r.Top, r.Left, r.Bottom, r.Right]);
  TB := abs(r.Bottom - r.Top);
  LR := abs(r.Left - r.Right);
  if  TB < LR then begin //tray on top or bottom
     if r.Top > (screen.height / 2) then //on bottom
        tray.Bottom := TB
     else
       tray.Top := TB;
  end else begin
    if r.Right > (screen.width / 2) then //on right
       tray.Right := LR
    else
      tray.Left := LR;
  end;
  tray.Bottom := tray.Bottom + GetSystemMetrics(4) + GetSystemMetrics(15); //SM_CYCCAPTION SM_CYMENU;
  {$ENDIF}
  // Control.caption := format('%d  %d',[tray.Bottom, GetSystemMetrics(4)]);
  //Control.caption := format('%dx%d %dx%d  scale %d',[Control.width, Control.height, screen.width, screen.height, ScaleX(100,FromDPI)]);
  if ((Control.Height+tray.Top+tray.Bottom) > (Screen.Height)) or (((Control.Width+tray.Left+tray.Right) > (Screen.Width)))  then begin
     Control.Height := Screen.Height - tray.Top - tray.Bottom;
     Control.Top := tray.Top + 1;
     Control.Width := Screen.Width - tray.Left - tray.Right - 12;
     Control.Left := tray.Left + 1;
     (Control as TForm).Position:= poDesigned;
  end;

end;

procedure ScaleDPI(Control: TControl; FromDPI: integer);
var
  i: integer;
  WinControl: TWinControl;
begin
  with Control do begin
    Left := ScaleX(Left, FromDPI);
    Top := ScaleY(Top, FromDPI);
    Width := ScaleX(Width, FromDPI);
    Height := ScaleY(Height, FromDPI);
  end;

  if Control is TWinControl then begin
    WinControl := TWinControl(Control);
    if WinControl.ControlCount = 0 then
      exit;
    for i := 0 to WinControl.ControlCount - 1 do
      ScaleDPI(WinControl.Controls[i], FromDPI);
  end;
  if Control is TForm then
     ScaleForm(Control, FromDPI);
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

