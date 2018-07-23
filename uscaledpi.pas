unit uscaledpi;
 //http://wiki.lazarus.freepascal.org/High_DPI
{$IFDEF FPC}{$mode delphi}  {$H+}{$ENDIF}

interface

uses
   {$IFDEF Linux} StrUtils, FileUtil, Process, Classes,SysUtils, {$ENDIF}
   Forms, Graphics, Controls, ComCtrls, Grids, OpenGLContext;

procedure HighDPI(FromDPI: integer);
procedure ScaleDPI(Control: TControl; FromDPI: integer);
procedure HighDPILinux(FontSz: integer);
//function getFontScale(FontSz: integer): single;

implementation

procedure ScaleDPI(Control: TControl; FromDPI: integer);
var
  i, s: integer;
  WinControl: TWinControl;
begin
  with Control do
  begin
    Left := ScaleX(Left, FromDPI);
    if (Control is TOpenGLControl) then begin
      if (FromDPI < 64) then begin
         writeln('Turning off multi-sampling [high DPI]');
         (Control as TOpenGLControl).MultiSampling := 1; //high scaling factors exhaust video memory
      end;
      Top := ScaleY(Top,FromDPI);
    end else

    {$IFDEF LINUX} //strange minimum size and height on Lazarus 1.6.2
    if (Control is TTrackBar) then begin
      //i := 22;
      //s := ScaleY(Height, FromDPI);
      s := 0;
      //Height := ScaleY(Height, FromDPI);
      i := (s) div 3;
      Top := ScaleY(Top, FromDPI) - i ;

    end else begin
       Top :=ScaleY(Top, FromDPI);
       Height := ScaleY(Height, FromDPI);
    end;
    {$ELSE}
           Height := ScaleY(Height, FromDPI);
    Top :=ScaleY(Top, FromDPI);

    {$ENDIF}
    if not (Control is TOpenGLControl) then
       Width := ScaleX(Width, FromDPI);
    if (Control is TStringGrid) then begin
       (Control as TStringGrid).DefaultColWidth := ScaleY((Control as TStringGrid).DefaultColWidth, FromDPI);
      (Control as TStringGrid).DefaultRowHeight := ScaleY((Control as TStringGrid).DefaultRowHeight, FromDPI);
    end;
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
function str2XPix(str: string): integer;
// '1920x1080+0+0' -> 1920   1280x778+0+0
var
  s: string;
begin
     result := 0;
     if length(str) < 1 then exit;
     if not (str[1] in ['0'..'9']) then exit;
     if not AnsiContainsText(str, 'x') then exit;
     if not AnsiContainsText(str, '+') then exit;
     s := copy(str, 1, PosEx('x',str)-1);
     result := strtointdef(s,0);
end;

function getFontScaleXRANDR(): single;
var
  AProcess: TProcess;
  Exe, mmStr: String;
  dpi, mm: single;
  i, k, xPix: integer;
  AStringList, BStringList: TStringList;
begin
  result := 1.0;
  Exe := FindDefaultExecutablePath('xrandr');
  if length(Exe) < 1 then begin
     Exe := '/opt/X11/bin/xrandr';
     //Exe := '/Users/rorden/vx.sh';
     if not fileexists(Exe) then
        Exe := '';
  end;
  writeln('xrandr : '+Exe);
  if length(Exe) < 1 then exit;
  if not FileExists(Exe) then exit;
  result := 1;
  AProcess := TProcess.Create(nil);
  AProcess.Executable:=Exe;
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
  AProcess.Execute;
  if (AProcess.ExitCode = 0) then begin
     AStringList := TStringList.Create;
     BStringList := TStringList.Create;
     AStringList.LoadFromStream(AProcess.Output);
     if AStringList.Count > 0 then begin  //"uint32 2"
        for i := 0 to (AStringList.Count-1) do begin
            if not AnsiContainsText(AStringList.Strings[i], 'connected') then continue;
            writeln(AStringList.Strings[i]);
            BStringList.DelimitedText   := AStringList.Strings[i];
            if (BStringList.Count < 5) then continue;
            k := 0;
            xPix := -1;
            while (k < (BStringList.Count-1)) and (xPix < 1) do begin
                xPix := str2XPix(BStringList.Strings[k]);
                k := k + 1;
            end;
            if xPix < 1 then continue;
            mmStr := BStringList.Strings[BStringList.Count-3];
            if length(mmStr) < 3 then continue;  //"9mm"
            if mmStr[length(mmStr)] <> 'm' then continue;
            if mmStr[length(mmStr)-1] = 'c' then
               mm := 10.0 //cm
            else if mmStr[length(mmStr)-1] = 'm' then
                 mm := 1.0 //mm
            else
                continue;
            delete(mmStr,length(mmStr)-1,2);
            mm := strtointdef(mmStr,0)*mm;
            if mm <= 0 then continue;
            dpi := xPix/( mm/25.4);
            writeln(format(' Xpix %d Xmm %g dpi %g',[xPix, mm, dpi]));
            //Form1.Memo1.lines.Add( inttostr(xPix)+':'+floattostr(mm)+' dpi '+floattostr(dpi));
            result := dpi / 192;
            if (result < 1) then result := 1;
            break;
        end; //for i: each line of output
     end; //if output
     AStringList.Free;
     BStringList.Free;
  end;
  AProcess.Free;
end;


function getFontScale(FontSz: integer): single;
var
  AProcess: TProcess;
  Exe, Str: String;
  AStringList: TStringList;
begin
  result := 1.0;
  if (Screen.PixelsPerInch > 48) and (FontSz > 10) then
     result := (FontSz/10) * (72/Screen.PixelsPerInch);
     //result := Screen.PixelsPerInch / 96;
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
    	writeln('gsettings get org.gnome.desktop.interface scaling-factor : '+AStringList.Strings[0]);
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
        writeln('gsettings get org.gnome.desktop.interface text-scaling-factor : '+AStringList.Strings[0]);
        result := result * strtofloatdef(AStringList.Strings[0], 1.0);
     if result <= 0 then result := 1; //some machines report "0" for 1
     AStringList.Free;
  end;
  AProcess.Free;
  writeln(format('Detected screen scaling %g', [result]));
end;
{$ENDIF}

procedure HighDPILinux(FontSz: integer);
var
  i, FromDPI: integer;
  scale: single = 0;
begin
  if (paramcount > 1) then begin
     i := 1;
     while (i < (paramcount)) do begin
         //writeln(upcase(paramstr(i))) ;
         if upcase(paramstr(i)) = '-D' then begin
            scale := strtofloatdef(paramstr(i+1),1);
            writeln(format('Custom scaling %g', [scale]));
            if scale = 0 then exit;
         end;
         i := i + 1;
     end;
  end;
  if scale < 0 then  //e.g. -1
     scale := getFontScaleXRANDR();
  if scale = 0 then begin
    scale := 1;
    {$IFDEF LINUX}
    scale := getFontScale(FontSz);
    //if scale = 1 then  scale := getFontScaleXRANDR();
    {$ENDIF}
  end;
  if  (scale = 0) then exit;
  FromDPI := round( 96/scale);
  writeln(format('Scale .. %g dpi %d',[scale, FromDPI]));
  for i := 0 to Screen.FormCount - 1 do
    ScaleDPI(Screen.Forms[i], FromDPI);
  writeln('Done scaling ...');

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

