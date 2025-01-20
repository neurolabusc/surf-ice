unit trx;
//compile; inspect streamlines for file 'dpsv.trx'
// fpc trx.pas; ./trx .\dpsv.trx
{$m+}{$H+}// directive to be used for using constructor

{$MODE OBJFPC}
interface

uses
  StrUtils, SysUtils, Zipper, Classes;

type
  TScalar = record
    scalar: array of single;
    Name: string;
    isDPV: boolean;
  end;

  TTRX = class
  private
    UnZipper: TUnZipper;
    raw: TMemoryStream;
  public
    ok: boolean;
    n_streamlines, n_vtx: integer;
    offsets: array of int32;
    positions: array of single;
    scalars: array of TScalar;
    constructor Create(fnm: string);
    procedure CreateOutZipStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    procedure DoneOutZipStream(Sender: TObject; var AStream: TStream;
      AItem: TFullZipFileEntry);
    procedure Close;
    destructor Destroy; override;
  end;

implementation

procedure TTRX.CreateOutZipStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  AStream := TMemorystream.Create;
end;

procedure TTRX.DoneOutZipStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  AStream.Position := 0;
  //Memo1.lines.LoadFromStream(Astream);
  raw.position := 0;
  raw.Clear;
  raw.copyfrom(Astream, Astream.size);
  Astream.Free;
end;

function HalfToFloat(Half: word): single;
  // https://galfar.vevb.net/wp/2011/16bit-half-float-in-pascaldelphi/
var
  Dst, Sign, Mantissa: longword;
  Exp: longint;
begin
  // Extract sign, exponent, and mantissa from half number
  Sign := Half shr 15;
  Exp := (Half and $7C00) shr 10;
  Mantissa := Half and 1023;
  if (Exp > 0) and (Exp < 31) then
  begin
    // Common normalized number
    Exp := Exp + (127 - 15);
    Mantissa := Mantissa shl 13;
    Dst := (Sign shl 31) or (longword(Exp) shl 23) or Mantissa;
    // Result := Power(-1, Sign) * Power(2, Exp - 15) * (1 + Mantissa / 1024);
  end
  else if (Exp = 0) and (Mantissa = 0) then
  begin
    // Zero - preserve sign
    Dst := Sign shl 31;
  end
  else if (Exp = 0) and (Mantissa <> 0) then
  begin
    // Denormalized number - renormalize it
    while (Mantissa and $00000400) = 0 do
    begin
      Mantissa := Mantissa shl 1;
      Dec(Exp);
    end;
    Inc(Exp);
    Mantissa := Mantissa and not $00000400;
    // Now assemble normalized number
    Exp := Exp + (127 - 15);
    Mantissa := Mantissa shl 13;
    Dst := (Sign shl 31) or (longword(Exp) shl 23) or Mantissa;
    // Result := Power(-1, Sign) * Power(2, -14) * (Mantissa / 1024);
  end
  else if (Exp = 31) and (Mantissa = 0) then
  begin
    // +/- infinity
    Dst := (Sign shl 31) or $7F800000;
  end
  else
  begin //if (Exp = 31) and (Mantisa <> 0) then
    // Not a number - preserve sign and mantissa
    Dst := (Sign shl 31) or $7F800000 or (Mantissa shl 13);
  end;
  // Reinterpret LongWord as Single
  Result := PSingle(@Dst)^;
end;

procedure printf(s: string);
begin
     {$IFDEF UNIX}
	 writeln(s);
	 {$ELSE}
     {$IFDEF CONSOLE}
	   writeln(s);
	   {$ENDIF}
   {$ENDIF}
end;

//https://www.freepascal.org/docs-html/fcl/zipper/tunzipper.html
constructor TTRX.Create(fnm: string);
var
  gb: single;
  n, bpp, i, j, k: integer;
  nm: string;
  f16: array of uint16; //no native float16 data type
  f32: array of single;
  f64: array of double;
  i8: array of int8;
  i16: array of int16;
  i32: array of int32;
  i64: array of int64;
  ui8: array of uint8;
  ui16: array of uint16;
  ui32: array of uint32;
  ui64: array of uint64;
begin
  scalars := nil;
  UnZipper := TUnZipper.Create;
  ok := True;
  raw := TMemoryStream.Create;
  try
    UnZipper.FileName := fnm;
    UnZipper.Examine;
    if (UnZipper.Entries.Count < 2) then
      raise Exception.Create('TRX files must have at least 2 entries');
    for i := 0 to UnZipper.Entries.Count - 1 do
    begin
      nm := UnZipper.Entries.Entries[i].ArchiveFileName;
      if AnsiEndsStr('.json', nm) or AnsiEndsStr('\', nm) or AnsiEndsStr('/', nm) or
        AnsiEndsStr('.bit', nm) then
        continue;
      UnZipper.OnCreateStream := @CreateOutZipStream;
      UnZipper.OnDoneStream := @DoneOutZipStream;
      UnZipper.UnZipFile(nm);
      if (raw.size < 1) then continue;
      if AnsiEndsStr('8', nm) then
        bpp := 1
      else if AnsiEndsStr('16', nm) then
        bpp := 2
      else if AnsiEndsStr('32', nm) then
        bpp := 4
      else if AnsiEndsStr('64', nm) then
        bpp := 8
      else //to do: handle bit
        raise Exception.Create('Unknown datatype ' + nm);
      n := raw.size div bpp;
      gb := raw.size / 1073741824;
      if (gb > 1.0) then
        printf('Warning: large TRX field "' + nm + '": ' + floattostrf(gb, ffFixed, 16, 2) + 'gb');
      if (n < 1) then continue;
      if AnsiEndsStr('.float16', nm) or AnsiEndsStr('.float32', nm) or
        AnsiEndsStr('.float64', nm) then
      begin
        SetLength(f32, n);
        if AnsiEndsStr('.float16', nm) then
        begin
          SetLength(f16, n);
          Move(TByteArray(raw.Memory^)[0], f16[0], bpp * n);
          for j := 0 to (n - 1) do
            f32[j] := HalfToFloat(f16[j]);
          SetLength(f16, 0);
        end
        else if AnsiEndsStr('.float32', nm) then
        begin
          Move(TByteArray(raw.Memory^)[0], f32[0], bpp * n);
        end
        else if AnsiEndsStr('.float64', nm) then
        begin
          SetLength(f64, n);
          Move(TByteArray(raw.Memory^)[0], f64[0], bpp * n);
          for j := 0 to (n - 1) do
            f32[j] := f64[j];
          SetLength(f64, 0);
        end;
      end
      else
      begin //integer
        SetLength(ui32, n);
        if AnsiEndsStr('.int8', nm) then
        begin
          SetLength(i8, n);
          Move(TByteArray(raw.Memory^)[0], i8[0], bpp * n);
          for j := 0 to (n - 1) do
            ui32[j] := i8[j];
          SetLength(i8, 0);
        end
        else if AnsiEndsStr('.int16', nm) then
        begin
          SetLength(i16, n);
          Move(TByteArray(raw.Memory^)[0], i16[0], bpp * n);
          for j := 0 to (n - 1) do
            ui32[j] := i16[j];
          SetLength(i16, 0);
        end
        else if AnsiEndsStr('.int32', nm) then
        begin
          SetLength(i32, n);
          Move(TByteArray(raw.Memory^)[0], i32[0], bpp * n);
          for j := 0 to (n - 1) do
            ui32[j] := i32[j];
          SetLength(i32, 0);
        end
        else if AnsiEndsStr('.int64', nm) then
        begin
          SetLength(i64, n);
          Move(TByteArray(raw.Memory^)[0], i64[0], bpp * n);
          for j := 0 to (n - 1) do
            ui32[j] := i64[j];
          SetLength(i64, 0);
        end
        else if AnsiEndsStr('.uint8', nm) then
        begin
          SetLength(ui8, n);
          Move(TByteArray(raw.Memory^)[0], ui8[0], bpp * n);
          for j := 0 to (n - 1) do
            ui32[j] := ui8[j];
          SetLength(ui8, 0);
        end
        else if AnsiEndsStr('.uint16', nm) then
        begin
          SetLength(ui16, n);
          Move(TByteArray(raw.Memory^)[0], ui16[0], bpp * n);
          for j := 0 to (n - 1) do
            ui32[j] := ui16[j];
          SetLength(i16, 0);
        end
        else if AnsiEndsStr('.uint32', nm) then
        begin
          Move(TByteArray(raw.Memory^)[0], ui32[0], bpp * n);
        end
        else if AnsiEndsStr('.uint64', nm) then
        begin
          SetLength(ui64, n);
          Move(TByteArray(raw.Memory^)[0], ui64[0], bpp * n);
          for j := 0 to (n - 1) do
            ui32[j] := ui64[j];
          SetLength(ui64, 0);
        end;
      end;
      if (length(nm) > 4) and (AnsiStartsStr('dpv/', nm) or AnsiStartsStr('dps/', nm) or
        AnsiStartsStr('dpv\', nm) or AnsiStartsStr('dps\', nm)) then
      begin
        j := length(scalars);
        setlength(scalars, j + 1); //add new scalar
        scalars[j].Name := ChangeFileExt(ExtractFileName(nm), '');
        SetLength(scalars[j].scalar, n);
        if length(ui32) = n then
        begin
          for k := 0 to (n - 1) do
            scalars[j].scalar[k] := ui32[k];
        end;
        if length(f32) = n then
        begin
          for k := 0 to (n - 1) do
            scalars[j].scalar[k] := f32[k];
        end;
        scalars[j].isDPV := AnsiStartsStr('dpv', nm);
      end;
      if AnsiStartsStr('offsets.', nm) then
      begin
        SetLength(offsets, n);
        for j := 0 to (n - 1) do
          offsets[j] := ui32[j];

      end;
      if AnsiStartsStr('positions.3.', nm) then
      begin
        SetLength(positions, n);
        for j := 0 to (n - 1) do
          positions[j] := f32[j];
      end;
      SetLength(f32, 0);
      SetLength(ui32, 0);
    end; // for i to UnZipper.Entries.Count
  except
    ok := False;
  end;
  n_vtx := length(positions) div 3;
  n_streamlines := length(offsets) - 1;
  //e.g. fencepost: three offsets [0,5,18] for two streamlines
  if (offsets[n_streamlines] > n_vtx) then
    ok := False;
  if (offsets[n_streamlines] < n_vtx) then
  begin //fence post problem:
    // https://github.com/tee-ar-ex/trx-python/pull/33
    n_streamlines += 1;
    setLength(offsets, n_streamlines + 1);
    offsets[n_streamlines] := n_vtx;
  end;
  Raw.Free;
  UnZipper.Free;
end;

procedure TTRX.Close;
var
  i: integer;
begin
  SetLength(positions, 0);
  SetLength(offsets, 0);
  if scalars <> nil then
  begin  //close all open properties/scalars
    for i := 0 to (length(scalars) - 1) do
    begin
      scalars[i].scalar := nil;
      scalars[i].Name := '';
    end;
    scalars := nil;
  end;
end;

destructor TTRX.Destroy;
begin
  self.Close;
  inherited;
end; // Destroy()

end.