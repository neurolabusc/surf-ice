program FiberQuant;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp//, define_types //track
  { you can add units after this };

type

  { TFiberQuant }

  TFiberQuant = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

type
  TFloat = single;
  TTrack = array of single;
    TFByte =  File of Byte;
    TPoint3f = packed record
    X: single;
    Y: single;
    Z: single
  end;

procedure ReadLnBin(var f: TFByte; var s: string);
const
  kEOLN = $0A;
var
   bt : Byte;
begin
     s := '';
     while (not  EOF(f)) do begin
           Read(f,bt);
           if bt = kEOLN then exit;
           s := s + Chr(bt);
     end;
end;

procedure SwapSingle(var s : single);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word);
      1:(Sngl : single);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  s := outguy.Sngl;
end; // SwapSingle()

procedure SwapLongInt(var s : LongInt);
type
  swaptype = packed record
    case byte of
      0:(Word1,Word2 : word); //word is 16 bit
      1:(Long:LongInt);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
  outguy:swaptype;
begin
  inguy := @s; //assign address of s to inguy
  outguy.Word1 := swap(inguy^.Word2);
  outguy.Word2 := swap(inguy^.Word1);
  s := outguy.Long;
end; // SwapLongInt()

procedure showmessage(msg: string);
begin
     writeln(msg);
end;

function asInt(s : single): longint;
type
  swaptype = packed record
    case byte of
      0:(Lng : longint);
      1:(Sngl : single);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
begin
  inguy := @s; //assign address of s to inguy
  result := inguy^.Lng;
end; // asInt()

function asSingle(i : longint): single;
type
  swaptype = packed record
    case byte of
      0:(Lng : longint);
      1:(Sngl : single);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
begin
  inguy := @i; //assign address of s to inguy
  result := inguy^.Sngl;
end; // asSingle()

function LoadVtk(const FileName: string; var n_count: integer; var tracks: TTrack): boolean;
//Read BINARY VTK mesh
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bvtk/readVtk.m
// ftp://ftp.tuwien.ac.at/visual/vtk/www/FileFormats.pdf
//  "The VTK data files described here are written in big endian form"
label
   666;
var
   f: TFByte;
   strlst: TStringList;
   str: string;
   nVtx, inPos, outPos, vtx, i, v, num_v, n_items, fsz: integer;
   vert: array of TPoint3f;
   items: array of LongInt;
begin
  result := false;
  AssignFile(f, FileName);
  Reset(f,1);
  fsz := filesize(f);
  if fsz < 64 then goto 666;
  ReadLnBin(f, str); //signature: '# vtk DataFile'
  if pos('VTK', UpperCase(str)) <> 3 then begin
    goto 666;
  end;
  ReadLnBin(f, str); //comment: 'Comment: created with MRIcroS'
  ReadLnBin(f, str); //kind: 'BINARY' or 'ASCII'
  if (pos('BINARY', UpperCase(str)) <> 1) then begin  // '# vtk DataFile'
     closefile(f);
     showmessage('Unable to load VTK files in ASCII format (binary only).');
     exit;
  end;
  ReadLnBin(f, str); // kind, e.g. "DATASET POLYDATA" or "DATASET STRUCTURED_ POINTS"
  if pos('POLYDATA', UpperCase(str)) = 0 then begin
    showmessage('Only able to read VTK images saved as POLYDATA, not '+ str);
    goto 666;
  end;
  ReadLnBin(f, str); // number of vert, e.g. "POINTS 685462 float"
  if pos('POINTS', UpperCase(str)) <> 1 then begin
    showmessage('Expected header to report "POINTS" not '+ str);
    goto 666;
  end;
  strlst:=TStringList.Create;
  num_v := 0;
  strlst.DelimitedText := str;
  num_v := StrToIntDef(strlst[1],0);
  if (num_v < 1) or (pos('FLOAT', UpperCase(strlst[2])) <> 1) then begin
    showmessage('Expected at least 1 point of type FLOAT, not '+ str);
    goto 666;
  end;
  if fsz < (filepos(f)+ (3 * 4 * num_v)) then begin
     showmessage('File too small to contain this many vertices!');
     goto 666;
  end;
  setlength(vert, num_v); //vert = zeros(num_f, 9);
  blockread(f, vert[0], 3 * 4 * num_v);
  ReadLnBin(f, str); // number of vert, e.g. "POLYGONS 1380 5520"
  if str = '' then ReadLnBin(f, str);
  if pos('POLYGONS', UpperCase(str)) > 0 then begin
    showmessage('This is a mesh file: rename with a ".vtk" extension and use File/Open to view: '+ str);
    goto 666;
  end;
  if pos('LINES', UpperCase(str)) <> 1 then begin
    showmessage('Expected header to report "LINES" not '+ str);
    goto 666;
  end;
  strlst.DelimitedText := str;
  n_count := StrToIntDef(strlst[1],0);
  n_items := StrToIntDef(strlst[2],0);
  strlst.free;
  if fsz < (filepos(f)+ (n_items * 4)) then begin
     showmessage('File too small to contain this many lines!');
     goto 666;
  end;
  setlength(items, n_items);
  blockread(f, items[0], n_items * 4);
  closefile(f);
  {$IFDEF ENDIAN_LITTLE}
  for i := 0 to (n_items -1) do
      SwapLongInt(items[i]);
  for i := 0 to (num_v -1) do begin
      SwapSingle(vert[i].X);
      SwapSingle(vert[i].Y);
      SwapSingle(vert[i].Z);
  end;
  {$ENDIF}
  vtx := n_items - n_count;
  setlength(tracks, vtx * 3 + n_count);
  inPos := 0;
  outPos := 0;
  for i := 1 to n_count do begin
      nVtx := items[inPos]; inc(inPos);
      tracks[outPos] := asSingle(nVtx); inc(outPos);
      for v := 1 to nVtx do begin
          vtx := items[inPos]; inc(inPos);
          tracks[outPos] := vert[vtx].X; inc(outPos);
          tracks[outPos] := vert[vtx].Y; inc(outPos);
          tracks[outPos] := vert[vtx].Z; inc(outPos);
          //if v < 4 then
          //   showmessage(format('%g %g %g',[vert[vtx].X, vert[vtx].Y, vert[vtx].Z]));
      end; //for each vertex in fiber
  end; //for each fiber
 result := true;
 exit;
666:
   closefile(f);
end;

function VecMinFloat3D(const A, B: TPoint3f): TPoint3f;
// Result = A - B
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

function DotProdFloat3D(const A, B: TPoint3f): TFloat;
// Dotproduct = A * B
begin
  Result := A.X * B.X + A.Y * B.Y + A.Z * B. Z;
end;

function NormSquaredFloat3D(const A: TPoint3f): TFloat;
// Square of the norm |A|
begin
  Result := A.X * A.X + A.Y * A.Y + A.Z * A.Z;
end;

function DistSquaredFloat3D(const A, B: TPoint3f): TFloat;
// Square of the distance from A to B
begin
  Result := NormSquaredFloat3D(VecMinFloat3D(A, B));
end;

procedure SimplifyFloat3D(var Tol2: TFloat; const Orig: array of TPoint3f;
  var Marker: array of boolean; j, k: integer);
// Simplify polyline in OrigList between j and k. Marker[] will be set to True
// for each point that must be included
var
  i, MaxI: integer; // Index at maximum value
  MaxD2: TFloat;    // Maximum value squared
  CU, CW, B: TFloat;
  DV2: TFloat;
  P0, P1, PB, U, W: TPoint3f;
begin
  // Is there anything to simplify?
  if k <= j + 1 then
    exit;

  P0 := Orig[j];
  P1 := Orig[k];
  U  := VecMinFloat3D(P1, P0); // Segment vector
  CU := DotProdFloat3d(U, U); // Segment length squared
  MaxD2 := 0;
  MaxI  := 0;

  // Loop through points and detect the one furthest away
  for i := j + 1 to k - 1 do
  begin
    W  := VecMinFloat3D(Orig[i], P0);
    CW := DotProdFloat3D(W, U);

    // Distance of point Orig[i] from segment
    if CW <= 0 then
    begin
      // Before segment
      DV2 := DistSquaredFloat3D(Orig[i], P0)
    end else
    begin
      if CW > CU then
      begin
        // Past segment
        DV2 := DistSquaredFloat3D(Orig[i], P1);
      end else
      begin
        // Fraction of the segment
        try
          B := CW / CU;
        except
          B := 0; // in case CU = 0
        end;
        PB.X := P0.X + B * U.X;
        PB.Y := P0.Y + B * U.Y;
        PB.Z := P0.Z + B * U.Z;
        DV2 := DistSquaredFloat3D(Orig[i], PB);
      end;
    end;

    // test with current max distance squared
    if DV2 > MaxD2 then
    begin
      // Orig[i] is a new max vertex
      MaxI  := i;
      MaxD2 := DV2;
    end;
  end;

  // If the furthest point is outside tolerance we must split
  if MaxD2 > Tol2 then
  begin // error is worse than the tolerance

    // split the polyline at the farthest vertex from S
    Marker[MaxI] := True;  // mark Orig[maxi] for the simplified polyline

    // recursively simplify the two subpolylines at Orig[maxi]
    SimplifyFloat3D(Tol2, Orig, Marker, j, MaxI); // polyline Orig[j] to Orig[maxi]
    SimplifyFloat3D(Tol2, Orig, Marker, MaxI, k); // polyline Orig[maxi] to Orig[k]
  end;
end;


function PolySimplifyFloat3D(Tol: TFloat; const Orig: array of TPoint3f;
  var Simple: array of TPoint3f): integer;
var
  i, N: integer;
  Marker: array of boolean;
  Tol2: TFloat;
begin
  Result := 0;
  //Simple := Copy(Orig, Low(Orig), Length(Orig));
  if length(Orig) < 2 then
    exit;
  Tol2 := sqr(Tol);

  // Create a marker array
  N := Length(Orig);
  SetLength(Marker, N);
  // Include first and last point
  Marker[0]     := True;
  Marker[N - 1] := True;
  // Exclude intermediate for now
  for i := 1 to N - 2 do
    Marker[i] := False;

  // Simplify
  SimplifyFloat3D(Tol2, Orig, Marker, 0, N - 1);

  // Copy to resulting list
  for i := 0 to N - 1 do begin
    if Marker[i] then
    begin
      Simple[Result] := Orig[i];
      inc(Result);
    end;
  end;
  //setlength(Simple,Result);
end;

function SimplifyMM(Tol: single; var n_count: integer; var tracks: TTrack): boolean;
var
   pos: TPoint3f;
   i, m, mi, xi, xm: integer;
   Orig, Simple: array of TPoint3f;
   xTracks: array of single;
procedure AddJoint(pt: TPoint3f);
begin
     xTracks[xi] := pt.X; inc(xi);
     xTracks[xi] := pt.Y; inc(xi);
     xTracks[xi] := pt.Z; inc(xi);
end;
begin
  //if (threshold < 0.5) then exit;
  if (n_count < 1) or (length(tracks) < 4) then exit;
  setlength(xTracks, length(tracks));
  i := 0;
  xi := 0;
  while i < length(tracks) do begin
        m :=   asInt( tracks[i]);
        xm := 0;
        inc(i);
        setlength(Orig, m);
        setlength(Simple, m);
        for mi := 0 to (m-1) do begin
            pos.X := tracks[i]; inc(i);
            pos.Y := tracks[i]; inc(i);
            pos.Z := tracks[i]; inc(i);
            Orig[mi] := pos;
        end;
        xm := PolySimplifyFloat3D(Tol, Orig, Simple);
        xTracks[xi] := asSingle(xm);
        inc(xi);
        for mi := 0 to (xm-1) do
            AddJoint(Simple[mi]);
  end;
  result := xi < i;
  if result then
     showmessage('Reduced to '+inttostr(round(100*xi/i))+'% of original size')
  else
      showmessage('Unable to further simplify this track with this threshold');
  //showmessage(format('%g %g',[tracks[0], xTracks[0]]));
  setlength(xTracks, xi);
  tracks := Copy(xTracks, Low(xTracks), Length(xTracks));
end;

function MemoryStreamAsString(vms: TMemoryStream): string;
//binary contents as ASCII string: http://forum.lazarus.freepascal.org/index.php?topic=15622.5;wap2
begin
   SetString(Result, vms.Memory, vms.Size)
end; //MemoryStreamAsString()

procedure SaveVtk(const FileName: string; var n_count: integer; var tracks: TTrack);
var
   f : TextFile;
   m, mi, i,j,num_v, k, nk, n_items: integer;
   items: array of integer;
   vert: array of single;
   outStream : TMemoryStream;
begin
    num_v := (length(tracks) - n_count) div 3;
    if (num_v < 1) or (n_count < 2) then begin
       showmessage('You need to open a mesh before you can save it');
       exit;
    end;
    AssignFile(f, FileName);
    ReWrite(f);
    WriteLn(f, '# vtk DataFile Version 3.0');
    WriteLn(f, 'vtk output');
    WriteLn(f, 'BINARY');
    WriteLn(f, 'DATASET POLYDATA');
    //serialize data
    setlength(vert, num_v * 3);
    n_items := n_count+num_v;
    setlength(items, n_items);
    j := 0;
    i := 0;
    k := 0;
    nk := 0;
    while i < length(tracks) do begin
        m :=   asInt( tracks[i]); inc(i);
        items[k] := m; inc(k);
        for mi := 0 to (m-1) do begin
              items[k] := nk; inc(k); inc(nk);
              vert[j] := tracks[i]; inc(i);  inc(j);
              vert[j] := tracks[i]; inc(i);  inc(j);
              vert[j] := tracks[i]; inc(i);  inc(j);
        end;
    end;
    {$IFDEF ENDIAN_LITTLE}
    for i := 0 to (n_items -1) do
        SwapLongInt(items[i]);
    for i := 0 to ((num_v*3) -1) do begin
        SwapSingle(vert[i]);
    end;
    {$ENDIF}
    //write points
    WriteLn(f, 'POINTS '+inttostr(num_v) +' float');  //POINTS 7361202 float
    outStream := TMemoryStream.Create;
    outStream.Write(pointer(vert)^, num_v * 3 * sizeOf(single));
    WriteLn(f, MemoryStreamAsString(outStream));
    outStream.Free;
    //write lines
    WriteLn(f, 'LINES '+inttostr(n_count) +' '+inttostr(n_items));   //LINES 50076 7411278
    outStream := TMemoryStream.Create;
    outStream.Write(pointer(items)^, (n_items) * sizeOf(longint));
    WriteLn(f, MemoryStreamAsString(outStream));
    outStream.Free;
    CloseFile(f);
end;

procedure SimplifyTracks(inname, outname: string; tol: single);
var

  n_count: integer;
  tracks: TTrack;
begin
  if not LoadVTK(inname, n_count, tracks) then exit;
  if not SimplifyMM(Tol,n_count, tracks) then exit;
  SaveVtk(outname, n_count, tracks);
  setlength(tracks,0);
end;

const
  kTol = 0.3;

procedure TFiberQuant.DoRun;
var
  ErrorMsg, inname, outname: String;
  Tol: single;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h:t:o', 'help tol out');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;
  // parse parameters
  if HasOption('h', 'help') or (ParamCount = 0) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  Tol := kTol;
  inname := '';
  outname := '';
  if HasOption('t','tol') then
     Tol:= StrToFloatDef(GetOptionValue('t','tol'), kTol);
  if HasOption('o','out') then
     outname := GetOptionValue('o','out');
  inname := ParamStr(ParamCount);
  if not fileexists(inname) then begin
     showmessage('Error: unable to find input file : '+inname);
     WriteHelp;
     Terminate;
     Exit;
  end;
  if outname = '' then
     outname := changefileext(inname, '_s.vtk');
  showmessage('Simplify tolerance '+floattostrf(Tol,ffGeneral, 4, 4)+': '+inname + ' -> ' + outname);
  SimplifyTracks(inname, outname, tol);
  Terminate;
end;

constructor TFiberQuant.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TFiberQuant.Destroy;
begin
  inherited Destroy;
end;

procedure TFiberQuant.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' [options] inputName');
  writeln('Options');
  writeln(' -h : show help');
  writeln(' -t : tolerance (e.g. 2 allows error up to 2mm)');
  writeln(' -o : output name');
  writeln('Examples');
  writeln(' '+ExeName+'  in.vtk');
  writeln(' '+ExeName+' -t 0.5 in.vtk');
  writeln(' '+ExeName+' -t 0.5 -o out.vtk in.vtk');
  {$IFDEF UNIX}
   writeln(' '+ExeName+' -t 1 -o "~/out dir/shrunk.vtk" "~/in dir/in.vtk"');
  {$ELSE}
    writeln(' '+ExeName+' -t 1 -o "c:\out dir\shrunk.vtk" "c:\in dir in.vtk"');
  {$ENDIF}
end;

var
  Application: TFiberQuant;
begin
  Application:=TFiberQuant.Create(nil);
  Application.Title:='FiberQuant';
  Application.Run;
  Application.Free;
end.

