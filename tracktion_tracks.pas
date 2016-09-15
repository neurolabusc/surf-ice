unit tracktion_tracks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, define_types, matmath, track_simplify;

Type

// const
//   kMaxScalars = 3; //maximum number of properties/scalars from TRK file, e.g. FA, pval, pval_corr
TScalar = record
  mn, mx: single; //full range of scalar
  mnView, mxView: single; //selected window for displaying scalar, e.g. if 0..1 with a grayscale color table than values <0 will be black and >1 will be white
  scalar: array of float;
  name: string;
end;
TTrack = class
  //utime: QWord;
  scale, minFiberLength, ditherColorFrac, maxObservedFiberLength  : single;
  origin, mxV, mnV : TPoint3f;
  isBusy, isRebuildList, isTubes, isWorldSpaceMM: boolean;
  TrackTubeSlices, //e.g. if 5 then cross section of fiber is pentagon
  n_count, n_faces, n_vertices, n_indices, minFiberLinks, LineWidth: integer;
  tracks: array of single;
  private
    procedure SetDescriptives;
  public
    constructor Create;
    function SimplifyMM(Tol, minLength: float): boolean;
    procedure SaveBfloat(const FileName: string);
    procedure SaveVtk(const FileName: string);
    procedure Close;
  end;


implementation

function vector2RGB(pt1, pt2: TPoint3F; var len: single): TPoint3f;
begin
  len := sqrt (sqr(pt1.X - pt2.X)  + sqr(pt1.Y - pt2.Y)  + sqr(pt1.Z - pt2.Z)  );
  if (len = 0) then begin
     result := ptf(0,0,0);
     exit;
  end;
  result.X := (abs(pt1.X - pt2.X)/len);
  result.Y := (abs(pt1.Y - pt2.Y)/len);
  result.Z := (abs(pt1.Z - pt2.Z)/len);
end;

procedure TTrack.SetDescriptives;
var
  {$IFDEF MNVEC} numV: integer; sumV : TPoint3f; {$ENDIF}
  len: single;
   i, m, mi: integer;
   mn, mx, pt, pt1 : TPoint3f;
   //{$DEFINE TRACK2CLIPBOARD} //verbose(!!!) reporting to check positions
   {$IFDEF TRACK2CLIPBOARD}str: string;{$ENDIF}
begin
  maxObservedFiberLength := 0;
  if (n_count < 1) or (length(tracks) < 4) then exit;
  mn := ptf(Infinity,Infinity,Infinity);
  mx := ptf(-Infinity, -Infinity, -Infinity);
  {$IFDEF MNVEC}
  numV := 0;
  sumV := ptf(0,0,0);
  {$ENDIF}
  {$IFDEF TRACK2CLIPBOARD}str := '';{$ENDIF}
  i := 0;
  //showmessage(format('--> %g %g %g',[tracks[1], tracks[2], tracks[3]]) );
  while i < length(tracks) do begin
        m :=   asInt( tracks[i]); inc(i);
        for mi := 1 to m do begin
            pt.X := tracks[i]; inc(i);
            pt.Y := tracks[i]; inc(i);
            pt.Z := tracks[i]; inc(i);
            {$IFDEF MNVEC}
            vectorAdd (sumV, pt);
            numV := numV + 1;
            {$ENDIF}
            minmax(pt, mn,mx);
            if mi = 1 then
               pt1 := pt;
            if mi = m then begin
              vector2RGB(pt1, pt, len);
              if len > maxObservedFiberLength then
                 maxObservedFiberLength := len;
             {$IFDEF TRACK2CLIPBOARD}
             str := str + format('%g%s%g%s%g%s%g%s%g%s%g%s%g%s',[vectorLength(pt1,pt),kTab,  pt1.X,kTab,pt1.Y,kTab,pt1.Z, kTab,pt.X,kTab,pt.Y,kTab,pt.Z, kEOLN]);
             {$ENDIF}

            end;
        end;
  end;
  {$IFDEF TRACK2CLIPBOARD}
  clipBoard.AsText:= str;
  {$ENDIF}
  {$IFDEF MNVEC}
  if numV > 0 then
     showmessage(format('%g %g %g', [sumV.X/numV, sumV.Y/numV, sumV.Z/numV]));
  {$ENDIF}
    mnV := mn;
    mxV := mx;
  //showmessage(format('%d  -> %g..%g %g..%g %g..%g max : %g', [length(tracks), mn.X, mx.X, mn.Y, mx.Y, mn.Z, mx.Z, maxObservedFiberLength]));
  (*scale := (mx.X - mn.X);
  if  (mx.Y - mn.Y) > scale then
      scale := (mx.Y - mn.Y);
  if  (mx.Z - mn.Z) > scale then
      scale := (mx.Z - mn.Z); *)
  origin.X := (0.5 * (mx.X - mn.X)) + mn.X;
  origin.Y := (0.5 * (mx.Y - mn.Y)) + mn.Y;
  origin.Z := (0.5 * (mx.Z - mn.Z)) + mn.Z;
  Scale := abs(mx.X - origin.X);
  if abs(mx.Y - origin.Y) > Scale then
        Scale := abs(mx.Y - origin.Y);
  if abs(mx.Z - origin.Z) > Scale then
        Scale := abs(mx.Z - origin.Z);
    //Scale := 2 * scale;
    //GLForm1.Caption := format('%g..%g %g..%g %g..%g',[mn.X,mx.X, mn.Y,mx.Y, mn.Z, mx.Z]) ;
end; // SetDescriptives()

constructor  TTrack.Create;
var
  i : integer;
begin
  i := 1;
     SetLength(tracks, 0);
     n_count := 0;
     n_faces := 0;
     n_vertices := 0;
     LineWidth := 2;
     scale := 0;
     maxObservedFiberLength := 0;
     ditherColorFrac := 0.3;
     minFiberLinks := 1;//minFiberLinks := 2;
     minFiberLength := 20;
     isRebuildList := true;
     isBusy := false;
     isTubes := true;
     isWorldSpaceMM := true; //assume image oriented in world space
end; // Create()

function TTrack.SimplifyMM(Tol, minLength: float): boolean;
var
   pos: TPoint3f;
   i, m, mi, xi, xm, n_countOut: integer;
   dx: single;
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
  result := false;
  if (n_count < 1) or (length(tracks) < 4) then exit;
  setlength(xTracks, length(tracks));
  i := 0;
  xi := 0;
  n_countOut := 0;
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
        dx := vectorDistance(Orig[0], Orig[m-1]);
        if dx > minLength then begin
           n_countOut := n_countOut + 1;
           xm := PolySimplifyFloat3D(Tol, Orig, Simple);
           xTracks[xi] := asSingle(xm);
           inc(xi);
           for mi := 0 to (xm-1) do
               AddJoint(Simple[mi]);
        end;
  end;
  result := xi < i;
  if result then
     showmessage(format('Reduced to %d%% of original size (fibers reduced from %d to %d)',[round(100*xi/i), n_count, n_countOut ]))
  else begin
      showmessage('Unable to further simplify this track with this threshold');
      exit;
  end;
  //showmessage(format('%g %g',[tracks[0], xTracks[0]]));
  n_count := n_countOut;
  setlength(xTracks, xi);
  tracks := Copy(xTracks, Low(xTracks), Length(xTracks));
  result := true;
end; //SimplifyMM()

procedure TTrack.SaveBfloat(const FileName: string);
//{$DEFINE GZ_BFLOAT}
var
  flt: array of single;
  i, o, m, mi, nflt: integer;
  {$IFDEF GZ_BFLOAT}
  mStream : TMemoryStream;
  zStream: TGZFileStream;
  {$ELSE}
  f: file;
  {$ENDIF}
  FileNameBf: string;
begin
  //   flt: array of single;
  // sz, nflt, i, outPos, nVtx, v : integer;
  if (n_count < 1) or (length(tracks) < 4) then exit;
  nflt := length(tracks) + n_count;
  setlength(flt, nflt);
  o := 0; //output position
  i := 0; //input position
  while i < length(tracks) do begin
      m :=   asInt( tracks[i]); inc(i);
      flt[o] := m; inc(o); // "N"
      flt[o] := m; inc(o); // "SeedIndex"
      for mi := 0 to (m-1) do begin
            flt[o] := tracks[i]; inc(i);  inc(o);
            flt[o] := tracks[i]; inc(i);  inc(o);
            flt[o] := tracks[i]; inc(i);  inc(o);
      end;
  end;
  {$IFDEF ENDIAN_LITTLE} //Camino data ALWAYS bigendian!
  for i := 0 to (nflt -1) do
      SwapSingle(flt[i]);
  {$ENDIF}
  {$IFDEF GZ_BFLOAT}
  FileNameBf := FileName + '.Bfloat.gz';
  mStream := TMemoryStream.Create;
  mStream.Write(flt[0], nflt * sizeof(single));
  mStream.Position := 0;
  zStream := TGZFileStream.Create(FileNameBf, gzopenwrite);
  zStream.CopyFrom(mStream, mStream.Size);
  zStream.Free;
  mStream.Free;
  {$ELSE}
  FileNameBf := changeFileExt(FileName, '.Bfloat');
  AssignFile(f, FileNameBf);
  ReWrite(f, sizeof(single));
  BlockWrite(f, flt[0], nflt);
  CloseFile(f);
  {$ENDIF}
end; //SaveBfloat()

function MemoryStreamAsString(vms: TMemoryStream): string;
//binary contents as ASCII string: http://forum.lazarus.freepascal.org/index.php?topic=15622.5;wap2
begin
   SetString(Result, vms.Memory, vms.Size)
end; //MemoryStreamAsString()

procedure TTrack.SaveVtk(const FileName: string);
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
        //showmessage(format('%d -> %d',[i, m]));
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
    //showmessage(format('yyyy %d %d',[n_items, num_v]));
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
end; //SaveVtk()

procedure TTrack.Close;
var i: integer;
begin
  i := 1;
     n_count := 0;
     n_vertices := 0;
     n_faces := 0;
     n_indices := 0;
     setlength(tracks, 0);
end;




end.

