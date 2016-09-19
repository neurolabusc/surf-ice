unit tracktion_tracks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, define_types, matmath, track_simplify, zstream;

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
    function SimplifyRemoveRedundant(Tol: float): boolean;
    function SimplifyMM(Tol, minLength: float): boolean;
    procedure SaveBfloat(const FileName: string);
    procedure SaveVtk(const FileName: string);
    procedure SaveTrk(const FileName: string);
    procedure Save(FileName: string);
    function Smooth: boolean;
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
  if (Tol = 0) and (minLength = 0) then exit; //nothing to do
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

function vectorDistanceSqr(A,B: TPoint3f): single; inline;
//do not apply sqrt for dramatic speedup!
begin
  //result := sqrt(sqr(A.X-B.X)+ sqr(A.Y-B.Y) + sqr(A.Z-B.Z));
  result := (sqr(A.X-B.X)+ sqr(A.Y-B.Y) + sqr(A.Z-B.Z));
end;

function TTrack.Smooth: boolean;
//smooth nodes
//  http://dev.theomader.com/gaussian-kernel-calculator/
var
   fiber, fibersmooth: array of TPoint3f;
   i, startPos, m, mi: integer;
begin
  result := false;
  if (n_count < 1) or (length(tracks) < 4) then exit;
  setlength(fiber, 1024);
  setlength(fibersmooth, 1024);
  i := 0;
  while i < length(tracks) do begin
        m :=   asInt( tracks[i]);
        inc(i);
        if m < 5 then begin
           i := i + 3 * m;
           continue; //skip this fiber
        end;
        if m > length(fiber) then begin
           setlength(fiber, m);
           setlength(fibersmooth, m);
        end;
        startPos := i;
        for mi := 0 to (m-1) do begin
            fiber[mi].X := tracks[i]; inc(i);
            fiber[mi].Y := tracks[i]; inc(i);
            fiber[mi].Z := tracks[i]; inc(i);
        end;
        fibersmooth[0] := fiber[0];
        fibersmooth[1] := fiber[1];
        fibersmooth[m-2] := fiber[m-2];
        fibersmooth[m-1] := fiber[m-1];
        for mi := 2 to (m-3) do begin
            fibersmooth[mi].X :=  fiber[mi-2].X*0.06136+ fiber[mi-1].X*0.24477+fiber[mi].X*00.38774+ fiber[mi+1].X*0.24477+ fiber[mi+2].X*0.06136;
            fibersmooth[mi].Y :=  fiber[mi-2].Y*0.06136+ fiber[mi-1].Y*0.24477+fiber[mi].Y*00.38774+ fiber[mi+1].Y*0.24477+ fiber[mi+2].Y*0.06136;
            fibersmooth[mi].Z :=  fiber[mi-2].Z*0.06136+ fiber[mi-1].Z*0.24477+fiber[mi].Z*00.38774+ fiber[mi+1].Z*0.24477+ fiber[mi+2].Z*0.06136;
        end;
        //replace fiber with fibersmooth
        i := startPos;
        for mi := 0 to (m-1) do begin
            tracks[i] := fibersmooth[mi].X; inc(i);
            tracks[i] := fibersmooth[mi].Y; inc(i);
            tracks[i] := fibersmooth[mi].Z; inc(i);
        end;

  end;
  setlength(fiber,0);
  result := true;
end; //Smooth()

type
TProps = record
  lo,hi: TPoint3f; //full range of scalar
  index: integer;
  unique: boolean;
end;
TPropsArray = array of TProps;


//http://stackoverflow.com/questions/24335585/quicksort-drama
procedure QuickSort(left, right: integer; var s: TPropsArray);
// left:      Index des 1. Elements, right: Index des letzten Elements
var
  l, r, lswap: integer;
  pivot: single;
begin
  if (right > left) then begin
    l := left;
    r := right;
    pivot := s[s[(right + left) div 2].index].lo.Z;
    while (l < r) do begin
      while s[s[l].index].lo.Z < pivot do
        l := l + 1;
      while s[s[r].index].lo.Z > pivot do
        r := r - 1;
      if (l <= r) then begin
        lswap := s[r].index;
        s[r].index := s[l].index;
        s[l].index := lswap;
        l := l + 1;
        r := r - 1;
      end;
    end;
    if (left < r) then
      QuickSort(left, r, s);
    if (right > l) then
      QuickSort(l, right, s);
  end;
end;

procedure SortArray(var s: TPropsArray);
var
 i : integer;
begin
     if length(s) < 1 then exit;
     for i := 0 to (length(s)-1) do  //set indices
         s[i].index := i;
     quicksort(low(s), high(s), s);
end;

function TTrack.SimplifyRemoveRedundant(Tol: float): boolean;
var
   pos, pos0: TPoint3f;
   track, i, j, m, mi: integer;
   xI, xJ : ^TProps;
   xTracks: TPropsArray;
   tolSqr, dx : single;
begin
    result := false;

  if (tol <= 0.0) then exit;
  if (n_count < 1) or (length(tracks) < 4) then exit;
  showmessage('Warning: SimplifyRemoveRedundant not optimized: please check for updates');
  tolSqr := sqr(tol); //e.g. if tol=3mm, then tolSqr=9, avoiding slow sqrt in pythagorean formule
  setlength(xTracks, n_count);
  //first pass: record endpoints of all tracks
  track := 0;
  i := 0;
  while i < length(tracks) do begin
        m :=   asInt( tracks[i]);
        inc(i);
        for mi := 0 to (m-1) do begin
            pos.X := tracks[i]; inc(i);
            pos.Y := tracks[i]; inc(i);
            pos.Z := tracks[i]; inc(i);
            if mi = 0 then
               pos0 := pos;
        end;
        xTracks[track].unique := true;
        if pos0.Z < pos.Z then begin
           xTracks[track].lo := pos0;
           xTracks[track].hi := pos;
        end else begin
            xTracks[track].hi := pos0;
            xTracks[track].lo := pos;
        end;
        track := track + 1;
  end;
  track := 0;
  //{$DEFINE SLOW_METHOD}
  {$IFDEF SLOW_METHOD}
  for i := 0 to (n_count - 2) do begin
      for j := (i + 1) to (n_count - 1) do begin
          if (xTracks[j].unique) and (vectorDistanceSqr(xTracks[i].lo, xTracks[j].lo) < tolSqr) and (vectorDistanceSqr(xTracks[i].hi, xTracks[j].hi) < tolSqr) then begin
             xTracks[j].unique := false;
             track := track + 1;
          end;
      end;
  end;
  {$ELSE}
  SortArray(xTracks);
  for i := 0 to (n_count - 2) do begin
    xI := @xTracks[xTracks[i].index];
    j := i + 1;
    repeat
          xJ := @xTracks[xTracks[j].index];
          dx :=  sqr(xJ^.lo.Z - xI^.lo.Z);
          //if dx < 0 then showmessage('sorting error');
          if (xJ^.unique) and (vectorDistanceSqr(xI^.lo, xJ^.lo) < tolSqr) and (vectorDistanceSqr(xI^.hi, xJ^.hi) < tolSqr) then begin
             xJ^.unique := false;
             track := track + 1;
          end;
          j := j + 1;
    until (j = n_count) or (dx > tolSqr);
  end;
  {$ENDIF}
  if track < 1 then exit; //no redundant tracks

  showmessage(format('Removed %d redundant tracks (endpoints nearer than %.4g)', [track, tol]));
  //remove redundant fibers
  j := 0;
  i := 0;
  track := 0;
  while i < length(tracks) do begin
        m :=   asInt( tracks[i]); inc(i);
        if xTracks[track].unique then begin
           tracks[j] := asSingle(m); inc(j);
        end;
        if xTracks[track].unique then
          for mi := 1 to (m * 3) do begin//*3 as each vertex stores XYZ
              tracks[j] := tracks[i]; inc(j); inc(i);
          end
        else
            i := i + (m * 3);
        track := track + 1;
  end;
  setlength(tracks,j);
  result := true;
end; //SimplifyRemoveRedundant()

type
 TTrackhdr = packed record //trackvis format
   id_string: array [1..6] of ansichar; //"TRACK*"
   dim: array [1..3] of smallInt;
   voxel_size: array [1..3] of single;
   origin: array [1..3] of single;  //not used!
   n_scalars: Smallint;
   scalar_name: array [1..200] of ansichar;
   n_properties: Smallint;
   property_name: array [1..200] of ansichar;
   vox_to_ras: array [1..4, 1..4] of single;
   reserved: array [1..444] of ansichar;
   voxel_order: array [1..4] of ansichar;
   pad2: array [1..4] of ansichar;
   image_orientation_patient: array [1..6] of single;
   pad1: array [1..2] of ansichar;
   invert_x: byte;
   invert_y: byte;
   invert_z: byte;
   swap_xy: byte;
   swap_yz: byte;
   swap_zx: byte;
   n_count : LongInt;
   version  : LongInt;
   hdr_size  : LongInt;
end;

procedure TTrack.SaveTrk(const FileName: string);
var
  //flt: array of single;
  //i, o, m, mi, nflt: integer;
  hdr:  TTrackhdr;
  mStream : TMemoryStream;
  zStream: TGZFileStream;
  FileNameBf: string;
begin
  if (n_count < 1) or (length(tracks) < 4) then exit;
  hdr.id_string[1] := 'T'; hdr.id_string[2] := 'R'; hdr.id_string[3] := 'A'; hdr.id_string[4] := 'C'; hdr.id_string[5] := 'K'; hdr.id_string[6] := chr(0);
  hdr.dim[1] := 181; hdr.dim[2] := 217; hdr.dim[3] := 181;
  hdr.voxel_size[1] := 1; hdr.voxel_size[2] := 1; hdr.voxel_size[3] := 1;
  hdr.origin[1] := 91; hdr.origin[2] := 125; hdr.origin[3] := -71;
  hdr.n_scalars:= 0;
  fillchar(hdr.scalar_name[1], sizeof(hdr.scalar_name), 0);
  hdr.n_properties:= 0;
  fillchar(hdr.property_name[1], sizeof(hdr.scalar_name), 0);
  hdr.vox_to_ras[1,1] := 1; hdr.vox_to_ras[1,2] := 0; hdr.vox_to_ras[1,3] := 0; hdr.vox_to_ras[1,4] := 0.5;
  hdr.vox_to_ras[2,1] := 0; hdr.vox_to_ras[2,2] := 1; hdr.vox_to_ras[2,3] := 0; hdr.vox_to_ras[2,4] := 0.5;
  hdr.vox_to_ras[3,1] := 0; hdr.vox_to_ras[3,2] := 0; hdr.vox_to_ras[3,3] := 1; hdr.vox_to_ras[3,4] := 0.5;
  hdr.vox_to_ras[4,1] := 0; hdr.vox_to_ras[4,2] := 0; hdr.vox_to_ras[4,3] := 0; hdr.vox_to_ras[4,4] := 1;
  fillchar(hdr.reserved[1], sizeof(hdr.reserved), 0);
  hdr.voxel_order[1] := 'R'; hdr.voxel_order[2] := 'A'; hdr.voxel_order[3] := 'S'; hdr.voxel_order[4] := chr(0);
  fillchar(hdr.pad2[1], sizeof(hdr.pad2), 0);
  fillchar(hdr.image_orientation_patient[1], sizeof(hdr.image_orientation_patient), 0);
  fillchar(hdr.pad1[1], sizeof(hdr.pad1), 0);
  hdr.invert_x:= 0; hdr.invert_y:= 0; hdr.invert_z:= 0;
  hdr.swap_xy := 0; hdr.swap_yz := 0; hdr.swap_zx := 0;
  hdr.n_count:= n_count;
  hdr.version := 2;
  hdr.hdr_size := sizeof(hdr);
  mStream := TMemoryStream.Create;
  mStream.Write(hdr, sizeof(hdr));
  mStream.Write(tracks[0], length(tracks) * sizeof(single) );
  mStream.Position := 0;
  if (ExtractFileExtGzUpper(Filename) = '.TRK.GZ') then begin
    FileNameBf := ChangeFileExtX(FileName, '.trk.gz');
    zStream := TGZFileStream.Create(FileNameBf, gzopenwrite);
    zStream.CopyFrom(mStream, mStream.Size);
    zStream.Free;
  end else begin
    FileNameBf := ChangeFileExtX(FileName, '.trk');
    mStream.SaveToFile(FileNameBf);
  end;
  mStream.Free;
end; //SaveTrk()

procedure TTrack.SaveBfloat(const FileName: string);
var
  flt: array of single;
  i, o, m, mi, nflt: integer;
  mStream : TMemoryStream;
  zStream: TGZFileStream;
  FileNameBf: string;
begin
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
  mStream := TMemoryStream.Create;
  mStream.Write(flt[0], nflt * sizeof(single));
  mStream.Position := 0;
  if (ExtractFileExtGzUpper(Filename) = '.BFLOAT.GZ') then begin
    FileNameBf := ChangeFileExtX(FileName, '.Bfloat.gz');
    zStream := TGZFileStream.Create(FileNameBf, gzopenwrite);
    zStream.CopyFrom(mStream, mStream.Size);
    zStream.Free;
  end else begin
    FileNameBf := ChangeFileExtX(FileName, '.Bfloat');
    mStream.SaveToFile(FileNameBf);
  end;
  mStream.Free;
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

procedure TTrack.Save(FileName: string);
var
  ext: string;
begin
    ext := ExtractFileExtGzUpper(Filename);
    if ext = '' then begin
       Filename := Filename +'.vtk';
       ext := '.VTK';
    end;
    if (ext = '.BFLOAT') or (ext = '.BFLOAT.GZ') then
       SaveBfloat(FileName)
    else if (ext = '.TRK') or (ext = '.TRK.GZ') then
        SaveTrk(FileName)
    else if (ext = '.VTK') then
        SaveVtk(FileName)
    else begin
         showmessage('Unable to save to format "'+ext+'"');
         Filename := Filename +'.vtk';
         SaveVtk(FileName);
    end;
    {$ifdef isTerminalApp}showmessage('Created file '+FileName);{$endif}
end;

procedure TTrack.Close;
begin
     n_count := 0;
     n_vertices := 0;
     n_faces := 0;
     n_indices := 0;
     setlength(tracks, 0);
end;




end.

