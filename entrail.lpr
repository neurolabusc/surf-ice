program entrail;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, nifti_loader, define_types, matmath, math,track,DateUtils;

const
  kVers = 'Entrail by Chris Rorden version 9Dec2016';
type
  TTrackingPrefs = record
    trackName, atlasName, outName : string;
  end;
  TFiber = record
    startROI, endROI: integer;
  end;
  TFiberQuant = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

procedure showMsg(msg: string);
begin
  writeln(msg);
end;

procedure showMat(vox2mmMat: TMat44);
begin
     showmsg(format('vox2mm= [%g %g %g %g; %g %g %g %g; %g %g %g %g; 0 0 0 1]',
        [vox2mmMat[1,1],vox2mmMat[1,2],vox2mmMat[1,3],vox2mmMat[1,4],
        vox2mmMat[2,1],vox2mmMat[2,2],vox2mmMat[2,3],vox2mmMat[2,4],
        vox2mmMat[3,1],vox2mmMat[3,2],vox2mmMat[3,3],vox2mmMat[3,4] ]) );
end;

function vox2mm(Pt: TPoint3f; vox2mmMat: TMat44) : TPoint3f; inline;
begin
     result.X := Pt.X*vox2mmMat[1,1] + Pt.Y*vox2mmMat[1,2] + Pt.Z*vox2mmMat[1,3] + vox2mmMat[1,4];
     result.Y := Pt.X*vox2mmMat[2,1] + Pt.Y*vox2mmMat[2,2] + Pt.Z*vox2mmMat[2,3] + vox2mmMat[2,4];
     result.Z := Pt.X*vox2mmMat[3,1] + Pt.Y*vox2mmMat[3,2] + Pt.Z*vox2mmMat[3,3] + vox2mmMat[3,4];
end;

procedure MatOK(var vox2mmMat: TMat44);
var
  Pt0,Pt1: TPoint3f;
begin
     Pt0 := vox2mm(ptf(0,0,0),vox2mmMat);
     Pt1 := vox2mm(ptf(1,1,1),vox2mmMat);
     vectorSubtract(Pt0,Pt1);
     if (Pt0.X <> 0) and (Pt0.Y <> 0) and (Pt0.Z <> 0) then exit;
     showmsg('NIfTI s-form does not make sense: result will be in voxels not mm');
     showMat(vox2mmMat);
     vox2mmMat := matrixSet(1,0,0,0, 0,1,0,0, 0,0,1,0);
end;

function endtrack (var p: TTrackingPrefs): boolean;
label
  666;
var
  startTime: TDateTime;
  atlas: TNIFTI;
  vox2mmMat: TMat44;
  track: TTrack;
  pos, nVox, maxROI, i, n, m: integer;
  str: string;
  pt: TPoint3f;
  vox: TPoint3i;
  atlasImg: TInts;
  fibers: array of TFiber;
  fiberConnect: array of array of integer;
  txtFile : TextFile;
begin
     result := false; //assume failure
     startTime := Now;
     showmsg(kVers);
     atlas := TNIFTI.Create;
     track := TTrack.Create;
     if not atlas.LoadFromFile(p.atlasName, kNiftiSmoothNone) then begin
        showmsg(format('Unable to load atlas named "%s"', [p.atlasName]));
        goto 666;
     end;
     nVox := atlas.hdr.dim[1] * atlas.hdr.dim[2] * atlas.hdr.dim[3];
     if (atlas.hdr.dim[1] < 3) or (atlas.hdr.dim[2] < 3) or (atlas.hdr.dim[3] < 3) then begin
        showmsg(format('Atlas must be 3D "%s"', [p.atlasName]));
        goto 666;
     end;
     vox2mmMat := atlas.mat;
     MatOK(vox2mmMat);
     //find and report spatial extent of atlas
     setlength(atlasImg, nVox);
     maxROI := 0;
     for pos := 0 to (nVox-1) do begin
         atlasImg[pos] := round(atlas.img[pos]);
         if (atlasImg[pos] > maxROI) then
            maxROI := atlasImg[pos];
         if (atlasImg[pos] < 0) then begin
            showmsg(format('Atlas regions must be positive values "%s"', [p.atlasName]));
            goto 666;
         end;
     end;
     if (maxROI < 2) then begin
        showmsg(format('Atlas regions define at least two regions "%s"', [p.atlasName]));
        goto 666;
     end;
     showmsg(format('Atlas has %d regions', [maxROI]));
     if not track.LoadFromFile(p.trackName) then begin
        showmsg(format('Unable to load tractography file "%s"', [p.trackName]));
        goto 666;
     end;
     showmsg(format(' Track fiber count: %d', [track.n_count]));
     //find all the end/start points
     setlength(fibers, track.n_count);
     i := 0;
     n := 0;
     while i < length(track.tracks) do begin
           m :=   asInt( track.tracks[i]); inc(i);
           if m < 2 then begin
              showmsg(format('Catastrophic error decoding tractography file "%s"', [p.trackName]));
              goto 666;
           end;
           //find ROI of first node
           pt.X := track.tracks[i]; inc(i);
           pt.Y := track.tracks[i]; inc(i);
           pt.Z := track.tracks[i]; inc(i);
           vox := atlas.mm2vox0(pt.X, pt.Y, pt.Z);
           fibers[n].startROI:= -1; //assume out of volume
           if (vox.X >= 0) and (vox.Y >= 0) and (vox.Z >= 0) and (vox.X < atlas.hdr.dim[1]) and (vox.Y < atlas.hdr.dim[2]) and (vox.Z < atlas.hdr.dim[3]) then
              fibers[n].startROI := atlasImg[vox.X + (vox.Y * atlas.hdr.dim[1]) + (vox.Z * atlas.hdr.dim[1] * atlas.hdr.dim[3])];
           //skip nodes between start and end
           i := i + (3 * (m-2));
           //find ROI of final node
           pt.X := track.tracks[i]; inc(i);
           pt.Y := track.tracks[i]; inc(i);
           pt.Z := track.tracks[i]; inc(i);
           vox := atlas.mm2vox0(pt.X, pt.Y, pt.Z);
           fibers[n].endROI:= -1; //assume out of volume
           if (vox.X >= 0) and (vox.Y >= 0) and (vox.Z >= 0) and (vox.X < atlas.hdr.dim[1]) and (vox.Y < atlas.hdr.dim[2]) and (vox.Z < atlas.hdr.dim[3]) then
              fibers[n].endROI := atlasImg[vox.X + (vox.Y * atlas.hdr.dim[1]) + (vox.Z * atlas.hdr.dim[1] * atlas.hdr.dim[3])];
           n := n + 1;
           if n > track.n_count then begin
              showmsg(format('Catastrophic error reading tractography file "%s"', [p.trackName]));
              goto 666;
           end;
     end;
     if track.n_count <> n then begin
        showmsg(format(' Catastrophic error parsing tractography file: %d %d', [track.n_count, n]));
        goto 666;
     end;
     //create matrix of connections
     if not track.isWorldSpaceMM then
        showmsg(format('Warning: tracks are not in world space "%s"', [p.trackName]));
     setlength(fiberConnect, maxROI+1, maxROI+1);
     for m := 0 to maxROI do
         for n := 0 to maxROI do
             fiberConnect[m,n] := 0;
     for i := 0 to (track.n_count-1) do begin
         m := fibers[i].startROI;
         n := fibers[i].endROI;
         if (m > n) then begin //sort order: only fill upper triangle
            n := fibers[i].startROI;
            m := fibers[i].endROI;
         end;
         if (m >= 0) and (n >= 0) then
            fiberConnect[m,n] := fiberConnect[m,n] + 1;
     end;
     //report results
     i := 0;
     for m := 1 to maxROI do
         for n := 1 to maxROI do
             i := i + fiberConnect[m,n];
     showmsg(format(' Fibers within regions %d (of total %d)', [i, track.n_count]));
     if i < 1 then goto 666;
     if length(p.outName) > 0 then begin
        showmsg(format(' Creating output %s', [p.outName]));
        AssignFile(txtFile, p.outName);
        ReWrite(txtFile);
        WriteLn(txtFile, kVers);
        WriteLn(txtFile, format('Atlas %s', [p.atlasName]));
        WriteLn(txtFile, format('Track %s', [p.trackName]));
        WriteLn(txtFile, format('Fibers within regions %d (of total %d)', [i, track.n_count]));
     end;
     for m := 1 to maxROI do begin
         str := '';
         for n := 1 to maxROI do begin
             if m < n then
                str := str + inttostr(fiberConnect[m,n]) + chr(9)
             else
                 str := str + inttostr(fiberConnect[n,m]) + chr(9);
         end;
         if length(p.outName) > 0 then
            writeln(txtFile, str)
         else
             showmsg(str);
     end;
     if length(p.outName) > 0 then
        CloseFile(txtFile);
     showmsg(format('Fiber tracking completed (%dms)', [ MilliSecondsBetween(Now, startTime)]));
    result := true;
666:
    //clean up
     atlas.Free;
     track.Close;
     setlength(fibers,0);
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
var
   xname: string;
begin
  xname := extractfilename(ExeName);
  showmsg(kVers);
  showmsg('Usage: '+ xname+ ' -a atlasname [-o outputname] trackname');
  showmsg(' Reports matrix of tracks that connect between every region in atlas');
  showmsg('Options');
  showmsg(' -h show help');
  showmsg(' -a atlas name (e.g. "jhu.nii")');
  showmsg(' -o output name (e.g. "basename.txt")');
  showmsg('Examples');
  {$IFDEF UNIX}
   showmsg(' '+xname+' -a "~/atlas/jhu.nii" "~/fiber.trk.gz"');
  {$ELSE}
   to do showmsg(' '+xname+' -t 1 -o "c:\out dir\shrunk.vtk" "c:\in dir in.vtk"');
  {$ENDIF}
end;

function FindNii(pth, n, x: string; var p: TTrackingPrefs; reportError: integer): boolean;
begin
     result := true;
     p.atlasName := pth+n+x;
     if fileexists(p.atlasName) then exit;
     result := false;
     if reportError <> 0 then
        showmsg(format('Unable to find "%s"',[p.atlasName]));
end;//FindNii()

function FindNiiFiles(var basename: string; var p: TTrackingPrefs): boolean;
var
   pth,n,x: string;
   i: integer;
begin
  result := true;
  FilenameParts (basename, pth,n, x);
  for i := 0 to 1 do begin
    x := '.nii.gz';
    if FindNii(pth, n, x, p, i) then exit;
    x := '.nii';
    if FindNii(pth, n, x, p, i) then exit;
  end;
  result := false;
end;// FindNiiFiles()

procedure TFiberQuant.DoRun;
var
  p : TTrackingPrefs = (trackName: ''; atlasName: ''; outName: '');
begin
  // parse parameters
  if HasOption('h', 'help') or (ParamCount = 0) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  if HasOption('a','a') then
     p.atlasName := GetOptionValue('a','a');
  if not FindNiiFiles(p.atlasName, p) then begin
     showmsg('Error: unable to find atlas file');
     WriteHelp;
     Terminate;
     Exit;
  end;
  if HasOption('o','o') then
     p.outName := GetOptionValue('o','o');
  p.trackName := ParamStr(ParamCount);
  if not fileexists(p.trackName) then begin
     WriteHelp;
     Terminate;
     Exit;
  end;
  if p.outName = '' then
     p.outName := changefileext(p.trackName, '.txt');
  endtrack(p);
  Terminate;
end;// DoRun()

var
  Application: TFiberQuant;
begin
  Application:=TFiberQuant.Create(nil);
  Application.Title:='Entrail';
  Application.Run;
  Application.Free;
end.

