program tracktion;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, nifti_loader, define_types, matmath, math,
tracktion_tracks;

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

procedure showmessage(msg: string);
begin
     writeln(msg);
end;

const
  mxTrkLen = 512;

Type
TNewTrack = record
   len: integer;
   dir: TPoint3f;
   pts: array [0..mxTrkLen] of TPoint3f;
end;


function track: boolean;
//http://individual.utoronto.ca/ktaylor/DTIstudio_mori2006.pdf
// http://www.ncbi.nlm.nih.gov/pubmed/16413083
//Specifically section 2.3 Fiber Tracking
const
  mskName = '/Users/rorden/Documents/pas/surfice/FA.nii.gz';
  v1Name = '/Users/rorden/Documents/pas/surfice/V1.nii.gz';
  mskThresh : single = 0.3;   //minFA 0.15
  stepSize : single = 0.5;
  maxAngleDeg : single = 45;
  minLength = 10;


label
  666;
var
   msk, v1: TNIFTI;
   mskMap :TImgRaw;
   nTrack, vx, i, x,y,z, nSee, nSeed, sliceVox, volVox: integer;
   YMap, ZMap: TInts;
   negTrk, posTrk: TNewTrack;
   minCosine: single;
function XYZ2vox(xi,yi,zi: integer): integer; inline;
//convert from 3D coordinates to 1D array
begin
     result := xi + YMap[yi] + ZMap[zi];
end;
(*function getDir(xi,yi,zi: integer): TPoint3f; inline;
var
   vxi: integer;
//convert from 3D coordinates to 1D array
begin
     vxi := xi + YMap[yi] + ZMap[zi];
     if mskMap[vxi] <> 1 then begin //FA out of range
        result := ptf(10,10,10);
        exit;
     end;
     result.X := v1.img[vxi]; //1st volume is X direction
     result.Y := v1.img[vxi+volVox]; //2nd volume is Y direction
     result.Z := v1.img[vxi+volVox+volVox]; //3rd volume is Z direciton
end;*)

function getVoxelIntensity(Pt: TPoint3f; vol: integer): single;
//http://paulbourke.net/miscellaneous/interpolation/
var
  PtLo, PtHi: TPoint3i;
  FracLo, FracHi: TPoint3f;
//convert from 3D coordinates to 1D array
begin
     //http://paulbourke.net/miscellaneous/interpolation/
     PtLo:= pti(trunc(Pt.x), trunc(Pt.y), trunc(Pt.z));
     PtHi := vectorAdd(PtLo, 1);
     FracHi.X := Pt.X - PtLo.X;
     FracHi.Y := Pt.Y - PtLo.Y;
     FracHi.Z := Pt.Z - PtLo.Z;
     FracLo := ptf(1,1,1);
     vectorSubtract(FracLo, FracHi);
     result := v1.img[XYZ2vox(PtLo.X, PtLo.Y, PtLo.Z)+vol*volVox] * FracLo.X *FracLo.Y * FracLo.Z //000
             + v1.img[XYZ2vox(PtHi.X, PtLo.Y, PtLo.Z)+vol*volVox] * FracHi.X *FracLo.Y * FracLo.Z //100
             + v1.img[XYZ2vox(PtLo.X, PtHi.Y, PtLo.Z)+vol*volVox] * FracLo.X *FracHi.Y * FracLo.Z //010
             + v1.img[XYZ2vox(PtLo.X, PtLo.Y, PtHi.Z)+vol*volVox] * FracLo.X *FracLo.Y * FracHi.Z //001
             + v1.img[XYZ2vox(PtHi.X, PtLo.Y, PtHi.Z)+vol*volVox] * FracHi.X *FracLo.Y * FracHi.Z //101
             + v1.img[XYZ2vox(PtLo.X, PtHi.Y, PtHi.Z)+vol*volVox] * FracLo.X *FracHi.Y * FracHi.Z //011
             + v1.img[XYZ2vox(PtHi.X, PtHi.Y, PtLo.Z)+vol*volVox] * FracHi.X *FracHi.Y * FracLo.Z //110
             + v1.img[XYZ2vox(PtHi.X, PtHi.Y, PtHi.Z)+vol*volVox] * FracHi.X *FracHi.Y * FracHi.Z //111
             ;
end;

function getDir(Pt: TPoint3f): TPoint3f; inline;
var
  iPt: TPoint3i;
//convert from 3D coordinates to 1D array
begin
     iPt:= pti(round(Pt.x), round(Pt.y), round(Pt.z));
     if mskMap[XYZ2vox(iPt.X, iPt.Y, iPt.Z)] <> 1 then begin //FA out of range
        result := ptf(10,10,10);
        exit;
     end;
     result.X := getVoxelIntensity(Pt, 0);
     result.Y := getVoxelIntensity(Pt, 1);
     result.Z := getVoxelIntensity(Pt, 2);

end;
procedure AddSteps(var Trk: TNewTrack; seedStart: TPoint3f; reverseDir: boolean);
var
   pos, dir: TPoint3f;
   cosine: single;
begin
     Trk.len := 0;
     pos := seedStart;
     Trk.dir := getDir(SeedStart);
     if reverseDir then
        vectorMult(dir,-1);
     while (Trk.dir.X < 5) and (Trk.len < mxTrkLen) do begin
           Trk.pts[Trk.len] := pos; //add previous point
           Trk.len := Trk.len + 1;
           vectorAdd(pos, vectorMult(Trk.dir, stepSize)); //move in new direction by step size
           dir := getDir(pos);
           cosine := vectorDot(dir, Trk.dir);
           if ( abs(cosine) < minCosine) then exit; //if steep angle: fiber ends
           if (cosine < 0) then
              vectorMult(dir,-1);
           Trk.dir := dir;
     end;
end; //AddStep
begin
     result := false;
     msk := TNIFTI.Create;
     v1 := TNIFTI.Create;
     minCosine := cos(DegToRad(maxAngleDeg));
     //load mask
     if not msk.LoadFromFile(mskName, kNiftiSmoothNone) then begin
        showmessage('Unable to load '+ mskName);
        goto 666;
     end;
     if (msk.minInten = msk.maxInten) then begin
        showmessage('Error: No variability in mask '+ mskName);
        goto 666;
     end;
     if specialsingle(mskThresh) then
        mskThresh := (0.5 * (msk.maxInten - msk.minInten))+ msk.minInten;
     if (mskThresh < msk.minInten) or (mskThresh > msk.maxInten) then begin
        mskThresh := (0.5 * (msk.maxInten - msk.minInten))+ msk.minInten;
        showmessage(format('Requested threshold make sense (image range %g..%g). Using %g.',[msk.minInten, msk.maxInten, mskThresh]));
        goto 666;
     end;
     //load V1
     v1.isLoad4D:= true;
     if not v1.LoadFromFile(v1Name, kNiftiSmoothNone) then begin
        showmessage('Unable to load '+ v1Name);
        goto 666;
     end;
     volVox := length(msk.img);
     if (volVox *3 ) <> length(v1.img) then begin
        showmessage(format('Error: v1 should have 3 times the voxels as the mask (voxels %d  vs %d). Check v1 has 3 volumes and image dimensions match', [length(v1.img), length(msk.img)] ));
        goto 666;
     end;
     //make arrays for converting from 3D coordinates to 1D array
     sliceVox := msk.hdr.dim[1] * msk.hdr.dim[2]; //voxels per slice
     setlength(YMap, msk.hdr.dim[2]);
     for i := 0 to (msk.hdr.dim[2]-1) do
         YMap[i] := i * msk.hdr.dim[1];
     setlength(ZMap, msk.hdr.dim[3]);
     for i := 0 to (msk.hdr.dim[3]-1) do
         ZMap[i] := i * sliceVox;

     //set byte mask: vs msk.img this is less memory and faster (int not float)
     setlength(mskMap, volVox);
     for i := 0 to (volVox -1) do
         mskMap[i] := 0;
     nSeed := 0;
     for i := 0 to (volVox -1) do
         if (msk.img[i] > mskThresh) then begin
            mskMap[i] := 1;
            nSeed := nSeed + 1;
         end;
     showmessage(format('Seed %d.',[nSeed]));
     msk.Close;
     //next: we will zero the edge so we do not need to do bounds checking
     for i := 0 to (sliceVox -1) do begin
         mskMap[i] := 0; //bottom slice
         mskMap[volVox-1-i] := 0; //top slice
     end;
     //erase left and right edges
     for z := 0 to (msk.hdr.dim[3]-1) do //for each slice
         for y := 0 to (msk.hdr.dim[2]-1) do begin //for each row
             mskMap[XYZ2vox(0,y,z)] := 0;
             mskMap[XYZ2vox(msk.hdr.dim[1]-1,y,z)] := 0;
         end;
     //erase anterior and posterior edges
     for z := 0 to (msk.hdr.dim[3]-1) do //for each slice
         for x := 0 to (msk.hdr.dim[1]-1) do begin //for each column
             mskMap[XYZ2vox(x,0,z)] := 0;
             mskMap[XYZ2vox(x,msk.hdr.dim[2]-1,z)] := 0;
         end;




     //map fibers
     nseed := 0;
     nsee := 0;
     ntrack := 0;
     for z := 1 to (msk.hdr.dim[3]-2) do //for each slice [except edge]
         for y := 1 to (msk.hdr.dim[2]-2) do //for each row [except edge]
             for x := 1 to (msk.hdr.dim[1]-2) do begin //for each column [except edge]
                 vx := XYZ2vox(x,y,z);
                 if (mskMap[vx] = 1) then begin
                    AddSteps(posTrk, ptf(x,y,z), false);
                    nseed := max(nseed, posTrk.len);
                    AddSteps(negTrk, ptf(x,y,z), true);
                    nsee := max(nsee, negTrk.len);
                    if ((posTrk.len+negTrk.len) >= minLength) then begin
                       ntrack := ntrack + 1;
                    end;

                 end; //FA above threshold: create new fiber

             end; //for x

    showmessage(format('See  %d  %d  %d.',[nSeed, nSee, ntrack]));


     result := true;
666:
     msk.Free;
     v1.Free;


end;

const
  kTol = 0.3;

procedure TFiberQuant.DoRun;
var
  ErrorMsg, inname, outname: String;
  Tol: single;
begin
     track; Terminate; exit;
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
  //SimplifyTracks(inname, outname, tol);
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
  Application.Title:='Tracktion';
  Application.Run;
  Application.Free;
end.

