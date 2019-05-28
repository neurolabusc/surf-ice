program tracktion;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, nifti_loader, define_types, matmath, math,
track, DateUtils;

const
  kMaxWayPoint = 8; //we can store 8 independent waypoint maps with 1-byte per pixel
type
  TTrackingPrefs = record
    mskName, v1Name, msk2Name, v2Name, outName: string;
    waypointName: array [0..(kMaxWayPoint-1)] of string;
    simplifyToleranceMM, simplifyMinLengthMM, mskThresh, stepSize, maxAngleDeg, bedpostExponent, redundancyToleranceMM : single;
    minLength, smooth, seedsPerVoxel: integer;
  end;
  { TFiberQuant }

  TFiberQuant = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp(var p: TTrackingPrefs); virtual;
  end;


procedure showMsg(msg: string);
begin
  writeln(msg);
end;

const
  mxTrkLen = 512;

type
  TNewTrack = record
    len: integer;
    dir: TPoint3f;
    pts: array [0..mxTrkLen] of TPoint3f;
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

function FindImgVal(var filename: string; out volume: integer): boolean;
// "/dir/img" will return "/dir/img.nii"; "img.nii,3" will return 3 as volume number
var
   p,n,x, basename: string;
   idx: integer;
begin
  result := true;
  basename := filename;
  volume := 0;
  idx := LastDelimiter(',',filename);
  if (idx > 0) and (idx < length(filename)) then begin
     x := copy(filename, idx+1, length(filename));
     volume := StrToIntDef(x,-1);
     if volume < 0 then
        showmsg('Expected positive integer after comma: '+filename)
     else
         filename := copy(filename, 1, idx-1);
     //if not file
     //showmsg(format('"%s" %d', [filename, volume]) );
  end;
  //FilenameParts (basename, pth,n, x);
  if fileexists(filename) then exit;
  FilenameParts (filename, p, n, x);
  filename := p + n + '.nii.gz';
  if fileexists(filename) then exit;
  filename := p + n + '.nii';
  if fileexists(filename) then exit;
  showmsg('Unable to find images "'+basename+'"');
  result := false;
end;// FindImgVol()

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

function track (var p: TTrackingPrefs): boolean;
//http://individual.utoronto.ca/ktaylor/DTIstudio_mori2006.pdf
// http://www.ncbi.nlm.nih.gov/pubmed/16413083
//Specifically section 2.3 Fiber Tracking
{$DEFINE BEDPOST} //if defined, input is bedpost output (multiple fibers)
const
  kChunkSize = 16384;
label
  666;
var
  startTime: TDateTime;
  {$IFDEF BEDPOST} isBedpost {$ENDIF}: boolean;
  msk, v1, {$IFDEF BEDPOST} msk2, v2 {$ENDIF}: TNIFTI;
  waypointBits: byte;
  mskMap, waypointMap :TImgRaw;
  vox2mmMat: TMat44;
  seedOrigin, pixDim : TPoint3f;
  waypointName: string;
  TrkPos, vx, i, j, x,y,z, sliceVox, volVox, seed, waypointVal: integer;
  YMap, ZMap: TInts;
  negTrk, posTrk: TNewTrack;
  minCosine: single;
  Trk: TTrack;
function XYZ2vox(xi,yi,zi: integer): integer; inline;
//convert from 3D coordinates to 1D array
begin
     result := xi + YMap[yi] + ZMap[zi];
end;//nested XYZ2vox()
{$IFDEF LINEAR_INTERPOLATE}
function getVoxelIntensity(Pt: TPoint3f; vol: integer): single;
//http://paulbourke.net/miscellaneous/interpolation/
var
  PtLo, PtHi: TPoint3i;
  FracLo, FracHi: TPoint3f;
  volOffset : integer;
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
     volOffset := vol*volVox;
     result := v1.img[XYZ2vox(PtLo.X, PtLo.Y, PtLo.Z)+volOffset] * FracLo.X *FracLo.Y * FracLo.Z //000
             + v1.img[XYZ2vox(PtHi.X, PtLo.Y, PtLo.Z)+volOffset] * FracHi.X *FracLo.Y * FracLo.Z //100
             + v1.img[XYZ2vox(PtLo.X, PtHi.Y, PtLo.Z)+volOffset] * FracLo.X *FracHi.Y * FracLo.Z //010
             + v1.img[XYZ2vox(PtLo.X, PtLo.Y, PtHi.Z)+volOffset] * FracLo.X *FracLo.Y * FracHi.Z //001
             + v1.img[XYZ2vox(PtHi.X, PtLo.Y, PtHi.Z)+volOffset] * FracHi.X *FracLo.Y * FracHi.Z //101
             + v1.img[XYZ2vox(PtLo.X, PtHi.Y, PtHi.Z)+volOffset] * FracLo.X *FracHi.Y * FracHi.Z //011
             + v1.img[XYZ2vox(PtHi.X, PtHi.Y, PtLo.Z)+volOffset] * FracHi.X *FracHi.Y * FracLo.Z //110
             + v1.img[XYZ2vox(PtHi.X, PtHi.Y, PtHi.Z)+volOffset] * FracHi.X *FracHi.Y * FracHi.Z //111
             ;
end;//nested
{$ELSE}
function getVoxelIntensity(Pt: TPoint3f; vol: integer): single;
// nearest neighbor
var
  PtLo: TPoint3i;
begin
     PtLo:= pti(round(Pt.x), round(Pt.y), round(Pt.z));
     result := v1.img[XYZ2vox(PtLo.X, PtLo.Y, PtLo.Z)+vol*volVox]; //000
end;//nested
{$ENDIF}
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
     vectorNormalize(result);
end;//nested
{$IFDEF BEDPOST}
function getVoxelIntensity2(Pt: TPoint3f; vol: integer): single;
// nearest neighbor
var
  PtLo: TPoint3i;
begin
     PtLo:= pti(round(Pt.x), round(Pt.y), round(Pt.z));
     result := v2.img[XYZ2vox(PtLo.X, PtLo.Y, PtLo.Z)+vol*volVox]; //000
end;//nested
function getDir2(Pt: TPoint3f; out frac1vs2: single): TPoint3f; inline;
var
  iPt: TPoint3i;
//convert from 3D coordinates to 1D array
begin
     iPt:= pti(round(Pt.x), round(Pt.y), round(Pt.z));
     if mskMap[XYZ2vox(iPt.X, iPt.Y, iPt.Z)] <> 1 then begin //FA out of range
        result := ptf(10,10,10);
        exit;
     end;
     result.X := getVoxelIntensity2(Pt, 0);
     result.Y := getVoxelIntensity2(Pt, 1);
     result.Z := getVoxelIntensity2(Pt, 2);
     vectorNormalize(result);
     frac1vs2 := msk2.img [XYZ2vox(round(Pt.x), round(Pt.y), round(Pt.z))];
end;//nested
{$ENDIF}
procedure AddSteps(var newTrk: TNewTrack; seedStart: TPoint3f; reverseDir: boolean);
var
   dirScaled, pos, dir {$IFDEF BEDPOST}, dir2 {$ENDIF}: TPoint3f;
   cosine {$IFDEF BEDPOST}, cosine2, frac1vs2 {$ENDIF}: single;
begin
     newTrk.len := 0;
     pos := seedStart;
     newTrk.dir := getDir(SeedStart);
     if reverseDir then
        newTrk.dir := vectorScale(newTrk.dir,-1);
     while (newTrk.dir.X < 5) and (newTrk.len < mxTrkLen) do begin
           newTrk.pts[newTrk.len] := pos; //add previous point
           newTrk.len := newTrk.len + 1;
           {$IFDEF ASSUME_ISOTROPIC}
           vectorAdd(pos, vectorMult(newTrk.dir, p.stepSize)); //move in new direction by step size
           {$ELSE}
           dirScaled := vectorScale(newTrk.dir, pixDim);
           vectorAdd(pos, vectorScale(dirScaled, p.stepSize)); //move in new direction by step size
           {$ENDIF}
           dir := getDir(pos);
           cosine := vectorDot(dir, newTrk.dir);
           {$IFDEF BEDPOST}
           if (isBedpost) then begin
              dir2 := getDir2(pos, frac1vs2);
              cosine2 := vectorDot(dir2, newTrk.dir);
              //cost function: to compare two vectors v1, v2
              // a.) cosine in range 0..1 is higher for lower bending angle
              // b.) mask2 is in range 0.5..1 with higher values meaning v1 more likely
              if ((abs(cosine2) * frac1vs2)  > abs(cosine)) then begin
                 cosine := cosine2;
                 dir := dir2;
              end;
           end;
           {$ENDIF}
           if ( abs(cosine) < minCosine) then exit; //if steep angle: fiber ends
           if (cosine < 0) and (dir.X < 5) then
              dir := vectorScale(dir,-1);
           newTrk.dir := dir;
     end;
end;//nested AddSteps()
procedure AddFiber;
var
   newVtx, newItems, outPos, i, iStop : integer;
   VtxBits : byte;
   Pt : TPoint3f;
begin
     newVtx := 0;
     if (posTrk.len > 1) then
        newVtx := newVtx + posTrk.len;
     if (negTrk.len > 1) then
        newVtx := newVtx + negTrk.len;
     if (posTrk.len > 1) and (negTrk.len > 1) then
       newVtx := newVtx - 1; //1st vertex shared by both
     if (newVtx < 2) then exit;
     if (waypointBits > 0) then begin
        VtxBits := 0;
        if posTrk.len > 1 then
           for i := 0 to (posTrk.len -1) do
               VtxBits := VtxBits or waypointMap[XYZ2vox(round(posTrk.pts[i].X), round(posTrk.pts[i].Y), round(posTrk.pts[i].Z) )];
        if negTrk.len > 1 then
           for i := 0 to (negTrk.len -1) do
               VtxBits := VtxBits or waypointMap[XYZ2vox(round(negTrk.pts[i].X), round(negTrk.pts[i].Y), round(negTrk.pts[i].Z) )];
        if VtxBits <> waypointBits then exit;
     end;
     newItems := 1 + (newVtx * 3); //each fiber: one element encoding number of vertices plus 3 values (X,Y,Z) for each vertex
     if length(Trk.tracks) < (TrkPos + newItems) then
        setlength(Trk.tracks, TrkPos + newItems + kChunkSize); //large ChunkSize reduces the frequency of the slow memory re-allocation
     Trk.tracks[TrkPos] := asSingle(newVtx);
     outPos := 1;
     if (negTrk.len > 1) then begin
       if (posTrk.len > 1) then
          iStop := 1 //do not save seed node if it is shared between positive and negative
       else
           iStop := 0;
       for i := (negTrk.len -1) downto iStop do begin
             Pt := negTrk.pts[i];
             Pt := vox2mm(Pt, vox2mmMat);
           Trk.tracks[TrkPos+outPos] := Pt.X; outPos := outPos + 1;
           Trk.tracks[TrkPos+outPos] := Pt.Y; outPos := outPos + 1;
           Trk.tracks[TrkPos+outPos] := Pt.Z; outPos := outPos + 1;
       end;
     end;
     if (posTrk.len > 1) then begin
       for i := 0 to (posTrk.len -1) do begin
           Pt := posTrk.pts[i];
           Pt := vox2mm(Pt, vox2mmMat);
           Trk.tracks[TrkPos+outPos] := Pt.X; outPos := outPos + 1;
           Trk.tracks[TrkPos+outPos] := Pt.Y; outPos := outPos + 1;
           Trk.tracks[TrkPos+outPos] := Pt.Z; outPos := outPos + 1;
       end;
     end;
     TrkPos := TrkPos + newItems;
     Trk.n_count := Trk.n_count + 1;
end;//nested AddFiber()
begin
     result := false;
     startTime := Now;
     msk := TNIFTI.Create;
     v1 := TNIFTI.Create;
     {$IFDEF BEDPOST}
     msk2 := TNIFTI.Create;
     v2 := TNIFTI.Create;
     {$ENDIF}
     Trk := TTrack.Create;
     TrkPos := 0; //empty TRK file
     minCosine := cos(DegToRad(p.maxAngleDeg));
     if (p.seedsPerVoxel < 1) or (p.seedsPerVoxel > 9) then begin
        showmsg('seedsPerVoxel must be between 1 and 9');
        p.seedsPerVoxel := 1;
     end;
     //load mask
     if not msk.LoadFromFile(p.mskName, kNiftiSmoothNone) then begin
        showmsg(format('Unable to load mask named "%s"', [p.mskName]));
        goto 666;
     end;
     //pixDim is normalized pixel scaling, so image with 1x1mm in plane and 2mm slice thickness will be 0.5,0.5,1.0
     pixDim := ptf(abs(msk.hdr.pixdim[1]),abs(msk.hdr.pixdim[2]),abs(msk.hdr.pixdim[3]));
     if min(pixDim.X,min(pixDim.Y,pixDim.Z)) <= 0 then
        pixDim := ptf(1,1,1);
     vectorReciprocal(pixDim); //in terms of voxels, move much less in the thicker direction than the thinner direction
     //vectorNormalize(pixDim); //would make vector length 1
     pixDim := vectorScale(pixDim, 1/max(pixDim.X,max(pixDim.Y,pixDim.Z))); //make longest component 1
     if min(pixDim.X,min(pixDim.Y,pixDim.Z)) < 0.5 then
        showmsg(format('Warning: pixel mm very anisotropic %g %g %g ',[msk.hdr.pixdim[1], msk.hdr.pixdim[2], msk.hdr.pixdim[3]]));
     //showmsg(format('pixel mm anisotropy %g %g %g ',[pixDim.X, pixDim.Y, pixDim.Z]));
     vox2mmMat := msk.mat;
     MatOK(vox2mmMat);
     if (msk.minInten = msk.maxInten) then begin
        showmsg('Error: No variability in mask '+ p.mskName);
        goto 666;
     end;
     if specialsingle(p.mskThresh) then
        p.mskThresh := (0.5 * (msk.maxInten - msk.minInten))+ msk.minInten;
     if (p.mskThresh < msk.minInten) or (p.mskThresh > msk.maxInten) then begin
        p.mskThresh := (0.5 * (msk.maxInten - msk.minInten))+ msk.minInten;
        showmsg(format('Requested threshold make sense (image range %g..%g). Using %g.',[msk.minInten, msk.maxInten, p.mskThresh]));
        goto 666;
     end;
     //load V1
     v1.isLoad4D:= true;
     if not v1.LoadFromFile(p.v1Name, kNiftiSmoothNone) then begin
        showmsg(format('Unable to load V1 named "%s"', [p.v1Name]));
        goto 666;
     end;
     volVox := length(msk.img);
     if (volVox *3 ) <> length(v1.img) then begin
        showmsg(format('Error: v1 should have 3 times the voxels as the mask (voxels %d  vs %d). Check v1 has 3 volumes and image dimensions match', [length(v1.img), length(msk.img)] ));
        goto 666;
     end;
     {$IFDEF BEDPOST}
     isBedpost := false;
     if (p.v2Name <> '') and (p.msk2Name <> '') then begin
       isBedpost := true;
       v2.isLoad4D:= true;
       if not v2.LoadFromFile(p.v2Name, kNiftiSmoothNone) then begin
          showmsg(format('Unable to load V2 named "%s"', [p.v2Name]));
          goto 666;
       end;
       if length(v1.img) <> length(v2.img) then begin
          showmsg(format('Dimension mismatch "%s" "%s"', [p.v1Name, p.v2Name] ));
          goto 666;
       end;
       if not msk2.LoadFromFile(p.msk2Name, kNiftiSmoothNone) then begin
          showmsg(format('Unable to load mask 2 named "%s"', [p.msk2Name]));
          goto 666;
       end;
       if length(msk.img) <> length(msk2.img) then begin
          showmsg(format('Dimension mismatch "%s" "%s"', [p.mskName, p.msk2Name] ));
          goto 666;
       end;
       for i := 0 to (volVox -1) do begin
           //set msk to be sum of probabilities for v1 and v2, set msk2 to my proportion of v1/(v1+v2)
           msk.img[i] := msk.img[i] + msk2.img[i];  //probability of both fibers is mean_f1samples + mean_f2samples
           //msk2 will be the cost function for selecting the 2nd fiber instead of the first
           // Intuitively, the probability of fiber 2 vs fiber 1, e.g. if mean_f1samples = 0.6 and mean_f1samples = 0.2 then frac1vs2 = 0.2/0.6 = 0.3333
           if msk.img[i] > 0 then begin
              msk2.img[i] := msk2.img[i] / msk.img[i];
              //however, we might want to adjust this weighting using sqr or sqrt
              msk2.img[i] := power(msk2.img[i],p.bedpostExponent); //exponent=0: f1 and f2 treated equally, 1: f1 and f2 proportional to probability, 100: strongly prefer f1
           end;
       end;
     end;
     {$ENDIF}
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
     for i := 0 to (volVox -1) do
         if (msk.img[i] > p.mskThresh) then
            mskMap[i] := 1;
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
     //check that voxels survive for mapping
     vx := 0;
     for i := 0 to (volVox -1) do
         if (mskMap[i] = 1) then
            vx := vx + 1;
     if (vx < 1) then begin //since we already have checked mskThresh, we only get this error if the only voxels surviving threshold were on the outer boundary
        showmsg(format(' No voxels have FA above %.3f',[p.mskThresh]));
        goto 666;
     end;
     {$IFDEF BEDPOST}
     if (isBedpost) then
        showmsg(format(' %d voxels have probabilities above %.3f',[vx, p.mskThresh]))
     else
     {$ENDIF}
     showmsg(format(' %d voxels have FA above %.3f',[vx, p.mskThresh]));
     //setup waypoints
     setlength(waypointMap, volVox);
     fillchar(waypointMap[0], volVox, 0);
     waypointBits := 0;
     for i := 0 to (kMaxWayPoint-1) do begin
         if length(p.waypointName[i]) < 1 then continue;
         waypointName := p.waypointName[i];
         if not FindImgVal(waypointName, waypointVal) then continue;
         if not msk.LoadFromFile(waypointName, kNiftiSmoothNone) then begin
            showmsg(format('Unable to load mask named "%s"', [waypointName]));
            goto 666;
         end;
         if volVox <> length(msk.img) then begin
            showmsg(format('Error: waypoint image should have same dimensions as other images (voxels %d  vs %d): %s', [volVox, length(msk.img), p.waypointName[i]] ));
            goto 666;
         end;
         vx := 0;
         x := 1 shl i;
         if waypointVal <> 0 then
            for j := 0 to (volVox -1) do
                if msk.img[j] = waypointVal then begin
                   waypointMap[j] := waypointMap[j] + x;
                   vx := vx + 1;
                end;
         if waypointVal = 0 then
            for j := 0 to (volVox -1) do
                if msk.img[j] <> 0 then begin
                   waypointMap[j] := waypointMap[j] + x;
                   vx := vx + 1;
                end;
         if vx > 0 then begin
            waypointBits := waypointBits + (1 shl i); //1,2,4,8,16
            showmsg(format('%s has %d voxels',[p.waypointName[i], vx]));
         end else
             showmsg(format('Warning: %s has NO surviving voxels. Intensity range %g..%g',[p.waypointName[i], msk.minInten, msk.maxInten ]));
     end;
     if waypointBits = 0 then
        setlength(waypointMap, 0);
     //free the mask image, as we use mskMap
     msk.Close;
     //map fibers
     negTrk.len := 0;
     posTrk.len := 0;
     RandSeed := 123; //make sure "random" seed placement is precisely repeated across runs
     for z := 1 to (msk.hdr.dim[3]-2) do begin //for each slice [except edge]
         for y := 1 to (msk.hdr.dim[2]-2) do begin //for each row [except edge]
             for x := 1 to (msk.hdr.dim[1]-2) do begin //for each column [except edge]
                 vx := XYZ2vox(x,y,z);
                 if (mskMap[vx] = 1) then begin
                    for seed := 1 to p.seedsPerVoxel do begin
                        if p.seedsPerVoxel = 1 then
                           seedOrigin := ptf(x,y,z)
                        else
                            seedOrigin := ptf(x+0.5-random ,y+0.5-random ,z+0.5-random);
                      AddSteps(posTrk, seedOrigin, false);
                      AddSteps(negTrk, seedOrigin, true);
                      if ((posTrk.len+negTrk.len) >= p.minLength) then
                         AddFiber;
                    end; //for each seed
                 end; //FA above threshold: create new fiber
             end; //for x
         end; //for y
         showmsg('xxx');
         showmsg(format('Completed %d/%d', [z, msk.hdr.dim[3])]));
     end; //for z
    setlength(Trk.tracks, TrkPos);
    //smooth tracks and simplify
    if length(Trk.tracks) < 1 then begin
      showmsg('No fibers found');
      goto 666;
    end;
    if p.smooth = 1 then
       Trk.Smooth;
    Trk.SimplifyRemoveRedundant(p.redundancyToleranceMM);
    Trk.SimplifyMM(p.simplifyToleranceMM, p.simplifyMinLengthMM);
    if p.smooth = 1 then //run a second time after simplification
       Trk.Smooth;
    //save data
    Trk.Save(p.outName);
    showmsg(format('Fiber tracking completed (%dms)', [ MilliSecondsBetween(Now, startTime)]));
    result := true;
666:
     msk.Free;
     v1.Free;
     {$IFDEF BEDPOST}
     msk2.Free;
     v2.Free;
     {$ENDIF}
     Trk.Close;
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

procedure TFiberQuant.WriteHelp (var p: TTrackingPrefs);
var
   xname: string;
begin
  xname := extractfilename(ExeName);
  showmsg('Tracktion by Chris Rorden version 14Feb2017');
  showmsg('Usage: '+ xname+ ' [options] basename');
  showmsg(' Requires dtifit V1 FA images (basename_V1.nii.gz, basename_FA.nii.gz)');
  showmsg('Options');
  showmsg(format(' -a maximum angle bend (degrees, default %.3g)', [p.maxAngleDeg]));
  showmsg(' -h show help');
  showmsg(format(' -l minimum length (mm, default %.3g)', [p.simplifyMinLengthMM]));
  showmsg(' -o output name (.bfloat, .bfloat.gz or .vtk; default "inputName.vtk")');
  showmsg(format(' -s simplification tolerance (mm, default %.3g)', [p.simplifyToleranceMM]));
  {$IFDEF BEDPOST}
  showmsg(format(' -t threshold (FA for dtifit, probability for bedpost) (default %.3g)', [p.mskThresh]));
  {$ELSE}
  showmsg(format(' -t threshold (FA for dtifit) (default %.3g)', [p.mskThresh]));
  {$ENDIF}
  showmsg(format(' -w waypoint name (up to %d; default: none)',[kMaxWayPoint]));
  showmsg(format(' -x bedpost exponent (0=sample p1/p2 equally, 2=strongly prefer p1, default %.3g)', [p.bedpostExponent]));
  showmsg(format(' -1 smooth (0=no, 1=yes, default %d)', [p.smooth]));
  showmsg(format(' -2 stepsize (voxels, voxels %.3g)', [p.stepSize]));
  showmsg(format(' -3 minimum steps (voxels, default %d)', [p.minLength]));
  showmsg(format(' -4 redundant fiber removal threshold (mm, default %g)', [p.redundancyToleranceMM]));
  showmsg(format(' -5 seeds per voxel (default %d)', [p.seedsPerVoxel]));
  showmsg('Examples');
  {$IFDEF UNIX}
   showmsg(' '+xname+' -t 0.2 -o "~/out/fibers.vtk" "~/img_V1.nii.gz"');
   showmsg(' '+xname+' -w BA44.nii -w BA3.nii "~/img_V1.nii"');
   {$IFDEF BEDPOST}
   showmsg(' '+xname+' dyads1.nii.gz"');
   {$ENDIF}
  {$ELSE}
   to do showmsg(' '+xname+' -t 1 -o "c:\out dir\shrunk.vtk" "c:\in dir in.vtk"');
  {$ENDIF}
end;

function FindDyads(pth: string; var p: TTrackingPrefs; reportError: integer; isGz: boolean): boolean;
var
  ext: string;
begin
     if isGz then
        ext := '.nii.gz'
     else
         ext := '.nii';
     result := true;
     p.v1Name := pth + 'dyads1'+ext;
     p.mskName := pth+ 'mean_f1samples'+ext;
     if fileexists(p.v1Name) and fileexists(p.mskName) then begin
        p.v2Name := pth + 'dyads2'+ext;
        p.msk2Name := pth+ 'mean_f2samples'+ext;
        if (not fileexists(p.v2Name)) or (not fileexists(p.msk2Name)) then begin
           p.v2Name := '';
           p.msk2Name := '';
        end;
        exit;
     end;
     result := false;
     if reportError <> 0 then
        showmsg(format('Unable to find bedpostX images "%s" and "%s"',[p.v1Name, p.mskName]));
end;//FindDyads()

function FindV1FA(pth, n, x: string; var p: TTrackingPrefs; reportError: integer): boolean;
begin
     result := true;
     p.v1Name := pth+n+'_V1'+x;
     if (not fileexists(p.v1Name)) and (x = '.nii') then
     	p.v1Name := pth+n+'_V1'+ '.nii.gz'; //Allow V1.nii.gz and FA.nii or vice versa
     p.mskName := pth+n+'_FA'+x;
     if fileexists(p.v1Name) and fileexists(p.mskName) then exit;
     if reportError <> 0 then
        showmsg(format('Unable to find "%s" and "%s"',[p.v1Name, p.mskName]));
     result := false;
     result := FindDyads(pth, p, reportError,false);
     if result then exit;
     result := FindDyads(pth, p, reportError,true);
end;//FindV1FA()

function FindNiiFiles(var basename: string; var p: TTrackingPrefs): boolean;
var
   pth,n,x: string;
   i: integer;
begin
  result := true;
  if DirectoryExists(basename) then begin // if ~/dir/bedpost.bedpostX/ then find ~/dir/bedpost.bedpostX/dyads1.nii.gz
    pth := basename;
    if pth[length(pth)] <> pathdelim then
     pth := pth + pathdelim; //e.g. ~/dir and ~/dir/ both become ~/dir/
    if FindDyads(pth, p, 1, true) then exit;
    if FindDyads(pth, p, 1, false) then exit;
  end;
  FilenameParts (basename, pth,n, x);
  for i := 0 to 1 do begin
    x := '.nii.gz';
    if FindV1FA(pth, n, x, p, i) then exit;
    x := '.nii';
    if FindV1FA(pth, n, x, p, i) then exit;
    if length(n) > 3 then begin //i_FA i_V1
       SetLength(n, Length(n) - 3);
       if FindV1FA(pth, n, x, p, i) then exit;
       x := '.nii.gz';
       if FindV1FA(pth, n, x, p, i) then exit;
    end;
  end;
  result := false;
end;// FindNiiFiles()

procedure TFiberQuant.DoRun;
var
  p : TTrackingPrefs = (mskName: ''; v1Name: ''; msk2Name: ''; v2Name: ''; outName: '';
    waypointName: ('','','','',  '','','','');
    simplifyToleranceMM: 0.2;
    simplifyMinLengthMM: 12;
    mskThresh: 0.15;
    stepSize: 0.5;
    maxAngleDeg: 45;
    bedpostExponent: 0.5;
    redundancyToleranceMM: 0;
    minLength: 1;
    smooth: 1;
    seedsPerVoxel: 1);
  basename: string;
  i: integer;
  nWaypoint: integer = 0;
  {$IFDEF BEDPOST}
  isThreshSpecified: boolean = false;
  {$ENDIF}
begin
  // parse parameters
  basename := 'test.nii,7';
  //FindImg(basename, nWaypoint); Terminate; exit;
  if HasOption('h', 'help') or (ParamCount = 0) then begin
    WriteHelp(p);
    Terminate;
    Exit;
  end;
  if HasOption('a','a') then
     p.maxAngleDeg := StrToFloatDef(GetOptionValue('a','a'), p.maxAngleDeg);
  if HasOption('l','l') then
     p.simplifyMinLengthMM := StrToFloatDef(GetOptionValue('l','l'), p.simplifyMinLengthMM);
  if HasOption('o','o') then
     p.outName := GetOptionValue('o','o');
  if HasOption('s','s') then
     p.simplifyToleranceMM := StrToFloatDef(GetOptionValue('s','s'), p.simplifyToleranceMM);
  if HasOption('t','t') then begin
     p.mskThresh := StrToFloatDef(GetOptionValue('t','t'), p.mskThresh);
     isThreshSpecified := true;
  end;
  if HasOption('x','x') then begin
     p.bedpostExponent := StrToFloatDef(GetOptionValue('a','a'), p.bedpostExponent);
     if p.bedpostExponent < 0 then
        p.bedpostExponent := 0;
  end;
  for i := 1 to (ParamCount-2) do begin
    if UpperCase(paramstr(i)) = ('-W') then begin
      if nWaypoint < kMaxWayPoint then
         p.waypointName[nWaypoint] := paramstr(i+1)
      else
          showmsg('Error: Too many waypoints requested');
       nWaypoint := nWaypoint + 1;
    end;
  end;
  if HasOption('1','1') then
     p.smooth := round(StrToFloatDef(GetOptionValue('1','1'), p.smooth));
  if HasOption('2','2') then
     p.stepSize := StrToFloatDef(GetOptionValue('2','2'), p.stepSize);
  if HasOption('3','3') then
     p.minLength := round(StrToFloatDef(GetOptionValue('3','3'), p.minLength));
  if HasOption('4','4') then
     p.redundancyToleranceMM := StrToFloatDef(GetOptionValue('4','4'), p.redundancyToleranceMM);
  if HasOption('5','5') then
     p.seedsPerVoxel := round(StrToFloatDef(GetOptionValue('5','5'), p.seedsPerVoxel));
  basename := ParamStr(ParamCount);
  if (not FileExists(basename)) and (FileExists(basename +'.bvec')) then
  	basename := basename +'.bvec';
  if not FindNiiFiles(basename, p) then begin
     WriteHelp(p);
     Terminate;
     Exit;
  end;
  if p.outName = '' then begin
     if DirectoryExists(basename) then begin
        if basename[length(basename)] = pathdelim then
           p.outName := basename + 'track.vtk'
        else
            p.outName := basename + pathdelim + 'track.vtk'
     end else
         p.outName := ChangeFileExtX(basename, '.vtk');
  end;
  if (not isThreshSpecified) and (p.v2Name <> '') then
     p.mskThresh := 0.01; //for bedpost, use 1% probability, where for FA we usually set a higher threshold (e.g. 0.15)
  track(p);
  Terminate;
end;// DoRun()

var
  Application: TFiberQuant;
begin
  Application:=TFiberQuant.Create(nil);
  Application.Title:='Tracktion';
  Application.Run;
  Application.Free;
end.

