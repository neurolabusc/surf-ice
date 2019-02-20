unit clustering;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, define_types, nifti_loader, dialogs;

function ApplyClusterThreshold(nii: TNIFTI; lThresh: single; lMinClusterVox: integer): boolean;
//lMinClusterVox = 0 : do not filter data
//lMinClusterVox = -1 : preserve largest cluster only


implementation
type
  TIntImg = array of longint;

procedure FindClusters (lXDim, lYDim, lZDim, lMinClusterVox: integer; var lClusterBuff0: TIntImg);
var
   lClusterSign,lClusterSz,lClusterFillValue,lQTail,lQHead,lSliceSz,lQSz,lInc,lVolSz, lMaxClusterVox: integer;//lScaledThresh
   lQra0: TIntImg;
const
     kFillValue = -2;
Procedure IncQra(var lVal, lQSz: integer);
begin
    inc(lVal);
    if lVal >= lQSz then
     lVal := 1;
end;

 procedure Check(lPixel: integer);
 begin
    if (lClusterBuff0[lPixel]=lClusterSign) then begin//add item
        //if lClusterFillValue = kFillvalue then  showmessage(inttostr(lPixel));
        incQra(lQHead,lQSz);
        inc(lClusterSz);
        lClusterBuff0[lPixel] := lClusterFillValue;
        lQra0[lQHead] := lPixel;
   end;
 end;

PROCEDURE RetirePixel; //FIFO cleanup , 1410: added 18-voxel check
VAR
   lVal,lValX,lXPos,lYPos,lZPos: integer;
BEGIN
   lVal := lQra0[lQTail];
   if lVal < 0 then begin
      //should never happen: unmarked voxel  = increment lQTail so not infinite loop
      incQra(lQTail,lQSz); //done with this pixel
      exit;
   end;
   lXpos := lVal mod lXdim; //0..Xdim
   lYpos := (lVal div lXdim) mod lYDim;
   lZpos := (lVal div lSliceSz);
   if (lXPos <= 0) or (lXPos >= (lXDim-1)) or
    (lYPos <= 0) or (lYPos >= (lYDim-1)) or
    (lZPos <= 0) or (lZPos >= (lZDim-1)) then
     // retire and exit
else begin
   Check(lVal-1); //left
   Check(lVal+1); //right
   Check(lVal-lXDim); //up
   Check(lVal+lXDim); //down
   Check(lVal-lSliceSz); //up
   Check(lVal+lSliceSz); //down
   //check plane above
   lValX := lVal + lSLiceSz;
   Check(lValX-1); //left
   Check(lValX+1); //right
   Check(lValX-lXDim); //up
   Check(lValX+lXDim); //down
   //check plane below
   lValX := lVal - lSLiceSz;
   Check(lValX-1); //left
   Check(lValX+1); //right
   Check(lValX-lXDim); //up
   Check(lValX+lXDim); //down
   //check diagonals of current plane
   Check(lVal-lXDim-1); //up, left
   Check(lVal-lXDim+1); //up, right

   Check(lVal+lXDim-1); //down, left
   Check(lVal+lXDim+1); //down, right
end; //not edge
   incQra(lQTail,lQSz); //done with this pixel
END;

procedure FillStart (lPt: integer); {FIFO algorithm: keep memory VERY low}
var lI: integer;
begin
  if (lClusterBuff0[lPt]<>lClusterSign) then exit;
  for lI := 1 to lQsz do
      lQra0[lI] := -1;
  lQHead := 0;
  lQTail := 1;
  Check(lPt);
  RetirePixel;
  // check that there was anything in the cluster at all
  //showmessage('head'+inttostr(lQHead)+'.'+inttostr(lQTail));
  //if lQHead > 2 then begin
    // and do the recursion to get rid of it
  while ((lQHead+1) <> lQTail) do begin//complete until all voxels in buffer have been tested
        RetirePixel;
        if (lQHead = lQSz) and (lQTail = 1) then
           exit; //break condition: avoids possible infinite loop where QTail is being incremented but QHead is stuck at maximum value
  end;
end;

procedure SelectClusters (lSign: integer);
var lInc: integer;
begin
     for lInc := 0 to (lVolSz-1) do begin
         if lClusterBuff0[lInc] = lSign then begin
            // measure size of the cluster and fill it with kFillValue
            lClusterSz := 0;
            lClusterSign := lSign;
            lClusterFillValue := kFillValue;
            FillStart(lInc);
            // now fill the cluster with its size (=1 if the voxel was isolated)
            lClusterFillValue := lClusterSz;
            lClusterSign := kFillValue;
            //if lClusterSz > 1 then ShowMessage(inttostr(lClusterSz)+'@'+inttostr(lInc));
            if lClusterSz > 1 then
                FillStart(lInc)
            else
                lClusterBuff0[lInc] := 1; //fill all voxels in cluster with size of voxel
            end;
     end;
end;

begin
     if (lMinClusterVox = 0) or (lMinClusterVox = 1) then exit;
     lVolSz := lXdim*lYdim*lZdim;
     lSliceSz := lXdim * lYdim;
     if (lXDim < 4) or (lYDim < 4) or (lZDim < 4) or (lVolSz < 1)  then exit;
     //Next - START count cluster size
     lQSz := lVolSz;
     Setlength(lQra0,lQSz+1 );
     //check positive clusters....
     SelectClusters(1);
     Setlength(lQra0, 0);
     //NEXT: mask image data with cluster size
     if (lMinClusterVox < 0) then begin
        //find largest cluster
        lMaxClusterVox  := lClusterBuff0[0];
        for lInc := 0 to (lVolSz-1) do
            if lClusterBuff0[lInc] > lMaxClusterVox then
               lMaxClusterVox := lClusterBuff0[lInc];
        //only preservelargest cluster
        for lInc := 0 to (lVolSz-1) do
            if lClusterBuff0[lInc] < lMaxClusterVox then
               lClusterBuff0[lInc] := 0;
     end else begin
       for lInc := 0 to (lVolSz-1) do
           if lClusterBuff0[lInc] < lMinClusterVox then
              lClusterBuff0[lInc] := 0;

     end;
end;



function ApplyClusterThreshold(nii: TNIFTI; lThresh: single; lMinClusterVox: integer): boolean;
var
  nVox, nSurvive, nSurvive2, i: integer;
  intimg: TIntImg;
  lAlmostThresh, lThreshAbs : single;
  lInvert: boolean = false;
begin
     result := true;
     nVox := nii.hdr.dim[1] * nii.hdr.dim[2] * nii.hdr.dim[3];
     if (lThresh = 0) or (lMinClusterVox = 0) or (lMinClusterVox = 1) or (nVox < lMinClusterVox) or (nVox <> length(nii.img)) then exit; //clustering has no effect
     lThreshAbs := abs(lThresh);
     setlength(intimg, nVox);
     if lThresh < 0 then begin
        lInvert := true;
        for i := 0 to (nVox -1) do
            nii.img[i] := -nii.img[i];
     end;
     nSurvive := 0;
     lAlmostThresh := 0; //largest value that does not exceed threshold
     for i := 0 to (nVox -1) do begin
         intimg[i] := 0;
         if (nii.img[i] > lThreshAbs) then begin
            intimg[i] := 1;
            nSurvive := nSurvive + 1;
         end else if (intimg[i] > lAlmostThresh) then
            lAlmostThresh := intimg[i];
     end;
     if nSurvive < lMinClusterVox then
          exit;
     FindClusters (nii.hdr.dim[1], nii.hdr.dim[2], nii.hdr.dim[3], lMinClusterVox, intimg);
     nSurvive2 := 0;
     for i := 0 to (nVox -1) do
            if(intimg[i] >= 0) then
               nSurvive2 := nSurvive2 + 1;
     if lMinClusterVox > 0 then
        showmessage(format('%d voxels exceed %g, of which %d are part of clusters larger than %d voxels',[nSurvive, lThresh, nSurvive2, lMinClusterVox]));
     for i := 0 to (nVox -1) do
         if (intimg[i] = 0) and (nii.img[i] > lThreshAbs) then
              nii.img[i] := lAlmostThresh;
     setlength(intimg,0);

     if lInvert then
        for i := 0 to (nVox -1) do
            nii.img[i] := -nii.img[i];
end;


(*procedure FindClusters (var lHdr: TMRIcroHdr; lXdim, lYDim, lZDim, lThreshClusterSz: integer);
var
   lThreshClusterSzM1,lClusterSign,lClusterSz,lClusterFillValue,lQTail,lQHead,lSliceSz,lQSz,lInc,lVolSz: integer;//lScaledThresh
   lClusterBuff, lQra: LongIntP;
   //lBuffIn32 : SingleP;
   //lBuffIn16 : SmallIntP;
   //lScaledThreshFloat: double;
  //lFdata: file;
const
     kFillValue = -2;
Procedure IncQra(var lVal, lQSz: integer);
begin
    inc(lVal);
    if lVal >= lQSz then
     lVal := 1;
end;

 procedure Check(lPixel: integer);
 begin
    if (lClusterBuff^[lPixel]=lClusterSign) then begin//add item
        //if lClusterFillValue = kFillvalue then  showmessage(inttostr(lPixel));
        incQra(lQHead,lQSz);
        inc(lClusterSz);
        lClusterBuff^[lPixel] := lClusterFillValue;
        lQra^[lQHead] := lPixel;
   end;
 end;


PROCEDURE RetirePixel; //FIFO cleanup , 1410: added 18-voxel check
VAR
   lXDimM,lVal,lValX,lXPos,lYPos,lZPos: integer;
BEGIN
   lVal := lQra^[lQTail];
   if lVal = 0 then begin
      //should never happen: unmarked voxel  = increment lQTail so not infinite loop
      incQra(lQTail,lQSz); //done with this pixel
      exit;
   end;
   lXpos := lVal mod lXdim;
   if lXpos = 0 then lXPos := lXdim;

   lYpos := (1+((lVal-1) div lXdim)) mod lYDim;
   if lYPos = 0 then lYPos := lYdim;

   lZpos := ((lVal-1) div lSliceSz)+1;
   if (lXPos <= 1) or (lXPos >= lXDim) or
    (lYPos <= 1) or (lYPos >= lYDim) or
    (lZPos <= 1) or (lZPos >= lZDim) then
     // retire and exit
else begin
lXDimM := lXDim;
   Check(lVal-1); //left
   Check(lVal+1); //right
   Check(lVal-lXDimM); //up
   Check(lVal+lXDimM); //down
   Check(lVal-lSliceSz); //up
   Check(lVal+lSliceSz); //down
   //check plane above
   lValX := lVal + lSLiceSz;
   Check(lValX-1); //left
   Check(lValX+1); //right
   Check(lValX-lXDimM); //up
   Check(lValX+lXDimM); //down
   //check plane below
   lValX := lVal - lSLiceSz;
   Check(lValX-1); //left
   Check(lValX+1); //right
   Check(lValX-lXDimM); //up
   Check(lValX+lXDimM); //down
   //check diagonals of current plane
   Check(lVal-lXDimM-1); //up, left
   Check(lVal-lXDimM+1); //up, right

   Check(lVal+lXDimM-1); //down, left
   Check(lVal+lXDimM+1); //down, right
end;{} //not edge
   incQra(lQTail,lQSz); //done with this pixel
END;

procedure FillStart (lPt: integer); {FIFO algorithm: keep memory VERY low}
var lI: integer;
begin
  if (lClusterBuff^[lPt]<>lClusterSign) then exit;
  for lI := 1 to lQsz do
      lQra^[lI] := 0;
  lQHead := 0;
  lQTail := 1;
  Check(lPt);
  RetirePixel;
  // check that there was anything in the cluster at all
  //showmessage('head'+inttostr(lQHead)+'.'+inttostr(lQTail));
  //if lQHead > 2 then begin
    // and do the recursion to get rid of it
  while ((lQHead+1) <> lQTail) do begin//complete until all voxels in buffer have been tested
        RetirePixel;
        if (lQHead = lQSz) and (lQTail = 1) then
           exit; //break condition: avoids possible infinite loop where QTail is being incremented but QHead is stuck at maximum value
  end;
  //end;
  //showmessage('alldone');
end;

procedure SelectClusters (lSign: integer);
var lInc: integer;
begin
     for lInc := 1 to lVolSz do begin
         if lClusterBuff^[lInc] = lSign then begin
            // measure size of the cluster and fill it with kFillValue
            lClusterSz := 0;
            lClusterSign := lSign;
            lClusterFillValue := kFillValue;
            FillStart(lInc);
            // now fill the cluster with its size (=1 if the voxel was isolated)
            lClusterFillValue := lClusterSz;
            lClusterSign := kFillValue;
            //if lClusterSz > 1 then ShowMessage(inttostr(lClusterSz)+'@'+inttostr(lInc));
            if lClusterSz > 1 then
                FillStart(lInc)
            else
                lClusterBuff^[lInc] := 1; //fill all voxels in cluster with size of voxel
            end;
     end;
end;

begin
     lVolSz := lXdim*lYdim*lZdim;
     lSliceSz := lXdim * lYdim;
     if (lXDim < 4) or (lYDim < 4) or (lZDim < 4) or (lVolSz < 1)  then exit;
     GetMem(lClusterBuff, lVolSz* sizeof(LongInt));
     for lInc := 1 to lVolSz do
            lClusterBuff^[lInc] := 0;
     for lInc := 1 to lVolSz do
         if lHdr.ScrnBuffer^[lInc] > 0 then
            lClusterBuff^[lInc] := 1;
     lThreshClusterSzM1 := lThreshClusterSz;
     if lThreshClusterSzM1 < 1 then
        lThreshClusterSzM1 := 1;
     if  (lThreshClusterSzM1 > 1) then begin
         //Next - START count cluster size
         lQSz := (lVolSz div 4)+8;
         GetMem(lQra,lQsz * sizeof(longint) );
         //check positive clusters....
         SelectClusters(1);
         Freemem(lQra);
         //END check clusters
     end; //only count clusters if minimum size > 1, otherwise simple intensity threshold...
     //NEXT: mask image data with cluster size

         for lInc := 1 to lVolSz do
             if lClusterBuff^[lInc] < lThreshClusterSzM1 then
                lHdr.ScrnBuffer^[lInc] := 0;
     Freemem(lClusterBuff);
end;

procedure ClusterScrnImg (var lHdr: TMRIcroHdr; var lClusterVox: integer);
begin
     if (lClusterVox  <= 1) then exit;
     if (lHdr.ImgBufferItems <> (lHdr.NIFTIhdr.dim[1]*lHdr.NIFTIhdr.dim[2]*lHdr.NIFTIhdr.dim[3]) ) then exit;
     FindClusters (lHdr, lHdr.NIFTIhdr.dim[1], lHdr.NIFTIhdr.dim[2], lHdr.NIFTIhdr.dim[3], lClusterVox);
end;    *)

end.

