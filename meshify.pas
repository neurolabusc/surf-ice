unit meshify;

{$mode objfpc}{$H+}

interface

uses
  meshify_simplify, LCLintf, Classes, SysUtils, mesh, nifti_loader, meshify_marchingcubes, dialogs,
  define_types, matmath, Forms, StdCtrls, Controls, Spin, clustering, nifti_types;

function Nii2Mesh(const FileName: string): boolean;
function Atlas2Mesh(const FileName: string): boolean;
function Nii2MeshCore(niiname, meshname: string; threshold, decimateFrac: single; minimumClusterVox, smoothStyle: integer): integer;


implementation

uses mainunit, prefs;

(*86
function  MeshPref(min, max: single; out Thresh, Decim: single; out SmoothStyle, MinClusterVox: integer): boolean;
var
    PrefForm: TForm;
    OkBtn: TButton;
    MinClusterVoxLabel, NoteLabel, ThreshLabel, DecimateLabel: TLabel;
    {$IFDEF USEFLOATSPIN}
    ThreshEdit: TFloatSpinEdit; //Cocoa TFloatSpinEdit is a bit wonky
    {$ELSE}
    MinClusterVoxEdit, ThreshEdit: TEdit;
    {$ENDIF}
    DecimateEdit: TSpinEdit;
    SmoothCombo: TComboBox;
begin
    PrefForm:=TForm.Create(nil);
    PrefForm.SetBounds(100, 100, 510, 212);
    PrefForm.Caption:='Volume to mesh preferences';
    PrefForm.Position := poScreenCenter;
    PrefForm.BorderStyle := bsDialog;
    //Note
    NoteLabel:=TLabel.create(PrefForm);
    NoteLabel.Caption:='See NITRC Surf Ice MediaWiki for usage notes';
    NoteLabel.Left := 8;
    NoteLabel.Top := 8;
    NoteLabel.Parent:=PrefForm;
    //Threshold
    ThreshLabel:=TLabel.create(PrefForm);
    ThreshLabel.Caption:=format('Threshold (%.4f..%.4f)',[min, max]);
    ThreshLabel.Left := 8;
    ThreshLabel.Top := 42;
    ThreshLabel.Parent:=PrefForm;
    Thresh := min + ((max - min) * 0.5);
    if (min < 1) and (max > 3) then
       Thresh := 2
    else if (min < -3) and (max > 0) then
       Thresh := -2;
    {$IFDEF USEFLOATSPIN}
    ThreshEdit:=TFloatSpinEdit.create(PrefForm);
    ThreshEdit.MaxValue := max;
    ThreshEdit.MinValue := min;
    ThreshEdit.Value:= Thresh;
    ThreshEdit.DecimalPlaces:= 3;
    {$ELSE}
    ThreshEdit:=TEdit.create(PrefForm);
    //ThreshEdit.Caption := FloatToStr(Thresh);
    ThreshEdit.Caption := FloatToStrF(Thresh, ffGeneral, 4, 4);
    {$ENDIF}
    ThreshEdit.Top := 42;
    ThreshEdit.Width := 92;
    ThreshEdit.Left := PrefForm.Width - ThreshEdit.Width - 8;
    ThreshEdit.Parent:=PrefForm;
    //
    MinClusterVoxLabel:=TLabel.create(PrefForm);
    MinClusterVoxLabel.Caption:='Minimum Cluster Size (vox)';
    MinClusterVoxLabel.Left := 8;
    MinClusterVoxLabel.Top := 72;
    MinClusterVoxLabel.Parent:=PrefForm;

    MinClusterVoxEdit:=TEdit.create(PrefForm);
    MinClusterVoxEdit.Caption := FloatToStrF(1, ffGeneral, 4, 4);
    MinClusterVoxEdit.Top := 72;
    MinClusterVoxEdit.Width := 92;
    MinClusterVoxEdit.Left := PrefForm.Width - ThreshEdit.Width - 8;
    MinClusterVoxEdit.Parent:=PrefForm;
    //Decimate
    DecimateLabel:=TLabel.create(PrefForm);
    DecimateLabel.Caption:='Decimation (100=large files, 10=degraded/small) ';
    DecimateLabel.Left := 8;
    DecimateLabel.Top := 102;
    DecimateLabel.Parent:=PrefForm;
    DecimateEdit:=TSpinEdit.create(PrefForm);
    DecimateEdit.Top := 102;
    DecimateEdit.Width := 92;
    DecimateEdit.Left := PrefForm.Width - DecimateEdit.Width - 8;
    DecimateEdit.MaxValue := 100;
    DecimateEdit.MinValue := 1;
    DecimateEdit.Value:= 25;
    DecimateEdit.Parent:=PrefForm;
    //Smooth
    SmoothCombo:=TComboBox.create(PrefForm);
    SmoothCombo.Left := 8;
    SmoothCombo.Top := 132;
    SmoothCombo.Width := PrefForm.Width -16;
    SmoothCombo.Items.Add('Raw (Jagged)');
    SmoothCombo.Items.Add('Masked smooth (Smooth except at brain mask)');
    SmoothCombo.Items.Add('Smooth (Eroded by brain mask)');
    SmoothCombo.ItemIndex:= 2;
    SmoothCombo.Style := csDropDownList;
    SmoothCombo.Parent:=PrefForm;
    //OK button
    OkBtn:=TButton.create(PrefForm);
    OkBtn.Caption:='OK';
    OkBtn.Top := 162;
    OkBtn.Width := 128;
    OkBtn.Left := PrefForm.Width - OkBtn.Width - 8;
    OkBtn.Parent:=PrefForm;
    OkBtn.ModalResult:= mrOK;
    {$IFNDEF Darwin} ScaleDPI(PrefForm, 96);{$ENDIF}
    PrefForm.ShowModal;
    {$IFDEF USEFLOATSPIN}
    Thresh := ThreshEdit.value;
    {$ELSE}
    Thresh := StrToFloatDef(ThreshEdit.Caption, Thresh);
    {$ENDIF}
    MinClusterVox := StrToIntDef(MinClusterVoxEdit.Caption, 1);
    Decim := DecimateEdit.value/100.0;
    SmoothStyle := SmoothCombo.ItemIndex;
    result :=  PrefForm.ModalResult = mrOK;
    FreeAndNil(PrefForm);
    if (Decim <= 0.0) then
       Decim := 0.01;
  end;
*)
function MeshPref(min, max: single; out Thresh, Decim: single; out SmoothStyle, MinClusterVox: integer): boolean;
var
    PrefForm: TForm;
    OkBtn: TButton;
    MinClusterVoxLabel, NoteLabel, ThreshLabel, DecimateLabel: TLabel;
    {$IFDEF USEFLOATSPIN}
    ThreshEdit: TFloatSpinEdit; //Cocoa TFloatSpinEdit is a bit wonky
    {$ELSE}
    MinClusterVoxEdit, ThreshEdit: TEdit;
    {$ENDIF}
    DecimateEdit: TSpinEdit;
    SmoothCombo, SingleCombo: TComboBox;
begin
    PrefForm:=TForm.Create(nil);
    //PrefForm.SetBounds(100, 100, 510, 212);
    PrefForm.AutoSize := True;
    PrefForm.BorderWidth := 8;

    PrefForm.Caption:='Volume to mesh preferences';
    PrefForm.Position := poScreenCenter;
    PrefForm.BorderStyle := bsDialog;
    //Note
    NoteLabel:=TLabel.create(PrefForm);
    NoteLabel.Caption:='See NITRC Surf Ice MediaWiki for usage notes';
    //NoteLabel.Left := 8;
    //NoteLabel.Top := 8;
    NoteLabel.AutoSize := true;
    NoteLabel.AnchorSide[akTop].Side := asrTop;
    NoteLabel.AnchorSide[akTop].Control := PrefForm;
    NoteLabel.BorderSpacing.Top := 6;
    NoteLabel.AnchorSide[akLeft].Side := asrLeft;
    NoteLabel.AnchorSide[akLeft].Control := PrefForm;
    NoteLabel.BorderSpacing.Left := 6;
    NoteLabel.Parent:=PrefForm;
    //Threshold
    ThreshLabel:=TLabel.create(PrefForm);
    ThreshLabel.Caption:=format('Threshold (%.4f..%.4f)',[min, max]);
    //ThreshLabel.Left := 8;
    //ThreshLabel.Top := 42;
    ThreshLabel.AutoSize := true;
    ThreshLabel.AnchorSide[akTop].Side := asrBottom;
    ThreshLabel.AnchorSide[akTop].Control := NoteLabel;
    ThreshLabel.BorderSpacing.Top := 6;
    ThreshLabel.AnchorSide[akLeft].Side := asrLeft;
    ThreshLabel.AnchorSide[akLeft].Control := PrefForm;
    ThreshLabel.BorderSpacing.Left := 6;

    ThreshLabel.Parent:=PrefForm;
    Thresh := min + ((max - min) * 0.5);
    if (min < 1) and (max > 3) then
       Thresh := 2
    else if (min < -3) and (max > 0) then
       Thresh := -2;
    {$IFDEF USEFLOATSPIN}
    ThreshEdit:=TFloatSpinEdit.create(PrefForm);
    ThreshEdit.MaxValue := max;
    ThreshEdit.MinValue := min;
    ThreshEdit.Value:= Thresh;
    ThreshEdit.DecimalPlaces:= 3;
    {$ELSE}
    ThreshEdit:=TEdit.create(PrefForm);
    //ThreshEdit.Caption := FloatToStr(Thresh);
    ThreshEdit.Caption := FloatToStrF(Thresh, ffGeneral, 4, 4);
    {$ENDIF}
    //ThreshEdit.Top := 42;
    //ThreshEdit.Width := 92;
    //ThreshEdit.Left := PrefForm.Width - ThreshEdit.Width - 8;
   ThreshEdit.Constraints.MinWidth:= 128;
   ThreshEdit.AutoSize := true;
   ThreshEdit.AnchorSide[akTop].Side := asrBottom;
   ThreshEdit.AnchorSide[akTop].Control := NoteLabel;
   ThreshEdit.BorderSpacing.Top := 4;
   ThreshEdit.AnchorSide[akLeft].Side := asrRight;
   ThreshEdit.AnchorSide[akLeft].Control := ThreshLabel;
   ThreshEdit.BorderSpacing.Left := 6;


    ThreshEdit.Parent:=PrefForm;
    //
    MinClusterVoxLabel:=TLabel.create(PrefForm);
    MinClusterVoxLabel.Caption:='Minimum Cluster Size (vox)';
    //MinClusterVoxLabel.Left := 8;
    //MinClusterVoxLabel.Top := 72;
    MinClusterVoxLabel.AutoSize := true;
    MinClusterVoxLabel.AnchorSide[akTop].Side := asrBottom;
    MinClusterVoxLabel.AnchorSide[akTop].Control := ThreshEdit;
    MinClusterVoxLabel.BorderSpacing.Top := 6;
    MinClusterVoxLabel.AnchorSide[akLeft].Side := asrLeft;
    MinClusterVoxLabel.AnchorSide[akLeft].Control := PrefForm;
    MinClusterVoxLabel.BorderSpacing.Left := 6;
    MinClusterVoxLabel.Parent:=PrefForm;
    MinClusterVoxEdit:=TEdit.create(PrefForm);
    MinClusterVoxEdit.Caption := FloatToStrF(1, ffGeneral, 4, 4);
    //MinClusterVoxEdit.Top := 72;
    //MinClusterVoxEdit.Width := 92;
    //MinClusterVoxEdit.Left := PrefForm.Width - ThreshEdit.Width - 8;
    MinClusterVoxEdit.Constraints.MinWidth:= 128;
    MinClusterVoxEdit.AutoSize := true;
    MinClusterVoxEdit.AnchorSide[akTop].Side := asrBottom;
    MinClusterVoxEdit.AnchorSide[akTop].Control := ThreshEdit;
    MinClusterVoxEdit.BorderSpacing.Top := 4;
    MinClusterVoxEdit.AnchorSide[akLeft].Side := asrRight;
    MinClusterVoxEdit.AnchorSide[akLeft].Control := MinClusterVoxLabel;
    MinClusterVoxEdit.BorderSpacing.Left := 6;

    MinClusterVoxEdit.Parent:=PrefForm;
    //Decimate
    DecimateLabel:=TLabel.create(PrefForm);
    DecimateLabel.Caption:='Decimation (100=large files, 10=degraded/small) ';
    //DecimateLabel.Left := 8;
    //DecimateLabel.Top := 102;
    DecimateLabel.AutoSize := true;
    DecimateLabel.AnchorSide[akTop].Side := asrBottom;
    DecimateLabel.AnchorSide[akTop].Control := MinClusterVoxEdit;
    DecimateLabel.BorderSpacing.Top := 6;
    DecimateLabel.AnchorSide[akLeft].Side := asrLeft;
    DecimateLabel.AnchorSide[akLeft].Control := PrefForm;
    DecimateLabel.BorderSpacing.Left := 6;
    DecimateLabel.Parent:=PrefForm;

    DecimateLabel.Parent:=PrefForm;
    DecimateEdit:=TSpinEdit.create(PrefForm);
    //DecimateEdit.Top := 102;
    //DecimateEdit.Width := 92;
    //DecimateEdit.Left := PrefForm.Width - DecimateEdit.Width - 8;
    DecimateEdit.MaxValue := 100;
    DecimateEdit.MinValue := 1;
    DecimateEdit.Value:= 25;

    DecimateEdit.Constraints.MinWidth:= 128;
    DecimateEdit.AutoSize := true;
    DecimateEdit.AnchorSide[akTop].Side := asrBottom;
    DecimateEdit.AnchorSide[akTop].Control := MinClusterVoxEdit;
    DecimateEdit.BorderSpacing.Top := 4;
    DecimateEdit.AnchorSide[akLeft].Side := asrRight;
    DecimateEdit.AnchorSide[akLeft].Control := DecimateLabel;
    DecimateEdit.BorderSpacing.Left := 6;

    DecimateEdit.Parent:=PrefForm;
    //Smooth
    SmoothCombo:=TComboBox.create(PrefForm);
    SmoothCombo.Items.Add('Raw (Jagged)');
    SmoothCombo.Items.Add('Masked smooth (Smooth except at brain mask)');
    SmoothCombo.Items.Add('Smooth (Eroded by brain mask)');
    SmoothCombo.ItemIndex:= 2;
    SmoothCombo.Style := csDropDownList;
    SmoothCombo.AutoSize := true;
    SmoothCombo.Constraints.MinWidth:= 400;
    SmoothCombo.AnchorSide[akTop].Side := asrBottom;
    SmoothCombo.AnchorSide[akTop].Control := DecimateEdit;
    SmoothCombo.BorderSpacing.Top := 6;
    SmoothCombo.AnchorSide[akLeft].Side := asrLeft;
    SmoothCombo.AnchorSide[akLeft].Control := PrefForm;
    SmoothCombo.BorderSpacing.Left := 6;
    SmoothCombo.AnchorSide[akRight].Side := asrRight;
    SmoothCombo.AnchorSide[akRight].Control := PrefForm;
    SmoothCombo.BorderSpacing.Right := 6;
    SmoothCombo.Parent:=PrefForm;
    SmoothCombo.Parent:=PrefForm;
    //SingleCombo
    SingleCombo:=TComboBox.create(PrefForm);
    SingleCombo.Items.Add('Multiple objects');
    SingleCombo.Items.Add('Largest object only');
    SingleCombo.ItemIndex:= 0;
    SingleCombo.Style := csDropDownList;
    SingleCombo.AutoSize := true;
    SingleCombo.Constraints.MinWidth:= 400;
    SingleCombo.AnchorSide[akTop].Side := asrBottom;
    SingleCombo.AnchorSide[akTop].Control := DecimateEdit;
    SingleCombo.BorderSpacing.Top := 6;
    SingleCombo.AnchorSide[akLeft].Side := asrLeft;
    SingleCombo.AnchorSide[akLeft].Control := PrefForm;
    SingleCombo.BorderSpacing.Left := 6;
    SingleCombo.AnchorSide[akRight].Side := asrRight;
    SingleCombo.AnchorSide[akRight].Control := PrefForm;
    SingleCombo.BorderSpacing.Right := 6;
    SingleCombo.Parent:=PrefForm;
    SingleCombo.Parent:=PrefForm;
    //OK button
    OkBtn:=TButton.create(PrefForm);
    OkBtn.Caption:='OK';
    //OkBtn.Top := 162;
    //OkBtn.Width := 128;
    //OkBtn.Left := PrefForm.Width - OkBtn.Width - 8;
    OkBtn.AutoSize := true;
    OkBtn.AnchorSide[akTop].Side := asrBottom;
    OkBtn.AnchorSide[akTop].Control := SingleCombo;
    OkBtn.BorderSpacing.Top := 6;
    OkBtn.AnchorSide[akLeft].Side := asrCenter;
    OkBtn.AnchorSide[akLeft].Control := PrefForm;
    //OkBtn.BorderSpacing.Left := 200;
    OkBtn.Constraints.MinWidth:= 64;
    OkBtn.Parent:=PrefForm;
    OkBtn.ModalResult:= mrOK;
    PrefForm.ShowModal;
   {$IFDEF USEFLOATSPIN}
    Thresh := ThreshEdit.value;
    {$ELSE}
    Thresh := StrToFloatDef(ThreshEdit.Caption, Thresh);
    {$ENDIF}
    MinClusterVox := StrToIntDef(MinClusterVoxEdit.Caption, 1);
    Decim := DecimateEdit.value/100.0;
    SmoothStyle := SmoothCombo.ItemIndex;
    if (SingleCombo.ItemIndex = 1) then
       MinClusterVox := -1;
    result :=  PrefForm.ModalResult = mrOK;
    FreeAndNil(PrefForm);
    if (Decim <= 0.0) then
       Decim := 0.01;
  end;

function Nii2MeshCore(niiname, meshname: string; threshold, decimateFrac: single; minimumClusterVox, smoothStyle: integer): integer;
var
    nii: TNIFTI;
    lMesh: TMesh;
    IsoSurfaceEx: TIsoSurfaceExtractor;
    vx: TPoint3f;
    i: integer;
begin
   result := -1;
   nii := TNIfTI.Create;
   nii.LoadFromFile(niiname, kNiftiSmoothNone);
   if length(nii.img) < 9 then begin
      nii.Free;
      exit;
   end;
   if (decimateFrac <= 0) or (decimateFrac > 1) then
      decimateFrac := 1.0;
   if smoothStyle = kNiftiSmoothMaskZero then
      nii.SmoothMaskZero
   else if smoothStyle = kNiftiSmooth then
      nii.Smooth;
   if specialsingle(threshold) then
    threshold := nii.minInten + ((nii.maxInten - nii.minInten) * 0.5);
   ApplyClusterThreshold(nii, threshold, minimumClusterVox);
   IsoSurfaceEx := TIsoSurfaceExtractor.Create(nii.hdr.dim[1], nii.hdr.dim[2],nii.hdr.dim[3], nii.img);
   lMesh := TMesh.Create;
  IsoSurfaceEx.MarchingCubes(threshold,lMesh.vertices, lMesh.faces);
  UnifyVertices (lMesh.faces, lMesh.vertices);
  ClusterVertex(lMesh.faces, lMesh.vertices, 0.0079295);
  if decimateFrac < 1.0 then
     if not ReducePatch(lMesh.faces, lMesh.vertices, decimateFrac) then exit;
  //v1 := ptf(-0.5, -0.5, -0.5); //1Sept2016: not required: middle voxel 0 based: see https://github.com/neurolabusc/spmScripts/blob/master/nii_makeDTI.m
  for i := 0 to (length(lMesh.vertices) -1) do begin //apply matrix to convert from voxels to mm (voxelspace -> worldspace)
      vx := lMesh.vertices[i];
      lMesh.vertices[i].X := vx.X*nii.mat[1,1] + vx.Y*nii.mat[1,2] + vx.Z*nii.mat[1,3] + nii.mat[1,4];
      lMesh.vertices[i].Y := vx.X*nii.mat[2,1] + vx.Y*nii.mat[2,2] + vx.Z*nii.mat[2,3] + nii.mat[2,4];
      lMesh.vertices[i].Z := vx.X*nii.mat[3,1] + vx.Y*nii.mat[3,2] + vx.Z*nii.mat[3,3] + nii.mat[3,4];
  end;
  //kludge follows: consider smarter way to detect reversed winding
  if (nii.mat[1,1] >= 0) then
     lMesh.ReverseFaces;
  IsoSurfaceEx.Free();
  nii.free;
  lMesh.SaveMesh(meshname);
  result := length(lMesh.vertices);
  lMesh.Free();
end; //Nii2MeshCore()

(*Type
  TUInt8s = array of uint8;

procedure PreserveLargestCluster(var lImg: TUInt8s; Xi,Yi,Zi: integer; lClusterValue,ValueForSmallClusters: byte);
var
  mx, i, j, XY, XYZ, qlo, qhi: integer;
  qimg, img32: TInt32s;
procedure checkPixel(vxl: integer);
begin
     if img32[vxl] <> -1 then exit; //already found or not a target
     qhi := qhi + 1;
     img32[vxl] := 1; //found
     qimg[qhi] := vxl; //location
end;//nested checkPixel()
procedure retirePixel();
var
  vxl: integer;
begin
     vxl := qimg[qlo];
     checkPixel(vxl-1);
     checkPixel(vxl+1);
     checkPixel(vxl-Xi);
     checkPixel(vxl+Xi);
     checkPixel(vxl-XY);
     checkPixel(vxl+XY);
     qlo := qlo + 1;
end;//nested retirePixel()
begin //main PreserveLargestCluster()
  if (Zi < 1) then exit;
  XY := Xi * Yi;
  XYZ := XY * Zi;
  setlength(img32, XYZ);
  setlength(qimg, XYZ);
  //set target voxels
  for i := 0 to (XYZ-1) do begin
      img32[i] := 0;
      if lImg[i] = lClusterValue then
         img32[i] := -1;
  end;
  //clear bottom and top slices
  for i := 0 to (XY-1) do
    img32[i] := 0;
  for i := (XYZ-1-XY) to (XYZ-1) do
    img32[i] := 0;
  //now seed each voxel
  mx := 0;
  for i := (XY) to (XYZ-1-XY) do begin
      if (img32[i] < 0) then begin //voxels not yet part of any region
         qlo := 0;
         qhi := -1;
         checkPixel(i);
         while qlo <= qhi do
           retirePixel();
         for j := 0 to qhi do
             img32[qimg[j]] := qhi + 1;
         if (qhi+1) > mx then mx := qhi + 1;
      end;
  end;
  if mx < 2 then begin
     qimg := nil;
     img32 := nil;
     exit;
  end;
  //delete voxels not part of largest cluster
  for i := 0 to (XYZ-1) do
      if img32[i] <> mx then
         img32[i] := 0;
  //recover bottom and top slices
  for i := 0 to (XY-1) do
      if (img32[i+XY] = mx) then
         img32[i] := mx;
  for i := (XYZ-1-XY) to (XYZ-1) do
      if (img32[i-XY] = mx) then
         img32[i] := mx;
  //apply filter to input image
  for i := 0 to (XYZ-1) do
      if img32[i] = 0 then
         lImg[i] := 0;
  qimg := nil;
  img32 := nil;
end;// PreserveLargestCluster()

procedure PreserveLargestObjectOnly(var nii: TNIFTI; lThresh: single);
var
  i,vx: integer;
  vol8: TUInt8s;
  halfThresh : single;
begin
   vx := nii.hdr.dim[1] * nii.hdr.dim[2] * nii.hdr.dim[3];
   if (vx < 1) then exit;
   SetLength(vol8, vx);
   for i := 0 to (vx-1) do begin
       vol8[i] := 0;
       if (nii.img[i] >= lThresh) then
          vol8[i] := 255;
   end;
   PreserveLargestCluster(vol8, nii.hdr.dim[1], nii.hdr.dim[2], nii.hdr.dim[3],255,0 );
   halfThresh := lThresh * 0.5;
   for i := 0 to (vx-1) do begin
       if ((nii.img[i] >= lThresh) and (vol8[i] < 255)) then
          nii.img[i] := halfThresh; //voxel does not survive
   end;
   SetLength(vol8, 0);

end;*)

function Nii2Mesh(const FileName: string): boolean;
var
  nii: TNIFTI;
  lMesh: TMesh;
  lThresh, lDecimate, s: single;
  lSmoothStyle: integer;
  IsoSurfaceEx: TIsoSurfaceExtractor;
  vx: TPoint3f;
  lPath,lName,lExt: string;
  i, lMinClusterVox: integer;
begin
  result := false;
   nii := TNIfTI.Create;
   nii.LoadFromFile(FileName, kNiftiSmoothNone);
   if length(nii.img) < 9 then begin
      nii.Free;
      exit;
   end;
   if not MeshPref(nii.minInten, nii.maxInten, lThresh, lDecimate, lSmoothStyle,lMinClusterVox) then exit;
   //lThresh := 3;  lDecimate := 0.2; lSmoothStyle := kNiftiSmoothNone;
  if lSmoothStyle = kNiftiSmoothMaskZero then
     nii.SmoothMaskZero
  else if lSmoothStyle = kNiftiSmooth then
     nii.Smooth;
  FilenameParts (FileName, lPath,lName,lExt);
  lExt := FloatToStrF(abs(lThresh),ffGeneral,6,0);
  lExt  := StringReplace(lExt, '.', 'p', [rfReplaceAll, rfIgnoreCase]);
  lExt  := StringReplace(lExt, ',', 'p', [rfReplaceAll, rfIgnoreCase]);
  ApplyClusterThreshold(nii, lThresh, lMinClusterVox);
  if (lThresh < 0) then
     lExt := 'NEG_' + lExt;
  GLForm1.SaveMeshDialog.Filename := lPath+lName+'_'+lExt+'.mz3';
  if (lThresh < 0) then begin //invert intensity
     for i := 0 to high(nii.img)  do
         nii.img[i] := -nii.img[i];
     lThresh := -lThresh;
     s := nii.maxInten;
     nii.maxInten := -nii.minInten;
     nii.minInten := -s;
  end;
  if (nii.maxInten < lThresh) or (nii.minInten > lThresh) then begin
      showmessage(format('All voxels on one side of threshold. Threshold= %g, min=%g, max=%g', [ lThresh, nii.minInten, nii.maxInten]));
      nii.Free;
      exit;
   end;
  //if singleObject then
  //   PreserveLargestObjectOnly(nii, lThresh);
  IsoSurfaceEx := TIsoSurfaceExtractor.Create(nii.hdr.dim[1], nii.hdr.dim[2],nii.hdr.dim[3], nii.img);
  lMesh := TMesh.Create;
  //IsoSurfaceEx.MarchingTetrahedra(Threshold,lMesh.vertices, lMesh.faces);
  IsoSurfaceEx.MarchingCubes(lThresh,lMesh.vertices, lMesh.faces);
  //showmessage( Format('converted= %d   f= %d', [length(lMesh.vertices), length(lMesh.faces)]));
  UnifyVertices (lMesh.faces, lMesh.vertices);
  ClusterVertex(lMesh.faces, lMesh.vertices, 0.0079295);
  //showmessage( Format('v= %d   f= %d', [length(lMesh.Vertices), length(lMesh.faces)]));
  if lDecimate < 1.0 then
     if not ReducePatch(lMesh.faces, lMesh.vertices, lDecimate) then exit;
  //v1 := ptf(-0.5, -0.5, -0.5); //1Sept2016: not required: middle voxel 0 based: see https://github.com/neurolabusc/spmScripts/blob/master/nii_makeDTI.m
  for i := 0 to (length(lMesh.vertices) -1) do begin //apply matrix to convert from voxels to mm (voxelspace -> worldspace)
      vx := lMesh.vertices[i];
      //vectorAdd(vx, v1);
      lMesh.vertices[i].X := vx.X*nii.mat[1,1] + vx.Y*nii.mat[1,2] + vx.Z*nii.mat[1,3] + nii.mat[1,4];
      lMesh.vertices[i].Y := vx.X*nii.mat[2,1] + vx.Y*nii.mat[2,2] + vx.Z*nii.mat[2,3] + nii.mat[2,4];
      lMesh.vertices[i].Z := vx.X*nii.mat[3,1] + vx.Y*nii.mat[3,2] + vx.Z*nii.mat[3,3] + nii.mat[3,4];
  end;
  //kludge follows: consider smarter way to detect reversed winding
  if (nii.mat[1,1] >= 0) then
     lMesh.ReverseFaces;
  IsoSurfaceEx.Free();
  nii.free;
  GLForm1.SaveMesh(lMesh, false);
  lMesh.Free();
  result := true;
end; // nii2mesh()

procedure save2Mesh(var nii: TNIfTI; lDecimate, lThresh: single; img: TImgScaled);
var
  IsoSurfaceEx: TIsoSurfaceExtractor;
  lMesh: TMesh;
  i: integer;
  vx: TPoint3f;
begin
  IsoSurfaceEx := TIsoSurfaceExtractor.Create(nii.hdr.dim[1], nii.hdr.dim[2],nii.hdr.dim[3], img);
  lMesh := TMesh.Create;
  IsoSurfaceEx.MarchingCubes(lThresh, lMesh.vertices, lMesh.faces);
  UnifyVertices (lMesh.faces, lMesh.vertices);
  ClusterVertex(lMesh.faces, lMesh.vertices, 0.0079295);
  if lDecimate < 1.0 then
    if not ReducePatch(lMesh.faces, lMesh.vertices, lDecimate) then exit;
 for i := 0 to (length(lMesh.vertices) -1) do begin //apply matrix to convert from voxels to mm (voxelspace -> worldspace)
     vx := lMesh.vertices[i];
     //vectorAdd(vx, v1);
     lMesh.vertices[i].X := vx.X*nii.mat[1,1] + vx.Y*nii.mat[1,2] + vx.Z*nii.mat[1,3] + nii.mat[1,4];
     lMesh.vertices[i].Y := vx.X*nii.mat[2,1] + vx.Y*nii.mat[2,2] + vx.Z*nii.mat[2,3] + nii.mat[2,4];
     lMesh.vertices[i].Z := vx.X*nii.mat[3,1] + vx.Y*nii.mat[3,2] + vx.Z*nii.mat[3,3] + nii.mat[3,4];
 end;
 IsoSurfaceEx.Free();
 //GLForm1.SaveMesh(lMesh, false);
 if gPrefs.SaveAsFormat = 4 then begin
    GLForm1.SaveMeshDialog.Filename := changefileext(GLForm1.SaveMeshDialog.Filename, '.ply');
end else if gPrefs.SaveAsFormat = 0 then begin
    GLForm1.SaveMeshDialog.Filename := changefileext(GLForm1.SaveMeshDialog.Filename, '.obj');
end else if gPrefs.SaveAsFormat = 1 then begin
    GLForm1.SaveMeshDialog.Filename := changefileext(GLForm1.SaveMeshDialog.Filename, '.gii');
end else begin
    GLForm1.SaveMeshDialog.Filename := changefileext(GLForm1.SaveMeshDialog.Filename, '.mz3');
end;
lMesh.SaveMesh(GLForm1.SaveMeshDialog.Filename);
 lMesh.Free();
end;

procedure quickSmooth(xDim,yDim,zDim: integer; var img: TImgScaled);
var
  nVox, i, xyDim: integer;
  temp: TImgScaled;
begin
     if (xDim < 3) or (yDim < 3) or (zDim < 3) then exit;
     nVox := xDim * yDim * zDim;
     setlength(temp, nVox);
     for i := 0 to (nVox -1) do
         temp[i] := 2 * img[i];
     for i := 1 to (nVox -1) do
         temp[i] := temp[i] + img[i-1];
     for i := 0 to (nVox -2) do
         temp[i] := temp[i] + img[i+1];
     for i := xDim to (nVox -1) do
         temp[i] := temp[i] + img[i-xDim];
     for i := 0 to (nVox -1 - xDim) do
         temp[i] := temp[i] + img[i+xDim];
     xyDim := xDim * yDim; //slice size
     for i := xyDim to (nVox -1) do
         temp[i] := temp[i] + img[i-xyDim];
     for i := 0 to (nVox -1 - xyDim) do
         temp[i] := temp[i] + img[i+xyDim];
     for i := 0 to (nVox -1) do
         img[i] := temp[i]/8.0;
     setlength(temp,0);
end;

function Atlas2Mesh(const FileName: string): boolean;
var
  nii: TNIFTI;
  lPath,lName,lExt: string;
  mn, mx, i, nVox, v, vOK: integer;
  lDecimate: single = 0.2;
  isSmooth: boolean;
  img: TImgScaled;
begin
  result := false;
   nii := TNIfTI.Create;
   nii.LoadFromFile(FileName, kNiftiSmoothNone);
   mn := round(nii.minInten + 1);
   mx := round(nii.maxInten);
   //mn := 4; mx := 4;
   nVox := length(nii.img);
   if (nVox < 9) or (mn > mx) or
      ((nii.hdr.datatype <> kDT_UINT8) and (nii.hdr.datatype <> kDT_INT8) and
      (nii.hdr.datatype <> kDT_UINT16) and (nii.hdr.datatype <> kDT_INT16) and
      (nii.hdr.datatype <> kDT_UINT32)) then begin
      showmessage('Image does not appear to be an atlas');
      nii.Free;
      exit;
   end;
   isSmooth := (MessageDlg('Smooth borders for '+inttostr(mx-mn+1)+' regions?', mtConfirmation, [mbNo, mbYes], 0) = mrYes);
   //showmessage(format('%d..%d',[mn, mx]));
   FilenameParts (FileName, lPath,lName,lExt);
   setlength(img, nVox);
   for i := mn to mx do begin
       for v := 0 to (nVox -1) do begin
           img[v] := 0;
           if (nii.img[v] = i) then
              img[v] := 1;
       end;
       if (isSmooth) then begin
          quickSmooth(nii.hdr.dim[1], nii.hdr.dim[2], nii.hdr.dim[3], img);
          quickSmooth(nii.hdr.dim[1], nii.hdr.dim[2], nii.hdr.dim[3], img);
       end;
       vOK := 0;
       for v := 0 to (nVox -1) do
           if (img[v] >= 0.5) then
              vOK := vOK + 1;
       GLForm1.SaveMeshDialog.Filename := lPath+lName+'_'+inttostr(i)+'.mz3';
       if vOK > 0 then
          save2Mesh(nii, lDecimate, 0.25, img);
   end;
   setlength(img, 0);
   result := true;
end; // atlas2mesh()

end.

