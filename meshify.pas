unit meshify;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF Windows}uscaledpi, {$ENDIF}
  meshify_simplify, LCLintf, Classes, SysUtils, mesh, nifti_loader, meshify_marchingcubes, dialogs,
  define_types, matmath, Forms, StdCtrls, Controls, Spin;

function Nii2Mesh(const FileName: string): boolean;

implementation

uses mainunit, prefs;

function  MeshPref(min, max: single; out Thresh, Decim: single; out SmoothStyle: integer): boolean;
var
    PrefForm: TForm;
    OkBtn: TButton;
    NoteLabel, ThreshLabel, DecimateLabel: TLabel;
    {$IFDEF USEFLOATSPIN}
    ThreshEdit: TFloatSpinEdit; //Cocoa TFloatSpinEdit is a bit wonky
    {$ELSE}
    ThreshEdit: TEdit;
    {$ENDIF}
    DecimateEdit: TSpinEdit;
    SmoothCombo: TComboBox;
begin
    PrefForm:=TForm.Create(nil);
    PrefForm.SetBounds(100, 100, 510, 182);
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
    //Decimate
    DecimateLabel:=TLabel.create(PrefForm);
    DecimateLabel.Caption:='Decimation (100=large files, 10=degraded/small) ';
    DecimateLabel.Left := 8;
    DecimateLabel.Top := 72;
    DecimateLabel.Parent:=PrefForm;
    DecimateEdit:=TSpinEdit.create(PrefForm);
    DecimateEdit.Top := 72;
    DecimateEdit.Width := 92;
    DecimateEdit.Left := PrefForm.Width - DecimateEdit.Width - 8;
    DecimateEdit.MaxValue := 100;
    DecimateEdit.MinValue := 1;
    DecimateEdit.Value:= 25;
    DecimateEdit.Parent:=PrefForm;
    //Smooth
    SmoothCombo:=TComboBox.create(PrefForm);
    SmoothCombo.Left := 8;
    SmoothCombo.Top := 102;
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
    OkBtn.Top := 138;
    OkBtn.Width := 128;
    OkBtn.Left := PrefForm.Width - OkBtn.Width - 8;
    OkBtn.Parent:=PrefForm;
    OkBtn.ModalResult:= mrOK;
    {$IFDEF Windows} ScaleDPI(PrefForm, 96);{$ENDIF}
    PrefForm.ShowModal;
    {$IFDEF USEFLOATSPIN}
    Thresh := ThreshEdit.value;
    {$ELSE}
    Thresh := StrToFloatDef(ThreshEdit.Caption, Thresh);
    {$ENDIF}
    Decim := DecimateEdit.value/100.0;
    SmoothStyle := SmoothCombo.ItemIndex;
    result :=  PrefForm.ModalResult = mrOK;
    FreeAndNil(PrefForm);
    if (Decim <= 0.0) then
       Decim := 0.01;
  end;

function Nii2Mesh(const FileName: string): boolean;
//Load NIfTI image as overlay
var
  //t: Dword;
  nii: TNIFTI;
  lMesh: TMesh;
  lThresh, lDecimate, s: single;
  lSmoothStyle: integer;
  IsoSurfaceEx: TIsoSurfaceExtractor;
  v1,vx: TPoint3f;
  lPath,lName,lExt: string;
  i: integer;
begin
  result := false;
   nii := TNIfTI.Create;
   nii.LoadFromFile(FileName, kNiftiSmoothNone);
   if length(nii.img) < 9 then begin
      nii.Free;
      exit;
   end;
   if not MeshPref(nii.minInten, nii.maxInten, lThresh, lDecimate, lSmoothStyle) then exit;
   //lThresh := 3;  lDecimate := 0.2; lSmoothStyle := kNiftiSmoothNone;
  if lSmoothStyle = kNiftiSmoothMaskZero then
     nii.SmoothMaskZero
  else if lSmoothStyle = kNiftiSmooth then
     nii.Smooth;
  FilenameParts (FileName, lPath,lName,lExt);
  lExt := FloatToStrF(abs(lThresh),ffGeneral,6,0);
  lExt  := StringReplace(lExt, '.', 'p', [rfReplaceAll, rfIgnoreCase]);
  lExt  := StringReplace(lExt, ',', 'p', [rfReplaceAll, rfIgnoreCase]);
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
  IsoSurfaceEx.Free();
  nii.free;

  GLForm1.SaveMesh(lMesh, false);
  lMesh.Free();
  result := true;
end; // nii2mesh()

end.

