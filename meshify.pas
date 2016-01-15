unit meshify;

{$mode objfpc}{$H+}

interface

uses
  LCLintf, Classes, SysUtils, mesh, nifti_loader, meshify_marchingcubes, dialogs,
  define_types, matmath, meshify_simplify, Forms, StdCtrls, Controls, Spin;

function Nii2Mesh(const FileName: string; SaveAsFormat: integer): boolean;

implementation

uses mainunit, prefs;

function  MeshPref(min, max: single; out Thresh, Decim: single; out SmoothStyle: integer): boolean;
var
    PrefForm: TForm;
    OkBtn: TButton;
    NoteLabel, ThreshLabel, DecimateLabel: TLabel;
    ThreshEdit: TFloatSpinEdit;
    DecimateEdit: TSpinEdit;
    SmoothCombo: TComboBox;
begin
    PrefForm:=TForm.Create(nil);
    PrefForm.SetBounds(100, 100, 410, 182);
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
    ThreshEdit:=TFloatSpinEdit.create(PrefForm);
    ThreshEdit.Top := 42;
    ThreshEdit.Width := 92;
    ThreshEdit.Left := PrefForm.Width - ThreshEdit.Width - 8;
    ThreshEdit.MaxValue := max;
    ThreshEdit.MinValue := min;
    ThreshEdit.Value:= min + ((max-min)*0.5);
    ThreshEdit.Parent:=PrefForm;
    //Decimate
    DecimateLabel:=TLabel.create(PrefForm);
    DecimateLabel.Caption:='Decimation (100=large files, 20=degraded/slow) ';
    DecimateLabel.Left := 8;
    DecimateLabel.Top := 72;
    DecimateLabel.Parent:=PrefForm;
    DecimateEdit:=TSpinEdit.create(PrefForm);
    DecimateEdit.Top := 72;
    DecimateEdit.Width := 92;
    DecimateEdit.Left := PrefForm.Width - DecimateEdit.Width - 8;
    DecimateEdit.MaxValue := 100;
    DecimateEdit.MinValue := 20;
    DecimateEdit.Value:= 50;
    DecimateEdit.Parent:=PrefForm;
    //Smooth
    SmoothCombo:=TComboBox.create(PrefForm);
    SmoothCombo.Left := 8;
    SmoothCombo.Top := 102;
    SmoothCombo.Width := PrefForm.Width -16;
    SmoothCombo.Items.Add('Raw (Jagged)');
    SmoothCombo.Items.Add('Masked smooth (Smooth except at brain mask)');
    SmoothCombo.Items.Add('Smooth (Eroded by brain mask)');
    SmoothCombo.ItemIndex:= 0;
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
    PrefForm.ShowModal;
    Thresh := ThreshEdit.value;
    Decim := DecimateEdit.value/100.0;
    SmoothStyle := SmoothCombo.ItemIndex;
    result :=  PrefForm.ModalResult = mrOK;
    FreeAndNil(PrefForm);
    if (Decim < 0.2) then begin
       Showmessage('Maximum decimation amount is 20');
       Decim := 0.2;
    end;

  end;

function Nii2Mesh(const FileName: string; SaveAsFormat: integer): boolean;
//Load NIfTI image as overlay
var
  //t: Dword;
  nii: TNIFTI;
  lMesh: TMesh;
  lThresh, lDecimate, s: single;
  lSmoothStyle: integer;
  IsoSurfaceEx: TIsoSurfaceExtractor;
  v1,vx: TPoint3f;
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
  if (lThresh < 0) then begin //invert intensity
     for i := 0 to (length(nii.img) - 1)  do
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
  //t := GetTickCount();
  IsoSurfaceEx.MarchingCubes(lThresh,lMesh.vertices, lMesh.faces);
  //showmessage( Format('converted= %d   f= %d', [length(lMesh.vertices), length(lMesh.faces)]));
  UnifyVertices (lMesh.faces, lMesh.vertices);
  ClusterVertex(lMesh.faces, lMesh.vertices, 0.0079295);
  //showmessage( Format('v= %d   f= %d', [length(lMesh.Vertices), length(lMesh.faces)]));
  if lDecimate < 1.0 then
     if not ReducePatch(lMesh.faces, lMesh.vertices, lDecimate) then exit;
  v1 := ptf(-0.5, -0.5, -0.5);
  for i := 0 to (length(lMesh.vertices) -1) do begin //apply matrix to convert from voxels to mm (voxelspace -> worldspace)
      vx := lMesh.vertices[i];
      vectorAdd(vx, v1);
      lMesh.vertices[i].X := vx.X*nii.mat[1,1] + vx.Y*nii.mat[1,2] + vx.Z*nii.mat[1,3] + nii.mat[1,4];
      lMesh.vertices[i].Y := vx.X*nii.mat[2,1] + vx.Y*nii.mat[2,2] + vx.Z*nii.mat[2,3] + nii.mat[2,4];
      lMesh.vertices[i].Z := vx.X*nii.mat[3,1] + vx.Y*nii.mat[3,2] + vx.Z*nii.mat[3,3] + nii.mat[3,4];
  end;
  if SaveAsFormat = kSaveAsGii then
     lMesh.SaveGii(FileName)
  else if SaveAsFormat = kSaveAsMz3 then
     lMesh.SaveMz3(FileName)
  else
      lMesh.SaveObj(FileName);
  IsoSurfaceEx.Free();
  lMesh.Free();
  nii.free;
  result := true;
  //showmessage('ms '+ inttostr(gettickcount()- t) );
end; // nii2mesh()

end.

