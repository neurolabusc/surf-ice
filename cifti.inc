//CIFTI file format loader https://www.nitrc.org/projects/cifti/

function isCIfTI(fnm: string): boolean;
var
   hdrSz, hdrSzSwap : int32; //NIFTI2 first 4 bytes MUST equal 540
   vox_offset: int64;
   f: file;
begin
  result := false;
  if not fileexists(fnm) then exit;
  if fsize(fnm) < 540 then exit;
  FileMode := fmOpenRead;
  AssignFile(f, fnm);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(f, 1);
  BlockRead(f, hdrSz, sizeof(hdrSz));
  Seek(f, 168); //https://nifti.nimh.nih.gov/pub/dist/doc/nifti2.h
  BlockRead(f, vox_offset, sizeof(vox_offset));
  FileMode := 2;
  CloseFile(f);
  hdrSzSwap := hdrSz;
  SwapLongInt(hdrSzSwap); //swap for big/little endian
  if (hdrSzSwap = 540) then
     showmessage('Unable to read byte-swapped NIfTI2 headers.');
  if (hdrSz = 540) and ((vox_offset - hdrSz) > 128) then
     result := true;
end;

{$IFDEF CIFTI_DEBUG}
function loadCifti(fnm: string; lOverlayIndex, lSeriesIndex: integer; isLoadCortexLeft: boolean): integer;
{$ELSE}
function TMesh.loadCifti(fnm: string; lOverlayIndex, lSeriesIndex: integer; isLoadCortexLeft: boolean): integer;
{$ENDIF}
label 131;
type
  TBrainModel = record
    IndexOffset, IndexCount, SurfaceNumberOfVertices,
    VertexIndicesStart, VertexIndicesEnd, BrainModelEnd: integer;
    isSparse, isPConn: boolean;
    ModelType, BrainStructure: string
  end;
  TNIFTIhdr2 = packed record //Next: analyze Format Header structure
    hdrSz : int32; //MUST BE 540
    magic: array [1..8] of ansichar;
    datatype, bitpix: int16;
    dim: array[0..7] of int64; //Data array dimensions
    intent_p1, intent_p2, intent_p3: double;
    pixdim: array[0..7]of double;
    vox_offset: int64;
    scl_slope,scl_inter, cal_max,cal_min, slice_duration, toffset: double;
    slice_start, slice_end: int64;
    descrip: array[1..80] of ansichar;
    aux_file: array[1..24] of ansichar;
    qform, sform: int32;
    quatern_b,quatern_c,quatern_d,
    qoffset_x,qoffset_y,qoffset_z: double;
    srow_x: array[0..3] of double;
    srow_y: array[0..3] of double;
    srow_z: array[0..3] of double;
    slice_code, xyzt_units, intent_code: int32;
    intent_name: array[1..16] of ansichar;
    dim_info: ansichar;
    unused_str: array[1..15] of ansichar;
  end; //TNIFTIhdr2 struct
var
  hdr: TNIFTIhdr2;
  hdrSzSwap : int32;
  lut : array of TRGBA;
  f: file;
  fmx: single;
  nVert, mx, nSeries, xmlLen,i,j,k, fsz,xmlPos, tablePos: integer;
  xmlStr, allBrainStructures, desiredBrainStructure : string;
  rawData: TFloats;
  vertexIndices: array of integer;
  bm: TBrainModel;
  isFoundBrainStructure, isCifti1: boolean;
function posExEnd(key: string): integer;
begin
  result := PosEx(key, xmlStr, xmlPos);
  if result > 0 then
     result := result + length(key);
end;

function readXStr(key: string): string;
var
  lStart, lEnd: integer;
begin
     result := '';
     lStart := posExEnd(key+'="');
     if lStart < 1 then exit;
     lEnd := PosEx('"', xmlStr, lStart);
     if lEnd < 1 then exit;
     result :=  copy(xmlStr, lStart, lEnd-lStart);
     //{$IFDEF CIFTI_DEBUG}showdebug(key+'->'+result); {$ENDIF}
end;
function readXInt(key: string): integer;
begin
     result := strtointdef(readXStr(key), 0);
end;
function readLut(idx: integer): TRGBA;
//<Label Key="1" Red="0.270588" Green="0" Blue="1" Alpha="1">R_V1_ROI</Label>     lut[i] := ReadLut(xmlStr, i);
var
  lStart, lEnd: integer;
begin
  result.R := 0;
  result.G := 0;
  result.B := 0;
  result.A := 0;
  xmlPos := tablePos; //random storage of labels, keys may not be sequential
  lStart := posExEnd('<Label Key="'+inttostr(idx)+'"');
  if lStart < 1 then exit;
  lEnd := PosEx('</Label>', xmlStr, lStart);
  xmlPos := lStart;
  if lEnd < 1 then exit;
  result.R := round(strtofloatDef(readXStr('Red'),0) * 255.0);
  result.G := round(strtofloatDef(readXStr('Green'),0) * 255);
  result.B := round(strtofloatDef(readXStr('Blue'),0) * 255.0);
  result.A := round(strtofloatDef(readXStr('Alpha'),0) * 255.0);
  //printf(format('%d r%d g%d b%d', [idx, result.R, result.G, result.B]));
  //result.A := 100;
end;

function readXArrayInt: integer;
var
  s: string;
begin
  s := '';
  while true do begin
        if xmlStr[xmlPos] in ['0'..'9'] then
           s := s + xmlStr[xmlPos]
        else if (s <> '') or (xmlPos >= bm.VertexIndicesEnd) then
             break;
        xmlPos := xmlPos + 1;
  end;
  result := strtointdef(s,0);
end;

function readBrainModel (out lbm: TBrainModel): boolean;
var
  brainModelStart: integer;
begin
  result := false;
  lbm.isPConn := false;
  brainModelStart := posEx('<BrainModel', xmlStr, xmlPos);
  lbm.BrainModelEnd :=  posExEnd('</BrainModel>');
  if (brainModelStart < 1) then begin
     brainModelStart := posEx('<Surface', xmlStr, xmlPos);
     lbm.BrainModelEnd :=  posExEnd('</Surface');
     lbm.isPConn := true;
  end;
  lbm.isSparse := (lbm.BrainModelEnd > 0);
  if lbm.BrainModelEnd < 1 then
     lbm.BrainModelEnd:=  posExEnd('/>');
  {$IFDEF CIFTI_DEBUG}showdebug(format('BrainModel pos %d  start %d -> end %d',[xmlPos, brainModelStart, lbm.brainModelEnd]));{$ENDIF}
  if (brainModelStart < 1) or (lbm.BrainModelEnd < 1)  then exit;
  lbm.IndexOffset := readXInt('IndexOffset');
  lbm.IndexCount := readXInt('IndexCount');
  if (isCifti1) then begin
     lbm.SurfaceNumberOfVertices := readXInt('SurfaceNumberOfNodes'); //CIFTI1
     lbm.VertexIndicesStart := posExEnd('<NodeIndices>'); //CIFTI1
     lbm.VertexIndicesEnd := posExEnd('</NodeIndices>'); //CIFTI1
  end else begin
      lbm.SurfaceNumberOfVertices := readXInt('SurfaceNumberOfVertices'); //CIFTI2
      lbm.VertexIndicesStart := posExEnd('<VertexIndices>'); //CIFTI2
      lbm.VertexIndicesEnd := posEx('</VertexIndices>', xmlStr, xmlPos); //CIFTI2
  end;
  if (lbm.IndexCount = 0) and (not lbm.isSparse) then
     lbm.IndexCount:= lbm.SurfaceNumberOfVertices;
  lbm.ModelType :=  readXStr('ModelType');
  lbm.BrainStructure :=  readXStr('BrainStructure');
  result := true;
  {$IFDEF CIFTI_DEBUG}showdebug(format(' BrainStructure %s IndexOffset %d  IndexCount %d SurfaceNumberOfVertices %d',[lbm.BrainStructure, lbm.IndexOffset, lbm.IndexCount, lbm.SurfaceNumberOfVertices]));{$ENDIF}
end;

begin
  {$IFDEF CIFTI_DEBUG}showdebug(format('Reading %s',[fnm]));{$ENDIF}
  result := -1;
  if isLoadCortexLeft then
     desiredBrainStructure := 'CIFTI_STRUCTURE_CORTEX_LEFT'
  else
     desiredBrainStructure := 'CIFTI_STRUCTURE_CORTEX_RIGHT';
  if not fileexists(fnm) then exit;
  fsz := FSize(fnm);
  if (fsz < (sizeof(hdr)+132)) then exit;
  FileMode := fmOpenRead;
  AssignFile(f, fnm);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(f, 1);
  BlockRead(f, hdr, sizeof(hdr));
  FileMode := 2;
  CloseFile(f);
  hdrSzSwap := hdr.hdrSz;
  SwapLongInt(hdrSzSwap);
  if sizeof(hdr) = hdrSzSwap then begin
     showmessage('Only able to read native endian NIFTI2 files');
     exit;
  end;
  if (hdr.hdrSz <> sizeof(hdr)) then exit;
  xmlLen := hdr.vox_offset - (sizeof(hdr)+4);
  if (xmlLen < 128) then exit;//not enough space for CIFTI
  if (xmlLen+sizeof(hdr)+4) > fsz then exit;
  if hdr.datatype <> kDT_FLOAT32 then begin
     showmessage('Currently CIfTI impport only supports FLOAT32 datatype');
     exit;
  end;
  //next: read XML
  setlength(xmlStr, xmlLen);
  FileMode := fmOpenRead;
  AssignFile(f, fnm);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(f, 1);
  Seek(f, sizeof(hdr) + 4); //NIfTI2 has 4 byte padding to be divisible by 8
  BlockRead(f, xmlStr[1], xmlLen);
  FileMode := 2;
  CloseFile(f);
  xmlPos := pos('<CIFTI Version="1', xmlStr);
  isCifti1 := (xmlPos > 0);
  if xmlPos < 1 then
     xmlPos := pos('<CIFTI Version="2', xmlStr);
  if xmlPos < 1 then begin
     if pos('<CIFTI Version="',xmlStr) > 0 then //future version of CIfTI?
        showmessage('This software supports CIfTI versions 1 or 2 (update or use "wb_command -file-convert"): '+fnm)
     else
         showmessage('Not a valid CIfTI file: '+fnm);
     exit; //XML is case sensitive
  end;
  allBrainStructures := '';
  isFoundBrainStructure := false;
  while ((not isFoundBrainStructure) and readBrainModel(bm)) do begin
        isFoundBrainStructure := (pos(desiredBrainStructure, bm.BrainStructure ) > 0);
        xmlPos := bm.BrainModelEnd;
        allBrainStructures := bm.BrainStructure+' '+allBrainStructures;
  end;
  if not isFoundBrainStructure then begin
     showmessage(format('Unable to find "%s", from available "%s"',[desiredBrainStructure, allBrainStructures]));
     exit;
  end;
  if (bm.isPConn) then begin
     showmessage('Not yet able to read pconn images (designed for dscalar.nii images)');
     exit;
  end;
  if (bm.isSparse) and (bm.VertexIndicesStart >= bm.VertexIndicesEnd)then begin
     showmessage('Unable to find array <VertexIndices>');
     exit;
  end;
  if bm.IndexCount < 1 then begin
     showmessage('No vertex indices');
     exit;
  end;
  {$IFNDEF CIFTI_DEBUG}
  if bm.SurfaceNumberOfVertices <> length(vertices) then begin
     showmessage(format('Expected %d vertices, but CIFTI has %d (load *.surf.gii BEFORE %s)',[length(vertices), bm.SurfaceNumberOfVertices, extractfilename(fnm)]));
     exit;
  end;
  {$ENDIF CIFTI_DEBUG}
  setlength(vertexIndices, bm.IndexCount);
  if bm.isSparse then begin
     xmlPos := bm.VertexIndicesStart;
     for i := 0 to (bm.IndexCount-1) do
         vertexIndices[i] := readXArrayInt;
  end else
      for i := 0 to (bm.IndexCount-1) do
         vertexIndices[i] := i;
  //read raw data
  if (isCifti1) then
     nSeries := hdr.dim[6]
  else
      nSeries := hdr.dim[5];
  if (lSeriesIndex > nSeries) then begin
     showmessage(format('CIfTI file has %d series (e.g. timepoints)',[nSeries]));
     exit;
  end;
  setlength(rawData, hdr.dim[5] * hdr.dim[6]);
  AssignFile(f, fnm);
  FileMode := fmOpenRead;  //Set file access to read only
  Reset(f, 1);
  seek(f,hdr.vox_offset);
  BlockRead(f, rawData[0], hdr.dim[5] * hdr.dim[6] * sizeof(single));
  FileMode := 2;
  CloseFile(f);
  {$IFDEF CIFTI_DEBUG}
  showdebug(format('index addresses %d..%d', [bm.VertexIndicesStart, bm.VertexIndicesEnd]));
  showdebug(format('index range %d..%d',[MinIntValue(vertexIndices), MaxIntValue(vertexIndices)]));
  showdebug('datatype '+inttostr(hdr.datatype));
  for i := 1 to 7 do
      showdebug('dim'+inttostr(i)+' '+inttostr(hdr.dim[i]));
  {$ELSE}
  setlength(overlay[lOverlayIndex].intensity, bm.SurfaceNumberOfVertices);
  for i := 0 to (bm.SurfaceNumberOfVertices-1) do
      overlay[lOverlayIndex].intensity[i] := 0;
  k := (bm.IndexOffset * nSeries);
  for i := 0 to (bm.IndexCount-1) do
      for j := 1 to nSeries do begin
          if j = lSeriesIndex then
             overlay[lOverlayIndex].intensity[vertexIndices[i]] := rawData[k];
          k := k + 1;
      end;
  {$ENDIF}
  //
  //<LabelTable>
  // <Label Key="0" Red="1" Green="1" Blue="1" Alpha="0">???</Label>
  // <Label Key="1" Red="0.270588" Green="0" Blue="1" Alpha="1">R_V1_ROI</Label>
  //TVertexRGBA
  // lut : array of TRGBA;

  xmlPos := pos('</LabelTable>', xmlStr);
  if xmlPos > 0 then begin
     xmlPos := pos('<LabelTable>', xmlStr);
     if xmlPos < 1 then goto 131;
     nVert := bm.SurfaceNumberOfVertices;
     fmx := overlay[lOverlayIndex].intensity[0];
     for i := 0 to (nVert-1) do
         if overlay[lOverlayIndex].intensity[i] > fmx then
            fmx := overlay[lOverlayIndex].intensity[i];
     mx := round(fmx);
     if mx < 1 then goto 131;
     tablePos := xmlPos;
     xmlPos := pos('<Label Key="'+inttostr(mx)+'"', xmlStr);
     if xmlPos < 1 then goto 131;
     setlength(lut, mx + 1);
     for i := 0 to mx do
         lut[i] := ReadLut(i);
     setlength(vertexRGBA, nVert);
     for i := 0 to (nVert-1) do begin
         j := round(overlay[lOverlayIndex].intensity[i]);
         if j < 0 then continue;
         vertexRGBA[i] := lut[j];
     end;
     setlength(overlay[lOverlayIndex].intensity, 0);
     //GLForm1.caption := 'xx'+inttostr(mx);
  end;
131:
  setlength(rawData,0);
  setlength(vertexIndices,0);
  result := nSeries;
end;
