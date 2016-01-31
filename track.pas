unit track;
{$mode objfpc}{$H+}
{$Include opts.inc} //compile for either dglOpenGL or glext
interface

uses
  {$IFDEF DGL} dglOpenGL, {$ELSE} gl, {$IFDEF COREGL}glext,{$ENDIF} {$ENDIF}
  Classes, SysUtils,  dialogs, matmath, math, define_types;
//http://www.trackvis.org/docs/?subsect=fileformat

Type

TTrack = class
  //utime: QWord;
  scale, minFiberLength, ditherColorFrac : single;
  origin : TPoint3f;
  isBusy, isRebuildList, isTubes: boolean;
  TrackTubeSlices, //e.g. if 5 then cross section of fiber is pentagon
  n_count, n_faces, n_vertices, n_indices, minFiberLinks, LineWidth: integer;
  {$IFDEF COREGL}
  vao, vbo : GLuint;
  {$ELSE}
  displayList : GLuint;
  {$ENDIF}
  tracks: array of single;
  private
    function LoadTrk(const FileName: string): boolean;
    function LoadVtkASCII(const FileName: string): boolean;
    function LoadVtk(const FileName: string): boolean;
    procedure SetDescriptives;
    procedure BuildListStrip;
    procedure BuildListTubes ;
  public
    constructor Create;
    function LoadFromFile(const FileName: string): boolean;
    procedure SaveVtk(const FileName: string);
    procedure Close;
    procedure DrawGL ;
  end;

implementation

uses mainunit, shaderu, {$IFDEF COREGL} gl_core_3d {$ELSE} gl_legacy_3d {$ENDIF};

procedure TTrack.Close;
begin
     n_count := 0;
     n_vertices := 0;
     n_faces := 0;
     n_indices := 0;
     setlength(tracks, 0);
end;

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

function mixRandRGBA(RGBin: TPoint3f; randAmount: single): TRGBA;
//randAmount  0..1 mix this proportion of random (grayscale) intensity
var
  s: single;
  v: TPoint3f;

begin
     v := RGBin;
     if (randAmount < 0) or (randAmount > 1.0) then exit;
     s := random();
     v.X := (randAmount * s) + ((1-randAmount) * v.X);
     v.Y := (randAmount * s) + ((1-randAmount) * v.Y);
     v.Z := (randAmount * s) + ((1-randAmount) * v.Z);
     result.R := round(UnitBound(v.X) * 255);
     result.G := round(UnitBound(v.Y) * 255);
     result.B := round(UnitBound(v.Z) * 255);
     result.A := 255;
end;

procedure TTrack.BuildListStrip;
// create displaylist where tracks are drawn as connected line segments
var
  vRGBA: TVertexRGBA;
  Indices: TInts;
  Verts, vNorms: TVertices;
  normRGB: TPoint3f;
  normRGBA : TRGBA;
  pts, norms: array of TPoint3f;
  len: single;
  maxLinks, m, mi, i,j, ntracks: integer;
  trackLinks : array of integer;
  startPt, endPt:TPoint3f;
begin
  maxLinks := 0;
  n_faces := 0;
  n_vertices := 0;
  TrackTubeSlices := 5;
  if (length(tracks) < 4) then exit;
  if minFiberLinks < 3 then minFiberLinks := 3; //minimum to compute normal;
  ntracks := length(tracks);
  //uTime := GetTickCount64();
  {$DEFINE TWOPASS_STRIP} //two passes is ~2 times quicker as we do not waste time re-allocating memory
  {$IFDEF TWOPASS_STRIP}
  //first pass: find mesh size
  setlength(trackLinks, ntracks);
  i := 0;
  while i < ntracks do begin
    trackLinks[i] := 0;
    m := asInt( tracks[i]);
    if m >= minFiberLinks then begin
       startPt.X := tracks[i+1];
       startPt.Y := tracks[i+2];
       startPt.Z := tracks[i+3];
       j := (3 * (m-1));
       endPt.X := tracks[j+i+1];
       endPt.Y := tracks[j+i+2];
       endPt.Z := tracks[j+i+3];
       normRGB := vector2RGB(startPt, endPt, len);
       if len >= minFiberLength then begin
          trackLinks[i] := m;
          n_vertices := n_vertices + m;
          {$IFDEF COREGL}
          n_faces := n_faces + m + 3;//adjacent start + adjacent end + primitive restart!
          {$ELSE}
          n_faces := n_faces + m + 1;//primitive restart!
          {$ENDIF}
          if (m > maxLinks) then
             maxLinks := m;
       end;
    end; //len >= minFiberLength
    i := i + 1 + (3 * m);
  end;
  if (maxLinks < 1) then exit;
  //allocate memory
  setlength(pts, maxLinks);
  setlength(norms, maxLinks);
  setlength(vRGBA, n_vertices);
  setlength(Verts, n_vertices);
  setlength(vNorms, n_vertices);
  setlength(Indices, n_faces);
  //second pass: fill arrays
  i := 0;
  n_vertices := 0;
  n_faces := 0;
  while i < ntracks do begin
        m :=   asInt( tracks[i]);
        if trackLinks[i] >= minFiberLinks then begin
          inc(i);
          for mi := 0 to (m-1) do begin
              pts[mi].X := tracks[i]; inc(i);
              pts[mi].Y := tracks[i]; inc(i);
              pts[mi].Z := tracks[i]; inc(i);
          end;
          normRGB := vector2RGB(pts[0], pts[m-1], len);
          normRGBA := mixRandRGBA(normRGB, ditherColorFrac);
          for mi := 1 to (m-2) do
              norms[mi] := normalDirection(pts[mi-1], pts[mi+1]); //line does not have a surface normal, but a direction
          //create as line strip - repeat start/end to end primitive
          for mi := 0 to (m-1) do begin
              vRGBA[mi+n_vertices] := normRGBA;
              Verts[mi+n_vertices] := pts[mi];
              vNorms[mi+n_vertices] := norms[mi];
              {$IFDEF COREGL}
               Indices[mi+1+n_faces] := mi+n_vertices;
              {$ELSE}
              Indices[mi+n_faces] := mi+n_vertices;
              {$ENDIF}
          end;
          {$IFDEF COREGL} // COREGL : glDrawElements(GL_LINE_STRIP_ADJACENCY... ; LEGACY : glBegin(GL_LINE_STRIP);
          Verts[n_vertices] := Verts[n_vertices+1];
          Verts[n_vertices+m-1] := Verts[n_vertices+m-2];
          Indices[n_faces] := n_vertices;
          Indices[m+n_faces+1] := Indices[m+n_faces];
          Indices[m+n_faces+2] := kPrimitiveRestart;
          n_faces := n_faces + m + 3;
          {$ELSE}
          Indices[m+n_faces] := kPrimitiveRestart;
          n_faces := n_faces + m + 1;
          {$ENDIF}
          n_vertices := n_vertices + m ;

        end else
            i := i + 1 + (3 * m);
  end;
  {$ELSE}
   {$IFDEF COREGL} Use two pass or change code below for GL_LINE_STRIP_ADJACENCY {$ENDIF}
  i := 0;
  while i < ntracks do begin
        m :=   asInt( tracks[i]); inc(i);
        if m >= minFiberLinks then begin
          setlength(pts, m);
          setlength(norms, m);
          for mi := 0 to (m-1) do begin
              pts[mi].X := tracks[i]; inc(i);
              pts[mi].Y := tracks[i]; inc(i);
              pts[mi].Z := tracks[i]; inc(i);
          end;
          normRGB := vector2RGB(pts[0], pts[m-1], len);
          if len >= minFiberLength then begin
            normRGBA := mixRandRGBA(normRGB, ditherColorFrac);
            for mi := 1 to (m-2) do
                norms[mi] := normalDirection(pts[mi-1], pts[mi+1]); //line does not have a surface normal, but a direction
            setlength(vRGBA, n_vertices + m);
            setlength(Verts, n_vertices + m);
            setlength(vNorms, n_vertices + m);
            setlength(Indices, n_faces + m + 1);
            //create as line strip - repeat start/end to end primitive
            for mi := 0 to (m-1) do begin
                vRGBA[mi+n_vertices] := normRGBA;
                Verts[mi+n_vertices] := pts[mi];
                vNorms[mi+n_vertices] := norms[mi];
                Indices[mi+n_faces] := mi+n_vertices;
            end;
            Indices[m+n_faces] := kPrimitiveRestart;
            n_vertices := n_vertices + m ;
            n_faces := n_faces + m + 1;
          end;
        end else
            i := i + (3 * m);
  end;
  {$ENDIF}
  //uTime := GetTickCount64() - uTime;

  {$IFDEF COREGL}
  BuildDisplayListStrip(Indices, Verts, vNorms, vRGBA, LineWidth, vao, vbo);
  {$ELSE}
  displayList := BuildDisplayListStrip(Indices, Verts, vNorms, vRGBA, LineWidth);
  {$ENDIF}
  n_indices := length(Indices);
  n_vertices := 0;
  n_faces := 0;
end; // BuildList()

procedure TTrack.BuildListTubes ;
// create displaylist where tracks are drawn as connected cylinders
//const
//  kSlices = 5; //the cylinder is a pie cut into this many slices: fewer = simpler,  more = less boxy
var
  vRGBA: TVertexRGBA;
  normRGB: TPoint3f;
  normRGBA: TRGBA;
  pts: array of TPoint3f;
  len, radius: single;
  numCylVert, numVert,  mprev, m, mi, i, j, ntracks: integer;
  vertices: TVertices;
  faces, cylFace: TFaces;
  numFaces, numCylFace,  maxLinks: integer;
  cylVert: TVertices;
  startPt, endPt: TPoint3f;
  trackLinks : array of integer;
begin
  //tm := gettickcount64();
  n_indices := 0;
  n_faces := 0;
  n_vertices := 0;
  maxLinks := 0;
  if (length(tracks) < 4) then exit;
  if minFiberLinks < 3 then minFiberLinks := 3; //minimum to compute normal;
  MakeCylinder( 1, 71, cylFace, cylVert, TrackTubeSlices);
  numCylFace := length(cylFace);
  numCylVert := TrackTubeSlices;//numCylVert div 2; //number of faces for half of cylinder (top or bottom disk)
  ntracks := length(tracks);
  radius := LineWidth * 0.25;
  {$DEFINE TWOPASS} //two passes is ~4 times quicker as we do not waste time re-allocating memory
  {$IFDEF TWOPASS}
  //first pass: find mesh size
  setlength(trackLinks, ntracks);
  i := 0;
  while i < ntracks do begin
    trackLinks[i] := 0;
    m := asInt( tracks[i]);
    if m >= minFiberLinks then begin
       startPt.X := tracks[i+1];
       startPt.Y := tracks[i+2];
       startPt.Z := tracks[i+3];
       j := (3 * (m-1));
       endPt.X := tracks[j+i+1];
       endPt.Y := tracks[j+i+2];
       endPt.Z := tracks[j+i+3];
       normRGB := vector2RGB(startPt, endPt, len);
       if len >= minFiberLength then begin
          trackLinks[i] := m;
          n_vertices := n_vertices + (m * numCylVert);
          n_faces := n_faces + ((m - 1) * numCylFace); //fence post problem
          if (m > maxLinks) then
             maxLinks := m;
       end;
    end; //len >= minFiberLength
    i := i + 1 + (3 * m);
  end;
  if (n_faces < 1) then exit;
  //allocate memory
  setlength(vertices, n_vertices); //each node as 1 disk (bottom of cylinder)
  setlength(vRGBA, n_vertices); //each node as 1 disk (bottom of cylinder)
  setlength(faces, n_faces);
  setlength(pts, maxLinks);
  //second pass - load geometry for links that are long enough
  n_vertices := 0;
  n_faces := 0;
  i := 0;
  while i < ntracks do begin
    m := asInt( tracks[i]);
    if (trackLinks[i] >= minFiberLinks) then begin
       inc(i);
       for mi := 0 to (m-1) do begin
              pts[mi].X := tracks[i]; inc(i);
              pts[mi].Y := tracks[i]; inc(i);
              pts[mi].Z := tracks[i]; inc(i);
       end;
       normRGB := vector2RGB(pts[0], pts[m-1], len);
       numFaces := 0;
       numVert := 0;
       mprev := 0; //location of previous vertex
       for mi := 0 to (m-2) do begin
           makeCylinderEnd(radius, pts[mprev], pts[mi], pts[mi+1], cylVert, TrackTubeSlices);
           for j := 0 to (numCylFace - 1) do //add this cylinder
               faces[j+numFaces+n_faces] := vectorAdd(cylFace[j], numVert+n_vertices);
           numFaces := numFaces + numCylFace;
           for j := 0 to (numCylVert - 1) do //add bottom of this cylinder
               vertices[j+numVert+n_vertices] := cylVert[j];
           numVert := numVert + numCylVert;
           mprev := mi;
       end;
       //faces[numFaces+n_faces-1] := vectorAdd(cylFace[j], 0);
       makeCylinderEnd(radius, pts[m-2], pts[m-1], pts[m-1], cylVert, TrackTubeSlices);
       for j := 0 to (numCylVert - 1) do //add top of last cylinder
           vertices[j+numVert+n_vertices] := cylVert[j];

       numVert := numVert + numCylVert;
       normRGBA := mixRandRGBA(normRGB, ditherColorFrac);
       for j := 0 to ((m * numCylVert) -1) do
           vRGBA[j+n_vertices] := normRGBA;
       n_vertices := n_vertices + (m * numCylVert); //each node as 1 disk (bottom of cylinder)
       n_faces := n_faces + (m - 1) * numCylFace; //-1: fencepost error
    end else //len >= minFiberLength
        i := i + 1 + (3 * m);
  end;
  setlength(trackLinks,0);
  setlength(pts, 0);
{$ELSE}
  i := 0;
  while i < ntracks do begin
        m :=   asInt( tracks[i]); inc(i);
        if m >= minFiberLinks then begin
          setlength(pts, m);
          for mi := 0 to (m-1) do begin
              pts[mi].X := tracks[i]; inc(i);
              pts[mi].Y := tracks[i]; inc(i);
              pts[mi].Z := tracks[i]; inc(i);
          end;
          normRGB := vector2RGB(pts[0], pts[m-1], len);
          if len >= minFiberLength then begin
              setlength(vertices, n_vertices + (m * numCylVert) ); //each node as 1 disk (bottom of cylinder)
              setlength(faces, n_faces + ((m - 1) * numCylFace)  ); //-1: fencepost error
              numFaces := 0;
              numVert := 0;
              mprev := 0; //location of previous vertex
              for mi := 0 to (m-2) do begin
                  makeCylinderEnd(radius, pts[mprev], pts[mi], pts[mi+1], cylVert, kSlices);
                  for j := 0 to (numCylFace - 1) do //add this cylinder
                      //faces[j+numFaces+n_faces] := pti(cylFace[j].X+numVert, cylFace[j].Y+numVert, cylFace[j].Z+numVert) ;
                      faces[j+numFaces+n_faces] := vectorAdd(cylFace[j], numVert+n_vertices);
                  numFaces := numFaces + numCylFace;
                  for j := 0 to (numCylVert - 1) do //add bottom of this cylinder
                      vertices[j+numVert+n_vertices] := cylVert[j];
                  numVert := numVert + numCylVert;
                  mprev := mi;
              end;
              makeCylinderEnd(radius, pts[m-2], pts[m-1], pts[m-1], cylVert, kSlices);
              for j := 0 to (numCylVert - 1) do //add top of last cylinder
                  vertices[j+numVert+n_vertices] := cylVert[j];
              numVert := numVert + numCylVert;
            normRGBA := mixRandRGBA(normRGB, ditherColorFrac);
            setlength(vRGBA, n_vertices + (m * numCylVert) ); //each node as 1 disk (bottom of cylinder)
            for j := 0 to ((m * numCylVert) -1) do
                vRGBA[j+n_vertices] := normRGBA;
            n_vertices := n_vertices + (m * numCylVert); //each node as 1 disk (bottom of cylinder)
            n_faces := n_faces + (m - 1) * numCylFace; //-1: fencepost error
          end; //len >= minFiberLength
        end else
            i := i + (3 * m);
  end;
{$ENDIF}
  //utime := gettickcount64() - tm;

  {$IFDEF COREGL}
  BuildDisplayList(faces, vertices, vRGBA, vao, vbo, normRGBA);
  {$ELSE}
  displayList := BuildDisplayList(faces, vertices, vRGBA);
  {$ENDIF}
end;

procedure TTrack.DrawGL;
begin
  if (length(tracks) < 4) then exit;
  if isBusy then exit;
  isBusy := true;
  if isRebuildList then begin
    {$IFDEF COREGL}
    if vao <> 0 then
       glDeleteVertexArrays(1,@vao);
    if (vbo <> 0) then
       glDeleteBuffers(1, @vbo);
    vao := 0; vbo := 0;
    {$ELSE}
    if displayList <> 0 then
       glDeleteLists(displayList, 1);
    displayList := 0;
    {$ENDIF}
    if isTubes then //this is slow, so 'isRebuildList' ensures this is done only if the model has been changed
       BuildListTubes //upload geometry as a cylinder-based display list: http://www.songho.ca/opengl/gl_displaylist.html
    else
       BuildListStrip; //upload geometry as a line-based display list: http://www.songho.ca/opengl/gl_displaylist.html
    isRebuildList := false;

  end;
    {$IFDEF COREGL}
  //RunMeshGLSL (2,0,0,0); //disable clip plane
  glBindVertexArray(vao);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,vbo);
  if isTubes then begin
    //666 RunMeshGLSL (2,0,0,0);
    glDrawElements(GL_TRIANGLES, n_faces* 3, GL_UNSIGNED_INT, nil)
  end else if n_indices > 0 then begin
     //RunTrackGLSL(lineWidth, lPrefs);
     //GLForm1.Caption := inttostr(n_indices);
     glPrimitiveRestartIndex(kPrimitiveRestart);
     glDrawElements(GL_LINE_STRIP_ADJACENCY, n_indices, GL_UNSIGNED_INT, nil)  ;
  end;
  glBindVertexArray(0);
  {$ELSE}
  glCallList(DisplayList);
  {$ENDIF}
  isBusy := false;
end; // DrawGL()

(*procedure TTrack.DrawGL;
begin
  if (length(tracks) < 4) then exit;
  if isBusy then exit;
  isBusy := true;
    {$IFDEF COREGL}


  {$ELSE}
  if isRebuildList then begin
     if displayList <> 0 then
        glDeleteLists(displayList, 1);
     displayList := 0;
     if isTubes then //this is slow, so 'isRebuildList' ensures this is done only if the model has been changed
        BuildListTubes(clr) //upload geometry as a cylinder-based display list: http://www.songho.ca/opengl/gl_displaylist.html
     else
        BuildListStrip; //upload geometry as a line-based display list: http://www.songho.ca/opengl/gl_displaylist.html
     isRebuildList := false;
  end;
  glCallList(DisplayList);
  {$ENDIF}
  isBusy := false;
end; // DrawGL()
*)
type
 TTrackhdr = packed record //Next: analyze Format Header structure
   id_string: array [1..6] of ansichar; //TRACK*
   dim: array [1..3] of SmallInt;
   voxel_size: array [1..3] of Single;
   origin: array [1..3] of single;
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

constructor  TTrack.Create;
begin
     SetLength(tracks, 0);
     n_count := 0;
     n_faces := 0;
     n_vertices := 0;
     LineWidth := 2;
     {$IFDEF COREGL}
     vao := 0;
     vbo := 0;
     {$ELSE}
     displayList := 0;
     {$ENDIF}
     scale := 0;
     ditherColorFrac := 0.3;
     minFiberLinks := 2;
     minFiberLength := 20;
     isRebuildList := true;
     isBusy := false;
     isTubes := true;
end; // Create()

function TTrack.LoadVtkASCII(const FileName: string): boolean;
//Read ASCII VTK mesh
// ftp://ftp.tuwien.ac.at/visual/vtk/www/FileFormats.pdf
label
   666;
var
   f: TextFile;
   strlst: TStringList;
   str: string;
   nVtx, inPos, outPos, vtx, i, v, num_v, n_items: integer;
   vert: array of TPoint3f;
   items: array of LongInt;
begin
  result := false;
  AssignFile(f, FileName);
  Reset(f);
  ReadLn(f, str); //signature: '# vtk DataFile'
  if pos('VTK', UpperCase(str)) <> 3 then begin
    goto 666;
  end;
  ReadLn(f, str); //comment: 'Comment: created with MRIcroS'
  ReadLn(f, str); //kind: 'BINARY' or 'ASCII'
  if (pos('ASCII', UpperCase(str)) <> 1) then begin  // '# vtk DataFile'
     showmessage('Only able to read binary VTK file (convert with MRIcroS or another tool): '+str);
     goto 666;
  end;
  ReadLn(f, str); // kind, e.g. "DATASET POLYDATA" or "DATASET STRUCTURED_ POINTS"
  if pos('POLYDATA', UpperCase(str)) = 0 then begin
    showmessage('Only able to read VTK images saved as POLYDATA, not '+ str);
    goto 666;
  end;
  ReadLn(f, str); // number of vert, e.g. "POINTS 685462 float"
  if pos('POINTS', UpperCase(str)) <> 1 then begin
    showmessage('Expected header to report "POINTS" not '+ str);
    goto 666;
  end;
  strlst:=TStringList.Create;
  num_v := 0;
  strlst.DelimitedText := str;
  num_v := StrToIntDef(strlst[1],0);
  if (num_v < 1) or (pos('FLOAT', UpperCase(strlst[2])) <> 1) then begin
    showmessage('Expected at least 1 point of type FLOAT, not '+ str);
    goto 666;
  end;
  setlength(vert, num_v); //vert = zeros(num_f, 9);
  for i := 0 to (num_v - 1) do begin
      read(f,vert[i].X);
      read(f,vert[i].Y);
      read(f,vert[i].Z);
  end;
  ReadLn(f, str); if str = '' then ReadLn(f, str);
  ReadLn(f, str); if str = '' then ReadLn(f, str);
  if pos('POLYGONS', UpperCase(str)) > 0 then begin // number of vert, e.g. "POLYGONS 1380 5520"
    showmessage('This is a mesh file: rename with a ".vtk" extension and use File/Open to view: '+ str);
    goto 666;
  end;
  if pos('LINES', UpperCase(str)) <> 1 then begin
    showmessage('Expected header to report "LINES" not '+ str);
    goto 666;
  end;
  strlst.DelimitedText := str;
  n_count := StrToIntDef(strlst[1],0);
  n_items := StrToIntDef(strlst[2],0);
  strlst.free;
  setlength(items, n_items);
  for i := 0 to (n_items - 1) do
      read(f,items[i]);
  vtx := n_items - n_count;
  setlength(tracks, vtx * 3 + n_count);
  inPos := 0;
  outPos := 0;
  for i := 1 to n_count do begin
      nVtx := items[inPos]; inc(inPos);
      tracks[outPos] := asSingle(nVtx); inc(outPos);
      for v := 1 to nVtx do begin
          vtx := items[inPos]; inc(inPos);
          tracks[outPos] := vert[vtx].X; inc(outPos);
          tracks[outPos] := vert[vtx].Y; inc(outPos);
          tracks[outPos] := vert[vtx].Z; inc(outPos);
      end; //for each vertex in fiber
  end; //for each fiber
 result := true;
 exit;
666:
   closefile(f);
end;  //LoadVtkASCII()

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
end;

function TTrack.LoadVtk(const FileName: string): boolean;
//Read BINARY VTK mesh
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bvtk/readVtk.m
// ftp://ftp.tuwien.ac.at/visual/vtk/www/FileFormats.pdf
//  "The VTK data files described here are written in big endian form"
label
   666;
var
   f: TFByte;
   strlst: TStringList;
   str: string;
   nVtx, inPos, outPos, vtx, i, v, num_v, num_f, n_items: integer;
   vert: array of TPoint3f;
   items: array of LongInt;
begin
  result := false;
  AssignFile(f, FileName);
  Reset(f,1);
  ReadLnBin(f, str); //signature: '# vtk DataFile'
  if pos('VTK', UpperCase(str)) <> 3 then begin
    goto 666;
  end;
  ReadLnBin(f, str); //comment: 'Comment: created with MRIcroS'
  ReadLnBin(f, str); //kind: 'BINARY' or 'ASCII'
  if (pos('BINARY', UpperCase(str)) <> 1) then begin  // '# vtk DataFile'
     closefile(f);
     result := LoadVtkASCII(FileName);
     exit;
  end;
  ReadLnBin(f, str); // kind, e.g. "DATASET POLYDATA" or "DATASET STRUCTURED_ POINTS"
  if pos('POLYDATA', UpperCase(str)) = 0 then begin
    showmessage('Only able to read VTK images saved as POLYDATA, not '+ str);
    goto 666;
  end;
  ReadLnBin(f, str); // number of vert, e.g. "POINTS 685462 float"
  if pos('POINTS', UpperCase(str)) <> 1 then begin
    showmessage('Expected header to report "POINTS" not '+ str);
    goto 666;
  end;
  strlst:=TStringList.Create;
  num_v := 0;
  strlst.DelimitedText := str;
  num_v := StrToIntDef(strlst[1],0);
  if (num_v < 1) or (pos('FLOAT', UpperCase(strlst[2])) <> 1) then begin
    showmessage('Expected at least 1 point of type FLOAT, not '+ str);
    goto 666;
  end;
  setlength(vert, num_v); //vert = zeros(num_f, 9);
  blockread(f, vert[0], 3 * 4 * num_v);
  ReadLnBin(f, str); // number of vert, e.g. "POLYGONS 1380 5520"
  if str = '' then ReadLnBin(f, str);
  if pos('POLYGONS', UpperCase(str)) > 0 then begin
    showmessage('This is a mesh file: rename with a ".vtk" extension and use File/Open to view: '+ str);
    goto 666;
  end;
  if pos('LINES', UpperCase(str)) <> 1 then begin
    showmessage('Expected header to report "LINES" not '+ str);
    goto 666;
  end;
  strlst.DelimitedText := str;
  n_count := StrToIntDef(strlst[1],0);
  n_items := StrToIntDef(strlst[2],0);
  strlst.free;
  setlength(items, n_items);
  blockread(f, items[0], n_items * 4);
  closefile(f);
  {$IFDEF ENDIAN_LITTLE}
  for i := 0 to (n_items -1) do
      SwapLongInt(items[i]);
  for i := 0 to (num_v -1) do begin
      SwapSingle(vert[i].X);
      SwapSingle(vert[i].Y);
      SwapSingle(vert[i].Z);
  end;
  {$ENDIF}
  vtx := n_items - n_count;
  setlength(tracks, vtx * 3 + n_count);
  inPos := 0;
  outPos := 0;
  for i := 1 to n_count do begin
      nVtx := items[inPos]; inc(inPos);
      tracks[outPos] := asSingle(nVtx); inc(outPos);
      for v := 1 to nVtx do begin
          vtx := items[inPos]; inc(inPos);
          tracks[outPos] := vert[vtx].X; inc(outPos);
          tracks[outPos] := vert[vtx].Y; inc(outPos);
          tracks[outPos] := vert[vtx].Z; inc(outPos);
      end; //for each vertex in fiber
  end; //for each fiber
 result := true;
 exit;
666:
   closefile(f);
end;

function TTrack.LoadTrk(const FileName: string): boolean;
var
   f: File;
   fsz, ntracks: int64;
   hdr:  TTrackhdr;
   i,m,mi: integer;
   pt: TPoint3f;
begin
  isRebuildList := true;
  result := false;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  fsz := filesize(f);
  blockread(f, hdr, sizeof(TTrackhdr) );
  if (hdr.id_string[1] <> 'T') or (hdr.id_string[2] <> 'R')
  or (hdr.id_string[3] <> 'A') or (hdr.id_string[4] <> 'C')
  or (hdr.id_string[5] <> 'K') then exit;
  if (hdr.hdr_size = -402456576) then begin
     showmessage('Please convert the endian-ness of your track file.');
     CloseFile(f);
     exit;
  end;
  if (hdr.n_scalars <> 0) or (hdr.n_properties <> 0) then begin
     showmessage('This software does not support tracks with scalars or properties');
     CloseFile(f);
     exit;
  end;
  if(hdr.hdr_size <>sizeof(TTrackhdr) ) then exit;
  ntracks := (fsz - sizeof(TTrackhdr)) div sizeof(single);
  if ntracks < 4 then exit;
  setlength(tracks, ntracks);
  blockread(f, tracks[0], ntracks * sizeof(single) );
  CloseFile(f);
  result := true;
  if hdr.vox_to_ras[4,4] = 0 then begin
        //showmessage('This file does not record vox_to_ras: orientation may be incorrect');
        hdr.vox_to_ras[1,1] := 1; hdr.vox_to_ras[1,2] := 0; hdr.vox_to_ras[1,3] := 0; hdr.vox_to_ras[1,4] := 0;
        hdr.vox_to_ras[2,1] := 0; hdr.vox_to_ras[2,2] := 1; hdr.vox_to_ras[2,3] := 0; hdr.vox_to_ras[2,4] := 0;
        hdr.vox_to_ras[3,1] := 0; hdr.vox_to_ras[3,2] := 0; hdr.vox_to_ras[3,3] := 1; hdr.vox_to_ras[3,4] := 0;
  end;
  ntracks := length(tracks);
  i := 0;
  n_count := 0; // note hdr.n_count may not be set: determine it explicitly
  while i < ntracks do begin
        m :=   asInt( tracks[i]); inc(i);
        n_count := n_count + 1;
        for mi := 1 to m do begin
            pt.X := tracks[i];
            inc(i);
            pt.Y := tracks[i];
            inc(i);
            pt.Z := tracks[i];
            inc(i);
            tracks[i-3] := Pt.X*hdr.vox_to_ras[1,1] + Pt.Y*hdr.vox_to_ras[1,2] + Pt.Z*hdr.vox_to_ras[1,3] + hdr.vox_to_ras[1,4];
            tracks[i-2] := Pt.X*hdr.vox_to_ras[2,1] + Pt.Y*hdr.vox_to_ras[2,2] + Pt.Z*hdr.vox_to_ras[2,3] + hdr.vox_to_ras[2,4];
            tracks[i-1] := Pt.X*hdr.vox_to_ras[3,1] + Pt.Y*hdr.vox_to_ras[3,2] + Pt.Z*hdr.vox_to_ras[3,3] + hdr.vox_to_ras[3,4];
        end;
  end;
end;

procedure TTrack.SetDescriptives;
var
   i, m, mi: integer;
   mn, mx, pt : TPoint3f;
begin
  if (n_count < 1) or (length(tracks) < 4) then exit;
  mn := ptf(Infinity,Infinity,Infinity);
  mx := ptf(-Infinity, -Infinity, -Infinity);
  i := 0;
  //showmessage(format('--> %g %g %g',[tracks[1], tracks[2], tracks[3]]) );
  while i < length(tracks) do begin
        m :=   asInt( tracks[i]); inc(i);
        for mi := 1 to m do begin
            pt.X := tracks[i]; inc(i);
            pt.Y := tracks[i]; inc(i);
            pt.Z := tracks[i]; inc(i);
            minmax(pt, mn,mx);
        end;
  end;
  //showmessage(format('%d  -> %g..%g %g..%g %g..%g', [nstrip, mn.X, mx.X, mn.Y, mx.Y, mn.Z, mx.Z]));
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

function TTrack.LoadFromFile(const FileName: string): boolean;
var
   ext: string;
begin
  isBusy := true;
  Self.Close;
    result := false;

    if not FileExists(FileName) then exit;
    ext := UpperCase(ExtractFileExt(Filename));
    if (ext = '.TRK') then begin
         if not LoadTrk(FileName) then exit;
    end else  //(ext = '.FIB') 0r (ext = '.FIB') then begin
        LoadVtk(Filename);
    SetDescriptives;
    isRebuildList := true;
    result := true;
    isBusy := false;
end; // LoadFromFile()


end.
