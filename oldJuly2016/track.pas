unit track;
{$mode objfpc}{$H+}
{$Include opts.inc} //compile for either dglOpenGL or glext
interface

uses
  {$IFDEF DGL} dglOpenGL, {$ELSE} gl, {$IFDEF COREGL}glext,{$ENDIF} {$ENDIF}
  Classes, SysUtils,  dialogs, matmath, math, define_types, track_simplify, zstream;

Type

TTrack = class
  //utime: QWord;
  scale, minFiberLength, ditherColorFrac, maxObservedFiberLength  : single;
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
    function LoadBfloat(const FileName: string): boolean;
    function LoadPdb(const FileName: string): boolean;
    function LoadTck(const FileName: string): boolean;
    function LoadTrk(const FileName: string): boolean;
    function LoadVtkASCII(const FileName: string): boolean;
    function LoadVtk(const FileName: string): boolean;
    //procedure Simplify(Skip: integer);

    procedure SetDescriptives;
    procedure BuildListStrip;
    procedure BuildListTubes ;
  public
    constructor Create;
    function SimplifyMM(Tol: float): boolean;
    function LoadFromFile(const FileName: string): boolean;
    procedure SaveBfloat(const FileName: string);
    procedure SaveVtk(const FileName: string);
    procedure Close;
    procedure DrawGL;
    procedure CenterX;
  end;

implementation

uses mainunit, shaderu, {$IFDEF COREGL} gl_core_3d {$ELSE} gl_legacy_3d {$ENDIF};

procedure TTrack.CenterX;
var
  offset: single;
   i, m, mi: integer;
   mn, mx, pt : TPoint3f;
begin
  maxObservedFiberLength := 0;
  if (n_count < 1) or (length(tracks) < 4) then exit;
  mn := ptf(Infinity,Infinity,Infinity);
  mx := ptf(-Infinity, -Infinity, -Infinity);
  i := 0;
  while i < length(tracks) do begin
        m :=   asInt( tracks[i]); inc(i);
        for mi := 1 to m do begin
            pt.X := tracks[i];
            i := i + 3;
            minmax(pt, mn,mx);
        end;
  end;
  if (mn.X >= mx.X) then exit;
  offset := (mn.X + mx.X)/ 2;
  showmessage(floattostr(mn.X)+'..'+floattostr(mx.X)+' : '+floattostr(offset));
  i := 0;
  while i < length(tracks) do begin
        m :=   asInt( tracks[i]); inc(i);
        for mi := 1 to m do begin
            tracks[i] := tracks[i] - offset;
            i := i + 3;
        end;
  end;
end; // SetDescriptives()

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
  Indices,vType: TInts;
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
  n_indices := 0;
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
          n_vertices := n_vertices + 2*m+8; //Duplicate vertices + 8 for the Begin and End Imposter
          n_indices := n_indices + 2*m+11; //Same as Above + 3 Primitive Restart
          {$IFDEF COREGL}
          n_faces := n_faces + 2*m + 3;//adjacent start + adjacent end + primitive restart!
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
  setlength(vType, n_vertices);
  setlength(Verts, n_vertices);
  setlength(vNorms, n_vertices);
  setlength(Indices, n_indices);
  //second pass: fill arrays
  i := 0;
  n_vertices := 0;
  n_faces := 0;
  n_indices := 0;
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

          for mi := 0 to (m-2) do begin
              norms[mi] := normalDirection(pts[mi], pts[mi+1]); //line does not have a surface normal, but a direction
          end;

          //Add the first end imposter
          for mi:=0 to 3 do begin
              Verts[n_vertices] := pts[0];
              vNorms[n_vertices] := norms[0];
              if mi>1 then begin vType[n_vertices] := 1;
                      end else vType[n_vertices] := 2;
              vRGBA[n_vertices] := normRGBA;
              Indices[n_indices] := n_vertices;inc(n_indices);
              inc(n_vertices);
          end;

          Indices[n_indices] := kPrimitiveRestart;inc(n_indices);

          //Duplicate every vertice
          for mi := 0 to (m-2) do begin
              Verts[n_vertices] := pts[mi];
              vNorms[n_vertices] := norms[mi];
              vType[n_vertices] := 0;
              vRGBA[n_vertices] := normRGBA;
              Indices[n_indices] := n_vertices;inc(n_indices);
              inc(n_vertices);
              Verts[n_vertices] := pts[mi];
              vNorms[n_vertices] := norms[mi];
              vType[n_vertices] := 0;
              vRGBA[n_vertices] := normRGBA;
              Indices[n_indices] := n_vertices;inc(n_indices);
              inc(n_vertices);
          end;

          //The normal for the last vestice is different
          Verts[n_vertices] := pts[m-1];
          vNorms[n_vertices] := norms[m-2];
          vType[n_vertices] := 0;
          vRGBA[n_vertices] := normRGBA;
          Indices[n_indices] := n_vertices;inc(n_indices);
          inc(n_vertices);
          Verts[n_vertices] := pts[m-1];
          vNorms[n_vertices] := norms[m-2];
          vType[n_vertices] := 0;
          vRGBA[n_vertices] := normRGBA;
          Indices[n_indices] := n_vertices;inc(n_indices);
          inc(n_vertices);

          Indices[n_indices] := kPrimitiveRestart;inc(n_indices);

	  //Add the Last end imposter
          for mi:=0 to 3 do begin
              Verts[n_vertices] := pts[m-1];
              vNorms[n_vertices] := norms[m-2];
              if mi>1 then begin vType[n_vertices] := 1;
                      end else vType[n_vertices] := 2;
              vRGBA[n_vertices] := normRGBA;
              Indices[n_indices] := n_vertices;inc(n_indices);
              inc(n_vertices);
          end;

          Indices[n_indices] := kPrimitiveRestart;inc(n_indices);

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
  BuildDisplayListStrip(Indices, Verts, vNorms, vRGBA, vType, LineWidth, vao, vbo);
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
  //f: TextFile;
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
  B, startPt, endPt: TPoint3f;
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
       B := ptf(0,0,0); //need to generate random binormal
       for mi := 0 to (m-2) do begin
           makeCylinderEnd(radius, pts[mprev], pts[mi], pts[mi+1], cylVert, B, TrackTubeSlices);
           for j := 0 to (numCylFace - 1) do //add this cylinder
               faces[j+numFaces+n_faces] := vectorAdd(cylFace[j], numVert+n_vertices);
           numFaces := numFaces + numCylFace;
           for j := 0 to (numCylVert - 1) do //add bottom of this cylinder
               vertices[j+numVert+n_vertices] := cylVert[j];
           numVert := numVert + numCylVert;
           mprev := mi;
       end;
       //faces[numFaces+n_faces-1] := vectorAdd(cylFace[j], 0);
       makeCylinderEnd(radius, pts[m-2], pts[m-1], pts[m-1], cylVert, B, TrackTubeSlices);
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
  (*AssignFile(f, '~/Test.txt');
  ReWrite(f);
  writeln(f, floattostr(radius),' -> ',inttostr(TrackTubeSlices));
  i := 0;
  writeln(f, floattostr(pts[i].X),kTab, floattostr(pts[i].Y),kTab, floattostr(pts[i].Z) );
    i :=1;
  writeln(f, floattostr(pts[i].X),kTab, floattostr(pts[i].Y),kTab, floattostr(pts[i].Z) );
  writeln(f,'xxx');

  for i := 0 to (length(vertices)-1) do
      writeln(f, floattostr(vertices[i].X),kTab, floattostr(vertices[i].Y),kTab, floattostr(vertices[i].Z) );
  CloseFile(f); *)
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
    randseed := 123; //so that dither colors do not flicker when width is adjusted
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
     glDrawElements(GL_TRIANGLE_STRIP, n_indices, GL_UNSIGNED_INT, nil)  ;
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
     maxObservedFiberLength := 0;
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
     showmessage('Only able to read ASCII or binary VTK files: '+str);
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
   nVtx, inPos, outPos, vtx, i, v, num_v, n_items, fsz: integer;
   vert: array of TPoint3f;
   items: array of LongInt;
begin
  result := false;
  AssignFile(f, FileName);
  Reset(f,1);
  fsz := filesize(f);
  if fsz < 64 then goto 666;
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
  if fsz < (filepos(f)+ (3 * 4 * num_v)) then begin
     showmessage('File too small to contain this many vertices!');
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
  if fsz < (filepos(f)+ (n_items * 4)) then begin
     showmessage('File too small to contain this many lines!');
     goto 666;
  end;
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
          //if v < 4 then
          //   showmessage(format('%g %g %g',[vert[vtx].X, vert[vtx].Y, vert[vtx].Z]));
      end; //for each vertex in fiber
  end; //for each fiber
 result := true;
 exit;
666:
   closefile(f);
end;

procedure TTrack.SaveBfloat(const FileName: string);
//{$DEFINE GZ_BFLOAT}
var
  flt: array of single;
  i, o, m, mi, nflt: integer;
  {$IFDEF GZ_BFLOAT}
  mStream : TMemoryStream;
  zStream: TGZFileStream;
  {$ELSE}
  f: file;
  {$ENDIF}
  FileNameBf: string;
begin
  //   flt: array of single;
  // sz, nflt, i, outPos, nVtx, v : integer;
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
  {$IFDEF GZ_BFLOAT}
  FileNameBf := FileName + '.Bfloat.gz';
  mStream := TMemoryStream.Create;
  mStream.Write(flt[0], nflt * sizeof(single));
  mStream.Position := 0;
  zStream := TGZFileStream.Create(FileNameBf, gzopenwrite);
  zStream.CopyFrom(mStream, mStream.Size);
  zStream.Free;
  mStream.Free;
  {$ELSE}
  FileNameBf := changeFileExt(FileName, '.Bfloat');
  AssignFile(f, FileNameBf);
  ReWrite(f, sizeof(single));
  BlockWrite(f, flt[0], nflt);
  CloseFile(f);
  {$ENDIF}
end;

function TTrack.LoadBfloat(const FileName: string): boolean;
// http://www.nitrc.org/pipermail/camino-users/2014-April/000389.html
// http://camino.cs.ucl.ac.uk/index.php?n=Main.Fileformats
label
   666;
const
   kChunkSize = 16384;
var
  bytes : array of byte;
  mStream : TMemoryStream;
  zStream: TGZFileStream;
  ext: string;
   f: file;
   flt: array of single;
   sz, nflt, i, outPos, nVtx, v : integer;
begin
     result := false;
     ext := ExtractFileExtGzUpper(Filename);
     if (ext = '.BFLOAT.GZ') then begin
       mStream := TMemoryStream.Create;
       zStream := TGZFileStream.create(FileName, gzopenread);
       setlength(bytes, kChunkSize);
       repeat
              i := zStream.read(bytes[0],kChunkSize);
              mStream.Write(bytes[0],i) ;
       until i < kChunkSize;
       zStream.Free;

       sz :=  mStream.Size;
       if sz < 5 * sizeof(single) then begin//smallest file 3*N+2 floats
            ShowMessage(format('File too small to be Camino format: %s', [FileName]));
            mStream.Free;
            exit;
       end;
       nflt := sz div sizeof(single);
       setlength(flt, nflt);
       mStream.Position := 0;
       mStream.Read(flt[0], nflt * SizeOf(single));
       mStream.Free;

     end else begin
         AssignFile(f, FileName);
         FileMode := fmOpenRead;
         Reset(f,1);
         sz := FileSize(f);
         if sz < 5 * sizeof(single) then begin//smallest file 3*N+2 floats
            ShowMessage(format('File too small to be Camino format: %s', [FileName]));
            CloseFile(f);
            exit;
         end;
         nflt := sz div sizeof(single);
         setlength(flt, nflt);
         blockread(f, flt[0], nflt * SizeOf(single) );
         CloseFile(f);
     end;
     {$IFDEF ENDIAN_LITTLE} //Camino data ALWAYS bigendian!
     for i := 0 to (nflt -1) do
         SwapSingle(flt[i]);
     {$ENDIF}
     setlength(tracks, nflt);
     n_count := 0;
     outPos := 0;
     i := 0; //in position
     while i < nflt do begin
         nVtx := trunc(flt[i]); //"N"
         if (nVtx < 1) or ((flt[i] - nVtx) > 0.01 ) then
           goto 666;
         i := i + 2; //skip "N" and "seedIndex"
         if (i + (nVtx * 3)) > nflt then begin
           //showmessage(format('%d %d', [nflt, i + (nVtx * 3)]));
           goto 666;

         end;
         n_count := n_count + 1;
         tracks[outPos] := asSingle(nVtx); inc(outPos);
         for v := 1 to nVtx do begin
             tracks[outPos] := flt[i]; inc(outPos); inc(i);
             tracks[outPos] := flt[i]; inc(outPos); inc(i);
             tracks[outPos] := flt[i]; inc(outPos); inc(i);
         end; //for each vertex in fiber
         //n_vertex := n_vertex + numberOfpoints[i];
     end;
     setlength(tracks, outPos);
     Result := true;
     exit;
 666:
     showmessage('File is not in the Camino Streamline fiber tract format '+filename);
     n_count := 0;
     setlength(tracks,0);
end;

function TTrack.LoadPdb(const FileName: string): boolean;
//Version 2 is described here, version 3 reverse engineered.
// http://graphics.stanford.edu/projects/dti/software/pdb_format.html
type
 TPoint3d = packed record
    X,Y,Z: double;
  end;
label
   666;
var
   f: file;
   mat64: array [1..16] of double;//float32
   mat: TMat44;
   n_vertex, hdrsz, tmp, psz, vers, nVtx : LongWord; //uint32
   numberOfpoints: array of LongWord;
   vtx: TPoint3f;
   vert: array of TPoint3d;
   v, i,sz, outPos, inPos, pos: integer;
begin
     result := false;
     AssignFile(f, FileName);
     FileMode := fmOpenRead;
     Reset(f,1);
     sz := FileSize(f);
     if sz < 134 then begin//smallest file is 84 byte header + 50 byte triangle
        ShowMessage(format('File too small to be PDB format: %s', [FileName]));
        CloseFile(f);
        exit;
     end;
     blockread(f, hdrsz, SizeOf(LongWord) );
     if (hdrsz+16) > sz then begin
        ShowMessage(format('Header larger than file: %s', [FileName]));
        CloseFile(f);
        exit;
     end;
     blockread(f, mat64, SizeOf(double) * 16 );
     mat[1,1] := mat64[1]; mat[1,2] := mat64[2]; mat[1,3] := mat64[3]; mat[1,4] := mat64[4];
     mat[2,1] := mat64[5]; mat[2,2] := mat64[6]; mat[2,3] := mat64[7]; mat[2,4] := mat64[8];
     mat[3,1] := mat64[ 9]; mat[3,2] := mat64[10]; mat[3,3] := mat64[11]; mat[3,4] := mat64[12];
     mat[4,1] := mat64[13]; mat[4,2] := mat64[14]; mat[4,3] := mat64[15]; mat[4,4] := mat64[16];
     Seek(f, hdrsz- SizeOf(LongWord));
     blockread(f, vers, SizeOf(LongWord) );
     if (vers <> 3) and (vers <> 2) then begin
       ShowMessage(format('Expected PDB version 2 or 3, not %d', [vers]));
       CloseFile(f);
       exit;
     end;
     //Seek(f, hdrsz);
     blockread(f, tmp, SizeOf(LongWord) );
     n_count := tmp;
     //showmessage(inttostr(n_count));
     if vers = 2 then begin
        outPos := 0;
        for i := 0 to  (n_count -1) do begin
            blockread(f, psz, SizeOf(LongWord) );
            pos := filepos(f);
            blockread(f, nVtx, SizeOf(LongWord) );
            //showmessage(inttostr(nVtx)+'xx'+inttostr(psz));
            seek(f, pos + psz);
            setlength(vert, nVtx);
            blockread(f, vert[0], nVtx * SizeOf(TPoint3d) );
            //showmessage(format('%g %g %g', [vert[0].X, vert[0].Y, vert[0].Z]));
            setlength(tracks, outPos+ 1 + 3 * nVtx);
            tracks[outPos] := asSingle(nVtx); inc(outPos);
            inPos := 0;
            for v := 1 to nVtx do begin //for each vertex in this fiber
                vtx.X :=  vert[inPos].X;
                vtx.Y :=  vert[inPos].Y;
                vtx.Z :=  vert[inPos].Z;
                //if (v = 1) and (i = 0) then showmessage(format('%g %g %g', [vtx.X, vtx.Y, vtx.Z]));
                vectorTransform(vtx, mat);
                //if (v = 1) and (i = 0) then showmessage(format('%g %g %g', [vtx.X, vtx.Y, vtx.Z]));
                tracks[outPos] := vtx.X; inc(outPos);
                tracks[outPos] := vtx.Y; inc(outPos);
                tracks[outPos] := vtx.Z; inc(outPos);
                inc(inPos);
            end; //for each vertex in fiber
        end; //for each fiber
        CloseFile(f);
        result := true;
        exit;
     end;
     setlength(numberOfpoints, n_count);
     blockread(f, numberOfpoints[0], n_count * SizeOf(LongWord) );
     n_vertex := 0;
     for i := 0 to (n_count -1) do
         n_vertex := n_vertex + numberOfpoints[i];
     setlength(vert, n_vertex);
     blockread(f, vert[0], n_vertex * SizeOf(TPoint3d) );
     setlength(tracks, (n_vertex * 3) + n_count);
     outPos := 0;
     inPos := 0;
     for i := 0 to (n_count -1) do begin
         nVtx := numberOfpoints[i];
         tracks[outPos] := asSingle(nVtx); inc(outPos);
         for v := 1 to nVtx do begin
             vtx.X :=  vert[inPos].X;
             vtx.Y :=  vert[inPos].Y;
             vtx.Z :=  vert[inPos].Z;
             //if (v = 1) and (i = 0) then showmessage(format('%g %g %g', [vtx.X, vtx.Y, vtx.Z]));
             vectorTransform(vtx, mat);
             //if (v = 1) and (i = 0) then showmessage(format('%g %g %g', [vtx.X, vtx.Y, vtx.Z]));
             tracks[outPos] := vtx.X; inc(outPos);
             tracks[outPos] := vtx.Y; inc(outPos);
             tracks[outPos] := vtx.Z; inc(outPos);
             inc(inPos);
         end; //for each vertex in fiber
         //n_vertex := n_vertex + numberOfpoints[i];
     end;
  CloseFile(f);
  Result := true;
  {$IFNDEF ENDIAN_LITTLE}
  byteswapping required, PDB always little endian!
  {$ENDIF}
  exit;
 666:


end;

function TTrack.LoadTck(const FileName: string): boolean;
//Read BINARY TCK fibers
// https://github.com/MRtrix3/mrtrix3/blob/master/matlab/write_mrtrix_tracks.m
const
  kInf : single = 1.0 / 0.0;
label
   555,666;
var
   f: TFByte;
   strlst: TStringList;
   str, strEnd: string;
   nVtx, inPos, outPos, i, v, n_items, fsz, offset: integer;
   vert: array of TPoint3f;
   lOK: boolean;
begin
  result := false;
  AssignFile(f, FileName);
  Reset(f,1);
  fsz := filesize(f);
  if fsz < 64 then goto 666;
  ReadLnBin(f, str); //signature: 'mrtrix tracks'
  if pos('MRTRIX', UpperCase(str)) <> 1 then begin
    goto 666;
  end;
  strlst:=TStringList.Create;
  lOK := true;
  n_count := 0;
  offset := 0;
  while (not EOF(f)) and (pos('END', UpperCase(str)) <> 1) do begin
        strlst.DelimitedText := str;
        strEnd := strlst[strlst.Count-1];
        if (pos('DATATYPE:', str) = 1) and (strEnd <> 'FLOAT32LE') then begin  // "datatype: Float32LE"
           showmessage('Unknown datatype, expected "Float32LE, not : '+ str);
           lOK := false;
        end;
        if (pos('NUM_TRACKS:', str) = 1) or (pos('COUNT:', UpperCase(str)) = 1) then   //"count: 6"
           //either 'num_tracks' or 'count', see https://github.com/vistalab/vistasoft/blob/master/fileFilters/mrtrix/dtiImportFibersMrtrix.m
          n_count := StrToIntDef(strEnd,0);
        if (pos('FILE:', str) = 1) then   //"count: 6"
           offset := StrToIntDef(strEnd,0);

        ReadLnBin(f, str); //next line
        str := UpperCase(str);
  end;
  strlst.free;
  n_items := (fsz - offset) div 12; //each item is 12 bytes long (3*32-bit floats)
  if (offset < 20) or (n_count < 1) or (not lOK) or (n_items < 1) then begin  // '# vtk DataFile'
    showmessage('Unable to read TCK format file : '+ FileName);
    closefile(f);
    exit;
  end;
  setlength(vert, n_items); //vert = zeros(num_f, 9);
  setlength(tracks, n_items * 3); //each triplet may require up to 3 floats to store
  seek(f,offset);
  blockread(f, vert[0], n_items * 12); //each item is 12 bytes long (3*32-bit floats)
  inPos := 0;
  outPos := 0;
  for i := 0 to (n_items -1) do begin
      if specialsingle(vert[i].X) then begin
        if ( vert[i].X = kInf) then goto 555;
        nVtx := i - inPos;
        //showmessage(inttostr(nVtx)+'  '+inttostr(i)+'  '+inttostr(n_items));
        if nVtx > 0 then begin
          tracks[outPos] := asSingle(nVtx); inc(outPos);
          for v := 1 to nVtx do begin
                    tracks[outPos] := vert[inPos].X; inc(outPos);
                    tracks[outPos] := vert[inPos].Y; inc(outPos);
                    tracks[outPos] := vert[inPos].Z; inc(outPos);
                    inc(inPos);
          end; //for each vertex in fiber
          inc(inPos);
        end; //at least one value
      end;

  end;
555:
  setlength(tracks, outPos);

  //showmessage(inttostr(outPos)+'  '+inttostr(n_items));

  {$IFNDEF ENDIAN_LITTLE}
   byteswapping required!
  {$ENDIF}
  result := true;
666:
   closefile(f);
end; //LoadTck()


function TTrack.LoadTrk(const FileName: string): boolean;
// http://www.trackvis.org/docs/?subsect=fileformat

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
  len: single;
   i, m, mi: integer;
   mn, mx, pt, pt1 : TPoint3f;
begin
  maxObservedFiberLength := 0;
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
            if mi = 1 then
               pt1 := pt;
            if mi = m then begin
              vector2RGB(pt1, pt, len);
              if len > maxObservedFiberLength then
                 maxObservedFiberLength := len;
            end;
        end;
  end;
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

(*function jointAngle(prevPos, pos, nextPos: TPoint3f): single;
//Compute dot product of line tangents
var
   v1, v2, v3 : TPoint3f; //Tangent
begin
  //compute Normal and Binormal for each tangent
  v1 := pos;
  vectorNormalize(v1);
  v2 := nextPos;
  vectorNormalize(v2);
  vectorSubtract(v2, v1); //next line tangent
  v3 := prevPos;
  vectorNormalize(v3);
  vectorSubtract(v1, v3); //prior line tangent

  result := vectorDot(v1,v2);
end; //jointAngle()   *)


(*procedure TTrack.Simplify(Skip: integer);
var
   pos: TPoint3f;
   i, m, mi, xi, xm, xIndexPos: integer;
   xTracks: array of single;
procedure AddJoint(pt: TPoint3f);
begin
     xTracks[xi] := pt.X; inc(xi);
     xTracks[xi] := pt.Y; inc(xi);
     xTracks[xi] := pt.Z; inc(xi);
     inc(xm);
end;
begin
  //if (threshold < 0.5) then exit;
  if Skip < 2 then exit;
  if (n_count < 1) or (length(tracks) < 4) then exit;
  setlength(xTracks, length(tracks));
  i := 0;
  xi := 0;
  showmessage(format('--> %g %g %g',[tracks[1], tracks[2], tracks[3]]) );
  while i < length(tracks) do begin
        m :=   asInt( tracks[i]);
        xIndexPos := xi;
        xm := 0;
        inc(i);
        inc(xi);
        for mi := 1 to m do begin
            pos.X := tracks[i]; inc(i);
            pos.Y := tracks[i]; inc(i);
            pos.Z := tracks[i]; inc(i);
            if (mi = 1) or (mi = m) then
               AddJoint(pos)
            else if (mi mod Skip) = 0 then
               AddJoint(pos);
        end;
        xTracks[xIndexPos] := asSingle(xm);
  end;
  showmessage(inttostr(i)+'->'+inttostr(xi));
  //showmessage(format('%g %g',[tracks[0], xTracks[0]]));
  setlength(xTracks, xi);
  tracks := Copy(xTracks, Low(xTracks), Length(xTracks));
end;*)

//function PolySimplifyFloat3D(Tol: TFloat; const Orig: array of TPoint3f; var Simple: array of TPoint3f): integer;
function TTrack.SimplifyMM(Tol: float): boolean;
var
   pos: TPoint3f;
   i, m, mi, xi, xm: integer;
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
  if (n_count < 1) or (length(tracks) < 4) then exit;
  setlength(xTracks, length(tracks));
  i := 0;
  xi := 0;
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
        xm := PolySimplifyFloat3D(Tol, Orig, Simple);
        xTracks[xi] := asSingle(xm);
        inc(xi);
        for mi := 0 to (xm-1) do
            AddJoint(Simple[mi]);
  end;
  result := xi < i;
  if result then
     showmessage('Reduced to '+inttostr(round(100*xi/i))+'% of original size')
  else
      showmessage('Unable to further simplify this track with this threshold');
  //showmessage(format('%g %g',[tracks[0], xTracks[0]]));
  setlength(xTracks, xi);
  tracks := Copy(xTracks, Low(xTracks), Length(xTracks));
end;

function TTrack.LoadFromFile(const FileName: string): boolean;
var
   ext: string;
begin
  isBusy := true;
  Self.Close;
    result := false;
    maxObservedFiberLength := 0; //in case of error
    if not FileExists(FileName) then exit;
    //ext := UpperCase(ExtractFileExt(Filename));
    ext := ExtractFileExtGzUpper(Filename);
    if (ext = '.BFLOAT') or (ext = '.BFLOAT.GZ') then begin
         if not LoadBfloat(FileName) then exit;
    end else if (ext = '.PDB') then begin
         if not LoadPdb(FileName) then exit;
    end else if (ext = '.TCK') then begin
         if not LoadTck(FileName) then exit;
    end else if (ext = '.TRK') then begin
         if not LoadTrk(FileName) then exit;
    end else  //(ext = '.FIB') 0r (ext = '.FIB') then begin
        LoadVtk(Filename);
    //Simplify(5);
    //SimplifyMM(3);
    SetDescriptives;
    isRebuildList := true;
    result := true;
    isBusy := false;
end; // LoadFromFile()


end.
