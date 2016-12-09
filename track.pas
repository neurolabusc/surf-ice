unit track;
{$mode objfpc}{$H+}
{$Include opts.inc} //compile for either dglOpenGL or glext
interface

uses
  {$ifndef isTerminalApp}
    ClipBrd,dialogs,colorTable,
  {$endif}
  {$IFDEF DGL} dglOpenGL, {$ELSE} gl, {$IFDEF COREGL}glext,{$ENDIF} {$ENDIF}
  Classes, SysUtils, math, define_types, matmath, track_simplify, zstream;

Type

// const
//   kMaxScalars = 3; //maximum number of properties/scalars from TRK file, e.g. FA, pval, pval_corr
TScalar = record
  mn, mx: single; //full range of scalar
  mnView, mxView: single; //selected window for displaying scalar, e.g. if 0..1 with a grayscale color table than values <0 will be black and >1 will be white
  scalar: array of float;
  name: string;
end;
TTrack = class
  //utime: QWord;
  scale, minFiberLength, ditherColorFrac, maxObservedFiberLength  : single;
  origin, mxV, mnV : TPoint3f;
  isBusy, isRebuildList, isTubes, isWorldSpaceMM: boolean;
  TrackTubeSlices, //e.g. if 5 then cross section of fiber is pentagon
  n_count, n_faces, n_vertices, n_indices, minFiberLinks, LineWidth: integer;
  {$IFDEF COREGL}
  vao, vbo : GLuint;
  {$ELSE}
  displayList : GLuint;
  {$ENDIF}
  tracks: array of single;
  scalarLUT: TLUT;
  scalarSelected: integer;
  scalars: array of TScalar;
  private
    function LoadBfloat(const FileName: string): boolean;
    function LoadDat(const FileName: string): boolean;
    function LoadPdb(const FileName: string): boolean;
    function LoadTck(const FileName: string): boolean;
    function LoadTrk(const FileName: string): boolean;
    function LoadVtkASCII(const FileName: string): boolean;
    function LoadVtk(const FileName: string): boolean;
    procedure SetDescriptives;
    procedure SetScalarDescriptives;
    {$ifndef isTerminalApp}
    procedure BuildListStrip;
    procedure BuildListTubes ;
    {$endif}
  public
    constructor Create;
    function SimplifyMM(Tol, minLength: float): boolean;
    function LoadFromFile(const FileName: string): boolean;
    procedure SaveBfloat(const FileName: string);
    procedure SaveVtk(const FileName: string);
    procedure SaveTrk(const FileName: string);
    procedure Save(FileName: string);
    function Smooth: boolean;
    function SimplifyRemoveRedundant(Tol: float): boolean;
    procedure Close;
    {$ifndef isTerminalApp}
    procedure DrawGL;
    {$endif}
    procedure CenterX;
    destructor Destroy; override;
  end;

implementation

{$ifndef isTerminalApp}
uses mainunit, shaderu, {$IFDEF COREGL} gl_core_3d {$ELSE} gl_legacy_3d {$ENDIF};
{$endif}

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
  setDescriptives;
end; // CenterX()

procedure TTrack.Close;
var i: integer;
begin
  i := 1;
  {$ifndef isTerminalApp}
  scalarLUT := UpdateTransferFunction(i); //red-yellow
  {$endif}
  scalarSelected := -1; //none: color based on direction
     n_count := 0;
     n_vertices := 0;
     n_faces := 0;
     n_indices := 0;
     setlength(tracks, 0);
     if scalars <> nil then begin  //close all open properties/scalars
        for i := 0 to (length(scalars)-1) do begin
          scalars[i].scalar := nil;
          scalars[i].name := '';
        end;
        scalars := nil;
     end;
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

{$ifndef isTerminalApp}

function RGBA2pt3f(RGBA: TRGBA): TPoint3f;
begin
     result.X := RGBA.R/255;
     result.Y := RGBA.G/255;
     result.Z := RGBA.B/255;
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
  maxLinks, m, mi, i,j, ntracks, nfiber, nvertex: integer;
  trackLinks : array of integer;
  startPt, endPt:TPoint3f;
  isScalarPerFiberColor : boolean = false;
  isScalarPerVertexColor: boolean = false;
begin
  randomize;
  //GLForm1.Caption := inttostr(random(666));
 if (ScalarSelected >= 0) and (length(Scalars) > ScalarSelected) then begin
   // GLForm1.Caption := format('%d %d %d %d %d',[ScalarSelected, length(Scalars), length(Scalars[ScalarSelected].scalar), n_count, random(666)]);
   if  length(Scalars[ScalarSelected].scalar) = n_count then
       isScalarPerFiberColor := true //one color per fiber
   else
       isScalarPerVertexColor := true;
 end;
 //if isScalarPerVertexColor then
 //   GLForm1.Caption := format('%d %d %d %d %d',[ScalarSelected, length(Scalars), length(Scalars[ScalarSelected].scalar), n_count, random(666)]);
  maxLinks := 0;
  n_faces := 0;
  n_vertices := 0;
  n_indices := 0;
  TrackTubeSlices := 5;
  if (length(tracks) < 4) then exit;
  //if minFiberLinks < 3 then minFiberLinks := 3; //minimum to compute normal;
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
  nfiber := 0;
  nvertex := 0;
  while i < ntracks do begin
        m :=   asInt( tracks[i]);
        if trackLinks[i] >= minFiberLinks then begin
          inc(i);
          for mi := 0 to (m-1) do begin
              pts[mi].X := tracks[i]; inc(i);
              pts[mi].Y := tracks[i]; inc(i);
              pts[mi].Z := tracks[i]; inc(i);
          end;
          if isScalarPerFiberColor then begin
            normRGBA := inten2rgb1(Scalars[ScalarSelected].scalar[nfiber], Scalars[ScalarSelected].mnView, Scalars[ScalarSelected].mxView,  scalarLUT );
            normRGB := RGBA2pt3f(normRGBA);
          end else
              normRGB := vector2RGB(pts[0], pts[m-1], len);
          normRGBA := mixRandRGBA(normRGB, ditherColorFrac);
          if isScalarPerVertexColor then
                 normRGBA :=  inten2rgb1(Scalars[ScalarSelected].scalar[nvertex], Scalars[ScalarSelected].mnView, Scalars[ScalarSelected].mxView,  scalarLUT );
          for mi := 0 to (m-2) do begin
              norms[mi] := normalDirection(pts[mi], pts[mi+1]); //line does not have a surface normal, but a direction
          end;
          //Add the first end imposter
          for mi:=0 to 3 do begin
              Verts[n_vertices] := pts[0];
              vNorms[n_vertices].x := -norms[0].x;
              vNorms[n_vertices].y := -norms[0].y;
              vNorms[n_vertices].z := -norms[0].z;
              if mi>1 then
                 vType[n_vertices] := 1
              else
                  vType[n_vertices] := 2;
              vRGBA[n_vertices] := normRGBA;
              Indices[n_indices] := n_vertices;inc(n_indices);
              inc(n_vertices);
          end;
          Indices[n_indices] := kPrimitiveRestart;inc(n_indices);
          //Duplicate every vertice
          for mi := 0 to (m-2) do begin
              if isScalarPerVertexColor then
                 normRGBA :=  inten2rgb1(Scalars[ScalarSelected].scalar[nvertex+mi], Scalars[ScalarSelected].mnView, Scalars[ScalarSelected].mxView,  scalarLUT );

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
          if isScalarPerVertexColor then
             normRGBA :=  inten2rgb1(Scalars[ScalarSelected].scalar[nvertex+m-1], Scalars[ScalarSelected].mnView, Scalars[ScalarSelected].mxView,  scalarLUT );

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
        nfiber := nfiber + 1;
        nvertex := nvertex + m;
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
  vRGBA: TVertexRGBA;
  normRGB: TPoint3f;
  normRGBA: TRGBA;
  pts: array of TPoint3f;
  len, radius: single;
  numCylVert, numVert,  mprev, m, mi, i, j, ntracks, nfiber, nvtx: integer;
  vertices: TVertices;
  faces, cylFace: TFaces;
  numFaces, numCylFace,  maxLinks: integer;
  cylVert: TVertices;
  B, startPt, endPt: TPoint3f;
  trackLinks : array of integer;
  perVertexScalars: array of single;
  isScalarPerFiberColor : boolean = false;
  isScalarPerVertexColor: boolean = false;
begin
  if (ScalarSelected >= 0) and (length(Scalars) > ScalarSelected) then begin
    if  length(Scalars[ScalarSelected].scalar) = n_count then
        isScalarPerFiberColor := true //one color per fiber
    else
        isScalarPerVertexColor := true;
  end;
  //tm := gettickcount64();
  n_indices := 0;
  n_faces := 0;
  n_vertices := 0;
  maxLinks := 0;
  if (length(tracks) < 4) then exit;
  //if minFiberLinks < 3 then minFiberLinks := 3; //minimum to compute normal;
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
       normRGB := vector2RGB(startPt, endPt, len);  //here only used for length
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
  //GLForm1.Caption := inttostr(n_faces);
  if (n_faces < 1) then exit;
  //allocate memory
  setlength(vertices, n_vertices); //each node as 1 disk (bottom of cylinder)
  setlength(vRGBA, n_vertices); //each node as 1 disk (bottom of cylinder)
  setlength(faces, n_faces);
  setlength(pts, maxLinks);
  setlength(perVertexScalars, maxLinks);
  //second pass - load geometry for links that are long enough
  n_vertices := 0;
  n_faces := 0;
  nfiber := 0;
  nvtx := 0;
  i := 0;
  while i < ntracks do begin
    m := asInt( tracks[i]);
    if (trackLinks[i] >= minFiberLinks) then begin
       inc(i);
       for mi := 0 to (m-1) do begin
              pts[mi].X := tracks[i]; inc(i);
              pts[mi].Y := tracks[i]; inc(i);
              pts[mi].Z := tracks[i]; inc(i);
              if isScalarPerVertexColor then
                 perVertexScalars[mi] := Scalars[ScalarSelected].scalar[nvtx+mi];
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
           if isScalarPerVertexColor then begin
              normRGBA := inten2rgb1(perVertexScalars[mi], Scalars[ScalarSelected].mnView, Scalars[ScalarSelected].mxView,  scalarLUT );
              for j := 0 to (numCylVert - 1) do
                  vRGBA[j+numVert+n_vertices] := normRGBA;
           end;
           numVert := numVert + numCylVert;
           mprev := mi;
       end;
       //faces[numFaces+n_faces-1] := vectorAdd(cylFace[j], 0);
       makeCylinderEnd(radius, pts[m-2], pts[m-1], pts[m-1], cylVert, B, TrackTubeSlices);
       for j := 0 to (numCylVert - 1) do //add top of last cylinder
           vertices[j+numVert+n_vertices] := cylVert[j];
       if isScalarPerVertexColor then begin
          normRGBA := inten2rgb1(perVertexScalars[m-1], Scalars[ScalarSelected].mnView, Scalars[ScalarSelected].mxView,  scalarLUT );
          for j := 0 to (numCylVert - 1) do
              vRGBA[j+numVert+n_vertices] := normRGBA;
       end;
       numVert := numVert + numCylVert;
       if not isScalarPerVertexColor then begin
         if isScalarPerFiberColor then begin
            normRGBA := inten2rgb1(Scalars[ScalarSelected].scalar[nfiber], Scalars[ScalarSelected].mnView, Scalars[ScalarSelected].mxView,  scalarLUT );
            normRGB := RGBA2pt3f(normRGBA);
         end;
         normRGBA := mixRandRGBA(normRGB, ditherColorFrac);
         for j := 0 to ((m * numCylVert) -1) do
             vRGBA[j+n_vertices] := normRGBA;
       end;
       n_vertices := n_vertices + (m * numCylVert); //each node as 1 disk (bottom of cylinder)
       n_faces := n_faces + (m - 1) * numCylFace; //-1: fencepost error
    end else //len >= minFiberLength
        i := i + 1 + (3 * m);
    nfiber := nfiber + 1;
    nvtx := nvtx + m;
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
{$endif}
constructor  TTrack.Create;
var
  i : integer;
begin
  i := 1;
  {$ifndef isTerminalApp}
  scalarLUT := UpdateTransferFunction(i); ; //red-yellow
  {$endif}
  scalarSelected := -1; //none: color based on direction
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
     minFiberLinks := 1;//minFiberLinks := 2;
     minFiberLength := 20;
     isRebuildList := true;
     isBusy := false;
     isTubes := true;
     isWorldSpaceMM := true; //assume image oriented in world space
     scalars := nil;
end; // Create()

function TTrack.LoadVtkASCII(const FileName: string): boolean;
//Read ASCII VTK mesh
// ftp://ftp.tuwien.ac.at/visual/vtk/www/FileFormats.pdf
// http://www.vtk.org/wp-content/uploads/2015/04/file-formats.pdf
// http://people.sc.fsu.edu/~jburkardt/data/vtk/vtk.html
label
   666;
var
   f: TextFile;
   strlst: TStringList;
   str: string;
   nVtx, inPos, outPos, vtx, i, v, num_v, n_items: integer;
   vert: array of TPoint3f;
   items: array of LongInt;
function ReadLnSkipBlank (var s: string): boolean;
begin
     result := false;
     s := '';
     while (s = '') and (not eof(f)) do begin
       ReadLn(f, s); //read any whitespace  http://people.sc.fsu.edu/~jburkardt/data/vtk/vtk.html
       s := trim(s);
     end;
     if s = '' then //eof(f)
        showmessage('LoadVtkASCII: Unexpected end of file')
     else
         result := true;
end; //ReadLnSkipBlank
function readItem (var s: string): boolean; //'LINES 1 348 347' will return successively 'LINES','1','348','347'
var
   ch: char;
   s2: string;
begin
  result := true;
  s := '';
  while (not eof(f)) do begin
       Read(f, ch);
       s2 := ch;
       s2 := trim(s2);
       if (s2 = '') and (s <> '') then
          exit; //white space after item
       s := s + s2;
  end;
  if s = '' then
     result := false; //eof
end; //readItem()
function readInt: integer; //'1 348 347\n2' will return successively'1','348','347','2'
var
   s: string;
begin
  readItem(s);
  result := strtointdef(s,0);
end; //readInt()

begin
  isRebuildList := true;
  result := false;
  AssignFile(f, FileName);
  Reset(f);
  ReadLn(f, str); //signature: '# vtk DataFile'
  if pos('VTK', UpperCase(str)) <> 3 then begin
    goto 666;
  end;
  ReadLn(f, str); //comment: 'Comment: created with MRIcroS'
  if not ReadLnSkipBlank(str) then goto 666; //kind: 'BINARY' or 'ASCII'
  if (pos('ASCII', UpperCase(str)) <> 1) then begin  // '# vtk DataFile'
     showmessage('Only able to read ASCII or binary VTK files: '+str);
     goto 666;
  end;
  if not ReadLnSkipBlank(str) then goto 666;// kind, e.g. "DATASET POLYDATA" or "DATASET STRUCTURED_ POINTS"
  if pos('POLYDATA', UpperCase(str)) = 0 then begin
    showmessage('Only able to read VTK images saved as POLYDATA, not '+ str);
    goto 666;
  end;
  if not ReadLnSkipBlank(str) then goto 666; //type: 'POINTS 347 float'
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
  if not readItem (str) then goto 666;  //Read one item at a time, as FSU data does not use EOLN: "LINES 1 348 347" not  "LINES 1 348\n347"
  if pos('POLYGONS', UpperCase(str)) > 0 then begin // number of vert, e.g. "POLYGONS 1380 5520"
    showmessage('This is a mesh file: rename with a ".vtk" extension and use File/Open to view: '+ str);
    goto 666;
  end;
  if pos('LINES', UpperCase(str)) <> 1 then begin
    showmessage('Expected header to report "LINES" not '+ str);
    goto 666;
  end;
  n_count := readInt;
  n_items := readInt;
  if (n_count < 1) or (n_items < 1) then goto 666;
  setlength(items, n_items);
  for i := 0 to (n_items - 1) do
      items[i] := readInt;
  vtx := n_items - n_count;
  setlength(tracks, vtx * 3 + n_count);
  inPos := 0;
  outPos := 0;
  for i := 1 to n_count do begin
      nVtx := items[inPos]; inc(inPos);
      //showmessage(format('%d %d', [items[inPos], items[inpos]]));
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
        //showmessage(format('%d -> %d',[i, m]));
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
    //showmessage(format('yyyy %d %d',[n_items, num_v]));
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
end; //SaveVtk()

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
  isRebuildList := true;
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
end; //LoadVtk()

procedure TTrack.SaveBfloat(const FileName: string);
var
  flt: array of single;
  i, o, m, mi, nflt: integer;
  mStream : TMemoryStream;
  zStream: TGZFileStream;
  FileNameBf: string;
begin
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
  mStream := TMemoryStream.Create;
  mStream.Write(flt[0], nflt * sizeof(single));
  mStream.Position := 0;
  if (ExtractFileExtGzUpper(Filename) = '.BFLOAT.GZ') then begin
    FileNameBf := ChangeFileExtX(FileName, '.Bfloat.gz');
    zStream := TGZFileStream.Create(FileNameBf, gzopenwrite);
    zStream.CopyFrom(mStream, mStream.Size);
    zStream.Free;
  end else begin
    FileNameBf := ChangeFileExtX(FileName, '.Bfloat');
    mStream.SaveToFile(FileNameBf);
  end;
  mStream.Free;
end; //SaveBfloat()

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
     isRebuildList := true;
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
end; //LoadBfloat()

function TTrack.LoadDat(const FileName: string): boolean;
// https://www.mristudio.org/wiki/faq
//  note this format does not store a matrix, so computing mm (worldspace) is impossible (no SForm, Origin offset, etc).
type
TDatHdr = packed record
  sFiberFileTag: array [1..8] of ansichar; // = "FiberDat"
  nFiberNr: int32; // number of fiber chain in this file
  nReserved: int32;
  fReserved: single;
  nImgWidth,nImgHeight, nImgSlices: int32; // image dimensions
  fPixelSizeWidth, fPixelSizeHeight, fSliceThickness: single; // voxel size
  cSliceOrientation: byte; // 0=coronal, 1=axial, 2=sagittal
  cSliceSequencing: byte; // 0=normal, 1=flipped
  Unused: array[1..82] of char;
end;
TFiberHdr = packed record
  nFiberLength: int32; // fiber length
  cReserved: char;
  rgbFiberColor: array [1..3] of char; // R-G-B, 3 bytes totally
  nSelectFiberStartPoint: int32;
  nSelectFiberEndPoint: int32;
end;
const
   kSig = 'FiberDat';
label
   666;
var
   f: File;
   hdr:  TDatHdr;
   fhdr: TFiberHdr;
   i, outPos, p, minFiberLinksMask, nPts: integer;
   pts: array of TPoint3f;
begin
  isRebuildList := true;
  result := false;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  blockread(f, hdr, sizeof(TDatHdr) );
  for i := 1 to length(kSig) do begin
      if (hdr.sFiberFileTag[i] <> kSig[i]) then begin
         showmessage('Not a valid MRI Studio fiber file (should begin with "'+kSig+'")');
         goto 666;
       end;
  end;
  if hdr.nFiberNr < 1 then goto 666;
  {$IFNDEF ENDIAN_LITTLE}
  byteswapping required, DAT always little endian!
  {$ENDIF}
  minFiberLinksMask := 2; //use large value, e.g. 32 to restrict loading
  //first pass: determine number of nodes  yyyy
  nPts := 0;
  n_count := 0;
  for i := 1 to hdr.nFiberNr do begin
      blockread(f, fhdr, sizeof(TFiberHdr) );
      if fhdr.nFiberLength < 1 then begin
         showmessage('MRI Studio file corrupted');
         goto 666;
      end;
      seek(f, filepos(f) + (fhdr.nFiberLength * sizeof(TPoint3f)));
      if fhdr.nFiberLength > minFiberLinksMask then begin
         n_count := n_count + 1;
         nPts := nPts + fhdr.nFiberLength;
      end;
  end;
  if n_count < 1 then begin
     showmessage(format('None of the %d fibers have at least %d nodes',[hdr.nFiberNr, minFiberLinksMask]));
     goto 666;
  end;
  if hdr.cSliceOrientation <> 1 then
    showmessage('Warning: slice orientation is not axial, scaling and orientation will be inaccurate');
  //second pass: load data
  //Showmessage(format('orient %d dim %d %d %d pixdim %g %g %g vertices %d fibers %d',[hdr.cSliceOrientation, hdr.nImgWidth, hdr.nImgHeight, hdr.nImgSlices, hdr.fPixelSizeWidth, hdr.fPixelSizeHeight, hdr.fSliceThickness, nPts, hdr.nFiberNr]));
  setlength(tracks, n_count  + (nPts * 3) );
  outPos := 0;
  seek(f, sizeof(TDatHdr));
  for i := 1 to hdr.nFiberNr do begin
      blockread(f, fhdr, sizeof(TFiberHdr) );
      setlength(pts, fhdr.nFiberLength);
      blockread(f, pts[0], int64(fhdr.nFiberLength) * sizeof(TPoint3f) );
      if fhdr.nFiberLength > minFiberLinksMask then begin
        tracks[outPos] := asSingle(fhdr.nFiberLength); inc(outPos);
        for p := 0 to (fhdr.nFiberLength - 1) do begin  //fPixelSizeWidth, fPixelSizeHeight, fSliceThickness
            tracks[outPos] := pts[p].X * hdr.fPixelSizeWidth; inc(outPos);
            tracks[outPos] := pts[p].Y * hdr.fPixelSizeHeight; inc(outPos);
            tracks[outPos] := pts[p].Z * hdr.fSliceThickness; inc(outPos);
        end;
      end;
  end;
  result := true;
  isWorldSpaceMM := false; //this format does not encode the origin offset or any other spatial transforms.
  666:
  CloseFile(f);
end; //LoadDat()

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
    isRebuildList := true;
    result := false;
     AssignFile(f, FileName);
     FileMode := fmOpenRead;
     Reset(f,1);
     sz := FileSize(f);
     if sz < 134 then begin//smallest file is 84 byte header + 50 byte triangle
        ShowMessage(format('File too small to be PDB format: %s', [FileName]));
        goto 666;
     end;
     blockread(f, hdrsz, SizeOf(LongWord) );
     if (hdrsz+16) > sz then begin
        ShowMessage(format('Header larger than file: %s', [FileName]));
        goto 666;
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
       goto 666;
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
        result := true;
        goto 666;
     end; //if version 2
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
  Result := true;
  {$IFNDEF ENDIAN_LITTLE}
  byteswapping required, PDB always little endian!
  {$ENDIF}
 666:
 CloseFile(f);
end; //LoadPdb()

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
  isRebuildList := true;
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
  {$IFNDEF ENDIAN_LITTLE}
   byteswapping required!
  {$ENDIF}
  result := true;
666:
   closefile(f);
end; //LoadTck()

procedure TTrack.SetScalarDescriptives;
//set range of scalars
var
  i, m: integer;
begin
     if length(Scalars) < 1 then exit;
     for i := low(Scalars) to high(Scalars) do begin
         if length(Scalars[i].scalar) < 1 then continue;
         Scalars[i].mn := Scalars[i].scalar[0];
         Scalars[i].mx := Scalars[i].scalar[0];
         for m := low(Scalars[i].scalar) to high(Scalars[i].scalar) do
             if Scalars[i].scalar[m] < Scalars[i].mn then
                Scalars[i].mn := Scalars[i].scalar[m];
         for m := low(Scalars[i].scalar) to high(Scalars[i].scalar) do
             if Scalars[i].scalar[m] > Scalars[i].mx then
                Scalars[i].mx := Scalars[i].scalar[m];
         Scalars[i].mnView := Scalars[i].mn; //show full range of values by default
         Scalars[i].mxView := Scalars[i].mx;
     end; //for each scalar
end; //SetScalarDescriptives()

type
 TTrackhdr = packed record //trackvis format
   id_string: array [1..6] of ansichar; //"TRACK*"
   dim: array [1..3] of smallInt;
   voxel_size: array [1..3] of single;
   origin: array [1..3] of single;  //not used!
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

function TTrack.LoadTrk(const FileName: string): boolean;
// http://www.trackvis.org/docs/?subsect=fileformat
// for test of vox2ras https://github.com/neurolabusc/spmScripts/blob/master/nii_makeDTI.m
label
   666;
const
   kChunkSize = 16384;
var
  bytes : array of byte;
  str: string;
   fsz, ntracks: int64;
   nVtx32: int32;
   hdr:  TTrackhdr;
   outPos, i,j,m,mi, v, nVtx: integer;
   scalarS32: single;
   pt: TPoint3f;
   vox2mmMat, zoomMat: TMat44;
   mStream: TMemoryStream;
   zStream : TGZFileStream;
begin
  isRebuildList := true;
  result := false;
  mStream := TMemoryStream.Create;
  if UpperCase(ExtractFileExt(FileName)) = '.GZ' then begin
    zStream := TGZFileStream.create(FileName, gzopenread);
    setlength(bytes, kChunkSize);
    repeat
         i := zStream.read(bytes[0],kChunkSize);
         mStream.Write(bytes[0],i) ;
    until i < kChunkSize;
    zStream.Free;
  end else
      mStream.LoadFromFile(FileName);
  fsz :=  mStream.Size;
  if fsz < (sizeof(TTrackhdr) + 5 * sizeof(single)) then begin//smallest file
     ShowMessage(format('File too small to be TRK format: %s', [FileName]));
     mStream.Free;
     exit;
  end;
  mStream.Position := 0;
  //mStream.Read(flt[0], nflt * SizeOf(single));
  mStream.Read(hdr, sizeof(TTrackhdr) );
  if (hdr.id_string[1] <> 'T') or (hdr.id_string[2] <> 'R')
  or (hdr.id_string[3] <> 'A') or (hdr.id_string[4] <> 'C')
  or (hdr.id_string[5] <> 'K') then begin
    showmessage('Not a valid TrakVis format file (missing "TRACK" signature)');
    goto 666;
  end;
  if (hdr.hdr_size = -402456576) then begin
     showmessage('Please convert the endian-ness of your track file.');
     goto 666;
  end;
  if (hdr.hdr_size <> sizeof(TTrackhdr)) or (hdr.n_count < 1) or (hdr.n_properties < 0) or (hdr.n_scalars < 0) then begin
     showmessage('Not a valid TrakVis format file (invalid properties)');
     goto 666;
  end;
  str := UpperCase(hdr.voxel_order[1]+hdr.voxel_order[2]+hdr.voxel_order[3]);
  (*if ((str[1] <> 'L') and (str[1] <> 'R')) or ((str[2] <> 'A') and (str[2] <> 'P')) or ((str[3] <> 'S') and (str[3] <> 'I')) then begin
     Showmessage('Unsupported TRK voxel order "'+str+'"');
     goto 666;
  end;*)
  mStream.Position := hdr.hdr_size;
  if (hdr.n_scalars <> 0) or (hdr.n_properties <> 0) then begin //slow to load if we have scalars...
    //unable to preallocate memory unless we know how many vertices!
    //first: determine number of vertices, allows us to compute memory requirements
    nVtx := (fsz - sizeof(TTrackhdr)) div sizeof(single);
    nVtx := nVtx - (hdr.n_count * (hdr.n_properties + 1)); //once per fiber: fiber length plus properties
    nVtx := nVtx div (3 + hdr.n_scalars); //for each vertex: X,Y,Z plus scalars
    ntracks := (nVtx * 3) + hdr.n_count; //we will store vertex X,Y,Z plus fiber length
    //allocate memory
    setlength(tracks, ntracks);
    setlength(scalars,hdr.n_scalars + hdr.n_properties);
    if (hdr.n_scalars > 0) then begin
      for i := 0 to (hdr.n_scalars-1) do begin
        SetString(str, PChar(@hdr.scalar_name[i*20]), 20);
        str := UpperCase(trim(str));
        scalars[i].name := str;
        setlength(scalars[i].scalar, nVtx); //one per vertex
      end;
    end;
    if (hdr.n_properties > 0) then begin
      for i := 0 to (hdr.n_properties-1) do begin
        SetString(str, PChar(@hdr.property_name[i*20]), 20);
        str := UpperCase(trim(str));
        scalars[hdr.n_scalars+i].name := str;
        setlength(scalars[hdr.n_scalars+i].scalar, hdr.n_count); //one per fiber
      end;
    end;
    //now read data
    outPos := 0;
    nVtx := 0;
    for i := 1 to hdr.n_count do begin
       //blockread(f, nVtx32, SizeOf(nVtx32)  );
       mStream.Read(nVtx32, SizeOf(nVtx32) );
       if nVtx32 < 2 then goto 666;
       tracks[outPos] := asSingle(nVtx32); inc(outPos);
       for v := 1 to nVtx32 do begin
           //blockread(f, pt, SizeOf(pt)  );
           mStream.Read(pt, SizeOf(pt) );
           tracks[outPos] := pt.X; inc(outPos);
           tracks[outPos] := pt.Y; inc(outPos);
           tracks[outPos] := pt.Z; inc(outPos);
           if hdr.n_scalars > 0  then
              for m := 0 to (hdr.n_scalars-1) do begin
                  //blockread(f, scalarS32, SizeOf(single)  );
                  mStream.Read(scalarS32, SizeOf(single) );
                  scalars[m].scalar[nVtx] := scalarS32;
              end;
           nVtx := nVtx + 1;
       end; //for each vertex in fiber
       if hdr.n_properties > 0  then //properties: one per fiber
          for m := 0 to (hdr.n_properties-1) do begin
              //blockread(f, scalarS32, SizeOf(single)  );
              mStream.Read(scalarS32, SizeOf(single) );
              scalars[hdr.n_scalars+m].scalar[i] := scalarS32;
          end;
    end;
    SetScalarDescriptives;
  end else begin //we can read much faster if there are no properties or scalars
      ntracks := (fsz - sizeof(TTrackhdr)) div sizeof(single);
      if ntracks < 4 then exit;
      setlength(tracks, ntracks);
      //blockread(f, tracks[0], ntracks * sizeof(single) );
      mStream.Read(tracks[0], ntracks * sizeof(single) );
  end;
  //GLForm1.Caption := format('props %d scalars %d count %d sz %d', [hdr.n_properties, hdr.n_scalars, hdr.n_count, ntracks]);
  //{$DEFINE TRK_VOXEL_SPACE}  //either voxel or world space http://nipy.org/nibabel/reference/nibabel.trackvis.html
  {$IFDEF TRK_VOXEL_SPACE}
  isWorldSpaceMM := false;
  i := 0;
  n_count := 0; // note hdr.n_count may not be set: determine it explicitly
  while i < ntracks do begin
    m :=   asInt( tracks[i]); inc(i);
    n_count := n_count + 1;
    for mi := 1 to m do begin
        i := i + 1;
    end;
  end;
  {$ELSE} //if voxel space, else rasmm world space
    if hdr.vox_to_ras[4,4] = 0 then begin
          //showmessage('This file does not record vox_to_ras: spatial orientation may be incorrect');
          isWorldSpaceMM := false;
          hdr.vox_to_ras[1,1] := 1; hdr.vox_to_ras[1,2] := 0; hdr.vox_to_ras[1,3] := 0; hdr.vox_to_ras[1,4] := 0;
          hdr.vox_to_ras[2,1] := 0; hdr.vox_to_ras[2,2] := 1; hdr.vox_to_ras[2,3] := 0; hdr.vox_to_ras[2,4] := 0;
          hdr.vox_to_ras[3,1] := 0; hdr.vox_to_ras[3,2] := 0; hdr.vox_to_ras[3,3] := 1; hdr.vox_to_ras[3,4] := 0;
    end;
    for i := 1 to 3 do
        if (hdr.voxel_size[i] = 0) then
           hdr.voxel_size[i] := 1.0; //avoid divide by zero
    zoomMat := matrixSet(1/hdr.voxel_size[1],0,0,-0.5, 0,1/hdr.voxel_size[2],0,-0.5,  0,0,1/hdr.voxel_size[3],-0.5);
    for i := 1 to 4 do
        for j := 1 to 4 do
            vox2mmMat[i,j] := hdr.vox_to_ras[i,j];
    vox2mmMat := matrixMult(vox2mmMat, zoomMat);
  //{$DEFINE TRK2CLIPBOARD} //copy header data to clipboard
  {$IFDEF TRK2CLIPBOARD}
  str := format('%s%svoxel_order = %s%svers = %d;%spixdim = [%g %g %g];%svox_to_ras= [%g %g %g %g; %g %g %g %g; %g %g %g %g; %g %g %g %g]%sraw2mm=[%g %g %g %g; %g %g %g %g; %g %g %g %g; %g %g %g %g]',[
         FileName, kEOLN,
         str, kEOLN,
         hdr.version, kEOLN,
         hdr.voxel_size[1], hdr.voxel_size[2], hdr.voxel_size[3], kEOLN,
        hdr.vox_to_ras[1,1], hdr.vox_to_ras[1,2], hdr.vox_to_ras[1,3], hdr.vox_to_ras[1,4],
        hdr.vox_to_ras[2,1], hdr.vox_to_ras[2,2], hdr.vox_to_ras[2,3], hdr.vox_to_ras[2,4],
        hdr.vox_to_ras[3,1], hdr.vox_to_ras[3,2], hdr.vox_to_ras[3,3], hdr.vox_to_ras[3,4],
        hdr.vox_to_ras[4,1], hdr.vox_to_ras[4,2], hdr.vox_to_ras[4,3], hdr.vox_to_ras[4,4], kEOLN,
        vox2mmMat[1,1], vox2mmMat[1,2], vox2mmMat[1,3], vox2mmMat[1,4],
        vox2mmMat[2,1], vox2mmMat[2,2], vox2mmMat[2,3], vox2mmMat[2,4],
        vox2mmMat[3,1], vox2mmMat[3,2], vox2mmMat[3,3], vox2mmMat[3,4],
        vox2mmMat[4,1], vox2mmMat[4,2], vox2mmMat[4,3], vox2mmMat[4,4]
        ]);
  clipBoard.AsText:= str;
  {$ENDIF}
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
              tracks[i-3] := Pt.X*vox2mmMat[1,1] + Pt.Y*vox2mmMat[1,2] + Pt.Z*vox2mmMat[1,3] + vox2mmMat[1,4];
              tracks[i-2] := Pt.X*vox2mmMat[2,1] + Pt.Y*vox2mmMat[2,2] + Pt.Z*vox2mmMat[2,3] + vox2mmMat[2,4];
              tracks[i-1] := Pt.X*vox2mmMat[3,1] + Pt.Y*vox2mmMat[3,2] + Pt.Z*vox2mmMat[3,3] + vox2mmMat[3,4];
              (*if (n_count = 1) and (mi = 1) then begin
                 str := format('%g %g %g -> %g %g %g', [Pt.X,Pt.Y,Pt.Z, tracks[i-3], tracks[i-2], tracks[i-1] ]);
                 clipBoard.AsText:= str;
              end;*)
          end;
    end;
  {$ENDIF}
  result := true;
666:
    mStream.Free;
end; // LoadTrk()

procedure TTrack.SetDescriptives;
var
  {$IFDEF MNVEC} numV: integer; sumV : TPoint3f; {$ENDIF}
  len: single;
   i, m, mi: integer;
   mn, mx, pt, pt1 : TPoint3f;
   //{$DEFINE TRACK2CLIPBOARD} //verbose(!!!) reporting to check positions
   {$IFDEF TRACK2CLIPBOARD}str: string;{$ENDIF}
begin
  maxObservedFiberLength := 0;
  if (n_count < 1) or (length(tracks) < 4) then exit;
  mn := ptf(Infinity,Infinity,Infinity);
  mx := ptf(-Infinity, -Infinity, -Infinity);
  {$IFDEF MNVEC}
  numV := 0;
  sumV := ptf(0,0,0);
  {$ENDIF}
  {$IFDEF TRACK2CLIPBOARD}str := '';{$ENDIF}
  i := 0;
  //showmessage(format('--> %g %g %g',[tracks[1], tracks[2], tracks[3]]) );
  while i < length(tracks) do begin
        m :=   asInt( tracks[i]); inc(i);
        for mi := 1 to m do begin
            pt.X := tracks[i]; inc(i);
            pt.Y := tracks[i]; inc(i);
            pt.Z := tracks[i]; inc(i);
            {$IFDEF MNVEC}
            vectorAdd (sumV, pt);
            numV := numV + 1;
            {$ENDIF}
            minmax(pt, mn,mx);
            if mi = 1 then
               pt1 := pt;
            if mi = m then begin
              vector2RGB(pt1, pt, len);
              if len > maxObservedFiberLength then
                 maxObservedFiberLength := len;
             {$IFDEF TRACK2CLIPBOARD}
             str := str + format('%g%s%g%s%g%s%g%s%g%s%g%s%g%s',[vectorLength(pt1,pt),kTab,  pt1.X,kTab,pt1.Y,kTab,pt1.Z, kTab,pt.X,kTab,pt.Y,kTab,pt.Z, kEOLN]);
             {$ENDIF}

            end;
        end;
  end;
  {$IFDEF TRACK2CLIPBOARD}
  clipBoard.AsText:= str;
  {$ENDIF}
  {$IFDEF MNVEC}
  if numV > 0 then
     showmessage(format('%g %g %g', [sumV.X/numV, sumV.Y/numV, sumV.Z/numV]));
  {$ENDIF}
    mnV := mn;
    mxV := mx;
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

function TTrack.SimplifyMM(Tol, minLength: float): boolean;
var
   pos: TPoint3f;
   i, m, mi, xi, xm, n_countOut: integer;
   dx: single;
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
  result := false;
  if (Tol = 0) and (minLength = 0) then exit; //nothing to do
  if (n_count < 1) or (length(tracks) < 4) then exit;
  setlength(xTracks, length(tracks));
  i := 0;
  xi := 0;
  n_countOut := 0;
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
        dx := vectorDistance(Orig[0], Orig[m-1]);
        if dx > minLength then begin
           n_countOut := n_countOut + 1;
           xm := PolySimplifyFloat3D(Tol, Orig, Simple);
           xTracks[xi] := asSingle(xm);
           inc(xi);
           for mi := 0 to (xm-1) do
               AddJoint(Simple[mi]);
        end;
  end;
  result := xi < i;
  if result then
     showmessage(format('Reduced to %d%% of original size (fibers reduced from %d to %d)',[round(100*xi/i), n_count, n_countOut ]))
  else begin
      showmessage('Unable to further simplify this track with this threshold');
      exit;
  end;
  //showmessage(format('%g %g',[tracks[0], xTracks[0]]));
  n_count := n_countOut;
  setlength(xTracks, xi);
  tracks := Copy(xTracks, Low(xTracks), Length(xTracks));
  result := true;
end; //SimplifyMM()

function TTrack.LoadFromFile(const FileName: string): boolean;
var
   ext: string;
begin
  isBusy := true;
  Self.Close;
    result := false;
    isWorldSpaceMM := true; //assume we can load in mm space, not voxel space
    maxObservedFiberLength := 0; //in case of error
    if not FileExists(FileName) then exit;
    //ext := UpperCase(ExtractFileExt(Filename));
    ext := ExtractFileExtGzUpper(Filename);
    if (ext = '.BFLOAT') or (ext = '.BFLOAT.GZ') then begin
         if not LoadBfloat(FileName) then exit;
    end else if (ext = '.DAT') then begin
         if not LoadDat(FileName) then exit;
    end else if (ext = '.PDB') then begin
         if not LoadPdb(FileName) then exit;
    end else if (ext = '.TCK') then begin
         if not LoadTck(FileName) then exit;
    end else if (ext = '.TRK') or (ext = '.TRK.GZ') then begin
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

procedure TTrack.SaveTrk(const FileName: string);
var
  //flt: array of single;
  //i, o, m, mi, nflt: integer;
  hdr:  TTrackhdr;
  mStream : TMemoryStream;
  zStream: TGZFileStream;
  FileNameBf: string;
begin
  if (n_count < 1) or (length(tracks) < 4) then exit;
  hdr.id_string[1] := 'T'; hdr.id_string[2] := 'R'; hdr.id_string[3] := 'A'; hdr.id_string[4] := 'C'; hdr.id_string[5] := 'K'; hdr.id_string[6] := chr(0);
  hdr.dim[1] := 181; hdr.dim[2] := 217; hdr.dim[3] := 181;
  hdr.voxel_size[1] := 1; hdr.voxel_size[2] := 1; hdr.voxel_size[3] := 1;
  hdr.origin[1] := 91; hdr.origin[2] := 125; hdr.origin[3] := -71;
  hdr.n_scalars:= 0;
  fillchar(hdr.scalar_name[1], sizeof(hdr.scalar_name), 0);
  hdr.n_properties:= 0;
  fillchar(hdr.property_name[1], sizeof(hdr.scalar_name), 0);
  hdr.vox_to_ras[1,1] := 1; hdr.vox_to_ras[1,2] := 0; hdr.vox_to_ras[1,3] := 0; hdr.vox_to_ras[1,4] := 0.5;
  hdr.vox_to_ras[2,1] := 0; hdr.vox_to_ras[2,2] := 1; hdr.vox_to_ras[2,3] := 0; hdr.vox_to_ras[2,4] := 0.5;
  hdr.vox_to_ras[3,1] := 0; hdr.vox_to_ras[3,2] := 0; hdr.vox_to_ras[3,3] := 1; hdr.vox_to_ras[3,4] := 0.5;
  hdr.vox_to_ras[4,1] := 0; hdr.vox_to_ras[4,2] := 0; hdr.vox_to_ras[4,3] := 0; hdr.vox_to_ras[4,4] := 1;
  fillchar(hdr.reserved[1], sizeof(hdr.reserved), 0);
  hdr.voxel_order[1] := 'R'; hdr.voxel_order[2] := 'A'; hdr.voxel_order[3] := 'S'; hdr.voxel_order[4] := chr(0);
  fillchar(hdr.pad2[1], sizeof(hdr.pad2), 0);
  fillchar(hdr.image_orientation_patient[1], sizeof(hdr.image_orientation_patient), 0);
  fillchar(hdr.pad1[1], sizeof(hdr.pad1), 0);
  hdr.invert_x:= 0; hdr.invert_y:= 0; hdr.invert_z:= 0;
  hdr.swap_xy := 0; hdr.swap_yz := 0; hdr.swap_zx := 0;
  hdr.n_count:= n_count;
  hdr.version := 2;
  hdr.hdr_size := sizeof(hdr);
  mStream := TMemoryStream.Create;
  mStream.Write(hdr, sizeof(hdr));
  mStream.Write(tracks[0], length(tracks) * sizeof(single) );
  mStream.Position := 0;
  if (ExtractFileExtGzUpper(Filename) = '.TRK.GZ') then begin
    FileNameBf := ChangeFileExtX(FileName, '.trk.gz');
    zStream := TGZFileStream.Create(FileNameBf, gzopenwrite);
    zStream.CopyFrom(mStream, mStream.Size);
    zStream.Free;
  end else begin
    FileNameBf := ChangeFileExtX(FileName, '.trk');
    mStream.SaveToFile(FileNameBf);
  end;
  mStream.Free;
end; //SaveTrk()

procedure TTrack.Save(FileName: string);
var
  ext: string;
begin
    ext := ExtractFileExtGzUpper(Filename);
    if ext = '' then begin
       Filename := Filename +'.vtk';
       ext := '.VTK';
    end;
    if (ext = '.BFLOAT') or (ext = '.BFLOAT.GZ') then
       SaveBfloat(FileName)
    else if (ext = '.TRK') or (ext = '.TRK.GZ') then
        SaveTrk(FileName)
    else if (ext = '.VTK') then
        SaveVtk(FileName)
    else begin
         showmessage('Unable to save to format "'+ext+'"');
         Filename := Filename +'.vtk';
         SaveVtk(FileName);
    end;
    {$ifdef isTerminalApp}showmessage('Created file '+FileName);{$endif}
end;

function TTrack.Smooth: boolean;
//smooth nodes
//  http://dev.theomader.com/gaussian-kernel-calculator/
var
   fiber, fibersmooth: array of TPoint3f;
   i, startPos, m, mi: integer;
begin
  result := false;
  if (n_count < 1) or (length(tracks) < 4) then exit;
  setlength(fiber, 1024);
  setlength(fibersmooth, 1024);
  i := 0;
  while i < length(tracks) do begin
        m :=   asInt( tracks[i]);
        inc(i);
        if m < 5 then begin
           i := i + 3 * m;
           continue; //skip this fiber
        end;
        if m > length(fiber) then begin
           setlength(fiber, m);
           setlength(fibersmooth, m);
        end;
        startPos := i;
        for mi := 0 to (m-1) do begin
            fiber[mi].X := tracks[i]; inc(i);
            fiber[mi].Y := tracks[i]; inc(i);
            fiber[mi].Z := tracks[i]; inc(i);
        end;
        fibersmooth[0] := fiber[0];
        fibersmooth[1] := fiber[1];
        fibersmooth[m-2] := fiber[m-2];
        fibersmooth[m-1] := fiber[m-1];
        for mi := 2 to (m-3) do begin
            fibersmooth[mi].X :=  fiber[mi-2].X*0.06136+ fiber[mi-1].X*0.24477+fiber[mi].X*00.38774+ fiber[mi+1].X*0.24477+ fiber[mi+2].X*0.06136;
            fibersmooth[mi].Y :=  fiber[mi-2].Y*0.06136+ fiber[mi-1].Y*0.24477+fiber[mi].Y*00.38774+ fiber[mi+1].Y*0.24477+ fiber[mi+2].Y*0.06136;
            fibersmooth[mi].Z :=  fiber[mi-2].Z*0.06136+ fiber[mi-1].Z*0.24477+fiber[mi].Z*00.38774+ fiber[mi+1].Z*0.24477+ fiber[mi+2].Z*0.06136;
        end;
        //replace fiber with fibersmooth
        i := startPos;
        for mi := 0 to (m-1) do begin
            tracks[i] := fibersmooth[mi].X; inc(i);
            tracks[i] := fibersmooth[mi].Y; inc(i);
            tracks[i] := fibersmooth[mi].Z; inc(i);
        end;

  end;
  setlength(fiber,0);
  result := true;
end;// Smooth()

type
TProps = record
  lo,hi: TPoint3f; //full range of scalar
  index: integer;
  unique: boolean;
end;
TPropsArray = array of TProps;

//http://stackoverflow.com/questions/24335585/quicksort-drama
procedure QuickSort(left, right: integer; var s: TPropsArray);
var
  l, r, lswap: integer;
  pivot: single;
begin
  if (right > left) then begin
    l := left;
    r := right;
    pivot := s[s[(right + left) div 2].index].lo.Z;
    while (l < r) do begin
      while s[s[l].index].lo.Z < pivot do
        l := l + 1;
      while s[s[r].index].lo.Z > pivot do
        r := r - 1;
      if (l <= r) then begin
        lswap := s[r].index;
        s[r].index := s[l].index;
        s[l].index := lswap;
        l := l + 1;
        r := r - 1;
      end;
    end;
    if (left < r) then
      QuickSort(left, r, s);
    if (right > l) then
      QuickSort(l, right, s);
  end;
end;

procedure SortArray(var s: TPropsArray);
var
 i : integer;
begin
  if length(s) < 1 then exit;
  for i := 0 to (length(s)-1) do  //set indices
     s[i].index := i;
  quicksort(low(s), high(s), s);
end;


function TTrack.SimplifyRemoveRedundant(Tol: float): boolean;
var
   pos, pos0: TPoint3f;
   track, i, j, m, mi: integer;
   xI, xJ : ^TProps;
   xTracks: TPropsArray;
   tolSqr, dx : single;
begin
  result := false;
  if (tol <= 0.0) then exit;
  if (n_count < 1) or (length(tracks) < 4) then exit;
  showmessage('Warning: SimplifyRemoveRedundant not optimized: please check for updates');
  tolSqr := sqr(tol); //e.g. if tol=3mm, then tolSqr=9, avoiding slow sqrt in pythagorean formule
  setlength(xTracks, n_count);
  //first pass: record endpoints of all tracks
  track := 0;
  i := 0;
  while i < length(tracks) do begin
        m :=   asInt( tracks[i]);
        inc(i);
        for mi := 0 to (m-1) do begin
            pos.X := tracks[i]; inc(i);
            pos.Y := tracks[i]; inc(i);
            pos.Z := tracks[i]; inc(i);
            if mi = 0 then
               pos0 := pos;
        end;
        xTracks[track].unique := true;
        if pos0.Z < pos.Z then begin
           xTracks[track].lo := pos0;
           xTracks[track].hi := pos;
        end else begin
            xTracks[track].hi := pos0;
            xTracks[track].lo := pos;
        end;
        track := track + 1;
  end;
  track := 0;
  //{$DEFINE SLOW_METHOD}
  {$IFDEF SLOW_METHOD}
  for i := 0 to (n_count - 2) do begin
      for j := (i + 1) to (n_count - 1) do begin
          if (xTracks[j].unique) and (vectorDistanceSqr(xTracks[i].lo, xTracks[j].lo) < tolSqr) and (vectorDistanceSqr(xTracks[i].hi, xTracks[j].hi) < tolSqr) then begin
             xTracks[j].unique := false;
             track := track + 1;
          end;
      end;
  end;
  {$ELSE}
  SortArray(xTracks);
  for i := 0 to (n_count - 2) do begin
    xI := @xTracks[xTracks[i].index];
    j := i + 1;
    repeat
          xJ := @xTracks[xTracks[j].index];
          dx :=  sqr(xJ^.lo.Z - xI^.lo.Z);
          //if dx < 0 then showmessage('sorting error');
          if (xJ^.unique) and (vectorDistanceSqr(xI^.lo, xJ^.lo) < tolSqr) and (vectorDistanceSqr(xI^.hi, xJ^.hi) < tolSqr) then begin
             xJ^.unique := false;
             track := track + 1;
          end;
          j := j + 1;
    until (j = n_count) or (dx > tolSqr);
  end;
  {$ENDIF}
  if track < 1 then exit; //no redundant tracks
  showmessage(format('Removed %d redundant tracks (endpoints nearer than %.4g)', [track, tol]));
  //remove redundant fibers
  j := 0;
  i := 0;
  track := 0;
  while i < length(tracks) do begin
        m :=   asInt( tracks[i]); inc(i);
        if xTracks[track].unique then begin
           tracks[j] := asSingle(m); inc(j);
        end;
        if xTracks[track].unique then
          for mi := 1 to (m * 3) do begin//*3 as each vertex stores XYZ
              tracks[j] := tracks[i]; inc(j); inc(i);
          end
        else
            i := i + (m * 3);
        track := track + 1;
  end;
  setlength(tracks,j);
  result := true;
end; //SimplifyRemoveRedundant()

destructor TTrack.Destroy;
begin
  self.Close;
  inherited;
end; // Destroy()


end.
