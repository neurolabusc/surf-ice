unit mesh;
{$Include opts.inc} //compile for either dglOpenGL or glext
{$mode objfpc}{$H+}
interface

uses
  {$IFDEF DGL} dglOpenGL, {$ELSE} gl, glext, {$ENDIF}
  {$IFDEF CTM} ctm_loader, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, strutils,
  base64, zstream, LcLIntf, nifti_loader, colorTable, matmath,
  define_types;

const
  kMinOverlayIndex = 1;
  kMaxOverlays = 32;
type
  TSphere = packed record
     X: single;
     Y: single;
     Z: single;
     Clr: single;
     Radius: single;
   end;
  //TVertexArray = array of TPoint3f;
  //TFaceArray = array of TPoint3i;

 TOverlay = record
    LUTvisible: boolean;
    LUTindex : integer;
    LUT: TLUT;
    minIntensity, maxIntensity, windowScaledMin, windowScaledMax: single;
    filename: string;
    intensity: TFloats;
    //next: if loaded as mesh...
    faces : TFaces;//array of TPoint3i;
    vertices: TVertices;
 end;

 TNodePrefs = record //preferences for nodes
    minEdge, maxEdge, maxEdgeAbs, minEdgeThresh, maxEdgeThresh, minNodeColor, maxNodeColor,  minNodeSize, maxNodeSize,
    minNodeThresh, maxNodeThresh, scaleNodeSize, scaleEdgeSize, thresholdNodeFrac: single;
    nodeLUTindex , edgeLUTindex: integer;
     isNodeThresholdBySize, isNoNegEdge, isNoPosEdge, isNoLeftNodes,isNoRightNodes, isNodeColorVaries, isEdgeColorVaries, isEdgeSizeVaries, isEdgeShowNeg, isEdgeShowPos : boolean;
 end;

type
  TMesh = class
    scale, vertexRgbaSaturation, vertexRgbaAlpha : single;
    origin : TPoint3f;
    {$IFDEF COREGL}
    vao, vbo, vaoOverlay, vboOverlay: GLuint;
    nFacesOverlay: integer;
    {$ELSE}
    displayList, displayListOverlay : GLuint;
    {$ENDIF}
    isZDimIsUp, isRebuildList, isBusy, isNode, isFreeSurferMesh, isVisible, isAdditiveOverlay : boolean;
    nodePrefs: TNodePrefs;
    OpenOverlays, OverlayTransparency : integer;
    overlay: array [kMinOverlayIndex..kMaxOverlays] of TOverlay;
    nodes: array of TSphere;
    edges:  array of array of Single;
    faces : array of TPoint3i;
    vertices: array of TPoint3f;
    vertexRGBA : array of TRGBA;
    //overlay: array of single;
  private
    function CheckMesh: boolean;
    procedure SetOverlayDescriptives(lOverlayIndex: integer);
    procedure SetDescriptives;
    procedure MakeSphere;
    procedure BuildList  (Clr: TRGBA);
    procedure BuildListOverlay (Clr: TRGBA);
    function LoadAnnot(const FileName: string): boolean;
    function LoadGcs(const FileName: string): boolean;
    function LoadGii(const FileName: string; lOverlayIndex, lOverlayItem: integer): integer;
    function LoadMz3(const FileName: string; lOverlayIndex : integer): boolean;
    procedure LoadAsc_Srf(const FileName: string);
    procedure LoadCtm(const FileName: string);
    procedure LoadCurv(const FileName: string; lOverlayIndex: integer);
    procedure LoadNode(const FileName: string);
    procedure LoadNv(const FileName: string);
    procedure LoadObj(const FileName: string);
    procedure LoadOff(const FileName: string);
    procedure LoadPial(const FileName: string);
    procedure LoadPly(const FileName: string);
    procedure LoadStl(const FileName: string);
    procedure LoadVtk(const FileName: string);
    procedure LoadW(const FileName: string; lOverlayIndex: integer);
    procedure LoadNii(const FileName: string; lOverlayIndex: integer; isSmooth: boolean);
    procedure LoadMeshAsOverlay(const FileName: string; lOverlayIndex: integer);
  public
    procedure MakePyramid;
    procedure DrawGL (Clr: TRGBA);
    procedure Node2Mesh;
    procedure ReverseFaces;
    procedure SwapYZ;
    procedure SwapZY;
    function LoadFromFile(const FileName: string): boolean;
    function LoadEdge(const FileName: string): boolean;
    function LoadOverlay(const FileName: string; isSmooth: boolean): boolean;
    procedure CloseOverlays;
    procedure Close;
    constructor Create;
    procedure SaveMz3(const FileName: string);
    procedure SaveGii(const FileName: string);
    procedure SaveObj(const FileName: string);
    procedure SaveOverlay(const FileName: string; OverlayIndex: integer);
    destructor  Destroy; override;
  end;

implementation

uses  shaderu, meshify_simplify,{$IFDEF COREGL} gl_core_3d {$ELSE} gl_legacy_3d {$ENDIF};

{$IFDEF COREGL}
function mixRGBA(c1, c2: TRGBA; frac2: single): TRGBA;
var
   frac1: single;
begin
  frac2 := ((c2.a * frac2 )/255.0);
  frac1 := 1 - frac2;
  result.R := round(c1.R*frac1 + c2.R*frac2) ;
  result.G := round(c1.G*frac1 + c2.G*frac2);
  result.B := round(c1.B*frac1 + c2.B*frac2);
  //if frac1 >= 0.5 then //result.a = max(c1.a, c2.a)
  //   result.A := c1.A
  //else
  //    result.A := c2.A;
end;
(*function mixRGBA(c1, c2: TRGBA; frac1: single ): TRGBA;
var
  frac2: single;
begin
  frac2 := 1 - frac1;
  result.R := round(c1.R*frac1 + c2.R*frac2) ;
  result.G := round(c1.G*frac1 + c2.G*frac2);
  result.B := round(c1.B*frac1 + c2.B*frac2);
  if frac1 >= 0.5 then //result.a = max(c1.a, c2.a)
     result.A := c1.A
  else
      result.A := c2.A;
end;  *)
{$ENDIF}

function blendRGBA(c1, c2: TRGBA ): TRGBA;
var
  frac1, frac2: single;
begin
  result := c1;
  if c2.A = 0 then exit;
  result := c2;
  if c1.A = 0 then exit;
  frac1 := c1.A / (c1.A+c2.A);
  frac2 := 1 - frac1;
  result.R := round(c1.R*frac1 + c2.R*frac2) ;
  result.G := round(c1.G*frac1 + c2.G*frac2);
  result.B := round(c1.B*frac1 + c2.B*frac2);
  if frac1 >= 0.5 then //result.a = max(c1.a, c2.a)
     result.A := c1.A
  else
      result.A := c2.A;
end;

function maxRGBA(c1, c2: TRGBA ): TRGBA;
begin
  result := c1;
  if c2.A = 0 then exit;
  if (c2.R > result.R) then
     result.R := c2.R;
  if (c2.G > result.G) then
     result.G := c2.G;
  if (c2.B > result.B) then
     result.B := c2.B;
  if (c2.A > result.A) then
     result.A := c2.A;
end;

function inten2rgb(intensity, mn, mx: single; lut: TLUT): TRGBA;
begin
  if  (mn < 0) and (mx < 0) then begin
    if intensity >= mx then begin
       result := lut[0];
    end else if intensity <= mn then
       result := lut[255]
    else
         result := lut[round(255* (1.0-   (intensity-mn)/(mx-mn)))];
  end else begin
     if intensity <= mn then begin
        result := lut[0];
     end else if intensity >= mx then
        result := lut[255]
     else
          result := lut[round(255*(intensity-mn)/(mx-mn))];
  end;
end;

function desaturateRGBA ( rgba: TRGBA; frac: single; alpha: byte): TRGBA;
var
  y: single;
begin
  //convert RGB->YUV http://en.wikipedia.org/wiki/YUV
    y := 0.299 * rgba.r + 0.587 * rgba.g + 0.114 * rgba.b;
    result.r := round(y * (1-frac) + rgba.r * frac);
    result.g := round(y * (1-frac) + rgba.g * frac);
    result.b := round(y * (1-frac) + rgba.b * frac);
    result.a := alpha;
end;

procedure TMesh.BuildList (Clr: TRGBA);
var
  i,c: integer;
  mn, mx: single;
  rgb: TRGBA;
  vRGBA, vRGBAmx :TVertexRGBA;
  vZeroNeighbor: array of boolean;
  isOverlayPainting : boolean = false;
begin
  if (length(faces) < 1) or (length(vertices) < 3) then exit;
  isOverlayPainting := false;
  if  (OpenOverlays > 0)  then  //ignore overlays if they are all meshes rather than vertex colors
       for c :=  OpenOverlays downto 1 do
           if (overlay[c].LUTvisible) and (length(overlay[c].intensity) = length(vertices)) then
              isOverlayPainting := true;
  //if  (OpenOverlays > 0) or (length(vertexRGBA) = length(vertices)) then begin  // <- works, but slower if all overlays are meshes
  if  (isOverlayPainting) or (length(vertexRGBA) = length(vertices)) then begin
     rgb := RGBA(Clr.R, Clr.G, Clr.B, 0);
     setlength(vRGBA, length(vertices));
     if (length(vertexRGBA) = length(vertices)) then begin
       c := round(vertexRgbaAlpha * 255);
        if vertexRgbaSaturation >= 1 then begin
          for i := 0 to (length(vertices)-1) do begin
                vRGBA[i].r := vertexRGBA[i].r; vRGBA[i].g := vertexRGBA[i].g;  vRGBA[i].b := vertexRGBA[i].b;  vRGBA[i].a := c;
          end;
        end else begin
            for i := 0 to (length(vertices)-1) do
               vRGBA[i] := desaturateRGBA ( vertexRGBA[i], vertexRgbaSaturation, C);
        end;
        {$IFDEF COREGL}
        if (vertexRgbaAlpha < 1.0) then
          for i := 0 to (length(vertices)-1) do
              vRGBA[i] := mixRGBA( Clr, vRGBA[i], vertexRgbaAlpha);
        {$ENDIF}
     end else begin
         for i := 0 to (length(vertices)-1) do
             vRGBA[i] := rgb;
     end;
     if  (OpenOverlays > 0) then begin
        if isAdditiveOverlay then begin
          setlength(vRGBAmx, length(vertices));
          for i := 0 to (length(vertices)-1) do
              vRGBAmx[i] := rgb;
          for c :=  OpenOverlays downto 1 do begin
            if (overlay[c].LUTvisible) and (length(overlay[c].intensity) = length(vertices)) then begin
               if overlay[c].windowScaledMax > overlay[c].windowScaledMin then begin
                  mn := overlay[c].windowScaledMin;
                  mx := overlay[c].windowScaledMax;
               end else begin
                   mx := overlay[c].windowScaledMin;
                   mn := overlay[c].windowScaledMax;
               end;
               for i := 0 to (length(vertices)-1) do begin
                   rgb := inten2rgb(overlay[c].intensity[i], mn, mx, overlay[c].LUT);
                   vRGBAmx[i] := maxRGBA(vRGBAmx[i], rgb);
               end; //for i
            end; //if visible
          end; //for c
          for i := 0 to (length(vertices)-1) do
              vRGBA[i] := blendRGBA(vRGBA[i],vRGBAmx[i]);
        end else begin
            for c :=  OpenOverlays downto 1 do begin
              if (overlay[c].LUTvisible) and (length(overlay[c].intensity) = length(vertices)) then begin
                 if overlay[c].windowScaledMax > overlay[c].windowScaledMin then begin
                    mn := overlay[c].windowScaledMin;
                    mx := overlay[c].windowScaledMax;
                 end else begin
                     mx := overlay[c].windowScaledMin;
                     mn := overlay[c].windowScaledMax;
                 end;
                 for i := 0 to (length(vertices)-1) do begin
                     rgb := inten2rgb(overlay[c].intensity[i], mn, mx, overlay[c].LUT);
                     vRGBA[i] := blendRGBA(vRGBA[i], rgb);
                 end; //for i
              end; //if visible
            end;  //for c
       end;  //if isAdditiveOverlay else
       if (length(vertexRGBA) < 1) then begin //feather edges if regions without overlay have alpha = 0
         setlength(vZeroNeighbor, length(vertices));
         for i := 0 to (length(vertices)-1) do
             vZeroNeighbor[i] := false;
         for i := 0 to (length(faces)-1) do begin
             if (vRGBA[faces[i].X].A = 0) or (vRGBA[faces[i].Y].A = 0) or (vRGBA[faces[i].Z].A = 0) then begin
                vZeroNeighbor[faces[i].X] := true;
                vZeroNeighbor[faces[i].Y] := true;
                vZeroNeighbor[faces[i].Z] := true;
             end;
         end;
         for i := 0 to (length(vertices)-1) do
             if(vZeroNeighbor[i]) then
                 vRGBA[i].a :=   vRGBA[i].a shr 1; //make edges more transparent
       end; //end feather edges
       {$IFDEF COREGL} //with new openGL we mix here
       mx := 1.0 - OverlayTransparency/100;
       if mx < 0 then mx := 0;
       if mx > 1 then mx := 1;
       //if (OverlayTransparency > 0 ) and (OverlayTransparency <= 100) then
       for i := 0 to (length(vertices)-1) do
              vRGBA[i] := mixRGBA( Clr, vRGBA[i], mx);
       {$ELSE}  //with old GLSL we mix in the shader
       if (OverlayTransparency > 0 ) and (OverlayTransparency <= 100) then
          for i := 0 to (length(vertices)-1) do
              vRGBA[i].a := round( vRGBA[i].a * (1 - (OverlayTransparency /100)) );
       {$ENDIF}
     end; // if OpenOverlays > 0
     {$IFDEF COREGL}
     BuildDisplayList(faces, vertices, vRGBA, vao, vbo, Clr);
     {$ELSE}
     displayList:= BuildDisplayList(faces, vertices, vRGBA);
     {$ENDIF}
  end else begin
     setLength(vRGBA,0);
     {$IFDEF COREGL}
     BuildDisplayList(faces, vertices, vRGBA,vao, vbo, Clr);
     {$ELSE}
     displayList:= BuildDisplayList(faces, vertices, vRGBA);
     {$ENDIF}
  end;
  //glEndList();
end; // BuildList()

procedure TMesh.BuildListOverlay (Clr: TRGBA);
var
  c, i, nVert, nFace, sumFace, sumVert, nMeshOverlay: integer;
  oFaces: TFaces;
  oVerts: TVertices;
  vRGBA: TVertexRGBA;
begin
  if (length(faces) < 1) or (length(vertices) < 3) or (OpenOverlays < 1) then
     exit;
  nMeshOverlay := 0;
  for c := 1 to OpenOverlays do
      if (overlay[c].LUTvisible) and (length(overlay[c].vertices) > 2) then
         nMeshOverlay := nMeshOverlay + 1;
  if nMeshOverlay < 1 then exit;
  nMeshOverlay := 0;
  sumVert := 0;
  sumFace := 0;
  for c := 1 to OpenOverlays do begin
      nVert := length(overlay[c].vertices);
      nFace := length(overlay[c].faces);
      if (overlay[c].LUTvisible) and (nVert > 2) and (nFace > 0) then begin
            setlength(oFaces, sumFace + nFace);
            for i := 0 to (nFace -1) do
                oFaces[i+sumFace] := vectorAdd(overlay[c].faces[i], sumVert);
            setlength(oVerts, sumVert + nVert);
            for i := 0 to (nVert -1) do
                oVerts[i+sumVert] := overlay[c].vertices[i];
        setlength(vRGBA, sumVert + nVert);
        for i := 0 to (nVert -1) do
            vRGBA[i+sumVert] := overlay[c].LUT[192];
         sumVert := sumVert + nVert;
         sumFace := sumFace + nFace;
      end;
  end; //for each overlay
  {$IFDEF COREGL}
  nFacesOverlay := length(oFaces);
  BuildDisplayList(oFaces, oVerts, vRGBA, vaoOverlay, vboOverlay, Clr);
  {$ELSE}
  displayListOverlay := BuildDisplayList(oFaces, oVerts, vRGBA);
  {$ENDIF}
end; // BuildListOverlay()

procedure TMesh.DrawGL (Clr: TRGBA);
begin
  if (length(faces) < 1) or (length(vertices) < 3) then exit;
  isBusy := true;

  if isRebuildList then begin//only if the model has been changed
      isRebuildList := false;
      if isNode then
           Node2Mesh;
      {$IFDEF COREGL}
      if vao <> 0 then
         glDeleteVertexArrays(1,@vao);
      if (vbo <> 0) then
         glDeleteBuffers(1, @vbo);
      if vaoOverlay <> 0 then
         glDeleteVertexArrays(1,@vaoOverlay);
      if (vboOverlay <> 0) then
         glDeleteBuffers(1, @vboOverlay);
      vao := 0; vbo := 0; vaoOverlay := 0; vboOverlay := 0;
      {$ELSE}
      glDeleteLists(displayList, 1);
      displayList := 0;
      glDeleteLists(displayListOverlay, 1);
      displayListOverlay := 0;
      {$ENDIF}
      BuildList (Clr); //upload geometry as a display list: http://www.songho.ca/opengl/gl_displaylist.html
      BuildListOverlay (Clr);
  end;
  {$IFDEF COREGL}
  //RunMeshGLSL (2,0,0,0); //disable clip plane

  if isVisible then begin
    glBindVertexArray(vao);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,vbo);
    glDrawElements(GL_TRIANGLES, Length(faces)* 3, GL_UNSIGNED_INT, nil);
    glBindVertexArray(0);
  end;
  if (vaoOverlay <> 0) and (vboOverlay <> 0) and (nFacesOverlay > 0) then begin
     RunOverlayGLSL;
     glBindVertexArray(vaoOverlay);
     glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,vboOverlay);
     glDrawElements(GL_TRIANGLES, nFacesOverlay * 3, GL_UNSIGNED_INT, nil);
     glBindVertexArray(0);


  end;
  {$ELSE}
  if isVisible then
     glCallList(DisplayList);
  if (displayListOverlay <> 0) then begin
    RunOverlayGLSL;
     glCallList(displayListOverlay);
  end;
  {$ENDIF}

  isBusy := false;
end; // DrawGL()

procedure TMesh.SwapYZ;
var
   swap: TPoint3f;
   i : integer;
begin
     if (length(vertices) < 1) or  (length(faces) < 1) then exit;
     for i := 0 to (length(vertices) - 1) do begin
         swap.X := vertices[i].X;
         swap.Y := -vertices[i].Z;
         swap.Z := vertices[i].Y;
         vertices[i] := swap;
     end;
     SetDescriptives;
     isRebuildList := true;
end;

procedure TMesh.SwapZY;
var
   swap: TPoint3f;
   i : integer;
begin
     if (length(vertices) < 1) or  (length(faces) < 1) then exit;
     for i := 0 to (length(vertices) - 1) do begin
         swap.X := vertices[i].X;
         swap.Y := vertices[i].Z;
         swap.Z := -vertices[i].Y;
         vertices[i] := swap;
     end;
     SetDescriptives;
     isRebuildList := true;
end;

procedure TMesh.ReverseFaces; //reverse face winding to reverse front and back faces
var
   oldFaces : array of TPoint3i;
   i: integer;
begin
  if (length(faces) < 1) then begin
     showmessage('No mesh is open: unable to reverse faces');
     exit;
  end;
  if isBusy then exit;
  isBusy := true;
  oldFaces := Copy(faces, Low(faces), Length(faces));
  for i := 0 to (length(faces)-1) do begin
      faces[i].X := oldFaces[i].Z;
      faces[i].Z := oldFaces[i].X;
  end;
  isRebuildList := true;
  isBusy := false;
end;

procedure TMesh.SetDescriptives;
var
   mn, mx: TPoint3f;
   i: integer;
begin
     if length(vertices) < 1 then exit;
     mx := vertices[0];
     mn := mx;
     (*for i := 0 to (length(vertices) - 1) do begin
         vertices[i].X := 10 * vertices[i].X;
         vertices[i].Y := 10 * vertices[i].Y;
         vertices[i].Z := 10 * vertices[i].Z;
     end;*)
     for i := 0 to (length(vertices) - 1) do
         minMax(vertices[i], mn, mx);
     origin.X := (0.5 * (mx.X - mn.X)) + mn.X;
     origin.Y := (0.5 * (mx.Y - mn.Y)) + mn.Y;
     origin.Z := (0.5 * (mx.Z - mn.Z)) + mn.Z;
     Scale := abs(mx.X - origin.X);
     if abs(mx.Y - origin.Y) > Scale then
        Scale := abs(mx.Y - origin.Y);
     if abs(mx.Z - origin.Z) > Scale then
        Scale := abs(mx.Z - origin.Z);
end; // SetDescriptives()

{$DEFINE SPHERE_BY_SUBDIVIDE} //we can make a sphere via Polyhedra Subdivision or Parametric Surfaces  http://prideout.net/blog/?p=44
//with 2 subdivisions we get a sphere with 162 vertices, 320 faces similar to the 15-step sphere that is 286 vertices and 528 faces
//see also http://richardssoftware.net/Home/Post?id=60
{$IFDEF SPHERE_BY_SUBDIVIDE}
// 0,1,2,3 subdivision faces 20, 80, 320, 1280; vertices: 12, 42, 162, 642
function NormMidPoint(a,b: TPoint3f): TPoint3f;
begin
     result := a;
     vectorAdd(result,b);
     vectorNormalize(result);
end;

procedure Subdivide(var verts: TVertices; var faces: TFaces);
var
   faceIndex,n, nv, nf: integer;
begin
   nv := length(verts);
   nf := length(faces);
   n := nf;
   for faceIndex := 0 to (n -1) do begin
       //Create three new verts at the midpoints of each edge:
       setlength(verts, nv + 3);
       verts[nv] := NormMidPoint(verts[faces[faceIndex].X],verts[faces[faceIndex].Y]);
       verts[nv+1] := NormMidPoint(verts[faces[faceIndex].Y],verts[faces[faceIndex].Z]);
       verts[nv+2] := NormMidPoint(verts[faces[faceIndex].X],verts[faces[faceIndex].Z]);
       //Split the current triangle into four smaller triangles:
       setlength(faces, nf + 3);
       faces[nf] := pti(nv,nv + 1, nv + 2);
       faces[nf+1] := pti(faces[faceIndex].X, nv, nv + 2);
       faces[nf+2] := pti(nv, faces[faceIndex].Y,nv + 1);
       faces[faceIndex] := pti(nv + 2, nv + 1, faces[faceIndex].Z);
       nf := nf + 3;
       nv := nv + 3;
   end;
end;

procedure TMesh.MakeSphere;
begin
     //http://prideout.net/blog/?p=44
  setlength(vertices, 12);
  vertices[0] := ptf(0.000,  0.000,  1.0);
  vertices[1] := ptf(0.894,  0.000,  0.447);
  vertices[2] := ptf(0.276,  0.851,  0.447);
  vertices[3] := ptf(-0.724,  0.526,  0.447);
  vertices[4] := ptf(-0.724, -0.526,  0.447);
  vertices[5] := ptf(0.276, -0.851,  0.447);
  vertices[6] := ptf(0.724,  0.526, -0.447);
  vertices[7] := ptf(-0.276,  0.851, -0.447);
  vertices[8] := ptf(-0.894,  0.000, -0.447);
  vertices[9] := ptf(-0.276, -0.851, -0.447);
  vertices[10] := ptf(0.724, -0.526, -0.447);
  vertices[11] := ptf(0.000,  0.000, -1.0);
  setlength(faces, 20);
  faces[0] := pti(0,1,2);
  faces[1] := pti(0,2,3);
  faces[2] := pti(0,3,4);
  faces[3] := pti(0,4,5);
  faces[4] := pti(0,5,1);
  faces[5] := pti(7,6,11);
  faces[6] := pti(8,7,11);
  faces[7] := pti(9,8,11);
  faces[8] := pti(10,9,11);
  faces[9] := pti(6,10,11);
  faces[10] := pti(6,2,1);
  faces[11] := pti(7,3,2);
  faces[12] := pti(8,4,3);
  faces[13] := pti(9,5,4);
  faces[14] := pti(10,1,5);
  faces[15] := pti(6,7,2);
  faces[16] := pti(7,8,3);
  faces[17] := pti(8,9,4);
  faces[18] := pti(9,10,5);
  faces[19] := pti(10,6,1);
  Subdivide(vertices, faces); //v=42, f=80
  Subdivide(vertices, faces); //v=162, f=320
  //Subdivide(vertices, faces); //v=396, f=1280
  UnifyVertices(faces, vertices); //e.g. drops vertices from 312->162
end;
{$ELSE}
procedure TMesh.MakeSphere;
//for another method see http://blog.andreaskahler.com/2009/06/creating-icosphere-mesh-in-code.html
const
  step = 15; //must evenly divide 180: 5, , 15, 20, 30, 45
var
   i,v, f, num_v, num_f, nx, vx : integer;
   latitude, longitude, cosx, sinx, r1, h1: single;
begin
     nx := 360 div step;
     num_v := 2 + ((180 div step)-1) * (360 div step);
     num_f := 2 * (360 div step) * ((180 div step)-1);
     setlength(vertices, num_v);
     setlength(faces, num_f);
     latitude := -90+step;
     vertices[0] := ptf(0,-1.0,0);
     v := 1;
     f := 0;
     while latitude < 90.0 do begin
           r1 := cos(latitude * pi / 180.0);
           h1 := sin(latitude * pi / 180.0);
           longitude := 0;
           while longitude < 360.0 do begin
                 cosx := cos(longitude * PI / 180.0);
                 sinx := -sin(longitude * PI / 180.0);
                 vertices[v] := ptf(r1 * cosx, h1, r1 * sinx);
                 if ((longitude + step) < 360.0) then
                     vx := v +1
                 else
                     vx := v - nx + 1;
                 if (latitude-step) > -90 then begin
                    faces[f] := pti(vx,v,v-nx);
                    f := f + 1;
                    faces[f] := pti(vx-nx,vx,v-nx);
                    f := f + 1;
                 end else begin
                   faces[f] := pti(vx,v,0);
                   f := f + 1;
                 end;
                 inc(v);
                 longitude := longitude + step;
           end; //while longitude <= 360
           latitude := latitude + step;
     end; //while latitude < 90
     vertices[v] := ptf(0,1,0);
     for i := 1 to nx-1 do begin
         faces[f] := pti(v-i-1,v-i,v);
         f := f + 1;
     end;
     //faces[f] := pti(v,v-nx,v-1);
     faces[f] := pti(v-1,v-nx,v);
     inc(v);
     //showmessage(format('%d %d  -> %d %d', [num_v, num_f, v, f]));
end;
{$ENDIF}


procedure TMesh.MakePyramid;
begin
     {$IFDEF SPHERE}
     MakeSphere;
     {$ELSE}
     setlength(vertices, 5);
     vertices[0] := ptf(0,0,0.5);
     vertices[1] := ptf(0.5,0.5,-0.5);
     vertices[2] := ptf(0.5,-0.5,-0.5);
     vertices[3] := ptf(-0.5,-0.5,-0.5);
     vertices[4] := ptf(-0.5,0.5,-0.5);
     setlength(faces, 4);
     faces[0] := pti(2,1,0);
     faces[1] := pti(3,2,0);
     faces[2] := pti(4,3,0);
     faces[3] := pti(1,4,0);
     {$ENDIF}
     SetDescriptives;
end; // MakePyramid()

constructor TMesh.Create;
begin
     {$IFDEF COREGL}
     vao := 0;
     vbo := 0;
     vaoOverlay := 0;
     vboOverlay := 0;
     {$ELSE}
     displayList := 0;
     displayListOverlay := 0;
     {$ENDIF}
     isZDimIsUp := true;
     isVisible := true;
     OpenOverlays := 0;
     OverlayTransparency := 0;
     isRebuildList := true;
     setlength(faces,0);
     setlength(vertices,0);
     setlength(nodes, 0);
     setlength(edges,0,0);
     setlength(vertexRGBA,0);
     isFreeSurferMesh := false;
     vertexRgbaSaturation := 1;
     vertexRgbaAlpha := 1;
     nodePrefs.scaleNodeSize := 2.0;
     nodePrefs.isNodeColorVaries:= true;
     nodePrefs.isNodeThresholdBySize := true;
     nodePrefs.NodeLUTindex := 3;
     //nodePrefs.thresholdNodeSize := 0.0;

     nodePrefs.edgeLUTindex := 1;
     nodePrefs.isNoLeftNodes:= false;
     nodePrefs.isNoRightNodes:= false;

     nodePrefs.isNoNegEdge:= false;
     nodePrefs.isNoPosEdge:= false;

     nodePrefs.isEdgeShowNeg := true;
     nodePrefs.isEdgeShowPos := true;
     nodePrefs.isEdgeColorVaries := true;
     nodePrefs.isEdgeSizeVaries := true;
     nodePrefs.scaleEdgeSize := 1;

     origin := ptf(0,0,0);
     scale := 0;
     //MakePyramid;
     isBusy := false;
     isNode := false;
end; // Create()

(*function TMesh.LoadSrf(const FileName: string): boolean;
//http://support.brainvoyager.com/automation-aamp-development/23-file-formats/375-users-guide-23-the-format-of-srf-files.html
//Version 4.0 appears little endian, notes out of data as reserved is not 0 for sample images
var
   ver1s, originX, originY,originZ: single;
   expectedSz, sz, res2i, num_v, num_f: longint;

begin
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  sz := FileSize(f);
  blockread(f, ver1s, 4); //version, e.g. 4.0
  blockread(f, res2i, 4); //reserved
  blockread(f, num_v, 4); //NrOfVertices
  blockread(f, num_f, 4); //NrOfVertices
  blockread(f, originX, 4); //MeshCenterX
  blockread(f, originY, 4); //MeshCenterY
  blockread(f, originZ, 4); //MeshCenterZ
  expectedSz := (7 * 4 * num_v)
  result := false;
end;*)

procedure TMesh.LoadPial(const FileName: string);
//simple format used by Freesurfer  BIG-ENDIAN
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bpial/readPial.m
// http://www.grahamwideman.com/gw/brain/fs/surfacefileformats.htm
var
   f: File;
   i, sz, num_EOLN: integer;
   b: byte;
   num_v, num_f : LongWord;
   sig : Int64;
   vValues: array [1..3] of single;
   fValues: array [1..3] of LongWord;
begin
     AssignFile(f, FileName);
     FileMode := fmOpenRead;
     Reset(f,1);
     blockread(f, sig, 8 ); //since these files do not have a file extension, check first 8 bytes "0xFFFFFE creat"
     {$IFDEF ENDIAN_LITTLE}
     if (sig <> 8386095523118907391) then begin
     {$ELSE}
     if (sig <> -1771902246540) then begin
     {$ENDIF}
        CloseFile(f);
        exit;
     end;
     sz := FileSize(f);
     num_EOLN := 0;
     while (filepos(f) < sz) and (num_EOLN < 2) do begin
           blockread(f, b, 1 );
           if b = $0A then inc(num_EOLN);
     end;
     if num_EOLN < 2 then begin
        showmessage('File corrupted');
        CloseFile(f);
        exit;
     end;
     blockread(f, num_v, 4 ); //uint32
     blockread(f, num_f, 4 ); //uint32
     {$IFDEF ENDIAN_LITTLE}
     SwapLongWord(num_v);
     SwapLongWord(num_f);
     {$ENDIF}
     if (num_v < 3) or (num_f < 1) or (sz < (filepos(f)+(num_v*12) + (num_f * 12) )) then begin
        showmessage('File corrupted: file must be large enough for '+inttostr(num_v)+' vertices and '+inttostr(num_f)+' triangles');
        CloseFile(f);
        exit;
     end;
     setlength(vertices, num_v); //vertices = zeros(num_f, 9);
     setlength(faces, num_f);
     {$IFDEF ENDIAN_LITTLE}
     for i := 0 to (num_v-1) do begin
         blockread(f, vValues, 4 * 3 );
         SwapSingle(vValues[1]);
         SwapSingle(vValues[2]);
         SwapSingle(vValues[3]);
         vertices[i] := ptf(vValues[1], vValues[2], vValues[3]);
     end;
     for i := 0 to (num_f-1) do begin
         blockread(f, fValues, 4 * 3 );
         SwapLongWord(fValues[1]);
         SwapLongWord(fValues[2]);
         SwapLongWord(fValues[3]);
         faces[i] := pti(fValues[1], fValues[2], fValues[3]);
     end;
     {$ELSE}
     warning following two lines not tested on big endian computers!
     blockread(f, vertices[0], 4 * 3 * num_v);
     blockread(f, faces[0], 4 * 3 * num_f);
     {$ENDIF}
     CloseFile(f);
end; // LoadPial()

procedure ReadlnSafe(var f: TextFile; var v1, v2, v3, v4: single);
//read up to 3 values, ignore all lines that begin with '#' comment
var
   strlst : TStringList;
   str: string;
begin
  v1 := 0; v2 := 0; v3 := 0; v4 := 0;
  while not EOF(f) do begin
        ReadLn(f, str); //make sure to run CheckMesh after this, as these are indexed from 1!
        if (length(str) > 0) and (str[1] <> '#') then begin
           strlst:=TStringList.Create;
           strlst.DelimitedText := str;
           v1 := strtofloat(strlst[0]);
           if (strlst.count > 1) then
              v2 := strtofloat(strlst[1]);
           if (strlst.count > 2) then
              v3 := strtofloat(strlst[2]);
           if (strlst.count > 3) then
              v4 := strtofloat(strlst[3]);
           strlst.free;
           exit;
        end;
  end
end;

procedure TMesh.LoadNV(const FileName: string);
//simple format used by BrainNet Viewer https://www.nitrc.org/projects/bnv/
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bnv/readNv.m
var
   f: TextFile;
   i, num_v, num_f: integer;
   v1,v2,v3, v4: single;
begin
     AssignFile(f, FileName);
     Reset(f);
     ReadlnSafe(f, v1,v2,v3, v4);
     num_v := round(v1);
     if num_v < 3 then begin
        showmessage('Corrupt file: must have at least 3 vertices');
        CloseFile(f);
        exit;
     end;
     setlength(vertices, num_v);
     for i := 0 to (num_v - 1) do
         ReadlnSafe(f, vertices[i].X, vertices[i].Y, vertices[i].Z, v4);
     ReadLn(f, num_f);
     if num_f < 3 then begin
        showmessage('Corrupt file: must have at least 1 triangle');
     end;
     setlength(faces, num_f);
     for i := 0 to (num_f - 1) do begin
         ReadlnSafe(f, v1,v2,v3, v4);
         faces[i].X := round(v1);
         faces[i].Y := round(v2);
         faces[i].Z := round(v3);
     end;
     CloseFile(f);
end; // LoadNV()

procedure TMesh.LoadOff(const FileName: string);
//http://paulbourke.net/dataformats/off/
var
   f: TextFile;
   i, num_v, num_f: integer;
   s: string;
   v1,v2,v3, v4: single;
begin
     AssignFile(f, FileName);
     Reset(f);
     Readln(f,s);
     if (pos('OFF', s) <> 1) then begin
        showmessage('Corrupt file: OFF files must begin with OFF');
        CloseFile(f);
        exit;
     end;
     ReadlnSafe(f, v1,v2,v3, v4);
     num_v := round(v1);
     num_f := round(v2);
     if (num_v < 3) or (num_f < 1) then begin
        showmessage('Corrupt file: must have at least 3 vertices and one face');
        CloseFile(f);
        exit;
     end;
     setlength(vertices, num_v);
     for i := 0 to (num_v - 1) do
         ReadlnSafe(f, vertices[i].X, vertices[i].Y, vertices[i].Z, v4);
     setlength(faces, num_f);
     for i := 0 to (num_f - 1) do begin
         ReadlnSafe(f, v1,v2,v3, v4);
         if (round(v1) <> 3) then begin
            setlength(faces, 0);
            setlength(vertices, 0);
            showmessage('Only able to read triangular meshes: this OFF file has a face with '+inttostr(round(v1))+' faces');
            CloseFile(f);
            exit;
         end;
         faces[i].X := round(v2);
         faces[i].Y := round(v3);
         faces[i].Z := round(v4);
     end;
     CloseFile(f);
end; // LoadOff()

// ascii format created by FreeSurfers' mris_convert
procedure TMesh.LoadASC_SRF(const FileName: string);
//Freesurfer ascii format (almost identical to LoadNV)
// see SPM's read_freesurfer_file or http://www.grahamwideman.com/gw/brain/fs/surfacefileformats.htm
var
   f: TextFile;
   i, num_v, num_f: integer;
begin
     AssignFile(f, FileName);
       Reset(f);
       Readln(f);  //% #!ascii
       try
          ReadLn(f, num_v, num_f);
       except
         num_v := 0;
       end;
       if (num_v < 3) or (num_f < 1) then begin
          showmessage('Corrupt file: must have at least 3 vertices and one face');
          CloseFile(f);
          exit;
       end;
       setlength(vertices, num_v);
       for i := 0 to (num_v - 1) do
           ReadLn(f, vertices[i].X, vertices[i].Y, vertices[i].Z);
       setlength(faces, num_f);
       for i := 0 to (num_f - 1) do
           ReadLn(f, faces[i].X, faces[i].Y, faces[i].Z); //make sure to run CheckMesh after this, as these are indexed from 1!
       CloseFile(f);
end; // LoadASC_SRF()

procedure ScaleTranslate (var lMesh: TMesh; Scale, X, Y, Z: single);
var
   i, n: integer;
begin
     n := length(lMesh.vertices);
     for i := 0 to (n - 1) do begin
         lMesh.vertices[i].X := (lMesh.vertices[i].X * Scale) + X;
         lMesh.vertices[i].Y := (lMesh.vertices[i].Y * Scale) + Y;
         lMesh.vertices[i].Z := (lMesh.vertices[i].Z * Scale) + Z;
     end;
end;

function Value2Frac (val, min, max: single): single;

begin
     if val < min then
        result := 0
     else if val >= max then
        result := 1
     else if (min = max) then //frac=min=max !!!
          result := 0.5
     else if min < max then
        result := (val-min)/(max-min)
     else
        result := (val-max)/(min-max); //min/max order wrong!
end;

procedure TMesh.Node2Mesh;
var
   i,j, v,f, n, nNode, nNodeThresh, nEdgeThresh, vert, face,  sphereF, sphereV,  cylF, cylV: integer;
   denom,frac, radius, thresholdNode: single;
   lSphere: TMesh;
   cylFace: TFaces;
   cylVert: TVertices;
   nodeColorVaries, edgeColorVaries: boolean;
   cLUT, cLUTneg : TLUT;
   clr : TRGBA;
function isNodeSurvives(ii: integer): boolean;
begin
        result := false;
        if nodePrefs.isNodeThresholdBySize then begin
           if (nodes[ii].Radius < nodePrefs.minNodeThresh) then exit;
        end else begin
           if (nodes[ii].clr < nodePrefs.minNodeThresh) then exit;
        end;
        if (nodePrefs.isNoRightNodes) and (nodes[ii].X > 0)  then exit;
        if (nodePrefs.isNoLeftNodes) and (nodes[ii].X < 0)  then exit;
        result := true;
end; //isNodeSurvives()
function isEdgeSurvives(ii,jj: integer): boolean;
begin
     result := false;
     if abs(edges[ii,jj]) < nodePrefs.minEdgeThresh then exit;
     if edges[ii,jj] = 0 then exit;
     if (nodePrefs.isNoNegEdge) and (edges[ii,jj] < 0) then exit;
     if (nodePrefs.isNoPosEdge) and (edges[ii,jj] > 0) then exit;
     if (nodePrefs.isNoRightNodes) and ((nodes[ii].X > 0) or (nodes[jj].X > 0)) then exit;
     if (nodePrefs.isNoLeftNodes) and ((nodes[ii].X < 0) or (nodes[jj].X < 0)) then exit;
     result := true;
end; //isEdgeSurvives()
begin

     nNode := length(nodes);
     if nNode < 1 then exit;
     nNodeThresh := 0;
     if nodePrefs.scaleNodeSize > 0 then begin
       for n := 0 to (nNode -1) do
           if isNodeSurvives(n) then //if (nodes[n].Radius >= thresholdNodeSize) then
              inc(nNodeThresh);
     end;
     nodeColorVaries := false;
     if (nodePrefs.isNodeColorVaries) and (nodePrefs.minNodeColor <> nodePrefs.maxNodeColor) then
        nodeColorVaries := true;
     nEdgeThresh := 0;
     edgeColorVaries := false;
     if (nodePrefs.isEdgeColorVaries) and (nodePrefs.maxEdge <> nodePrefs.minEdge) then
        edgeColorVaries := true;
     //thresholdEdgeSize := nodePrefs.threshEdge * nodePrefs.maxEdge;
     if (nodePrefs.scaleEdgeSize > 0) and (length(Edges) > 0) then begin
        for i := 0 to (nNode - 2) do
           for j := (i+1) to (nNode -1) do
               if isEdgeSurvives(i,j) then
                  inc(nEdgeThresh);
     end;
     if (nEdgeThresh = 0) and (nNodeThresh = 0) then exit;
     lSphere := TMesh.Create;
     lSphere.MakeSphere;
     sphereF := length(lSphere.faces);
     sphereV := length(lSphere.vertices);
     MakeCylinder( 1, 71, cylFace, cylVert);
     cylF := length(cylFace);
     cylV := length(cylVert);
     //showmessage(inttostr(sphereF)+ '  '+inttostr(sphereV));
     setlength(self.faces, sphereF * nNodeThresh + CylF * nEdgeThresh);
     setlength(self.vertices, sphereV * nNodeThresh + CylV * nEdgeThresh);
     setlength(self.vertexRGBA, sphereV * nNodeThresh + CylV * nEdgeThresh);
     face := 0;
     vert := 0;
     cLUT := UpdateTransferFunction (nodePrefs.NodeLUTindex);
      if nNodeThresh > 0 then begin
       for n := 0 to (nNode-1) do begin
           if isNodeSurvives(n) then begin //if (nodes[n].Radius >= thresholdNodeSize) then begin
              if nodeColorVaries then begin
                 if nodePrefs.isNodeThresholdBySize then
                    clr := cLUT[round(255 * Value2Frac(nodes[n].Clr, nodePrefs.minNodeColor, nodePrefs.maxNodeColor) )]
                 else
                    clr := cLUT[round(255 * Value2Frac(nodes[n].Clr, nodePrefs.minNodeThresh, nodePrefs.maxNodeThresh) )]
              end else
                  clr := cLUT[192];
              lSphere.MakeSphere; //new unit sphere
             //showmessage(format('%g %g %g %g',[ nodes[n].Radius, nodes[n].X, nodes[n].Y, nodes[n].Z]));
             ScaleTranslate(lSphere, nodes[n].Radius * nodePrefs.scaleNodeSize, nodes[n].X, nodes[n].Y, nodes[n].Z);
             for f := 0 to (sphereF-1) do begin
                 faces[face].X := lSphere.faces[f].X + vert;
                 faces[face].Y := lSphere.faces[f].Y + vert;
                 faces[face].Z := lSphere.faces[f].Z + vert;
                 face := face + 1;
             end;
             for v := 0 to (sphereV-1) do begin
                 vertexRGBA[vert] := clr;
                 vertices[vert] := lSphere.vertices[v];
                 vert := vert + 1;
             end;
           end; //node larger than threshold
       end; //for each node
      end; //at least on node
     // draw cyl
     if nEdgeThresh > 0 then begin
       cLUT := UpdateTransferFunction (nodePrefs.edgeLUTindex);
       i := nodePrefs.edgeLUTindex + 1;
       cLUTneg := UpdateTransferFunction (i);
       if (nodePrefs.minEdgeThresh <> nodePrefs.maxEdgeThresh) then
          denom := 1 /(nodePrefs.maxEdgeThresh - nodePrefs.minEdgeThresh)
       else
           denom := 1;
       for i := 0 to (nNode - 2) do begin
           for j := (i+1) to (nNode -1) do begin
               if isEdgeSurvives(i,j) then begin //if (abs(edges[i,j]) >= thresholdEdgeSize) then begin
                 if abs(edges[i,j]) >= nodePrefs.maxEdgeThresh then
                    frac := 1
                 else
                    frac := (abs(edges[i,j])-nodePrefs.minEdgeThresh) * denom; //1.0 - abs((nodePrefs.maxEdge-abs(edges[i,j])) * denom );
                 if edges[i,j] > 0 then begin
                   if edgeColorVaries then
                      clr := cLUT[round(254 * frac )+1]
                   else
                       clr := cLUT[192];
                 end else begin
                     if edgeColorVaries then
                        clr := cLUTneg[round(254 * frac )+1]
                     else
                         clr := cLUTneg[192];
                 end;
                 if nodePrefs.isEdgeSizeVaries then
                    radius := (nodePrefs.scaleEdgeSize * 0.1) + frac * nodePrefs.scaleEdgeSize
                 else
                     radius := nodePrefs.scaleEdgeSize * 0.5;
                 MakeCylinder(radius, ptf(nodes[i].X, nodes[i].Y, nodes[i].Z), ptf(nodes[j].X, nodes[j].Y, nodes[j].Z), cylFace, cylVert);
                 for f := 0 to (cylF-1) do begin
                     faces[face].X := cylFace[f].X + vert;
                     faces[face].Y := cylFace[f].Y + vert;
                     faces[face].Z := cylFace[f].Z + vert;
                     face := face + 1;
                 end; //for each face
                 for v := 0 to (cylV-1) do begin
                     vertexRGBA[vert] := clr;
                     vertices[vert] := cylVert[v];
                     vert := vert + 1;
                 end; //for v, each vertex
               end; //edge > thresh
           end; //for j: columns
       end; //for i: rows
     end; //at least one node
     lSphere.Free;
end;

procedure TMesh.LoadNode(const FileName: string);
//BrainNet Node And Edge Connectome Files
//http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0068910
label 666;
var
   f: TextFile;
   str: string;
   num_node,i: integer;
   strlst : TStringList;
begin
  strlst:=TStringList.Create;
  AssignFile(f, FileName);
     Reset(f);
     num_node := 0;
     while not EOF(f) do begin
           ReadLn(f, str); //make sure to run CheckMesh after this, as these are indexed from 1!
           if (length(str) > 0) and (str[1] <> '#') then
               inc(num_node);
     end;
     if (num_node < 1) then goto 666;
     setlength(self.nodes, num_node);
     Reset(f);
     num_node := 0;
     while not EOF(f) do begin
           ReadLn(f, str); //make sure to run CheckMesh after this, as these are indexed from 1!
           if (length(str) > 0) and (str[1] <> '#') then begin
              strlst.DelimitedText := str;
              if (strlst.count > 4) then begin
                 self.nodes[num_node].X := strtofloat(strlst[0]);
                 self.nodes[num_node].Y := strtofloat(strlst[1]);
                 self.nodes[num_node].Z := strtofloat(strlst[2]);
                 self.nodes[num_node].Clr := strtofloat(strlst[3]);
                 self.nodes[num_node].radius := strtofloat(strlst[4]);
                 inc(num_node);
              end;
           end;
     end;
     CloseFile(f);
     strlst.free;
     self.isNode := true;
     nodePrefs.minNodeColor := nodes[0].Clr;
     nodePrefs.maxNodeColor := nodes[0].Clr;
     nodePrefs.minNodeSize := nodes[0].radius;
     nodePrefs.maxNodeSize := nodes[0].radius;
     for i := 0 to (num_node -1) do begin
        if nodes[i].Clr < nodePrefs.minNodeColor then
           nodePrefs.minNodeColor := nodes[i].Clr;
        if nodes[i].Clr > nodePrefs.maxNodeColor then
           nodePrefs.maxNodeColor := nodes[i].Clr;
        if nodes[i].radius < nodePrefs.minNodeSize then
           nodePrefs.minNodeSize := nodes[i].radius;
        if nodes[i].radius > nodePrefs.maxNodeSize then
           nodePrefs.maxNodeSize := nodes[i].radius;
     end;
     if (nodePrefs.minNodeSize = nodePrefs.maxNodeSize) then begin
        NodePrefs.isNodeThresholdBySize := false;
        NodePrefs.minNodeThresh := nodePrefs.minNodeColor;
        NodePrefs.maxNodeThresh := nodePrefs.maxNodeColor;
     end else begin
         NodePrefs.isNodeThresholdBySize := true;
         NodePrefs.minNodeThresh := nodePrefs.minNodeSize;
         NodePrefs.maxNodeThresh := nodePrefs.maxNodeSize;
     end;
     Node2Mesh; //build initially so we have accurate descriptives
     //showmessage(floattostr(NodePrefs.minNodeThresh )+' '+floattostr(NodePrefs.maxNodeThresh ));
     isRebuildList := true;
     exit;
 666:
  showmessage('Unable to load Nodes');
  CloseFile(f);
  strlst.free;
end; // LoadNode()

function TMesh.LoadEdge(const FileName: string): boolean;
//BrainNet Node And Edge Connectome Files
//http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0068910
label 666;
var
   f: TextFile;
   str: string;
   num_edge, min_edge, max_edge,r,c: integer;
   strlst : TStringList;
begin
  result := false;
  setlength(edges,0,0);
  if not FileExists(FileName) then exit;
  if (length(nodes) < 2) then begin
       showmessage('You must load a NODE file before loading your EDGE file.');
       exit;
  end;
  strlst:=TStringList.Create;
  AssignFile(f, FileName);
  Reset(f);
  num_edge := 0;
  min_edge := maxint;
  max_edge := 0;
  while not EOF(f) do begin
        ReadLn(f, str); //make sure to run CheckMesh after this, as these are indexed from 1!
        if (length(str) > 0) and (str[1] <> '#') then begin
           inc(num_edge);
           strlst.DelimitedText := str;
           if strlst.count < min_edge then
              min_edge := strlst.count;
           if strlst.count > max_edge then
              max_edge := strlst.count;
        end;
  end; //read each line
  if (num_edge < 2) then begin
     showmessage('No edges found');
     goto 666;
  end;
  if (num_edge <> max_edge) or (min_edge <> max_edge) then begin
     showmessage(format('Unable to parse edge file: found %d lines, but %d..%d columns', [num_edge, min_edge, max_edge]));
     goto 666;
  end;
  if (num_edge > length(nodes)) then begin
       showmessage('Edge and node files do not match: '+inttostr(length(nodes))+' nodes loaded, but edge file describes '+inttostr(num_edge)+' nodes.');
       goto 666;
  end;
  Reset(f);
  setlength(edges, length(nodes), length(nodes));
  for r := 0 to (length(nodes) -1) do
      for c := 0 to (length(nodes) -1) do
          edges[r,c] := 0; //BrainNet allows files with more nodes than edges
  r := 0; //for each row
  while not EOF(f) do begin
        ReadLn(f, str); //make sure to run CheckMesh after this, as these are indexed from 1!
        if (length(str) > 0) and (str[1] <> '#') then begin
           strlst.DelimitedText := str;
           for c := 0 to (num_edge-1) do  //for each column
               edges[r,c] := StrToFloatDef(strlst[c],0);
           inc(r);
        end;
  end; //read each line
  nodePrefs.maxEdgeAbs := abs(edges[0,1]);
  for r := 0 to (num_edge - 2) do
      for c := (r+1) to (num_edge -1) do
          if (abs(edges[r,c]) > nodePrefs.maxEdgeAbs) then
             nodePrefs.maxEdgeAbs := abs(edges[r,c]);
  nodePrefs.maxEdge := edges[0,1];
  for r := 0 to (num_edge - 2) do
      for c := (r+1) to (num_edge -1) do
          if (edges[r,c] > nodePrefs.maxEdge) then
             nodePrefs.maxEdge := edges[r,c];
  nodePrefs.minEdge := edges[0,1];
  for r := 0 to (num_edge - 2) do
      for c := (r+1) to (num_edge -1) do
          if (edges[r,c] < nodePrefs.minEdge) then
             nodePrefs.minEdge := edges[r,c];
  //Node2Mesh;
  nodePrefs.minEdgeThresh := 0;
  nodePrefs.maxEdgeThresh :=  nodePrefs.maxEdgeAbs;
  isRebuildList := true;
  result := true;
 666:
  CloseFile(f);
  strlst.free;
end; // LoadNode()

procedure TMesh.LoadObj(const FileName: string);
//WaveFront Obj file used by Blender
// https://en.wikipedia.org/wiki/Wavefront_.obj_file
label
   666;
var
   f: TextFile;
   s : string;
   strlst : TStringList;
   i, num_v, num_f, num_f_quad, num_v_alloc, num_f_alloc: integer;
   quadfaces : TFaces;//array of TPoint3i;
begin
     AssignFile(f, FileName);
     //first pass: count faces and vertices
     Reset(f);
     num_v := 0;
     num_f := 0;
     num_f_quad := 0;
     setlength(quadfaces, num_f_quad);
     strlst:=TStringList.Create;
     while not EOF(f) do begin
        readln(f,s);
        strlst.DelimitedText := s;
        if (strlst.count > 3) and ( (strlst[0]) = 'f') then inc(num_f);
        if (strlst.count > 3) and ( (strlst[0]) = 'v') then inc(num_v);
     end;
     //showmessage(inttostr(num_v)+'  '+inttostr(num_f));
     if (num_v < 3) or (num_f < 1) then begin
        CloseFile(f);
        strlst.free;
        exit;
     end;
     //second pass: load faces and vertices
     Reset(f);
     setlength(vertices, num_v);
     setlength(faces, num_f);
     num_v_alloc := num_v;
     num_f_alloc := num_f;
     num_f := 0;
     num_v := 0;
     DefaultFormatSettings.DecimalSeparator := '.';
     while not EOF(f) do begin
        readln(f,s);
        strlst.DelimitedText := s;
        if (strlst.count > 3) and ( (strlst[0]) = 'f') then begin
           //warning: need to handle "f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3"
           if (num_f >= num_f_alloc) then begin
              showmessage('Catastrophic error reading OBJ faces');
              goto 666;
           end;
           //for i := 1 to 3 do
           for i := 1 to (strlst.count-1) do
               if (pos('/', strlst[i]) > 1) then
                  strlst[i] := Copy(strlst[i], 1, pos('/', strlst[i])-1);
           faces[num_f].X := strtoint(strlst[1]) - 1;  //-1 since "A valid vertex index starts from 1"
           faces[num_f].Y := strtoint(strlst[2]) - 1;  //-1 since "A valid vertex index starts from 1"
           faces[num_f].Z := strtoint(strlst[3]) - 1;  //-1 since "A valid vertex index starts from 1"
           if strlst.count > 4 then begin //data stored as a quad: create two triangles
              setlength(quadfaces, num_f_quad+1);
              quadfaces[num_f_quad].X := strtoint(strlst[1]) - 1;  //-1 since "A valid vertex index starts from 1"
              quadfaces[num_f_quad].Y := strtoint(strlst[3]) - 1;  //-1 since "A valid vertex index starts from 1"
              quadfaces[num_f_quad].Z := strtoint(strlst[4]) - 1;  //-1 since "A valid vertex index starts from 1"
              inc(num_f_quad);
           end;
           //if num_f < 3 then
           //   showmessage(format('%d %d %d',[faces[num_f].X, faces[num_f].Y, faces[num_f].Z]));
           inc(num_f);
        end;
        if (strlst.count > 3) and ( (strlst[0]) = 'v') then begin
           if (num_v >= num_v_alloc) then begin
              showmessage('Catastrophic error reading OBJ vertices');
              goto 666;
           end;
           vertices[num_v].X := strtofloat(strlst[1]);
           vertices[num_v].Y := strtofloat(strlst[2]);
           vertices[num_v].Z := strtofloat(strlst[3]);
           //if num_v < 3 then
           //   showmessage(format('%g %g %g',[vertices[num_v].X, vertices[num_v].Y, vertices[num_v].Z]));
           inc(num_v);
        end;
     end;
     //showmessage(format('%d %g %g %g',[num_v, vertices[num_v-1].X, vertices[num_v-1].Y, vertices[num_v-1].Z]));
666:
     CloseFile(f);
     strlst.free;
     if (num_f_quad > 0) then begin //if any of the faces are quads: add 2nd triangle for each quad
        setlength(faces, num_f+num_f_quad);
        for i := 0 to (num_f_quad-1) do
            faces[i+num_f] := quadfaces[i];
     end;
end; // LoadObj()

procedure TMesh.LoadPly(const FileName: string);
// https://en.wikipedia.org/wiki/PLY_(file_format)
// http://paulbourke.net/dataformats/ply/

var
   fb: file;
   f: TextFile;
   isSwap, isVertexSection, isAscii, isLittleEndian, isUint32 : boolean;
   redOffset, greenOffset, blueOffset, AlphaOffset,
   hdrSz, sz, i, j,  num_v, num_f, num_vx, num_header_lines, vertexOffset, indexSectionExtraBytes: integer;
   str: string;
   byt: byte;
   flt: single;
   strlst : TStringList;
   i32: array [1..3] of longword;
   i16: array [1..3] of word;
   binByt: array of byte;
begin
  AssignFile(f, FileName);
  Reset(f);
  ReadLn(f, str);
  if pos('PLY', UpperCase(str)) <> 1 then begin
    showmessage('Not a PLY file');
    closefile(f);
    exit;
  end;
  strlst:=TStringList.Create;
  num_header_lines := 1;
  isAscii := false;
  isLittleEndian := false;
  isUint32 := true; //assume uint32 not short int16
  isVertexSection := false;
  indexSectionExtraBytes := 0;
  vertexOffset := 0;
  redOffset := 0; greenOffset := 0; blueOffset := 0; AlphaOffset := 0;
  while not EOF(f) do begin
     ReadLn(f, str);
     num_header_lines := num_header_lines + 1;
     if pos('END_HEADER', UpperCase(str)) = 1 then Break;
     if pos('FORMAT', UpperCase(str)) = 1 then begin
        strlst.DelimitedText := str;
        if pos('ASCII', UpperCase(strlst[1])) = 1 then
            isAscii := true;
        if pos('BINARY_LITTLE_ENDIAN', UpperCase(strlst[1])) = 1 then
            isLittleEndian := true;
     end;
     if pos('ELEMENT VERTEX', UpperCase(str)) = 1 then begin
        strlst.DelimitedText := str;
        num_v := StrToIntDef(strlst[2], 0); // "element vertex 62"
        isVertexSection := true;
     end;
     if pos('ELEMENT FACE', UpperCase(str)) = 1 then begin
        strlst.DelimitedText := str;
        num_f := StrToIntDef(strlst[2], 0); // "element face 120"
        isVertexSection := false;
     end;
     //detect "short" or "uint" from "property list uchar uint vertex_indices"

     if (isVertexSection) and (pos('PROPERTY', UpperCase(str)) = 1) then begin
        strlst.DelimitedText := str;
        if (strlst.count > 2) and (pos('RED', UpperCase(strlst[2])) = 1) then begin
           redOffset := vertexOffset;
           if (pos('UCHAR', UpperCase(strlst[1])) <> 1) then begin
             showmessage('Expected colors of data type "UCHAR", not "'+str+'"');
             closefile(f);
             exit;
           end;
        end;
        if (strlst.count > 2) and (pos('GREEN', UpperCase(strlst[2])) = 1) then
           greenOffset := vertexOffset;
        if (strlst.count > 2) and (pos('BLUE', UpperCase(strlst[2])) = 1) then
           blueOffset := vertexOffset;
        if (strlst.count > 2) and (pos('ALPHA', UpperCase(strlst[2])) = 1) then
           alphaOffset := vertexOffset;
        //showmessage(str+ inttostr(strlst.count));
        if isAscii then
          vertexOffset := vertexOffset + 1  //for ASCII we count items not bytes
        else if (pos('CHAR', UpperCase(strlst[1])) = 1) or (pos('UCHAR', UpperCase(strlst[1])) = 1) then
           vertexOffset := vertexOffset + 1
        else if (pos('SHORT', UpperCase(strlst[1])) = 1) or (pos('USHORT', UpperCase(strlst[1])) = 1) then
           vertexOffset := vertexOffset + 2
        else if (pos('INT', UpperCase(strlst[1])) = 1) or (pos('UINT', UpperCase(strlst[1])) = 1) or (pos('FLOAT', UpperCase(strlst[1])) = 1) then
           vertexOffset := vertexOffset + 4
        else if (pos('DOUBLE', UpperCase(strlst[1])) = 1) then
           vertexOffset := vertexOffset + 8
        else begin
            showmessage('Unexpected data type : "'+UpperCase(strlst[1])+'"');
            closefile(f);
            exit;
        end;
     end; //Vertex section properties
     if (not isVertexSection) and (pos('PROPERTY', UpperCase(str)) = 1) then begin
        //n.b. Wiki and MeshLab use  'VERTEX_INDICES' but Bourke uses "VERTEX_INDEX"
        strlst.DelimitedText := str;
        if (strlst.count > 4) and (pos('VERTEX_INDEX', UpperCase(strlst[4])) = 1) and (pos('SHORT', UpperCase(strlst[3])) = 1) then
           isUint32 := false
        else if (strlst.count > 4) and (pos('VERTEX_INDICES', UpperCase(strlst[4])) = 1) and (pos('SHORT', UpperCase(strlst[3])) = 1) then
           isUint32 := false
        else if (strlst.count > 2)  then begin
           if (pos('CHAR', UpperCase(strlst[1])) = 1) or  (pos('UCHAR', UpperCase(strlst[1])) = 1) then
              indexSectionExtraBytes := indexSectionExtraBytes + 1;
           if (pos('SHORT', UpperCase(strlst[1])) = 1) or  (pos('USHORT', UpperCase(strlst[1])) = 1) then
              indexSectionExtraBytes := indexSectionExtraBytes + 2;
           if (pos('INT', UpperCase(strlst[1])) = 1) or  (pos('UINT', UpperCase(strlst[1])) = 1)  or  (pos('FLOAT', UpperCase(strlst[1])) = 1) then
              indexSectionExtraBytes := indexSectionExtraBytes + 4;
           if (pos('DOUBLE', UpperCase(strlst[1])) = 1) then
              indexSectionExtraBytes := indexSectionExtraBytes + 8;
        end;
     end; //face section properties

  end;
  if EOF(f) or (num_v < 3) or (num_f < 1) then begin
    showmessage('Not a PLY file');
    closefile(f);
    exit;
  end;
  setlength(vertices, num_v);
  setlength(faces, num_f);
  if redOffset > 2 then
     setlength(vertexRGBA,num_v);
  if isAscii then begin
    if redOffset > 2 then begin
       sz := redOffset;
       if (greenOffset > sz) then sz := greenOffset;
       if (blueOffset > sz) then sz := blueOffset;
       if (alphaOffset > sz) then sz := alphaOffset;
       for i := 0 to (num_v - 1) do begin
           read(f, vertices[i].X, vertices[i].Y, vertices[i].Z);  //XYZ are items 0,1,2
           for j := 3 to (sz) do begin
               read(f, flt);
               if j = redOffset then vertexRGBA[i].R := round(flt);
               if j = greenOffset then vertexRGBA[i].G := round(flt);
               if j = blueOffset then vertexRGBA[i].B := round(flt);
               if j = alphaOffset then vertexRGBA[i].A := round(flt);
           end;
           readln(f);
       end;
    end else
        for i := 0 to (num_v - 1) do
            readln(f, vertices[i].X, vertices[i].Y, vertices[i].Z);
    for i := 0 to (num_f - 1) do begin
      readln(f, num_vx, faces[i].X, faces[i].Y, faces[i].Z);
      if num_vx <> 3 then begin
          showmessage('only able to read triangle-based PLY files. (Hint: open with MeshLab and export as CTM format)');
          closefile(f);
          exit;
      end;
    end;
    closefile(f);
  end else begin //if ASCII else Binary
    closefile(f);
    isSwap := false;
    {$IFDEF ENDIAN_LITTLE}
    if not isLittleEndian then begin
    {$ELSE}
    if isLittleEndian then begin
    {$ENDIF}
      //showmessage('unsupported binary PLY feature: swapped bytes');
      //exit;
      isSwap := true;
    end;
    if vertexOffset < 12 then begin
       showmessage('Binary PLY files should have at least 12 bytes per vertex');
       exit;
    end;
    AssignFile(fb, FileName);
    FileMode := fmOpenRead;
    Reset(fb,1);
    num_vx := 0;
    sz := filesize(fb);
    i := 0;
    while (num_vx < num_header_lines) and (i < sz) do begin
          blockread(fb, byt, 1 );
          if byt = $0A then
             num_vx := num_vx + 1;
          i := i + 1;
    end;
    hdrSz := i;
    if (num_vx < num_header_lines) then begin
       closefile(fb);
       exit;
    end;
    if vertexOffset > 12 then begin
       setlength(binByt, vertexOffset);
       for i := 0 to (num_v -1) do begin
           blockread(fb, binByt[0], vertexOffset );//sizeof(clrV) );
           vertices[i].X := asSingle(binByt[0],binByt[1],binByt[2],binByt[3]);
           vertices[i].Y := asSingle(binByt[4],binByt[5],binByt[6],binByt[7]);
           vertices[i].Z := asSingle(binByt[8],binByt[9],binByt[10],binByt[11]);
           if redOffset > 0 then begin
              vertexRGBA[i].R := binByt[redOffset];
              vertexRGBA[i].G := binByt[greenOffset];
              vertexRGBA[i].B := binByt[blueOffset];
              if alphaOffset > 0 then
                 vertexRGBA[i].A := binByt[alphaOffset];
           end;
       end;
       i := 0;
    end else
        blockread(fb, vertices[0], 3 * 4 * num_v);
    if isSwap then begin
          for i := 0 to (num_v -1) do begin
              SwapSingle(vertices[i].X);
              SwapSingle(vertices[i].Y);
              SwapSingle(vertices[i].Z);
          end;
    end; //swapped
    for i := 0 to (num_f -1) do begin

        setlength(binByt, indexSectionExtraBytes);

        blockread(fb, byt, 1 );
        if byt <> 3 then begin
                showmessage('Only able to read triangle-based PLY files. Solution: open and export with MeshLab. Index: '+inttostr(i)+ ' Header Bytes '+inttostr(hdrSz)+' bytesPerVertex: '+inttostr(vertexOffset)+' faces: ' + inttostr(byt));
                closefile(fb);
                setlength(faces,0);
                setlength(vertices,0);
                exit;
        end;
        if isSwap then begin
           if isUint32 then begin
              blockread(fb, i32[1], 3 * 4 );
              SwapLongWord(i32[1]);
              SwapLongWord(i32[2]);
              SwapLongWord(i32[3]);
              faces[i] := pti(i32[1], i32[2], i32[3]);  //winding order matches MeshLab
           end else begin
               blockread(fb, i16[1], 3 * 2 );
               faces[i] := pti(swap(i16[1]), swap(i16[2]), swap(i16[3])); //winding order matches MeshLab
           end;
        end else begin
          if isUint32 then begin
             blockread(fb, i32[1], 3 * 4 );
             faces[i] := pti(i32[1], i32[2], i32[3]);  //winding order matches MeshLab
          end else begin
              blockread(fb, i16[1], 3 * 2 );
              faces[i] := pti(i16[1], i16[2], i16[3]); //winding order matches MeshLab
          end;

        end; //is Swapped else unSwapped
        if (indexSectionExtraBytes > 0) then
           blockread(fb, binByt[0], indexSectionExtraBytes);
    end;
    closefile(fb);
   end; //if ascii else binary
   strlst.Free;
end; // LoadPly()

function KeyStringInt(key, str: string): integer;
// KeyStringInt('Dim0', 'Dim0="5124"') returns 5123
var
   s, e: integer;
   txt : string;
begin
     result := 0;
     s := pos(key, Str);
     if s < 1 then exit;
     s := posEx('"', Str, s);
     if s < 1 then exit;
     e := posEx('"', Str, s+2);
     if (e < 1) then exit;
     txt := Copy(Str, s+1, e-s-1);
     result := StrToIntDef(txt,0);
end;  // KeyStringInt()

function MemoryStreamAsString(vms: TMemoryStream): string;
//binary contents as ASCII string: http://forum.lazarus.freepascal.org/index.php?topic=15622.5;wap2
begin { MemoryStreamAsString }
   SetString(Result, vms.Memory, vms.Size)
end; { MemoryStreamAsString }

procedure TMesh.SaveObj(const FileName: string);
//create WaveFront object file
// https://en.wikipedia.org/wiki/Wavefront_.obj_file
var
   f : TextFile;
   FileNameObj: string;
   i : integer;
begin
  if (length(faces) < 1) or (length(vertices) < 3) then begin
     showmessage('You need to open a mesh before you can save it');
     exit;
  end;
  if not CheckMesh then exit;
  FileNameObj := changeFileExt(FileName, '.obj');
  AssignFile(f, FileNameObj);
  ReWrite(f);
  WriteLn(f, '# WaveFront Object format image created with Surf Ice');
  for i := 0 to (length(vertices)-1) do
      WriteLn(f, 'v ' + floattostr(vertices[i].X)+' '+floattostr(vertices[i].Y)+' '+ floattostr(vertices[i].Z));
  for i := 0 to (length(faces)-1) do
      WriteLn(f, 'f ' + inttostr(faces[i].X+1)+' '+inttostr(faces[i].Y+1)+' '+ inttostr(faces[i].Z+1)); //+1 since "A valid vertex index starts from 1 "
  //fprintf(fid, '# WaveFront Object format image created with MRIcroS\n');
  //fprintf(fid, 'v %.12g %.12g %.12g\n', vertex');
  //fprintf(fid, 'f %d %d %d\n', (face)');
  CloseFile(f);
end;

procedure SaveMz3Core(const FileName: string; Faces: TFaces; Vertices: TVertices; vertexRGBA: TVertexRGBA; intensity: TFloats);
const
 kMagic =  23117; //"MZ"
var
  isFace, isVert, isRGBA, isScalar: boolean;
  Magic, Attr: uint16;
  nFace, nVert, nSkip: uint32;
  mStream : TMemoryStream;
  zStream: TGZFileStream;
  FileNameMz3: string;
begin
  FileNameMz3 := changeFileExt(FileName, '.mz3');
  nFace := length(Faces);
  isFace := nFace > 0;
  nVert := length(Vertices);
  isVert := nVert > 0;
  isRGBA := false;
  if length(vertexRGBA) > 0 then begin
     isRGBA := true;
     nVert := length(vertexRGBA);
  end;
  isScalar := false;
  if length(intensity) > 0 then begin
     isScalar := true;
     nVert := length(intensity);
  end;
  if (nFace = 0) and (nVert = 0)  then exit;
  magic := kMagic;
  Attr := 0;
  if isFace then Attr := Attr + 1;
  if isVert then Attr := Attr + 2;
  if isRGBA then Attr := Attr + 4;
  if isScalar then Attr := Attr + 8;
  nSkip := 0; //do not pad header with any extra data
  mStream := TMemoryStream.Create;
  mStream.Write(Magic,2);
  mStream.Write(Attr,2);
  mStream.Write(nFace,4);
  mStream.Write(nVert,4);
  mStream.Write(nSkip,4);
  if isFace then
     mStream.Write(Faces[0], nFace * sizeof(TPoint3i));
  if isVert  then
     mStream.Write(Vertices[0], nVert * 3 * sizeof(single));
  if isRGBA then
     mStream.Write(vertexRGBA[0], nVert * 4 * sizeof(byte));
  if isScalar then
     mStream.Write(intensity[0], nVert * sizeof(single));
  mStream.Position := 0;
  zStream := TGZFileStream.Create(FileNameMz3, gzopenwrite);
  zStream.CopyFrom(mStream, mStream.Size);
  zStream.Free;
  mStream.Free;
end;

procedure TMesh.SaveMz3(const FileName: string);
var
  i: TFloats;
begin
  setlength(i,0);
  SaveMz3Core(Filename, Faces,Vertices,vertexRGBA, i);

end;

procedure TMesh.SaveOverlay(const FileName: string; OverlayIndex: integer);
var
  f: TFaces;
  v: TVertices;
  c: TVertexRGBA;
  i: TFloats;
begin
  if OverlayIndex > OpenOverlays then exit;
  setlength(f,0);
  setlength(v,0);
  setlength(c,0);
  setlength(i,0);
  if (length(overlay[OverlayIndex].faces) > 0) and (length(overlay[OverlayIndex].vertices) > 0) then
     SaveMz3Core(Filename, overlay[OverlayIndex].Faces, overlay[OverlayIndex].Vertices,c,i)
  else if length(overlay[OverlayIndex].intensity) > 1 then
     SaveMz3Core(Filename, f,v,c, overlay[OverlayIndex].intensity);
end;

procedure TMesh.SaveGii(const FileName: string);
var
   f : TextFile;
   FileNameGii : string;
   comp: TCompressionstream;
   outStream    : TMemoryStream;
   nBytes : integer;
   base64: string;
begin
  FileNameGii := changeFileExt(FileName, '.gii');
  if (length(faces) < 1) or (length(vertices) < 3) then begin
     showmessage('You need to open a mesh before you can save it');
     exit;
  end;
  if not CheckMesh then exit;
  AssignFile(f, FileNameGii);
  ReWrite(f);
  WriteLn(f, '<?xml version="1.0" encoding="UTF-8"?>');
  WriteLn(f, '<!DOCTYPE GIFTI SYSTEM "http://www.nitrc.org/frs/download.php/115/gifti.dtd">');
  WriteLn(f, '<GIFTI Version="1.0"  NumberOfDataArrays="2">');
  WriteLn(f, '   <MetaData/>');
  WriteLn(f, '   <LabelTable/>');
  WriteLn(f, '   <DataArray  ArrayIndexingOrder="RowMajorOrder"'); // ColumnMajorOrder for transposed data
  WriteLn(f, '               DataType="NIFTI_TYPE_INT32"');
  WriteLn(f, '               Dim0="',inttostr(length(faces)) ,'"');
  WriteLn(f, '               Dim1="3"');
  WriteLn(f, '               Dimensionality="2"');
  WriteLn(f, '               Encoding="GZipBase64Binary" ');
 {$IFDEF ENDIAN_LITTLE}
  WriteLn(f, '               Endian="LittleEndian"');
 {$ELSE}
  WriteLn(f, '               Endian="BigEndian"');
 {$ENDIF}
  WriteLn(f, '               ExternalFileName=""');
  WriteLn(f, '               ExternalFileOffset=""');
  WriteLn(f, '               Intent="NIFTI_INTENT_TRIANGLE"> ');
  WriteLn(f, '      <MetaData>');
  WriteLn(f, '      </MetaData>');
    nBytes :=  length(faces) * 12;
    outStream := TMemoryStream.Create;
    comp := TCompressionStream.Create(clMax, outStream);
    comp.Write(faces[0], nBytes);
    comp.Free;
    base64 :=  EncodeStringBase64(MemoryStreamAsString(outStream));
    //showmessage('face compression: '+inttostr(nBytes)+' -> '+inttostr(outStream.Size)+' -> '+inttostr(length(base64)));
    outStream.Free;
  WriteLn(f, '      <Data>', base64, '</Data>');
  WriteLn(f, '   </DataArray>');
  WriteLn(f, '   <DataArray  ArrayIndexingOrder="RowMajorOrder"');  // ColumnMajorOrder for transposed data
  WriteLn(f, '               DataType="NIFTI_TYPE_FLOAT32"');
  WriteLn(f, '               Dim0="',inttostr(length(vertices)) ,'"');
  WriteLn(f, '               Dim1="3"');
  WriteLn(f, '               Dimensionality="2"');
  WriteLn(f, '               Encoding="GZipBase64Binary"');
  {$IFDEF ENDIAN_LITTLE}
   WriteLn(f, '               Endian="LittleEndian"');
  {$ELSE}
   WriteLn(f, '               Endian="BigEndian"');
  {$ENDIF}
  WriteLn(f, '               ExternalFileName=""');
  WriteLn(f, '               ExternalFileOffset=""');
  WriteLn(f, '               Intent="NIFTI_INTENT_POINTSET">');
  WriteLn(f, '      <MetaData>');
  WriteLn(f, '      </MetaData>');
  WriteLn(f, '      <CoordinateSystemTransformMatrix>');
  WriteLn(f, '         <DataSpace><![CDATA[NIFTI_XFORM_UNKNOWN]]></DataSpace>');
  WriteLn(f, '         <TransformedSpace><![CDATA[NIFTI_XFORM_UNKNOWN]]></TransformedSpace>');
  WriteLn(f, '         <MatrixData>1.000000 0.000000 0.000000 0.000000 0.000000 1.000000 0.000000 0.000000 0.000000 0.000000 1.000000 0.000000 0.000000 0.000000 0.000000 1.000000 </MatrixData>');
  WriteLn(f, '      </CoordinateSystemTransformMatrix>');
    nBytes :=  length(vertices) * 12;
    outStream := TMemoryStream.Create;
    comp := TCompressionStream.Create(clMax, outStream);
    comp.Write(vertices[0], nBytes);
    comp.Free;
    base64 :=  EncodeStringBase64(MemoryStreamAsString(outStream));
    //showmessage('vertex compression: '+inttostr(nBytes)+' -> '+inttostr(outStream.Size)+' -> '+inttostr(length(base64)));
    outStream.Free;
  WriteLn(f, '      <Data>',base64, '</Data>');
  WriteLn(f, '   </DataArray>');
  WriteLn(f, '</GIFTI>');
  CloseFile(f);
end;

//Next: GIfTI specific types
type
    TLongIntArray = array of LongInt;
TGiiLabel = record
   index: Longint;
   R,G,B,A: byte;
 end;
TGiiLabelArray = array of TGiiLabel;

procedure TransposeDat (var v: TLongIntArray);
var
   n, i,j, nr,nc: integer;
   vIn: TLongIntArray;
begin
     n :=  Length(v);
     vIn := Copy(v, Low(v), n);
     nr := 3;
     nc := n div nr;
     j := 0;
     for i := 0 to (nc-1) do begin
        v[j] := vIn[i]; inc(j);
        v[j] := vIn[i+nc]; inc(j);
        v[j] := vIn[i+nc+nc]; inc(j);
    end;
end;

function ParseFloat(key : string; var Str: string): double;
// ParseFloat('Index="', '<Label Index="14474380"' ) will return 14474380
var
   vStart,vEnd : integer;
begin
  result := 1;
  vStart := posEx(key, Str);
  if vStart < 1 then exit;
  vStart := vStart + length(key);
  vEnd := posEx('"', Str, vStart+1);
  if vEnd <= vStart then exit;
  //showmessage( Copy(Str, vStart, vEnd-vStart) );
  result := strToFloatDef(Copy(Str, vStart, vEnd-vStart), 1);
end;

function readLabelTable(Str: string): TGiiLabelArray;
var
   tStart, tEnd, lStart,lEnd, nLabel: integer;
   Hdr: string;
begin
  setlength(result,0);
  tEnd := posEx('</LabelTable>', Str); // header end
  if tEnd < 1 then exit;
  tStart := pos('<LabelTable>', Str); //read first data array
  if (tStart < 1) or (tStart > tEnd) then exit;
  //first pass: count number of labels
  lEnd := tStart+8;
  nLabel := 0;
  repeat
        lStart := posEx('<Label', Str, lEnd);
        lEnd := posEx('</Label>', Str, lEnd);
        if (lEnd > 0) and (lStart > 0) then begin
           lEnd := lEnd + 8;
           nLabel := nLabel + 1;
        end;
  until (lEnd < 1) or (lEnd > tEnd) ;
  //showmessage(inttostr(nLabel));
  if nLabel < 1 then exit;
  //second pass: read labels into dynamic array
  setlength(result,nLabel);
  lEnd := tStart+8;
  nLabel := 0;
  repeat
        lStart := posEx('<Label', Str, lEnd);
        lEnd := posEx('</Label>', Str, lEnd);
        if (lEnd > 0) and (lStart > 0) then begin
           Hdr := Copy(Str, lStart, lEnd);
           result[nLabel].index := round(ParseFloat('Index="', Hdr));
           if result[nLabel].index = 1 then
              result[nLabel].index := round(ParseFloat('Key="', Hdr));
           result[nLabel].R := round(255*ParseFloat('Red="', Hdr));
           result[nLabel].G := round(255*ParseFloat('Green="', Hdr));
           result[nLabel].B := round(255*ParseFloat('Blue="', Hdr));
           result[nLabel].A := round(255*ParseFloat('Alpha="', Hdr));
           lEnd := lEnd + 8;
           nLabel := nLabel + 1;
        end;
  until (lEnd < 1) or (lEnd > tEnd) ;
end;

function TMesh.LoadGii(const FileName: string; lOverlayIndex, lOverlayItem: integer): integer;
label
   666;
var
  decomp :  Tdecompressionstream;
  gz:  TMemoryStream;
  f: file;
  labelTable : TGiiLabelArray;
  Str, Hdr, debase64: string;
  dat : TLongIntArray;
  s: single;
  i,j, szExpected, szRead,  daStart, dhEnd, daEnd, ddStart,ddEnd, Dim0, Dim1, nOverlays: integer;
  isOverlay, isAscii, isVertColor, isInt32, isFloat32, isBase64, isBase64Gz, isLEndian, isFace, isVert, isTransposed: boolean;
begin
  result := 0;
  nOverlays := 0;
  gz := TMemoryStream.Create;
  AssignFile(f, FileName);
  Reset(f,1);
  FileMode := fmOpenRead;
  szRead := FileSize(f);
  SetLength(Str, szRead);
  BlockRead(f, Str[1],szRead);
  CloseFile(f);
  labelTable := readLabelTable(Str);
  daStart := pos('<DataArray', Str); //read first data array
  while daStart > 0 do begin //read each dataArray
    dhEnd := posEx('>', Str, daStart); // header end
    daEnd := posEx('</DataArray>', Str, daStart); // data array end
    ddStart := posEx('<Data>', Str, daStart) + 6; // data start
    ddEnd := posEx('</Data>', Str, daStart); // data end
    if (dhEnd < 1) or (daEnd < 1) then goto 666;
    Hdr := Copy(Str, daStart, dhEnd-daStart+1);
    isLEndian :=  pos('Endian="LittleEndian"', Hdr) > 0;
    isAscii :=  pos('Encoding="ASCII"', Hdr) > 0;
    isBase64Gz :=  pos('Encoding="GZipBase64Binary"', Hdr) > 0;
    isBase64 :=  pos('Encoding="Base64Binary"', Hdr) > 0;
    Dim0 := KeyStringInt('Dim0', Hdr);
    Dim1 := KeyStringInt('Dim1', Hdr);
    isInt32 :=  pos('DataType="NIFTI_TYPE_INT32"', Hdr) > 0;
    if not isInt32 then
       isInt32 :=  pos('DataType="NIFTI_TYPE_UINT32"', Hdr) > 0;
    isFloat32 :=  pos('DataType="NIFTI_TYPE_FLOAT32"', Hdr) > 0;
    isFace := pos('Intent="NIFTI_INTENT_TRIANGLE"', Hdr) > 0; //faces
    isVert := pos('Intent="NIFTI_INTENT_POINTSET"', Hdr) > 0; //vertices
    isVertColor := pos('Intent="NIFTI_INTENT_LABEL"', Hdr) > 0; //vertex colors
    isOverlay :=  pos('Intent="NIFTI_INTENT_NONE"', Hdr) > 0; //??? Lets hope this is a fMRI statistical map
    if not isOverlay then
       isOverlay :=  pos('Intent="NIFTI_INTENT_SHAPE"', Hdr) > 0; //curvature map
    if not isOverlay then
       isOverlay :=  pos('Intent="NIFTI_INTENT_TTEST"', Hdr) > 0; //curvature map
    if (isOverlay) and  (Dim1 = 3) then begin
       isVertColor := true;
       isOverlay := false;
    end;
    if isOverlay then
       nOverlays := nOverlays + 1;
    isTransposed := pos('ColumnMajorOrder"', Hdr) > 0;
    if (Dim1 = 0) then Dim1 := 1;
    if (isOverlay) and (lOverlayIndex = 0) then begin
       Showmessage('Please load GIfTI mesh (using File/Open) BEFORE loading the GIFTI overlay (using Overlay/Add)');
       goto 666;
    end;
    if (isOverlay) and (Dim0 <> length(vertices)) then begin
       Showmessage(format('GIFTI overlay has a different number of vertices than the background mesh (%d vs %d)',[Dim0 , length(vertices)]));
       goto 666;
    end;
    if (isVert and isAscii) or (isOverlay and isAscii)  or (isVertColor and isAscii) or (isFace and isVertColor) then begin
       Showmessage('Unable to read GIFTI files with ASCII data. Solution: convert to more efficient BINARY data');
       goto 666;
    end;
    if ((isVert) and (isFloat32) and (Dim1 = 3)) or ((lOverlayItem = nOverlays) and (isOverlay) and (isFloat32) and (Dim1 = 1))
      or ((isVertColor) and (isFloat32) and (Dim1 = 3))  or ((isVertColor) and (isInt32) and (Dim1 = 1)) or ((isFace) and (isInt32) and (Dim1 = 3)) then begin
       if  ((isBase64) or (isBase64Gz))  and (Dim0 > 0) and (ddStart > 6) and (ddEnd > ddStart) and (isLEndian) then begin
        debase64 :=  DecodeStringBase64(Copy(Str, ddStart, ddEnd-ddStart)); //raw GZ binary, see  http://lazarus-ccr.sourceforge.net/docs/fcl/base64/decodestringbase64.html
        if (Dim1 <> 3) and (isVertColor) and (length(labelTable) < 1) then begin
           showmessage('Error found Intent="NIFTI_INTENT_LABEL" without a "<LabelTable>"');
           goto 666;
        end;
        if (length(debase64) < 4)  then begin
           showmessage('Impossibly small deflated stream');
           goto 666;
        end;
        szExpected :=  4 * dim0 * Dim1;
        if isBase64Gz then begin
           if (ord(debase64[1]) <> $78) then begin
            showmessage('Deflate compressed stream should begin with 0x78, not '+inttohex(ord(debase64[1]), 2));
            goto 666;
          end;
          gz.Write(debase64[1], length(debase64));
          debase64 := ''; // free memory
          gz.Position := 0;
          decomp := Tdecompressionstream.Create(gz);
          setlength(dat, Dim0 * 3);
          szRead := decomp.Read(dat[0], szExpected);
          decomp.Free;
          gz.Clear;
        end else begin
            szRead := length(debase64);
            setlength(dat,szExpected);
            Move(debase64[1], dat[0], szExpected);
        end;
          if szExpected <> szRead then begin
             showmessage(format('decompressed size incorrect %d != %d',[szExpected, szRead]));
             goto 666;
          end;
          if (isTransposed) and (Dim1 = 3) then
             TransposeDat(dat);
          if isVertColor then begin
             setlength(vertexRGBA,Dim0);
             if (Dim1 = 3) then begin
                s := asSingle(dat[0]);
                for j := 0 to ((Dim0*3)-1) do
                    if asSingle(dat[j]) > s then
                       s := asSingle(dat[j]);
                if s > 1 then
                   s := 1
                else
                    s := 255;
                j := 0;
                for i := 0 to (Dim0-1) do begin
                    vertexRGBA[i].R := round(s *asSingle(dat[j])); inc(j);
                    vertexRGBA[i].G := round(s *asSingle(dat[j])); inc(j);
                    vertexRGBA[i].B := round(s *asSingle(dat[j])); inc(j);
                end;
             end else begin
               for i := 0 to (Dim0-1) do begin
                   j := 0;
                   while (j < length(labelTable)) and (dat[i] <> labelTable[j].index) do
                         j := j + 1; //find label: note label table indices often not sequential see NITRC example lh.aparc.gii
                   vertexRGBA[i].R := labelTable[j].R;
                   vertexRGBA[i].G := labelTable[j].G;
                   vertexRGBA[i].B := labelTable[j].B;
                   vertexRGBA[i].A := labelTable[j].A;
               end;

             end;
          end else if isOverlay then begin
              setlength(overlay[lOverlayIndex].intensity, length(vertices));
              for i := 0 to (Dim0-1) do
                  overlay[lOverlayIndex].intensity[i] := asSingle(dat[i]);
              //vxt(dat);
          end
        else if isFace then begin
            if lOverlayIndex > 0 then begin
              setlength(overlay[lOverlayIndex].faces, Dim0);
               j := 0;
               for i := 0 to (Dim0-1) do begin
                   overlay[lOverlayIndex].faces[i].X := dat[j]; inc(j);
                   overlay[lOverlayIndex].faces[i].Y := dat[j]; inc(j);
                   overlay[lOverlayIndex].faces[i].Z := dat[j]; inc(j);
               end;
            end else begin
               setlength(faces, Dim0);
                j := 0;
                for i := 0 to (Dim0-1) do begin
                    faces[i].X := dat[j]; inc(j);
                    faces[i].Y := dat[j]; inc(j);
                    faces[i].Z := dat[j]; inc(j);
                end;
            end;
          end else if isVert then begin
              if lOverlayIndex > 0 then begin
                setlength(overlay[lOverlayIndex].vertices, Dim0);
                j := 0;
                for i := 0 to (Dim0-1) do begin
                   overlay[lOverlayIndex].vertices[i].X := asSingle(dat[j]); inc(j);
                   overlay[lOverlayIndex].vertices[i].Y := asSingle(dat[j]); inc(j);
                   overlay[lOverlayIndex].vertices[i].Z := asSingle(dat[j]); inc(j);
                end;
              end else begin
                setlength(vertices, Dim0);
                j := 0;
                for i := 0 to (Dim0-1) do begin
                   vertices[i].X := asSingle(dat[j]); inc(j);
                   vertices[i].Y := asSingle(dat[j]); inc(j);
                   vertices[i].Z := asSingle(dat[j]); inc(j);
                end;
              end;
          end;
          setlength(dat, 0);  //release
        end; //valid format
    end; //if vertex of face data
    daStart := posEx('<DataArray', Str, daEnd); //attempt to read next dataArray
  end; //while true
  str := ''; //free
  gz.Free;
  if (length(vertexRGBA) > 0) and (length(vertices) = 0) then begin
     showmessage('Please load GIfTI mesh (using File/Open) BEFORE loading the GIFTI labels (using Overlay/Add)');
     setlength(vertexRGBA,0);
  end;
  if (length(vertexRGBA) > 0) and (length(vertexRGBA) <> length(vertices)) then begin
     showmessage('Number of vertices in NIFTI_INTENT_LABEL does not  number of vertices in NIFTI_INTENT_POINTSET');
     setlength(vertexRGBA,0);
  end;
  if  (length(vertices) < 3) and (length(faces) > 0) then begin
      showmessage('Error: this GIFTI image describes faces but not vertices. Solution: merge separate GIFTI files into a single file.');
      goto 666;
  end;
  if  (length(vertices) > 2) and (length(faces) < 1) then begin
      showmessage('Error: this GIFTI image describes vertices but not faces. Solution: merge separate GIFTI files into a single file.');
      goto 666;
  end;
  if (length(vertices) < 3) or (length(faces) < 1) then begin //just in case vertices OR faces set but not both
     showmessage('Unable to interpret this GIFTI file: it does not seem to describe faces, vertices, labels or intensities');
     goto 666;
  end;
  isRebuildList := true;
  if nOverlays > 0 then
     result := nOverlays
  else
      result := 1;
  exit;
 666:
 setlength(faces, 0); //just in case faces were read but not vertices
 setlength(vertices, 0);
 setlength(vertexRGBA,0);
end;

procedure TMesh.LoadCtm(const FileName: string);
begin
  setlength(faces, 0); //just in case faces were read but not vertices
  setlength(vertices, 0);
  setlength(vertexRGBA,0);
  {$IFDEF CTM} readCTM(FileName, Faces, Vertices, vertexRGBA); {$ENDIF}
 //readCTM(const FileName: string; var Faces: TFaces;  var Verts: TVertices; var vertexRGBA : TVertexRGBA): boolean;
end;


function LoadMz3Core(const FileName: string; var Faces: TFaces; var Vertices: TVertices; var vertexRGBA: TVertexRGBA; var intensity: TFloats): boolean;
const
 kMagic =  23117; //"MZ"
 kChunkSize = 16384;
label 666;
var
  i: integer;
  bytes : array of byte;
  Magic, Attr: uint16;
  nFace, nVert, nSkip: uint32;
  isFace, isVert, isRGBA, isScalar: boolean;
  mStream : TMemoryStream;
  zStream: TGZFileStream;
begin
     result := false;
     setlength(Faces,0);
     setlength(Vertices,0);
     setlength(vertexRGBA,0);
     setlength(intensity,0);
     if not fileexists(Filename) then exit;
     mStream := TMemoryStream.Create;
     zStream := TGZFileStream.create(FileName, gzopenread);
     setlength(bytes, kChunkSize);
     repeat
            i := zStream.read(bytes[0],kChunkSize);
            mStream.Write(bytes[0],i) ;
     until i < kChunkSize;
     zStream.Free;
     if mStream.Size < 72 then exit; //6 item header, 3 vertices (3 values, XYZ), 1 face (3 indices) each 4 bytes
     mStream.Position := 0;
     mStream.Read(Magic,2);
     mStream.Read(Attr,2);
     mStream.Read(nFace,4);
     mStream.Read(nVert,4);
     mStream.Read(nSkip,4);
     if (magic <> kMagic) then goto 666;
     isFace := (Attr and 1) > 0;
     isVert := (Attr and 2) > 0;
     isRGBA := (Attr and 4) > 0;
     isScalar := (Attr and 8) > 0;
     if (Attr > 15) then begin
        showmessage('Unsupported future format '+ inttostr(Attr));
        goto 666;
     end;
     if (nFace = 0) and (isFace) then goto 666;
     if (nVert = 0) and ((isVert) or (isRGBA) or (isScalar) ) then goto 666;
     if nSkip > 0 then
        mStream.Seek(nSkip, soFromCurrent);
     result := true;
     if isFace then begin
        setlength(Faces,  nFace);
        mStream.Read(Faces[0], nFace * 3 * sizeof(int32));
     end;
     if isVert then begin
        setlength(Vertices,  nVert);
        mStream.Read(Vertices[0], nVert * 3 * sizeof(single));
     end;
     if isRGBA then begin
        setlength(vertexRGBA, nVert);
        mStream.Read(vertexRGBA[0], nVert * 4 * sizeof(byte));
     end;
     if isScalar then begin
        setlength(intensity, nVert);
        mStream.Read(intensity[0], nVert * sizeof(single));
     end;
     result := true;
   666 :
     mStream.Free;
end; //loadMZ3

function TMesh.LoadMz3(const FileName: string; lOverlayIndex : integer): boolean;
var
  floats: TFloats;
  vtxRGBA: TVertexRGBA;
begin
  if lOverlayIndex < 1 then begin //save main image
     result := LoadMz3Core(Filename, Faces,Vertices,vertexRGBA,floats);
     if (length(Faces) < 1) and (length(floats) > 0) then
        Showmessage('Please load mesh (using File/Open) BEFORE loading the overlay (using Overlay/Add)');
     setlength(floats,0);
     exit;
  end;
  //otherwise, load overlay
  result := LoadMz3Core(Filename, Overlay[lOverlayIndex].Faces,Overlay[lOverlayIndex].Vertices,vtxRGBA,Overlay[lOverlayIndex].intensity);
  setlength(vtxRGBA,0);
  if not result then exit;
  if (length(Overlay[lOverlayIndex].intensity) > 0) and (length(overlay[lOverlayIndex].intensity)  <> length(Vertices)) then begin
     showmessage(format('This overlay has a different number of vertices (%d) than the background mesh (%d)', [length(overlay[lOverlayIndex].intensity), length(Vertices) ]));
     setlength(overlay[lOverlayIndex].intensity, 0);
     result := false;
  end;
end; //LoadMz3()

procedure TMesh.LoadStl(const FileName: string);
//Read STL mesh
// https://en.wikipedia.org/wiki/STL_(file_format)
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bstl/readStl.m
// similar to Doron Harlev http://www.mathworks.com/matlabcentral/fileexchange/6678-stlread
var
   f: file;
   triValues: array [1..9] of single;//float32
   txt80: array [1 .. 80] of char; //header
   num_f : LongWord; //uint32
   colorBytes: word; //uint16
   i, v, sz, min_sz: integer;
begin
     AssignFile(f, FileName);
     FileMode := fmOpenRead;
     Reset(f,1);
     sz := FileSize(f);
     if sz < 134 then begin//smallest file is 84 byte header + 50 byte triangle
        ShowMessage(format('File too small to be STL format: %s', [FileName]));
        CloseFile(f);
        exit;
     end;
     blockread(f, txt80, SizeOf(txt80) );
     if pos('SOLID', UpperCase(txt80)) = 1 then begin //this is a TEXT stl file
        Showmessage(format('Error: only able to read binary STL files (not ASCII text). Please convert your image: %s', [FileName]));
        CloseFile(f);
        exit;
    end;
    blockread(f, num_f, 4 ); //uint32
    //50 bytes for each triangle there are 12 fp32 and 1 unit16
    min_sz := 80+4+(50*num_f);
    if sz < min_sz then begin
       Showmessage(format('STL file too small (expected at least %d bytes) %s', [min_sz, FileName]));
       CloseFile(f);
       exit;
    end;
    setlength(vertices, num_f * 3);  //vertices = zeros(num_f, 9);
    v := 0;
    triValues[1] := 0;//unused: this line prevents compiler warning
    colorBytes := 0;//unused: this line prevents compiler warning
    for i := 1 to num_f do begin
        blockread(f, triValues, 4 * 3 ); ////fread(fid,3,'float32'); % normal coordinates, ignore
        blockread(f, triValues, 4 * 9 );  //vertices(i,:)=fread(fid,9,'float32'); %read vertex from triangles
        vertices[v+0] := ptf(triValues[1], triValues[2], triValues[3]);
        vertices[v+1] := ptf(triValues[4], triValues[5], triValues[6]);
        vertices[v+2] := ptf(triValues[7], triValues[8], triValues[9]);
        v := v + 3;
        blockread(f, colorBytes, 2);//fread(fid,1,'uint16'); % color bytes
    end;
    setlength(faces, num_f);
    v := 0;
    for i := 0 to (num_f-1) do begin
        faces[i] := pti(v,v+1,v+2);
        v := v + 3;
    end;
    CloseFile(f);
    CheckMesh; //Since STL requires vertex unification, make sure vertices and faces are valid
    UnifyVertices(faces, vertices);
end;

procedure TMesh.LoadVtk(const FileName: string);
//Read VTK mesh
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bvtk/readVtk.m
// http://www.ifb.ethz.ch/education/statisticalphysics/file-formats.pdf
// ftp://ftp.tuwien.ac.at/visual/vtk/www/FileFormats.pdf
//  "The VTK data files described here are written in big endian form"
label
   666;
var
   f: TFByte;//TextFile;
   strlst: TStringList;
   str: string;
   i, num_v, num_f, cnt: integer;
   nV: LongInt;
begin
  AssignFile(f, FileName);
  Reset(f,1);
  ReadLnBin(f, str); //signature: '# vtk DataFile'
  if pos('VTK', UpperCase(str)) <> 3 then begin
    showmessage('Not a VTK file');
    goto 666;
  end;
  ReadLnBin(f, str); //comment: 'Comment: created with MRIcroS'
  ReadLnBin(f, str); //kind: 'BINARY' or 'ASCII'
  if pos('BINARY', UpperCase(str)) <> 1 then begin  // '# vtk DataFile'
     showmessage('Only able to read binary VTK file (convert with MRIcroS or another tool):'+str);
     goto 666;
  end;
  ReadLnBin(f, str); // kind, e.g. "DATASET POLYDATA" or "DATASET STRUCTURED_ POINTS"
  if pos('POLYDATA', UpperCase(str)) = 0 then begin
    showmessage('Only able to read VTK images saved as POLYDATA, not '+ str);
    goto 666;
  end;
  ReadLnBin(f, str); // number of vertices, e.g. "POINTS 685462 float"
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
  num_v := num_v;
  setlength(vertices, num_v); //vertices = zeros(num_f, 9);
  blockread(f, vertices[0], 3 * 4 * num_v);
  ReadLnBin(f, str); // number of vertices, e.g. "POLYGONS 1380 5520"
  if str = '' then ReadLnBin(f, str);
  if pos('LINES', UpperCase(str)) > 0 then begin
    showmessage('This is a fiber file: rename with a ".fib" extension and use Tracks/Open to view: '+ str);
    goto 666;
  end;
  if pos('POLYGONS', UpperCase(str)) <> 1 then begin
    showmessage('Expected header to report "POLYGONS" not '+ str);
    goto 666;
  end;
  strlst.DelimitedText := str;
  num_f := StrToIntDef(strlst[1],0);
  cnt := StrToIntDef(strlst[2],0);
  strlst.free;
  if cnt <> (num_f * 4) then begin
     showmessage('Only able to read triangular meshes, not '+ str);
     goto 666;
  end;
  setlength(faces, num_f);
  for i := 0 to (num_f -1) do begin
      blockread(f, nV, sizeof(LongInt));
      {$IFDEF ENDIAN_LITTLE}
      SwapLongInt(nV);
      {$ENDIF}
      if (nV <> 3) then begin
         showmessage('VTK file is borked');
         goto 666;
      end;
      blockread(f, faces[i],  3 * 4);
  end;
  closefile(f);
  {$IFDEF ENDIAN_LITTLE} // VTK is ALWAYS big endian!
     for i := 0 to (num_f -1) do begin
         SwapLongInt(faces[i].X);
         SwapLongInt(faces[i].Y);
         SwapLongInt(faces[i].Z);
     end;
     for i := 0 to (num_v -1) do begin
         SwapSingle(vertices[i].X);
         SwapSingle(vertices[i].Y);
         SwapSingle(vertices[i].Z);
     end;
 {$ENDIF}
  exit;
666:
   closefile(f);
end;

function TMesh.CheckMesh: boolean;
//faces should be indexed for range 0..[number of triangles -1]
var
   i, mn, mx: integer;
   isNan: boolean;
begin
     result := true;
     if (length(vertices) < 3) or (length(faces) < 1) then exit;
     mx := faces[0].X;
     mn := mx;
     for i := 0 to (length(faces)-1) do begin
         MinMax(faces[i], mn, mx);
     end;
     if (mn <> 0) and (mx >= length(vertices)) then begin //incase faces indexed from 1, not 0!
        //Only do this if required - see Skull.ply which has unused vertex[0] http://people.sc.fsu.edu/~jburkardt/data/ply/ply.html
        for i := 0 to (length(faces)-1) do begin
            faces[i].X := faces[i].X - mn;
            faces[i].Y := faces[i].Y - mn;
            faces[i].Z := faces[i].Z - mn;
        end;
        mx := mx - mn;
        mn := mn - mn;
     end;
     if (mn < 0) or (mx >= length(vertices)) then begin
        showmessage('Error: mesh does not make sense: '+inttostr(length(vertices))+' vertices but '+inttostr(mx-mn+1)+' indices ('+inttostr(mn)+'..'+inttostr(mx)+')');
        result := false;
     end;
     isNan := false;
     for i := 0 to (length(vertices) -1) do
         if specialsingle( vertices[i].X) or specialsingle( vertices[i].Y) or specialsingle( vertices[i].Z) then
            isNan := true;
     if isNan then begin
        showmessage('Vertices are corrupted (infinity of NaN values)');
        result := false;
     end;
     if not result then
        MakePyramid;
     result := true;
end;

(*procedure TMesh.NormalizeSize;
var
   i: integer;
begin
     if (length(vertices) < 3) or (length(faces) < 1) then exit;
     if scale = 0 then exit;
     for i := 0 to (length(vertices)-1) do begin
         vertices[i].X := (vertices[i].X - origin.X)/ scale;
         vertices[i].Y := (vertices[i].Y - origin.Y)/ scale;
         vertices[i].Z := (vertices[i].Z - origin.Z)/ scale;
     end;
     SetDescriptives; //should yield scale := 1.0; origin  := ptf(0, 0, 0);
end; *)

function TMesh.LoadFromFile(const FileName: string): boolean;
var
   ext: string;
begin
  result := false;
     if not FileExists(FileName) then exit;
     isBusy := true;
     isNode := false;
     isFreeSurferMesh := false;
     ext := UpperCase(ExtractFileExt(Filename));
     isRebuildList := true;
     CloseOverlays;
     setlength(faces, 0);
     setlength(vertices, 0);
     if (ext = '.MZ3') then
        LoadMz3(Filename, 0);
     if (ext = '.CTM') then
         LoadCtm(Filename);
     if (ext = '.NV') then
         LoadNv(Filename);
     if (ext = '.OFF') then
         LoadOff(Filename);
     if (ext = '.PLY') then
         LoadPly(Filename);
     if (ext = '.GII') then
         LoadGii(Filename,0, 1);
     if (ext = '.VTK') then
         LoadVtk(Filename);
     if (ext = '.STL') then
         LoadStl(Filename);
     if (ext = '.NODE') then
         LoadNode(Filename);
     if (ext = '.SRF') or (ext = '.ASC') then
         LoadAsc_Srf(Filename);
     if (ext = '.OBJ') then
         LoadObj(Filename);
     if length(faces) < 1 then begin//not yet loaded - see if it is freesurfer format (often saved without filename extension)
        LoadPial(Filename);
        if length(faces) > 0 then
           isFreeSurferMesh := true;
     end;
     if length(faces) < 1 then //error loading file
        MakePyramid
     else
         result := true;
     CheckMesh;
     SetDescriptives;
     if not isZDimIsUp then
        SwapYZ;
     //showmessage(Filename+'  '+ inttostr(length(faces))+' '+inttostr(length(vertices)) );
     //NormalizeSize;
  isBusy := false;
end;

(* works, but Stc files are sparse, so better to use other routines for smoothing
procedure TMesh.LoadStc(const FileName: string; lOverlayIndex: integer);
//freesurfer STC format
//http://maki.bme.ntu.edu.tw/wp-content/uploads/2014/08/inverse_read_stc.m
var
   f: File;
   v, t, i, sz,mxt, n_timeXn_vertex: integer;
    mn, mx, epoch_begin_latency, sample_period: single;
   n_time, n_vertex, num_v, num_f: LongWord;
   vertex_data : array of longword;
   stc: array of single;
begin
     AssignFile(f, FileName);
     FileMode := fmOpenRead;
     Reset(f,1);
     sz := FileSize(f);
     if (sz < 20) then begin
        CloseFile(f);
        exit;
     end;
     blockread(f, epoch_begin_latency, 4); //since these files do not have a file extension, check first 8 bytes "0xFFFFFE creat"
     blockread(f, sample_period, 4 ); //uint32
     blockread(f, n_vertex, 4 ); //uint32
     {$IFDEF ENDIAN_LITTLE}
     SwapSingle(epoch_begin_latency);
     SwapSingle(sample_period);
     SwapLongWord(n_vertex);
     {$ENDIF}
     //showmessage(floattostr(epoch_begin_latency)+' '+floattostr(sample_period)+' '+inttostr(n_vertex));
     if (n_vertex < 1) then begin //not the correct format
        CloseFile(f);
        exit;
     end;
     setlength(vertex_data,  n_vertex);
     blockread(f,vertex_data[0], 4 * n_vertex);
     {$IFDEF ENDIAN_LITTLE}
     for i := 0 to (n_vertex -1) do
         SwapLongWord(vertex_data[i]);
     {$ENDIF}
     mn :=  vertex_data[0];
     mx := vertex_data[0];
     for i := 0 to (n_vertex -1) do begin
         if vertex_data[i] < mn then
            mn := vertex_data[i];
         if vertex_data[i] > mx then
            mx := vertex_data[i];
     end;
     blockread(f, n_time, 4 ); //uint32
     {$IFDEF ENDIAN_LITTLE}
     SwapLongWord(n_time);
     {$ENDIF}
     n_timeXn_vertex :=  n_time * n_vertex;
     if (sz < (filepos(f)+(n_timeXn_vertex*4))) then
        showmessage(inttostr(sz) + '  '+ floattostr(filepos(f)+(n_timeXn_vertex*4)));
     if (mn <0) or (mx > length(vertices)) then
        showmessage(floattostr(mn)+' '+floattostr(mx)+' '+floattostr(length(vertices)) );
     if (mn <0) or (mx > length(vertices)) or (sz < (filepos(f)+(n_timeXn_vertex*4))) then begin
        showmessage('File corrupted: overlay does not match background mesh.');
        CloseFile(f);
        exit;
     end;
     setlength(stc,  n_timeXn_vertex);
     blockread(f, stc[0], n_timeXn_vertex * 4 );
     {$IFDEF ENDIAN_LITTLE}
     for i := 0 to (n_timeXn_vertex-1) do
         SwapSingle(stc[i]); //FreeSurfer files are ALWAYS big endian
     {$ENDIF}
     CloseFile(f);
     //find time point with biggest effect size
     mx := 0;
     mxt := 1;
     i := 0;
     for t := 1 to n_time do begin
         mn := 0;
         for v := 1 to n_vertex do begin
             mn := mn + abs(stc[i]);
             i := i + 1;
         end;
         if mn > mx then begin
            mx := mn;
            mxt := t;
         end;
     end;
     //
     num_v := length(vertices);
     setlength(overlay[lOverlayIndex].intensity, num_v);
     for v := 0 to (num_v -1) do
         overlay[lOverlayIndex].intensity[v] := 0;
     mxt := (mxt-1) * n_vertex;
     for v := 0 to (n_vertex-1) do
         overlay[lOverlayIndex].intensity[vertex_data[v]] := stc[v+ mxt];
end; // LoadStc()*)

function TMesh.LoadGcs(const FileName: string): boolean;
const
  kMagic = 2880163277; //  $ABABCDCD; //signature for GCS files
//freesurfer GCS file provides vertex colors
//  https://surfer.nmr.mgh.harvard.edu/fswiki/LabelsClutsAnnotationFiles
var
   f: File;
   sz: integer;
    magic : LongWord;

begin
  result := false;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  sz := FileSize(f);
  blockread(f, magic, 4);
  {$IFDEF ENDIAN_LITTLE}  //FreeSurfer files ALWAYS big endian
  SwapLongWord(magic);
  {$ENDIF}
  if (sz < 20) or (magic <> kMagic) then begin //not a valid GCS file
     CloseFile(f);
     exit;
  end;
  showmessage('The undocumented FreeSurfer GCS format is not supported. Solution: convert file to annot format using "mris_ca_label -ml-annot '+FileName+' 7 ~/newfile.annot"');
  CloseFile(f);
  result := true;
end; //LoadGcs()

function asRGBA(i : longint): TRGBA;
type
  swaptype = packed record
    case byte of
      0:(Lng : longint);
      1:(rgba : TRGBA);
  end;
  swaptypep = ^swaptype;
var
  inguy:swaptypep;
begin
    {$IFNDEF ENDIAN_LITTLE}
    please check byte order on big endian systems!
    {$ENDIF}
  inguy := @i; //assign address of s to inguy
  result := inguy^.rgba;
end; // asRGBA()

function TMesh.LoadAnnot(const FileName: string): boolean;
//freesurfer Annotation file provides vertex colors
//  https://surfer.nmr.mgh.harvard.edu/fswiki/LabelsClutsAnnotationFiles
label
   666;
var
   f: File;
   v,i,j,sz: integer;
   vtxct : LongWord;
   idx: array of LongInt;
begin
  result := false;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  sz := FileSize(f);
  if (sz < 20) or (length(vertices) < 3) then  //not a valid annot file
        goto 666;
  blockread(f, vtxct, 4);
  {$IFDEF ENDIAN_LITTLE}  //FreeSurfer files ALWAYS big endian
  SwapLongWord(vtxct);
  {$ENDIF}
  if (vtxct <> length(vertices)) then begin
     showmessage('Annot file does not match currently loaded image: it describes '+inttostr(vtxct)+' vertices but the mesh has '+inttostr(length(vertices)));
     goto 666;
  end;
  if (sz < (4+ (vtxct * 8) )) then begin //too small
     showmessage('Annot file corrupted: file is smaller than expected');
     goto 666;
  end;
  setlength(idx, 2*vtxct);
  blockread(f, idx[0], 2*4*vtxct);
  {$IFDEF ENDIAN_LITTLE}  //FreeSurfer files ALWAYS big endian
  for i := 0 to ((2*vtxct)-1) do
      SwapLongInt(idx[i]);
  {$ENDIF}
  setlength(vertexRGBA, vtxct);
  j := 0;
  for i := 0 to (vtxct-1) do begin
      v := idx[j];
      if (v < 0) or (v >= vtxct) then begin
         showmessage(inttostr(i)+'/'+inttostr(vtxct)+'Index out of range '+inttostr(v));
         goto 666;
      end;
      vertexRGBA[v] := asRGBA(idx[j+1]);
      j := j + 2;
  end;
  result := true;
  CloseFile(f);
  exit;
 666:
   CloseFile(f);
 setlength(vertexRGBA, 0);
end; //LoadAnnot()

procedure TMesh.LoadCurv(const FileName: string; lOverlayIndex: integer);
//simple format used by Freesurfer  BIG-ENDIAN
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bpial/readPial.m
// http://www.grahamwideman.com/gw/brain/fs/surfacefileformats.htm
var
   f: File;
   i, sz: integer;
   num_v, num_f, ValsPerVertex : LongWord;
   sig : array [1..3] of byte;
begin
     AssignFile(f, FileName);
     FileMode := fmOpenRead;
     Reset(f,1);
     sz := FileSize(f);
     if (sz < 20) then begin
        CloseFile(f);
        exit;
     end;
     blockread(f, sig, 3 ); //since these files do not have a file extension, check first 8 bytes "0xFFFFFE creat"
     blockread(f, num_v, 4 ); //uint32
     blockread(f, num_f, 4 ); //uint32
     blockread(f, ValsPerVertex, 4 ); //uint32
     {$IFDEF ENDIAN_LITTLE}
     SwapLongWord(num_v);
     SwapLongWord(num_f);
     SwapLongWord(ValsPerVertex);
     {$ENDIF}
     if (sig[1] <> $FF) or (sig[2] <> $FF) or (sig[3] <> $FF) or (ValsPerVertex <> 1) then begin //not the correct format
        CloseFile(f);
        exit;
     end;
     if (num_v <> length(vertices)) or (num_f <> length(faces)) or (sz < (filepos(f)+(num_v*4))) then begin
        showmessage('File corrupted: overlay does not match background mesh: '+inttostr(num_v)+' vertices and '+inttostr(num_f)+' triangles');
        CloseFile(f);
        exit;
     end;
     setlength(overlay[lOverlayIndex].intensity, num_v); //vertices = zeros(num_f, 9);
     blockread(f,overlay[lOverlayIndex].intensity[0], 4 * num_v);
     {$IFDEF ENDIAN_LITTLE}
     for i := 0 to (num_v-1) do
         SwapSingle(overlay[lOverlayIndex].intensity[i]); //Curv files are ALWAYS big endian
     {$ENDIF}
     CloseFile(f);
end; // LoadCurv()

function fread3 (var f: File): LongWord;
//read 24-bit unsigned integer BIG ENDIAN
var
   b: array [1..3] of byte;
begin
   blockread(f, b, 3);
   {$IFDEF ENDIAN_LITTLE}
   result := b[1] shl 16 + b[2] shl 8 + b[3];  //FreeSurfer is Big-Endian
   {$ELSE}
   result := b[3] shl 16 + b[2] shl 8 + b[1];
   {$ENDIF}
end;

procedure TMesh.LoadW(const FileName: string; lOverlayIndex: integer);
//simple format used by Freesurfer  BIG-ENDIAN
// see FreeSurfer's write_wfile.m
var
   f: File;
   sz, i: integer;
   s: single;
   max_v, num_v, idx : LongWord;
   sig : word;
begin
     AssignFile(f, FileName);
     FileMode := fmOpenRead;
     Reset(f,1);
     sz := FileSize(f);
     blockread(f, sig, 2 ); //since these files do not have a file extension, check first 2 bytes "0x0000"
     num_v := fread3(f);
     if (sig <> 0) or (sz <>  ((num_v * 7)+5) ) then begin
        CloseFile(f);
        exit;
     end;
     max_v := length(vertices);
     if (num_v > max_v) then begin
        showmessage('This W file has MORE vertices than the background image');
        CloseFile(f);
        exit;
     end;
     setlength(overlay[lOverlayIndex].intensity, length(vertices)); //vertices = zeros(num_f, 9);
     for i := 0 to (max_v-1) do
         overlay[lOverlayIndex].intensity[i] := 0;
     for i := 0 to (num_v-1) do begin
         idx := fread3(f);
         if (idx > max_v) then begin
            showmessage('This W file describes MORE vertices than the background image');
            CloseFile(f);
            exit;
         end;
         blockread(f,s, 4);
         {$IFDEF ENDIAN_LITTLE}
         SwapSingle(s); //Curv files are ALWAYS big endian
         {$ENDIF}
         overlay[lOverlayIndex].intensity[idx] := s;
     end;
     CloseFile(f);
end; // LoadW()

procedure TMesh.LoadNii(const FileName: string; lOverlayIndex: integer; isSmooth: boolean);
//Load NIfTI image as overlay
var
   i, num_v: integer;
  nii: TNIFTI;
begin
   nii := TNIfTI.Create;
   nii.LoadFromFile(FileName, kNiftiSmoothMaskZero);
   num_v := length(vertices);
   setlength(overlay[lOverlayIndex].intensity, num_v);
   for i := 0 to (num_v-1) do
       overlay[lOverlayIndex].intensity[i] := nii.mm2intensity(vertices[i].X, vertices[i].Y, vertices[i].Z);
   nii.free;
end; // LoadNii()

procedure TMesh.LoadMeshAsOverlay(const FileName: string; lOverlayIndex: integer);
var
   lMesh: TMesh;
begin
     lMesh := TMesh.Create;
     if lMesh.LoadFromFile(Filename) then begin
        overlay[lOverlayIndex].faces := Copy(lMesh.faces, 0, MaxInt);
        overlay[lOverlayIndex].vertices := Copy(lMesh.vertices, 0, MaxInt);
        overlay[lOverlayIndex].minIntensity := 0;
        overlay[lOverlayIndex].maxIntensity := 0;
        overlay[lOverlayIndex].windowScaledMin:= 0;
        overlay[lOverlayIndex].windowScaledMax := 0;
     end else begin
         setlength(overlay[lOverlayIndex].faces, 0);
         setlength(overlay[lOverlayIndex].vertices, 0);

     end;
     lMesh.Free;
end; // LoadMeshAsOverlay()

procedure TMesh.SetOverlayDescriptives(lOverlayIndex: integer);
var
   mx, mn: single;
   i, num_v: integer;
begin
  num_v := length(overlay[lOverlayIndex].intensity);
  if (num_v < 3) then exit;
  mn := overlay[lOverlayIndex].intensity[0];
  mx := mn;
  for i := 0 to (num_v-1) do
      if mx < overlay[lOverlayIndex].intensity  [i] then mx := overlay[lOverlayIndex].intensity  [i];
  for i := 0 to (num_v-1) do
      if mn > overlay[lOverlayIndex].intensity  [i] then mn := overlay[lOverlayIndex].intensity  [i];
  overlay[lOverlayIndex].minIntensity := mn;
  overlay[lOverlayIndex].maxIntensity := mx;
  if (mx > 4) and (mn < -1) then begin
     overlay[lOverlayIndex].windowScaledMin:= 2;
     overlay[lOverlayIndex].windowScaledMax:= mx;
  end else if (mx <= 0) and (mn < -4) then begin
     overlay[lOverlayIndex].windowScaledMin:= -2;
     overlay[lOverlayIndex].windowScaledMax:= mn;
  end else begin
      overlay[lOverlayIndex].windowScaledMin:= mn;
      overlay[lOverlayIndex].windowScaledMax := mx;
  end;
  if (mn = mx) then begin
     showmessage('Error: no variability in overlay '+floattostr(mn));
     overlay[lOverlayIndex].intensity := nil; //release
     exit;
  end;
  //showmessage('overlay'+floattostr(mn)+'  '+floattostr(mx));
end; // SetOverlayDescriptives()

function TMesh.LoadOverlay(const FileName: string; isSmooth: boolean): boolean;
var
   i, nOverlays: integer;
   ext: string;
begin
  result := false;
  nOverlays := 1;
  if not FileExists(FileName) then exit;
  if (length(vertices) < 3) then begin
     showmessage('Unable to load overlay: load background mesh first');
     exit; //load background first
  end;
  if (OpenOverlays >= kMaxOverlays) then begin
     showmessage('Unable to add overlay: too many overlays open');
     exit;
  end;
  ext := UpperCase(ExtractFileExt(Filename));
  if (ext = '.ANNOT') then
     if LoadAnnot(FileName) then begin
        result := true;
        isRebuildList := true;
        exit; //not supported - but inform user
     end;
  if (ext = '.GCS') then
     if LoadGcs(FileName) then exit; //not supported - but inform user
  OpenOverlays := OpenOverlays + 1;
  setlength(Overlay[OpenOverlays].intensity,0);
  Overlay[OpenOverlays].LUTvisible:= true;
  Overlay[OpenOverlays].filename  := ExtractFilename(FileName);
  if OpenOverlays > 12 then
     Overlay[OpenOverlays].LUTindex := 0
  else
      Overlay[OpenOverlays].LUTindex := OpenOverlays;
  Overlay[OpenOverlays].LUT := UpdateTransferFunction (Overlay[OpenOverlays].LUTindex);
  if (ext = '.MZ3') then begin
     if not LoadMz3(FileName, OpenOverlays) then begin //unable to open as an overlay - perhaps vertex colors?
        OpenOverlays := OpenOverlays - 1;
        exit;
     end;
  end;
  if (ext = '.GII') then begin
     nOverlays := LoadGii(FileName, OpenOverlays, 1);
     if (nOverlays > 1) and ( (OpenOverlays+nOverlays-1) <= kMaxOverlays) then begin
        for i := 2 to nOverlays do begin //GIfTI files can store multiple overlays - see NITRC Caret GIfTI examples
            SetOverlayDescriptives(OpenOverlays);
            OpenOverlays := OpenOverlays + 1;
            setlength(Overlay[OpenOverlays].intensity,0);
            Overlay[OpenOverlays].LUTvisible:= true;
            Overlay[OpenOverlays].filename  := ExtractFilename(FileName);
            LoadGii(FileName, OpenOverlays, i);
            if OpenOverlays > 12 then
               Overlay[OpenOverlays].LUTindex := 0
            else
                Overlay[OpenOverlays].LUTindex := OpenOverlays;
        end;
     end else
         nOverlays := 1;
     if (length(overlay[OpenOverlays].faces) < 1 ) and (length(overlay[OpenOverlays].intensity) < 1 ) then begin //unable to open as an overlay - perhaps vertex colors?
        OpenOverlays := OpenOverlays - 1;
        exit;
     end;
  end;
  if (ext = '.NII') or (ext = '.IMG') or (ext = '.HDR')  or (ext = '.GZ') then
     LoadNii(FileName, OpenOverlays, isSmooth);
  if (length(overlay[OpenOverlays].intensity) < 1 )  then
       LoadW(FileName, OpenOverlays);
  //if (length(overlay[OpenOverlays].intensity) < 1 )   then
  //      LoadStc(FileName, OpenOverlays)
  if (length(overlay[OpenOverlays].intensity) < 1 )   then begin
      LoadCurv(FileName, OpenOverlays);
      if (length(overlay[OpenOverlays].intensity) > 0 ) then
         Overlay[OpenOverlays].LUTindex := 15;//CURV file
  end;

  if  (length(overlay[OpenOverlays].intensity) < 1 ) then begin
      LoadMeshAsOverlay(FileName, OpenOverlays);
  end else
      SetOverlayDescriptives(OpenOverlays);
  if Overlay[OpenOverlays].LUTindex > 13 then begin //CURV file
     Overlay[OpenOverlays].windowScaledMin:= -0.1;
     Overlay[OpenOverlays].windowScaledMax := 0.8;
  end;
  Overlay[OpenOverlays].LUT := UpdateTransferFunction (Overlay[OpenOverlays].LUTindex); //set color scheme
  isRebuildList := true;
  //showmessage(format('%d %d', [length(Overlay[OpenOverlays].intensity), length(Overlay[OpenOverlays].vertices) ]));
  result :=  (length(Overlay[OpenOverlays].intensity) > 0) or (length(Overlay[OpenOverlays].vertices) > 0);
  if not result then
     OpenOverlays := OpenOverlays - 1;
end; // LoadOverlay()

procedure TMesh.CloseOverlays;
var
   i: integer;
begin
     for i := kMinOverlayIndex to kMaxOverlays do begin
         setlength(overlay[i].intensity,0);
         setlength(overlay[i].faces,0);
         setlength(overlay[i].vertices,0);
     end;
     OpenOverlays := 0;
     setlength(vertexRGBA,0);
     isRebuildList := true;
end; // CloseOverlays()

procedure TMesh.Close;
begin
  isRebuildList := true;
  CloseOverlays;
  setlength(faces, 0);
  setlength(vertices, 0);
  setlength(nodes,0);
  setlength(edges,0);
end; // Close()

destructor TMesh.Destroy;
begin
  // Release memory, if glcontext obtained
  //would be nice to call
  // glDeleteLists(displayList, 1);
  inherited;
end; // Destroy()

end.
