unit mesh;
{$Include opts.inc}
{$mode objfpc}{$H+}
interface
{$DEFINE FASTGZ}
//{$DEFINE TIMER}
{$DEFINE TREFOIL} //use Trefoil Knot as default object (instead of pyramid)
uses
  //nifti_foreign,
  {$IFDEF DGL} dglOpenGL, {$ELSE DGL} {$IFDEF COREGL}glcorearb, {$ELSE} gl, {$ENDIF}  {$ENDIF DGL}
  {$IFDEF CTM} ctm_loader, {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, strutils, DateUtils,
  base64, zstream, LcLIntf, nifti_loader, colorTable, matmath, math,
  define_types, nifti_types, fileutil;
const
  kMinOverlayIndex = 1;
  kMaxOverlays = 256;
  kLUTinvisible = 0;
  kLUTtranslucent = 50;
  kLUTopaque = 100;
type
  TSphere = packed record
     X: single;
     Y: single;
     Z: single;
     Clr: single;
     Radius: single;
   end;

 TOverlay = record
    LUTinvert, AOmap: boolean;
    PaintMode: integer;
    OpacityPercent: integer;
    //LUTvisible: integer; //0=invisible, 1=translucent, 2=opaque
    LUTindex,atlasMaxIndex, volumes, CurrentVolume : integer;
    LUT: TLUT;
    minIntensity, maxIntensity, windowScaledMin, windowScaledMax: single;
    filename: string;
    isBinary: boolean;
    intensity: TFloats;
    //next: if loaded as mesh...
    faces : TFaces;
    vertices: TVertices;
    vertexRGBA : TVertexRGBA; //array of TRGBA;
    vertexAtlas: TInts; //what atlss regions does this vertex belong to, e.g. [7, 8, 17...] the 3rd vertex belongs to region 17
    atlasTransparentFilter: TBools;
    atlasHideFilter: TInts; //atlas show these atlas regions, e.g. if [7, 8, 22] then only regions 7,8 and 22 will be visible
 end;

 TNodePrefs = record //preferences for nodes
    minEdge, maxEdge, maxEdgeAbs, minEdgeThresh, maxEdgeThresh, minNodeColor, maxNodeColor,  minNodeSize, maxNodeSize,
    minNodeThresh, maxNodeThresh, scaleNodeSize, scaleEdgeSize, thresholdNodeFrac: single;
    nodeLUTindex , edgeLUTindex: integer;
     isNodeThresholdBySize, isNoNegEdge, isNoPosEdge, isNoLeftNodes,isNoRightNodes,isNoNodeWithoutEdge, isNodeColorVaries, isEdgeColorVaries, isEdgeSizeVaries, isEdgeShowNeg, isEdgeShowPos : boolean;
 end;

type
  TMesh = class
    scale, vertexRgbaSaturation, vertexRgbaAlpha : single;
    origin, mxV, mnV : TPoint3f;
    {$IFDEF COREGL}
    vao, vbo, vaoOverlay, vboOverlay: GLuint;
    nFacesOverlay: integer;
    {$ELSE}
    displayList, displayListOverlay : GLuint;
    {$ENDIF}
    isZDimIsUp, isRebuildList, isBusy, isNode, isFreeSurferMesh, isVisible, isAdditiveOverlay : boolean;
    nodePrefs: TNodePrefs;
    OpenOverlays, OverlayTransparency, AtlasMaxIndex : integer;
    overlay: array [kMinOverlayIndex..kMaxOverlays] of TOverlay;
    nodes: array of TSphere;
    edges:  array of array of Single;
    faces : array of TPoint3i;
    vertices: array of TPoint3f;
    vertexRGBA : array of TRGBA;
    vertexAtlas: TInts; //what atlas regions does this vertex belong to, e.g. [7, 8, 17...] the 3rd vertex belongs to region 17
    atlasTransparentFilter: TBools;
    atlasHideFilter: TInts; //atlas show these atlas regions, e.g. if [7, 8, 22] then only regions 7,8 and 22 will be visible
    tempIntensityLUT : TFloats; //only used to load without external files - flushed after LoadOverlay
    errorString: string;
    //overlay: array of single;
  private
    function CheckMesh: boolean;
    procedure SetOverlayDescriptives(lOverlayIndex: integer);
    procedure MinMaxPct(lOverlayIndex, num_v: integer; var mx, mn: single; isExcludeZero: boolean);
    procedure SetDescriptives;
    {$IFDEF TREFOIL} procedure MakeTrefoil; {$ENDIF}
    procedure MakeSphere;
    function SetLutIndex(layer:integer): integer;
    procedure BuildListCore(Clr: TRGBA; var f: TFaces; var v: TVertices; var vtxRGBA: TVertexRGBA; vRemap: TInts = nil; isWriteToGPU: boolean = true);
    //procedure BuildListCore(Clr: TRGBA; var f: TFaces; var v: TVertices; var vtxRGBA: TVertexRGBA; vRemap: TInts = nil);
    procedure BuildListPartialAtlas(Clr: TRGBA; vtxRGBA: TVertexRGBA);
    procedure BuildList(Clr: TRGBA);
    procedure FilterOverlay(c: integer; var f: TFaces; var v: TVertices; var vRGBA: TVertexRGBA);
    procedure BuildListOverlay(Clr: TRGBA);
    function Load1D(const FileName: string; lOverlayIndex: integer): boolean;
    function Load3Do(const FileName: string): boolean;
    function Load3ds(const FileName: string): boolean;
    function LoadAc(const FileName: string): boolean;
    function LoadByu(const FileName: string): boolean;
    function LoadAnnot(const FileName: string): boolean;
    function LoadNiml(const FileName: string): boolean;
    function loadCifti(fnm: string; lOverlayIndex, lSeriesIndex: integer; isLoadCortexLeft: boolean): integer;
    function LoadDae(const FileName: string): boolean; //only subset!
    function LoadDfs(const FileName: string): boolean;
    function LoadDxf(const FileName: string): boolean;
    function LoadGcs(const FileName: string): boolean;
    function LoadGii(const FileName: string; lOverlayIndex, lOverlayItem: integer; var HasOverlays: boolean): integer;
    function LoadGts(const FileName: string): boolean;
    function LoadIdtf(const FileName: string): boolean;
    function LoadJson(const FileName: string): boolean;
    function LoadLwo(const FileName: string): boolean;
    function LoadMesh(const FileName: string): boolean;
    function LoadMeshAscii(const FileName: string): boolean;
    function LoadVoxel2Vertex(const FileName: string; lOverlayIndex: integer): boolean;
    function LoadMs3d(const FileName: string): boolean;
    //function LoadVbo(const FileName: string): boolean;
    function LoadMz3(const FileName: string; lOverlayIndex : integer): boolean;
    function LoadPrwm(const FileName: string): boolean;
    function LoadObjMni(const FileName: string): boolean;
    function LoadOff(const FileName: string): boolean;
    function LoadOffBin(const FileName: string): boolean;
    function LoadPly2(const FileName: string): boolean;
    function CullUnusedVertices(): boolean;
    function LoadSrf(const FileName: string): boolean;
    function LoadSurf(const FileName: string): boolean;
    function LoadTri(const FileName: string): boolean;
    function LoadVrml(const FileName: string): boolean;
    function LoadWfr(const FileName: string): boolean; //EMSE wireframe
    function LoadAsc_Srf(const FileName: string): boolean;
    function LoadAsc_Sparse(const FileName: string): boolean;
    procedure LoadCtm(const FileName: string);
    function LoadDpv(const FileName: string; lOverlayIndex: integer): boolean;
    function LoadCol(const FileName: string; lOverlayIndex, lOverlayItem: integer): integer; //AFNI https://afni.nimh.nih.gov/afni/community/board/read.php?1,44391,44410
    function LoadAtlasMapCore(lOverlayIndex: integer; intensityLUT: TFloats): string;
    function LoadAtlasMap(const FileName: string; lOverlayIndex: integer): boolean;
    procedure LoadCurv(const FileName: string; lOverlayIndex: integer);
    procedure LoadMeshAsOverlay(const FileName: string; lOverlayIndex: integer);
    procedure LoadNii(const FileName: string; lOverlayIndex: integer; lLoadSmooth: boolean);
    function LoadCluster(const FileName: string): boolean;
    function LoadNodeTxt(const FileName: string): boolean;
    procedure LoadNode(const FileName: string; out isEmbeddedEdge: boolean);
    procedure LoadNv(const FileName: string);
    procedure LoadObj(const FileName: string);
    //procedure LoadFreeSurferQuad(const FileName: string);
    procedure LoadPial(const FileName: string);
    procedure LoadPly(const FileName: string);
    procedure LoadStl(const FileName: string);
    procedure LoadStlAscii(const FileName: string);
    function LoadVtkTxt(const FileName: string):boolean;
    procedure LoadVtk(const FileName: string);
    procedure LoadW(const FileName: string; lOverlayIndex: integer);
  public
    OverlappingOverlaysOverwrite: boolean;
    procedure MakePyramid;
    //function AtlasStatMapCore(AtlasName, StatName: string; Indices: TInts; Intensities: TFloats): string;
    //function AtlasMaxIndex: integer;
    procedure DrawGL (Clr: TRGBA; clipPlane: TPoint4f; isFlipMeshOverlay: boolean);
    procedure Node2Mesh;
    procedure ReverseFaces;
    procedure CenterOrigin;
    procedure SwapYZ;
    procedure SwapZY;
    function LoadFromFile(const FileName: string): boolean;
    function LoadEdge(const FileName: string; isEmbeddedEdge: boolean): boolean;
    function LoadOverlay(const FileName: string; lLoadSmooth: boolean): boolean;
    procedure CloseOverlaysCore;
    procedure CloseOverlays;
    function Atlas2Node(const FileName: string):boolean;
    procedure Close;
    constructor Create;
    (*procedure SaveVrml(const FileName: string; MeshColor: TColor = clWhite);
    procedure SaveMz3(const FileName: string; MeshColor: TColor = clWhite);
    procedure SaveGii(const FileName: string);
    procedure SaveObj(const FileName: string);
    procedure SavePly(const FileName: string; MeshColor: TColor = clWhite);*)
    procedure SaveMesh(const FileName: string; MeshColor: TColor = clWhite);
    procedure SaveOverlay(const FileName: string; OverlayIndex: integer);
    destructor  Destroy; override;
  end;

implementation

uses
  mainunit, {$IFDEF FASTGZ}SynZip, {$ENDIF}
  meshify_simplify,shaderu, {$IFDEF COREGL} gl_core_3d {$ELSE} gl_legacy_3d {$ENDIF};

(*function TMesh.Atlas2Node(const FileName: string):boolean;
const
  kT = chr(9);
label
   123;
var
   xROI, yROI, zROI, maxRoiDxIdx, nVert, maxROI, nFace, roi, nroiEdges, nroiVerts, nCOGs: integer;
   roiEdges, roiVerts: TInts;
   isEdge, isVert: TInts;
   roiDx, maxRoiDx: double;
   COGs: array of TPoint4f; //center of gravity
   j, i : integer;
   txt: TextFile;
begin
     result := false;
     nVert := length(vertices);
     nFace := length(faces);
     maxROI := AtlasMaxIndex;
     if (nFace < 1) or (nVert < 3) or (maxROI < 1) or (length(vertexAtlas) <>  nVert) then exit;
     setlength(COGs, maxROI+1);
     setlength(roiEdges, nVert); //list of vertices on edge of ROI
     setlength(roiVerts, nVert); //list all vertices in ROI
     setlength(isEdge, nVert);
     setlength(isVert, nVert);
     for i := 0 to maxROI do
         COGs[i] := pt4f(0,0,0,0);
     nCOGs := 0;
     for roi := 0 to maxROI do begin
         nroiEdges := 0;
         for i := 0 to nVert -1 do begin
             isEdge[i] := 0;
             isVert[i] := 0;
         end;
         //fillChar(isEdge,sizeOf(isEdge),0);
         //fillChar(isVert,sizeOf(isVert),0);
         for i := 0 to (nFace-1) do begin
             xROI := vertexAtlas[Faces[i].X];
             yROI := vertexAtlas[Faces[i].Y];
             zROI := vertexAtlas[Faces[i].Z];
             if (xROI <> roi) and (yROI <> roi) and (zROI <> roi) then continue; //this vertex not member of ROI
             //roiVerts[nroiVerts] := i;
             if (xROI = roi) then isVert[Faces[i].X] := 1;
             if (yROI = roi) then isVert[Faces[i].Y] := 1;
             if (zROI = roi) then isVert[Faces[i].Z] := 1;
             if (xROI = yROI) and (xROI = zROI) then continue; //all members of same ROI
             if (xROI = roi) then isEdge[Faces[i].X] := 1;
             if (yROI = roi) then isEdge[Faces[i].Y] := 1;
             if (zROI = roi) then isEdge[Faces[i].Z] := 1;
             nroiEdges := nroiEdges + 1;
         end;
         if nroiEdges < 1 then continue; //no edges found!
         //compute total number of edges, faces...
         nroiEdges := 0;
         for i := 0 to (nVert-1) do
             if isEdge[i] > 0 then begin
               roiEdges[nroiEdges] := i;
               nroiEdges := nroiEdges + 1;
             end;
         nroiVerts := 0;
         for i := 0 to (nVert-1) do
             if isVert[i] > 0 then begin
               roiVerts[nroiVerts] := i;
               nroiVerts := nroiVerts + 1;
             end;
         maxRoiDx := -infinity;
         maxRoiDxIdx := 0;
         for i := 0 to (nroiVerts -1) do begin //compute cost function for each vertex - furtherst from edges
             roiDx := 0;
             for j := 0 to (nroiEdges - 1) do
                 roiDx := roiDx + DistanceBetween(vertices[roiVerts[i]], vertices[roiEdges[j]]);
             if (roiDx <= maxRoiDx) then continue;
             maxRoiDx := roiDx;
             maxRoiDxIdx := roiVerts[i];
         end;
         COGs[nCOGs].x := vertices[maxRoiDxIdx].x;
         COGs[nCOGs].y := vertices[maxRoiDxIdx].y;
         COGs[nCOGs].z := vertices[maxRoiDxIdx].z;
         COGs[nCOGs].w := roi+1;
         nCOGs := nCOGs + 1;
     end;
     if nCOGs < 1 then goto 123;
     result := true;
     AssignFile(txt, FileName);
     rewrite(txt);
     for i := 0 to nCOGs-1 do begin
         if COGs[i].w < 1 then continue; //label not present
         writeln(txt, format('%g%s%g%s%g%s%d%s1', [COGs[i].x,kT,COGs[i].y,kT,COGs[i].z,kT,round(COGs[i].w),kT ]));
     end;
     CloseFile(txt);
     123:
     roiEdges := nil;
     roiEdges := nil;
     COGs := nil;
     isVert := nil;
     isEdge := nil;
     nearestNode := nil;
end; *)


{$DEFINE FXR}
{$IFDEF FXR}
function DistanceBetween(var a,b: TPoint3f): single;
begin
     result := sqrt(sqr(a.x-b.x)+sqr(a.y-b.y)+sqr(a.z-b.z));
end;

function AddPt3f(var a,b: TPoint3f): TPoint3f; //create float vector
begin
     result.X := a.X + b.X;
     result.Y := a.Y + b.Y;
     result.Z := a.Z + b.Z;
end;

function TMesh.Atlas2Node(const FileName: string):boolean;
const
  kT = chr(9);
label
   123;
var
   xROI, yROI, zROI, maxRoiDxIdx, nVert, maxROI, nFace, roi, nroiEdges, nroiVerts, nCOGs: integer;
   roiEdges, roiVerts: TInts;
   isEdge, isVert: TInts;
   cogDx, maxCogDX, roiDx, maxRoiDx, dx: single;
   vert, cog: TPoint3f;
   COGs: array of TPoint4f; //center of gravity
   j, i : integer;
   txt: TextFile;
   {$IFDEF TIMER}startTime : TDateTime;{$ENDIF}
begin
     {$IFDEF TIMER}startTime := Now;{$ENDIF}
     result := false;
     nVert := length(vertices);
     nFace := length(faces);
     maxROI := AtlasMaxIndex;
     //GLForm1.OverlayBox.Caption := 'a'+inttostr(maxROI);
     if (nFace < 1) or (nVert < 3) or (maxROI < 1) or (length(vertexAtlas) <>  nVert) then exit;
     setlength(COGs, maxROI+1);
     setlength(roiEdges, nVert); //list of vertices on edge of ROI
     setlength(roiVerts, nVert); //list all vertices in ROI
     setlength(isEdge, nVert);
     setlength(isVert, nVert);
     //GLForm1.OverlayBox.Caption := 'b'+inttostr(maxROI);

     for i := 0 to maxROI do
         COGs[i] := pt4f(0,0,0,0);
     nCOGs := 0;
     for roi := 0 to maxROI do begin
         nroiEdges := 0;
         for i := 0 to nVert -1 do begin
             isEdge[i] := 0;
             isVert[i] := 0;
         end;
         //fillChar(isEdge,sizeOf(isEdge),0);
         //fillChar(isVert,sizeOf(isVert),0);
         for i := 0 to (nFace-1) do begin
             xROI := vertexAtlas[Faces[i].X];
             yROI := vertexAtlas[Faces[i].Y];
             zROI := vertexAtlas[Faces[i].Z];
             if (xROI <> roi) and (yROI <> roi) and (zROI <> roi) then continue; //this vertex not member of ROI
             //roiVerts[nroiVerts] := i;
             if (xROI = roi) then isVert[Faces[i].X] := 1;
             if (yROI = roi) then isVert[Faces[i].Y] := 1;
             if (zROI = roi) then isVert[Faces[i].Z] := 1;
             if (xROI = yROI) and (xROI = zROI) then continue; //all members of same ROI
             if (xROI = roi) then isEdge[Faces[i].X] := 1;
             if (yROI = roi) then isEdge[Faces[i].Y] := 1;
             if (zROI = roi) then isEdge[Faces[i].Z] := 1;
             nroiEdges := nroiEdges + 1;
         end;
         if nroiEdges < 1 then continue; //no edges found!
         //compute total number of edges, faces...
         nroiEdges := 0;
         for i := 0 to (nVert-1) do
             if isEdge[i] > 0 then begin
               roiEdges[nroiEdges] := i;
               nroiEdges := nroiEdges + 1;
             end;
         nroiVerts := 0;
         for i := 0 to (nVert-1) do
             if isVert[i] > 0 then begin
               roiVerts[nroiVerts] := i;
               nroiVerts := nroiVerts + 1;
             end;
         //find center of mass
         cog := ptf(0,0,0);
         for i := 0 to (nroiVerts -1) do
             cog := AddPt3f(cog, vertices[roiVerts[i]]);
          cog.x := cog.x / nroiVerts;
          cog.y := cog.y / nroiVerts;
          cog.z := cog.z / nroiVerts;
         maxCogDX := infinity;
         maxRoiDx := -infinity;
         maxRoiDxIdx := 0;
         for i := 0 to (nroiVerts -1) do begin
             //compute cost function for each vertex - furtherst from edges
             // who on the island has the longest walk to the beach?
             roiDx := infinity;
             vert := vertices[roiVerts[i]];
             for j := 0 to (nroiEdges - 1) do begin
                 dx := DistanceBetween(vert, vertices[roiEdges[j]]);
                 if dx < roiDx then
                    roiDx := dx;
             end;
             if (roiDx < maxRoiDx) then continue;
             cogDx := DistanceBetween(vert, cog);
             if (roiDx = maxRoiDx) then begin
                if (cogDx > maxCogDx) then continue;
             end;
             maxCogDx := cogDx;
             maxRoiDx := roiDx;
             maxRoiDxIdx := roiVerts[i];
         end;
         //maxRoiDxIdx := roiVerts[random(nroiVerts)];
         //maxRoiDxIdx := roiEdges[random(nroiEdges)];
         COGs[nCOGs].x := vertices[maxRoiDxIdx].x;
         COGs[nCOGs].y := vertices[maxRoiDxIdx].y;
         COGs[nCOGs].z := vertices[maxRoiDxIdx].z;
         COGs[nCOGs].w := roi+1;
         nCOGs := nCOGs + 1;
     end;
     if nCOGs < 1 then goto 123;
     result := true;
     AssignFile(txt, FileName);
     rewrite(txt);
     for i := 0 to nCOGs-1 do begin
         if COGs[i].w < 1 then continue; //label not present
         writeln(txt, format('%g%s%g%s%g%s%d%s1', [COGs[i].x,kT,COGs[i].y,kT,COGs[i].z,kT,round(COGs[i].w),kT ]));
     end;
     CloseFile(txt);
     {$IFDEF TIMER}GLForm1.Caption :=  inttostr(MilliSecondsBetween(Now,StartTime)); {$ENDIF}
     123:
     roiEdges := nil;
     roiEdges := nil;
     COGs := nil;
     isVert := nil;
     isEdge := nil;
     //nearestNode := nil;
end;
{$ELSE}
function TMesh.Atlas2Node(const FileName: string):boolean;
const
  kT = chr(9);
label
   123;
var
   nVert, maxROI: integer;
   COGs, nearestNode: array of TPoint4f; //center of gravity
   i, idx : integer;
   dx: single;
   txt: TextFile;
begin
     result := false;
     nVert := length(vertices);
     maxROI := AtlasMaxIndex;
     if (nVert < 3) or (maxROI < 1) or (length(vertexAtlas) <>  nVert) then exit;
     //vertexAtlas: TInts; //what atlas regions does this vertex belong to, e.g. [7, 8, 17...] the 3rd vertex belongs to region 17
     //setlength(mass, maxROI+1);
     setlength(COGs, maxROI+1);
     for i := 0 to maxROI do
         COGs[i] := pt4f(0,0,0,0);
     for i := 0 to (nVert -1) do begin
         idx := vertexAtlas[i];
         COGs[idx].w := COGs[idx].w + 1;
         COGs[idx].x := COGs[idx].x + vertices[i].x;
         COGs[idx].y := COGs[idx].y + vertices[i].y;
         COGs[idx].z := COGs[idx].z + vertices[i].z;
     end;
     //set center of gravity =  mean
     for i := 0 to maxROI do begin
         COGs[i].x := COGs[i].x / COGs[i].w;
         COGs[i].y := COGs[i].y / COGs[i].w;
         COGs[i].z := COGs[i].z / COGs[i].w;
     end;
     //initialize nearest node - infinitely far away from COG
     setlength(nearestNode, maxROI+1);
     for i := 0 to maxROI do
         nearestNode[i] := pt4f(0,0,0,Infinity);
     //for each vertex - see if this is closest to COG
     for i := 0 to (nVert -1) do begin
         idx := vertexAtlas[i];
         dx := sqrt( sqr(COGs[idx].x-vertices[i].x) + sqr(COGs[idx].y-vertices[i].y) + sqr(COGs[idx].z-vertices[i].z)  );
         if (dx >= nearestNode[idx].w) then continue;
         nearestNode[idx] := pt4f(vertices[i].x, vertices[i].y, vertices[i].z, dx);
         result := true; //at least one node survives
     end;
     if not result then goto 123;
     AssignFile(txt, FileName);
     rewrite(txt);
     for i := 0 to maxROI do begin
         if COGs[i].w < 1 then continue; //label not present
         writeln(txt, format('%g%s%g%s%g%s%d%s1', [nearestNode[i].x,kT,nearestNode[i].y,kT,nearestNode[i].z,kT,i,kT ]));
     end;
     CloseFile(txt);
     123:
     COGs := nil;
     nearestNode := nil;
end;
{$ENDIF}

{$IFDEF FASTGZ}
function ExtractGz(fnm: string; var mStream : TMemoryStream; magic: word = 0): boolean;
//if magic <> 0, then a file that starts with magic will assume to uncompressed
//if magic = 0, a file will be assumed to be uncompressed if first two bytes are not $8B1F
var
	gz: TGZRead;
        sig: word;
        F : File Of byte;
	src : array of byte;
	cSz : int64; //uncompressed, compressed size
begin
	if not fileexists(fnm) then exit(false);
  	FileMode := fmOpenRead;
  	Assign (F, fnm);
  	Reset (F);
  	cSz := FileSize(F);
        sig := 0; //only to hide compiler warning
        blockread(F, sig, SizeOf(sig) );
        seek(F,0);
        //n.b. GZ header/footer is ALWAYS little-endian
        {$IFDEF ENDIAN_BIG}
        sig := Swap(sig);
        {$ENDIF}
        if ( ((magic = 0) and (sig <> $8B1F)) or ((magic <> 0) and (sig = magic)) ) then begin //hex: 1F 8B : gzip specific: will reject zlib format
           Close (F);
           mStream.LoadFromFile(fnm);
           exit(true);
        end;
  	setlength(src, cSz);
  	blockread(f, src[0], cSz );
  	CloseFile(f);
	result := gz.Init(@src[0], cSz);
	if not result then begin
		src := nil;
		exit;
	end;
	gz.ToStream(mStream, cSz);
	src := nil;
	gz.ZStreamDone;
end;
{$ELSE}
function ExtractGz(fnm: string; var mStream : TMemoryStream; magic: word = 0): boolean;
const
     kChunkSize = 65535;
var
   zStream : TGZFileStream;
   bytes : array of byte;
   i: integer;
begin
  zStream := TGZFileStream.create(fnm, gzopenread);
  result := (zStream <> nil);
  setlength(bytes, kChunkSize);
  repeat
        i := zStream.read(bytes[0],kChunkSize);
        mStream.Write(bytes[0],i) ;
  until i < kChunkSize;
  zStream.Free;
end;
{$ENDIF}


//{$IFDEF COREGL}
function mixRGBA(c1, c2: TRGBA; frac2: single): TRGBA;
var
   frac1: single;
begin
  frac2 := ((c2.a * frac2 )/255.0);
  frac1 := 1 - frac2;
  result.R := round(c1.R*frac1 + c2.R*frac2) ;
  result.G := round(c1.G*frac1 + c2.G*frac2);
  result.B := round(c1.B*frac1 + c2.B*frac2);
end;
//{$ENDIF}

procedure AddPt4f(var v: TPoint4f; c1,c2,c3: TRGBA); //create float vector
begin
     v.X := v.X + c1.r+ c2.r+ c3.r;
     v.Y := v.Y + c1.g+ c2.g+ c3.g;
     v.Z := v.Z + c1.b+ c2.b+ c3.b;
     v.W := v.W + c1.a+ c2.a+ c3.a;
end;

procedure invertDarken(var c: TRGBA; v, mn,mx: single);
var
   frac: single;
begin
        if (v <= mn) then exit;
    if (v >= mx) then begin
       c.R := 0;
       c.G := 0;
       c.B := 0;
       exit;
    end;
    frac := 1.0 - ((v-mn)/(mx-mn));
    c.R := round(c.R * frac);
    c.G := round(c.G * frac);
    c.B := round(c.B * frac);
end;

procedure darken(var c: TRGBA; v, mn,mx: single);
var
   frac: single;
begin
        if (v >= mx) then exit;
    if (v <= mn) then begin
       c.R := 0;
       c.G := 0;
       c.B := 0;
       exit;
    end;
    frac := (v-mn)/(mx-mn);
    c.R := round(c.R * frac);
    c.G := round(c.G * frac);
    c.B := round(c.B * frac);
end;

procedure TMesh.BuildListCore(Clr: TRGBA; var f: TFaces; var v: TVertices; var vtxRGBA: TVertexRGBA; vRemap: TInts = nil; isWriteToGPU: boolean = true);
var
  i,volInc, c: integer;
  mn, mx, frac: single;
  rgb, rgb0: TRGBA;
  vRGBA, vRGBAmx :TVertexRGBA;
  vNumNeighbor: array of integer;
  vSumRGBBA: array of TPoint4f;
  isOverlayPainting : boolean = false;
begin
  if (length(f) < 1) or (length(v) < 3) then exit;
  volInc := 0;
  isOverlayPainting := false;
  if  (OpenOverlays > 0)  then  //ignore overlays if they are all meshes rather than vertex colors
       for c :=  OpenOverlays downto 1 do
           if (overlay[c].OpacityPercent <> kLUTinvisible) and (length(overlay[c].intensity) >= length(v)) then
              isOverlayPainting := true;
  if  (isOverlayPainting) or (length(vtxRGBA) = length(v)) then begin
     rgb := RGBA(Clr.R, Clr.G, Clr.B, 0);
     setlength(vRGBA, length(v));
     if (length(vtxRGBA) = length(v)) then begin
       c := round(vertexRgbaAlpha * 255);
        if vertexRgbaSaturation >= 1 then begin
          for i := 0 to (length(v)-1) do begin
                vRGBA[i].r := vtxRGBA[i].r;
                vRGBA[i].g := vtxRGBA[i].g;
                vRGBA[i].b := vtxRGBA[i].b;
                vRGBA[i].a := c;
          end;
        end else begin
            for i := 0 to (length(v)-1) do
               vRGBA[i] := desaturateRGBA ( vtxRGBA[i], vertexRgbaSaturation, C);
        end;
        {$IFDEF COREGL}
        //2019: required only when no overlays open - try adjusting transparency of Altas
        if (OpenOverlays < 1) and (vertexRGBAAlpha < 1.0) then
          for i := 0 to (length(v)-1) do
              vRGBA[i] := mixRGBA( Clr, vRGBA[i], vertexRGBAAlpha);
        {$ENDIF}
     end else begin
         for i := 0 to (length(v)-1) do
             vRGBA[i] := rgb;
     end;
     if  (OpenOverlays > 0) then begin
        if isAdditiveOverlay then begin
          setlength(vRGBAmx, length(v));
          rgb0 := RGBA(0,0,0,0);
          for i := 0 to (length(v)-1) do
              vRGBAmx[i] := rgb0;
          for c :=  OpenOverlays downto 1 do begin
            if isFreeSurferLUT(overlay[c].LUTindex) then continue; //curve files ALWAYs darken
            volInc :=  length(v) *  (overlay[c].currentVolume - 1);
            if (not overlay[c].aoMap) and (overlay[c].OpacityPercent <> kLUTinvisible) and (length(overlay[c].intensity) >= length(v)) then begin
               frac :=  overlay[c].OpacityPercent/100;
               if (frac > 1) or (frac < 0) then frac := 1;
               if overlay[c].windowScaledMax > overlay[c].windowScaledMin then begin
                  mn := overlay[c].windowScaledMin;
                  mx := overlay[c].windowScaledMax;
               end else begin
                   mx := overlay[c].windowScaledMin;
                   mn := overlay[c].windowScaledMax;
               end;
               if vRemap <> nil then begin //filtered atlas: vertices have been decimated
                 for i := 0 to (length(v)-1) do begin
                   rgb := inten2rgb(overlay[c].intensity[vRemap[i]+volInc], mn, mx, overlay[c].LUT, overlay[c].PaintMode);
                   rgb.A := round(rgb.A * frac);
                   vRGBAmx[i] := maxRGBA(vRGBAmx[i], rgb);
                 end; //for i
               end else begin
                 for i := 0 to (length(v)-1) do begin
                     rgb := inten2rgb(overlay[c].intensity[i+volInc], mn, mx, overlay[c].LUT, overlay[c].PaintMode);
                     rgb.A := round(rgb.A * frac);
                     vRGBAmx[i] := maxRGBA(vRGBAmx[i], rgb);
                 end; //for i
               end;

            end; //if visible
          end; //for c
          //next curve files
          for c :=  OpenOverlays downto 1 do begin
            if not isFreeSurferLUT(overlay[c].LUTindex) then continue; //only curve files
            if (not overlay[c].aoMap) and (overlay[c].OpacityPercent <> kLUTinvisible) and (length(overlay[c].intensity) >= length(v)) then begin
               frac := overlay[c].OpacityPercent / 100;
               if (frac < 0) or (frac > 1) then frac := 1;
               if overlay[c].windowScaledMax > overlay[c].windowScaledMin then begin
                  mn := overlay[c].windowScaledMin;
                  mx := overlay[c].windowScaledMax;
               end else begin
                   mx := overlay[c].windowScaledMin;
                   mn := overlay[c].windowScaledMax;
               end;
               volInc :=  length(v) *  (overlay[c].currentVolume - 1);
               if vRemap <> nil then begin //filtered atlas: vertices have been decimated
                 for i := 0 to (length(v)-1) do begin
                     rgb := inten2rgb(overlay[c].intensity[vRemap[i]+volInc], mn, mx, overlay[c].LUT, overlay[c].PaintMode);
                     rgb.A := round(rgb.A * frac);
                     vRGBA[i] := blendRGBA(vRGBA[i], rgb);
                 end; //for i
               end else begin
                 for i := 0 to (length(v)-1) do begin
                     rgb := inten2rgb(overlay[c].intensity[i+volInc], mn, mx, overlay[c].LUT, overlay[c].PaintMode);
                     rgb.A := round(rgb.A * frac);
                     vRGBA[i] := blendRGBA(vRGBA[i], rgb);
                 end; //for i
               end;
            end; //if visible
          end;  //for c
          //finally composite
          for i := 0 to (length(v)-1) do
              vRGBA[i] := blendRGBA(vRGBA[i],vRGBAmx[i]);
        end else begin //not additive: mix overlays
          {$DEFINE NFX}
          {$IFDEF NFX}
          //see https://en.wikipedia.org/wiki/Alpha_compositing#Analytical_derivation_of_the_over_operator
          if OverlappingOverlaysOverwrite then begin
          for i := 0 to (length(v)-1) do
              vRGBA[i].A := 0;
          for c :=  OpenOverlays downto 1 do begin
              if (not overlay[c].aoMap) and (overlay[c].OpacityPercent <> kLUTinvisible) and (length(overlay[c].intensity) >= length(v)) then begin
                 frac := overlay[c].OpacityPercent / 100;
                 if overlay[c].windowScaledMax > overlay[c].windowScaledMin then begin
                    mn := overlay[c].windowScaledMin;
                    mx := overlay[c].windowScaledMax;
                 end else begin
                     mx := overlay[c].windowScaledMin;
                     mn := overlay[c].windowScaledMax;
                 end;
                 volInc :=  length(v) *  (overlay[c].currentVolume - 1);
                 if vRemap <> nil then begin //filtered atlas: vertices have been decimated
                   for i := 0 to (length(v)-1) do begin
                       rgb := inten2rgb(overlay[c].intensity[vRemap[i]+volInc], mn, mx, overlay[c].LUT, overlay[c].PaintMode);
                       rgb.A := round(rgb.A * frac);
                       vRGBA[i] := blendRGBAover(vRGBA[i], rgb);
                   end; //for i
                 end else begin
                   for i := 0 to (length(v)-1) do begin
                       rgb := inten2rgb(overlay[c].intensity[i+volInc], mn, mx, overlay[c].LUT, overlay[c].PaintMode);
                       rgb.A := round(rgb.A * frac);
                       vRGBA[i] := blendRGBAover(vRGBA[i], rgb);
                   end; //for i
                 end;
              end; //if visible
            end;  //for c
          end else begin
          {$ENDIF}
          for c :=  OpenOverlays downto 1 do begin
              if (not overlay[c].aoMap) and (overlay[c].OpacityPercent <> kLUTinvisible) and (length(overlay[c].intensity) >= length(v)) then begin
                 frac := overlay[c].OpacityPercent / 100;
                 if overlay[c].windowScaledMax > overlay[c].windowScaledMin then begin
                    mn := overlay[c].windowScaledMin;
                    mx := overlay[c].windowScaledMax;
                 end else begin
                     mx := overlay[c].windowScaledMin;
                     mn := overlay[c].windowScaledMax;
                 end;
                 volInc :=  length(v) *  (overlay[c].currentVolume - 1);
                 if vRemap <> nil then begin //filtered atlas: vertices have been decimated
                   for i := 0 to (length(v)-1) do begin
                       rgb := inten2rgb(overlay[c].intensity[vRemap[i]+volInc], mn, mx, overlay[c].LUT, overlay[c].PaintMode);
                       rgb.A := round(rgb.A * frac);
                       vRGBA[i] := blendRGBA(vRGBA[i], rgb);
                   end; //for i
                 end else begin
                   for i := 0 to (length(v)-1) do begin
                       rgb := inten2rgb(overlay[c].intensity[i+volInc], mn, mx, overlay[c].LUT, overlay[c].PaintMode);
                       rgb.A := round(rgb.A * frac);
                       vRGBA[i] := blendRGBA(vRGBA[i], rgb);
                   end; //for i
                 end;
              end; //if visible
            end;  //for c
          {$IFDEF NFX}
          end;
           {$ENDIF}
       end;
       //finish
       if  (length(vtxRGBA) < 1) then begin //feather edges of overlay
         setlength(vNumNeighbor, length(v));
         setlength(vSumRGBBA, length(v));
         for i := 0 to (length(v)-1) do begin
             vNumNeighbor[i] := 0;
             vSumRGBBA[i] := pt4f(0,0,0,0);
         end;
         for i := 0 to (length(f)-1) do begin
             AddPt4f(vSumRGBBA[f[i].X], vRGBA[f[i].X], vRGBA[f[i].Y], vRGBA[f[i].Z]);
             AddPt4f(vSumRGBBA[f[i].Y], vRGBA[f[i].X], vRGBA[f[i].Y], vRGBA[f[i].Z]);
             AddPt4f(vSumRGBBA[f[i].Z], vRGBA[f[i].X], vRGBA[f[i].Y], vRGBA[f[i].Z]);
             inc(vNumNeighbor[f[i].X],3);
             inc(vNumNeighbor[f[i].Y],3);
             inc(vNumNeighbor[f[i].Z],3);
         end;
         for i := 0 to (length(v)-1) do begin
             if (vNumNeighbor[i] > 0)  then begin //vertex at edge: neighbors both colored and uncolored v
                 vRGBA[i].a := round(vSumRGBBA[i].W / vNumNeighbor[i]);
                 if {(vRGBA[i].a < 255)  and} (vRGBA[i].a > 0) then begin
                    vRGBA[i].r := round( vSumRGBBA[i].X / vNumNeighbor[i]);
                    vRGBA[i].g := round( vSumRGBBA[i].Y / vNumNeighbor[i]);
                    vRGBA[i].b := round( vSumRGBBA[i].Z / vNumNeighbor[i]);
                 end;
             end;
         end;
         vNumNeighbor := nil;
         //vNumColorNeighbor := nil;
         vSumRGBBA := nil;
       end; //end feather edges
       (*if (length(vtxRGBA) < 1) then begin //feather edges if regions without overlay have alpha = 0
         setlength(vZeroNeighbor, length(v));
         for i := 0 to (length(v)-1) do
             vZeroNeighbor[i] := false;
         for i := 0 to (length(f)-1) do begin
             if (vRGBA[f[i].X].A = 0) or (vRGBA[f[i].Y].A = 0) or (vRGBA[f[i].Z].A = 0) then begin
                vZeroNeighbor[f[i].X] := true;
                vZeroNeighbor[f[i].Y] := true;
                vZeroNeighbor[f[i].Z] := true;
             end;
         end;
         for i := 0 to (length(v)-1) do
             if(vZeroNeighbor[i]) then
                 vRGBA[i].a :=   vRGBA[i].a shr 1; //make edges more transparent
       end; //end feather edges
       *)
       {$IFDEF COREGL} //with new openGL we mix here
       mx := 1.0 - OverlayTransparency/100;
       if mx < 0 then mx := 0;
       if mx > 1 then mx := 1;
       //if (OverlayTransparency > 0 ) and (OverlayTransparency <= 100) then
       for i := 0 to (length(v)-1) do
              vRGBA[i] := mixRGBA( Clr, vRGBA[i], mx);
       {$ELSE}  //with old GLSL we mix in the shader
       if (OverlayTransparency > 0 ) and (OverlayTransparency <= 100) then begin
         for i := 0 to (length(v)-1) do
              vRGBA[i].a := round( vRGBA[i].a * (1 - (OverlayTransparency /100)) );
       end;
       {$ENDIF}
     end; // if OpenOverlays > 0
     {$IFDEF COREGL}
     //the purpose of the next loop is to allow us to hide curvature
     for c :=  OpenOverlays downto 1 do begin
         volInc :=  length(v) *  (overlay[c].currentVolume - 1);

         if isFreeSurferLUT(overlay[c].LUTindex) then begin
           for i := 0 to (length(v)-1) do begin
             mn := ((overlay[c].intensity[i+volInc] + 1.0) * 0.5); //convert curvature to range 0..1
             if (mn < 0) then mn := 0;
             if (mn > 1) then mn := 1;
             mn := mn * 255.0;
               vRGBA[i].A := round(mn);
           end;
         end;
     end; //for c
     {$ENDIF}
     //aoMap
     //GLForm1.Caption := format ('%d %d %d', [length(v), random(888),length(vRGBA)]);
     //for i := 0 to (length(v)-1) do
     //    vRGBA[i].R := 0;
     for c :=  OpenOverlays downto 1 do begin
            if (overlay[c].aoMap) and (overlay[c].OpacityPercent <> kLUTinvisible) and (length(overlay[c].intensity) >= length(v)) then begin
               if overlay[c].windowScaledMax > overlay[c].windowScaledMin then begin
                  mn := overlay[c].windowScaledMin;
                  mx := overlay[c].windowScaledMax;
               end else begin
                   mx := overlay[c].windowScaledMin;
                   mn := overlay[c].windowScaledMax;
               end;
               volInc :=  length(v) *  (overlay[c].currentVolume - 1);
               if (overlay[c].OpacityPercent < 100) then
                  mx := ((mx - mn) * (overlay[c].OpacityPercent/100)) + mn;
                if (overlay[c].LUTinvert) then begin
                    if vRemap <> nil then begin //filtered atlas: vertices have been decimated
                      for i := 0 to (length(v)-1) do
                          invertDarken(vRGBA[i], overlay[c].intensity[vRemap[i]+volInc], mn,mx);
                    end else
                        for i := 0 to (length(v)-1) do
                            invertDarken(vRGBA[i], overlay[c].intensity[i+volInc], mn,mx);

                end else begin
                 if vRemap <> nil then begin //filtered atlas: vertices have been decimated
                   for i := 0 to (length(v)-1) do
                       darken(vRGBA[i], overlay[c].intensity[vRemap[i]+volInc], mn,mx);
                 end else
                     for i := 0 to (length(v)-1) do
                         darken(vRGBA[i], overlay[c].intensity[i+volInc], mn,mx);
               end;
               {$IFNDEF COREGL}
               for i := 0 to (length(v)-1) do
                   vRGBA[i].A := 255;
               {$ENDIF}
            end; //for each channel
     end; //aoMap
     if not isWriteToGPU then begin
       setlength(vtxRGBA, length(v));
       for i := 0 to (length(v)-1) do
           vRGBA[i].a := 255; //opaque
       vtxRGBA := Copy(vRGBA, Low(vRGBA), Length(vRGBA));
       exit;
     end;
     {$IFDEF COREGL}
     BuildDisplayList(f, v, vRGBA, vao, vbo, Clr);
     {$ELSE}
     displayList:= BuildDisplayList(f, v, vRGBA);
     {$ENDIF}
  end else begin
      Clr.A := 128; //just a bit less than 50% - only for hiding curvature
     setLength(vRGBA,0);
     if not isWriteToGPU then exit;
     {$IFDEF COREGL}
     BuildDisplayList(f, v, vRGBA,vao, vbo, Clr);
     {$ELSE}
     displayList:= BuildDisplayList(f, v, vRGBA);
     {$ENDIF}
  end;
end; // BuildListCore()

procedure FilterAtlas(Clr: TRGBA; maxROI: integer; faces: TFaces; vertices: TVertices; vertexAtlas: TInts; vtxRGBA: TVertexRGBA; atlasHideFilter: TInts; atlasTransparentFilter: TBools; var f: TFaces; var v: TVertices; var vRGBA: TVertexRGBA; var vRemap: TInts);
var
     i, j, nfOK, nvOK, nFace, nVert: integer;
     vOK: array of integer;
     filterLUT: array of boolean;
     fOK: array of boolean;
begin
  nVert := length(vertices);
  nFace := length(faces);
  if (nVert < 3) or (nFace < 1) or (maxROI < 1) or (length(vertexAtlas) <>  nVert) then exit;
  setlength(f,nFace);
  setlength(v,nVert);
  setlength(vRGBA,nVert);
  f := copy(faces, 0, maxint);
  v := copy(vertices, 0, maxint);
  vRGBA := copy(vtxRGBA, 0, maxint);
  if (length(atlasTransparentFilter) > 0) and (length(vtxRGBA) = nVert) then begin
    for i := 0 to (nVert-1) do begin
        j := vertexAtlas[i];
        if (j < 1) or (j >= length(atlasTransparentFilter)) then continue;
        if atlasTransparentFilter[j] then
           vRGBA[i] := Clr;
    end;
  end;
  if (length(atlasHideFilter) < 1) then exit;
    //set up lookup table - list of regions that survive
    setlength(filterLUT, maxROI+1);
    for i := 0 to maxROI do
        filterLUT[i] := false; //assume we will not filter these regions
    for i := 0 to (length(atlasHideFilter) - 1) do
        if (atlasHideFilter[i] > 0) and (atlasHideFilter[i] <= maxROI) then
           filterLUT[atlasHideFilter[i]] := true; //filter the specified region
    //mark surviving vertices
    setlength(vOK, nVert);
    nvOK := 0;
    for i := 0 to (nVert -1) do
        if (vertexAtlas[i] > 0) and (not filterLUT[vertexAtlas[i]]) then begin
             vOK[i] := nvOK;
             nvOK := nvOK + 1;
        end else
            vOK[i] := -1; //does not survive
    setlength(filterLUT, 0);
    //conditionals for unusual situations
    if nvOK = 0 then begin //nothing survives
      setlength(vOK,0);
      setlength(f,0);
      setlength(v,0);
      setlength(vRGBA,0);
      exit;
    end;
    if (nvOK = nVert) then begin //everything survives
       //BuildListCore(Clr, faces, vertices, vtxRGBA); //show complete array
       setlength(vOK,0);
       exit;
    end;
    //see how many faces survive
    setlength(fOK,nFace);
    nfOK := 0;
    for i := 0 to (nFace -1) do begin
        fOK[i] := (vOK[faces[i].X] >= 0) and (vOK[faces[i].Y] >= 0) and (vOK[faces[i].Z] >= 0);
        if fOK[i] then nfOK := nfOK + 1;
    end;
    //no faces survive
    if nvOK = 0 then begin //nothing survives
      setlength(vOK,0);
      setlength(fOK,0);
      exit;
    end;
    //copy surviving vertices
    setlength(v,nvOK);
    for i := 0 to (nVert -1) do
        if (vOK[i] >= 0) then
          v[vOK[i]] := vertices[i];
    //copy surviving vertices
    setlength(f,nfOK);
    j := 0;
    for i := 0 to (nFace -1) do
        if fOK[i] then begin
          f[j].X := vOK[faces[i].X];
          f[j].Y := vOK[faces[i].Y];
          f[j].Z := vOK[faces[i].Z];
          j := j + 1;
        end;
    //set vertex colors (if loaded)
    setlength(vRemap, nVert);
    if length(vtxRGBA) = nVert then begin
      j := 0;
      for i := 0 to (nVert -1) do begin
          if vOK[i] >= 0 then begin
            vRGBA[j] := vRGBA[i]; //note we read from vRGBA not vtxRGBA to preserve gray vertices
            vRemap[j] := i;
            j := j + 1;
          end; //if vertex survives
       end; //for each vertex
      setlength(vRGBA, nvOK);
    end; //if vertices are colored (vtxRGBA)
    setlength(vOK,0);
    setlength(fOK,0);
    //>> BuildListCore(Clr, f, v, vRGBA);
    //release filtered faces, vertices and colors
end;

procedure TMesh.BuildListPartialAtlas(Clr: TRGBA; vtxRGBA: TVertexRGBA);
var
  f: TFaces;
  v: TVertices;
  vRGBA: TVertexRGBA;
  vRemap: TInts;
begin
     FilterAtlas(Clr, AtlasMaxIndex, faces, vertices, vertexAtlas, vtxRGBA, atlasHideFilter, atlasTransparentFilter, f, v, vRGBA, vRemap);
     if length(v) > 0 then
        BuildListCore(Clr, f, v, vRGBA, vRemap);
     setlength(vRemap, 0);
     setlength(f,0);
     setlength(v,0);
     setlength(vRGBA,0);
end;

procedure TMesh.BuildList (Clr: TRGBA);
begin
  if (length(faces) < 1) or (length(vertices) < 3) then exit;
  if ((length(atlasTransparentFilter) > 0) or (length(atlasHideFilter) > 0)) and (length(vertexAtlas) =  length(vertices)) then begin
       BuildListPartialAtlas(Clr, vertexRGBA)    //hide parts of an overlay atlas
  end else
	BuildListCore(Clr, faces, vertices, vertexRGBA)
end;

procedure TMesh.FilterOverlay(c: integer; var f: TFaces; var v: TVertices; var vRGBA: TVertexRGBA);
//given an overlay where each vertex is a scalar intensity, only preserve vertices that exceed threshold
var
   mn, mx: single;
   i, j, nfOK, nvOK, nFace, nVert: integer;
   vOK: array of integer;
   fOK: array of boolean;
   rgb, rgba192: TRGBA;
begin
  setlength(v,0);
  setlength(f,0);
  nVert := length(overlay[c].vertices);
  nFace := length(overlay[c].faces);
  if (overlay[c].atlasMaxIndex > 0) or (overlay[c].OpacityPercent = kLUTinvisible) or (nVert < 3) or (nFace < 1) then exit;
  //if length(overlay[c].intensity) <> nVert then exit; //requires intensity values
  if length(overlay[c].intensity) < nVert then exit; //requires intensity values 2019
  if overlay[c].windowScaledMax > overlay[c].windowScaledMin then begin
     mn := overlay[c].windowScaledMin;
     mx := overlay[c].windowScaledMax;
  end else begin
      mx := overlay[c].windowScaledMin;
      mn := overlay[c].windowScaledMax;
  end;
  setlength(vOK, nVert);
  for i := 0 to (nVert -1) do
      vOK[i] := -1; //does not survive
  nvOK := 0;
  if (mn >= 0) then begin
    for i := 0 to (nVert -1) do
        if (overlay[c].intensity[i] >= mn) then begin
           vOK[i] := nvOK;
           nvOK := nvOK + 1;
        end;
  end else if (mx < 0) then begin
    for i := 0 to (nVert -1) do
        if (overlay[c].intensity[i] <= mx) then begin
           vOK[i] := nvOK;
           nvOK := nvOK + 1;
        end;
  end else //e.g. range -1..+3
      nvOK := nVert;
  if nvOK = 0 then begin //nothing survives
    setlength(vOK,0);
    exit;
  end;
  if (nvOK = nVert) then begin //everything survives
     setlength(vOK,0);
     setlength(v,nVert);
     setlength(f,nFace);
     v := overlay[c].vertices;
     f := overlay[c].faces;
     setlength(vRGBA, nvOK);
     rgba192 := overlay[c].LUT[192];
     for i := 0 to (nvOK -1) do begin
          rgb := inten2rgb(overlay[c].intensity[i], mn, mx, overlay[c].LUT, overlay[c].PaintMode);
          vRGBA[i].r := rgb.r;
          vRGBA[i].g := rgb.g;
          vRGBA[i].b := rgb.b;
          vRGBA[i].a := rgba192.a;
     end;
     exit;
  end;
  //see how many faces survive
  setlength(fOK,nFace);
  for i := 0 to (nFace -1) do
      fOK[i] := (vOK[overlay[c].faces[i].X] >= 0) and (vOK[overlay[c].faces[i].Y] >= 0) and (vOK[overlay[c].faces[i].Z] >= 0);
  nfOK := 0;
  for i := 0 to (nFace -1) do
      if fOK[i] then nfOK := nfOK + 1;
  //copy surviving vertices
  setlength(v,nvOK);
  for i := 0 to (nVert -1) do
      if (vOK[i] >= 0) then
        v[vOK[i]] := overlay[c].vertices[i];
  //copy surviving vertices
  setlength(f,nfOK);
  j := 0;
  for i := 0 to (nFace -1) do
      if fOK[i] then begin
        f[j].X := vOK[overlay[c].faces[i].X];
        f[j].Y := vOK[overlay[c].faces[i].Y];
        f[j].Z := vOK[overlay[c].faces[i].Z];
        j := j + 1;
      end;
  //set RGBA
  setlength(vRGBA, nvOK);
  rgba192 := overlay[c].LUT[192];
  //for i := 0 to (nvOK -1) do
  //    vRGBA[i] := rgba192;
  j := 0;
  for i := 0 to (nVert -1) do begin
      if vOK[i] >= 0 then begin
         rgb := inten2rgb(overlay[c].intensity[i], mn, mx, overlay[c].LUT, overlay[c].PaintMode);
         vRGBA[j].r := rgb.r;
         vRGBA[j].g := rgb.g;
         vRGBA[j].b := rgb.b;
         vRGBA[j].a := rgba192.a;
         j := j + 1;
      end;
   end;
  setlength(vOK,0);
  setlength(fOK,0);
end;

procedure TMesh.BuildListOverlay(Clr: TRGBA);
var
  c, i, nVert, nFace, sumFace, sumVert, nMeshOverlay: integer;
  isIntensityColored : boolean;
  //mn, mx: single;
  oFaces, sFaces, fFaces: TFaces;
  oVerts, sVerts, fVerts: TVertices;
  vRGBA, sRGBA, fRGBA: TVertexRGBA;
  vRemap: TInts;
  //sFaces, sVerts, sRGBA
  //rgb: TRGBA;
begin
  if (length(faces) < 1) or (length(vertices) < 3) or (OpenOverlays < 1) then
     exit;
  nMeshOverlay := 0;
  for c := 1 to OpenOverlays do
      if (overlay[c].OpacityPercent <> kLUTinvisible) and (length(overlay[c].vertices) > 2) then
         nMeshOverlay := nMeshOverlay + 1;
  if nMeshOverlay < 1 then exit;
  nMeshOverlay := 0;
  sumVert := 0;
  sumFace := 0;
  for c := 1 to OpenOverlays do begin
      nVert := length(overlay[c].vertices);
      nFace := length(overlay[c].faces);
      if (overlay[c].OpacityPercent = kLUTinvisible) or (nVert < 3) or (nFace < 1) then continue;
      //isIntensityColored := length(overlay[c].intensity) = nVert;
      isIntensityColored := length(overlay[c].intensity) >= nVert; //2019
      if (not isIntensityColored) then begin
        if (length(overlay[c].atlasHideFilter) > 0) or (length(overlay[c].atlasTransparentFilter) > 0) then begin
          (*setlength(fFaces,nFace);
          setlength(fVerts,nVert);
          setlength(fRGBA,nVert);
          fFaces := copy(overlay[c].faces, 0, maxint);
          fVerts := copy(overlay[c].vertices, 0, maxint);
          fRGBA := copy(overlay[c].vertexRGBA, 0, maxint);*)
          FilterAtlas(overlay[c].LUT[192], overlay[c].AtlasMaxIndex, overlay[c].faces, overlay[c].vertices, overlay[c].vertexAtlas, overlay[c].vertexRGBA, overlay[c].atlasHideFilter, overlay[c].atlasTransparentFilter, fFaces, fVerts, fRGBA, vRemap);
          setlength(vRemap, 0);
          nVert := length(fVerts);
          nFace := length(fFaces);
          if (nVert < 3) or (nFace < 1) then continue;
          setlength(oFaces, sumFace + nFace);
          for i := 0 to (nFace -1) do
              oFaces[i+sumFace] := vectorAdd(fFaces[i], sumVert);
          setlength(oVerts, sumVert + nVert);
          for i := 0 to (nVert -1) do
              oVerts[i+sumVert] := fVerts[i];//fVerts[i]; *)
          setlength(vRGBA, sumVert + nVert);
          for i := 0 to (nVert -1) do
              vRGBA[i+sumVert] := fRGBA[i];
          setlength(fFaces,0);
          setlength(fVerts,0);
          setlength(fRGBA,0);
          sumVert := sumVert + nVert;
          sumFace := sumFace + nFace;
          continue;
        end;
        setlength(oFaces, sumFace + nFace);
        for i := 0 to (nFace -1) do
            oFaces[i+sumFace] := vectorAdd(overlay[c].faces[i], sumVert);
        setlength(oVerts, sumVert + nVert);
        for i := 0 to (nVert -1) do
            oVerts[i+sumVert] := overlay[c].vertices[i];
        setlength(vRGBA, sumVert + nVert);
        for i := 0 to (nVert -1) do
            vRGBA[i+sumVert] := overlay[c].LUT[192];
        if (length(overlay[c].vertexRGBA) = nVert) then
           for i := 0 to (nVert -1) do
               vRGBA[i+sumVert] := overlay[c].vertexRGBA[i];
        (*if isIntensityColored then begin
          if overlay[c].windowScaledMax > overlay[c].windowScaledMin then begin
             mn := overlay[c].windowScaledMin;
             mx := overlay[c].windowScaledMax;
          end else begin
              mx := overlay[c].windowScaledMin;
              mn := overlay[c].windowScaledMax;
          end;
          for i := 0 to (nVert -1) do begin
               rgb := inten2rgb(overlay[c].intensity[i], mn, mx, overlay[c].LUT);
               vRGBA[i+sumVert].r := rgb.r;
               vRGBA[i+sumVert].g := rgb.g;
               vRGBA[i+sumVert].b := rgb.b;
           end;
         end;*) //isIntensityColored > 0
         sumVert := sumVert + nVert;
         sumFace := sumFace + nFace;
         continue;
      end; // not isIntensityColored
      //intensity colored follows
      // we will hide vertices below threshold....
      FilterOverlay(c, sFaces, sVerts, sRGBA);
      nFace := length(sFaces);
      nVert := length(sVerts);
      if (nFace < 3) or (nVert < 1) or (length(sRGBA) <> nVert) then begin
        setlength(sFaces, 0);
        setlength(sVerts, 0);
        setlength(sRGBA, 0);
        continue;
      end;
      setlength(oFaces, sumFace + nFace);
      for i := 0 to (nFace -1) do
          oFaces[i+sumFace] := vectorAdd(sFaces[i], sumVert);
      setlength(oVerts, sumVert + nVert);
      for i := 0 to (nVert -1) do
          oVerts[i+sumVert] := sVerts[i];
      setlength(vRGBA, sumVert + nVert);
      for i := 0 to (nVert -1) do
          vRGBA[i+sumVert] := sRGBA[i];
      sumVert := sumVert + nVert;
      sumFace := sumFace + nFace;
  end; //for each overlay
  {$IFDEF COREGL}
  nFacesOverlay := length(oFaces);
  BuildDisplayList(oFaces, oVerts, vRGBA, vaoOverlay, vboOverlay, Clr);
  {$ELSE}
  displayListOverlay := BuildDisplayList(oFaces, oVerts, vRGBA);
  {$ENDIF}
end; // BuildListOverlay()

procedure TMesh.DrawGL (Clr: TRGBA; clipPlane: TPoint4f; isFlipMeshOverlay: boolean);
begin
  if (isNode) and (isRebuildList) then
     //do NOT exit: we might generate new faces and vertices - e.g nodescale increased from zero
  else if (length(faces) < 1) or (length(vertices) < 3) then
     exit;
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
      BuildList(Clr); //upload geometry as a display list: http://www.songho.ca/opengl/gl_displaylist.html
      BuildListOverlay(Clr);
  end;
  {$IFDEF COREGL}
  if (isFlipMeshOverlay) and true then begin
      if isVisible then begin
        glBindVertexArray(vaoOverlay);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,vboOverlay);
        glDrawElements(GL_TRIANGLES, nFacesOverlay* 3, GL_UNSIGNED_INT, nil);
        glBindVertexArray(0);
      end;
      if (vaoOverlay <> 0) and (vboOverlay <> 0) and (nFacesOverlay > 0) then begin
         RunOverlayGLSL(clipPlane);
         glBindVertexArray(vao);
         glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,vbo);
         glDrawElements(GL_TRIANGLES,  Length(faces)* 3, GL_UNSIGNED_INT, nil);
         glBindVertexArray(0);
      end;
  end else begin
    if isVisible then begin
      glBindVertexArray(vao);
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,vbo);
      glDrawElements(GL_TRIANGLES, Length(faces)* 3, GL_UNSIGNED_INT, nil);
      glBindVertexArray(0);
    end;
    if (vaoOverlay <> 0) and (vboOverlay <> 0) and (nFacesOverlay > 0) then begin
       RunOverlayGLSL(clipPlane);
       glBindVertexArray(vaoOverlay);
       glBindBuffer(GL_ELEMENT_ARRAY_BUFFER,vboOverlay);
       glDrawElements(GL_TRIANGLES, nFacesOverlay * 3, GL_UNSIGNED_INT, nil);
       glBindVertexArray(0);
    end;
  end;
  {$ELSE}
  if (isFlipMeshOverlay) and (displayListOverlay <> 0) then begin
      if isVisible then
         glCallList(displayListOverlay);
      if (displayListOverlay <> 0) then begin
        RunOverlayGLSL(clipPlane);
        glCallList(displayList);
      end;
  end else begin
    if isVisible then
       glCallList(displayList);
    if (displayListOverlay <> 0) then begin
      RunOverlayGLSL(clipPlane);
      glCallList(displayListOverlay);
    end;
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
end; // SwapYZ()

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
end; // SwapZY()

procedure TMesh.CenterOrigin;
var
   i: integer;
begin
     if length(vertices) < 1 then begin
       showmessage('No mesh is open: unable to center origin');
       exit;
     end;
     vectorNegate(origin);
     for i := 0 to (length(vertices) - 1) do
         vectorAdd(vertices[i], origin);
     setDescriptives;
     isRebuildList := true;
     isBusy := false;
end; // CenterOrigin()

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
end; // ReverseFaces()

procedure TMesh.SetDescriptives;
var
   mn, mx: TPoint3f;
   i: integer;
begin
     if length(vertices) < 1 then exit;
     mx := vertices[0];
     mn := mx;
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
    mnV := mn;
    mxV := mx;
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
end; // NormMidPoint()

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

{$IFDEF TREFOIL}
procedure TMesh.MakeTrefoil;
//http://prideout.net/blog/?p=22
function EvaluateTrefoil(s, t: single): TPoint3f;
function Normalized(v: TPoint3f): TPoint3f;
var len: single;
begin
     len := sqrt(sqr(v.x) + sqr(v.y) + sqr(v.z));
     result := v;
     if len = 0 then
        exit;
     result.X := result.X / len;
     result.Y := result.Y / len;
     result.Z := result.Z / len;
end;
function Cross(v1, v2: TPoint3f): TPoint3f;
begin
 result.x:=(V1.y*V2.z)-(V1.z*V2.y);
 result.y:=(V1.z*V2.x)-(V1.x*V2.z);
 result.z:=(V1.x*V2.y)-(V1.y*V2.x);
end;
const
     TwoPi = 2 * Pi;
    a = 0.5;
    b = 0.3;
    c = 0.5;
    d = 0.1;
var
   u,v,r,x,y,z: single;
   q, qvn, ww, dv : TPoint3f;
begin
    u := (1 - s) * 2 * TwoPi;
    v := t * TwoPi;
    r := a + b * cos(1.5 * u);
    x := r * cos(u);
    y := r * sin(u);
    z := c * sin(1.5 * u);
    dv.x := -1.5 * b * sin(1.5 * u) * cos(u) -  (a + b * cos(1.5 * u)) * sin(u);
    dv.y := -1.5 * b * sin(1.5 * u) * sin(u) +
            (a + b * cos(1.5 * u)) * cos(u);
    dv.z := 1.5 * c * cos(1.5 * u);
    q := Normalized(dv);
    qvn.X := q.y;
    qvn.Y := -q.X;
    qvn.z := 0;
    qvn := Normalized(qvn);
    ww := Cross(q, qvn);
    result.x := x + d * (qvn.x * cos(v) + ww.x * sin(v));
    result.y := y + d * (qvn.y * cos(v) + ww.y * sin(v));
    result.z := z + d * ww.z * sin(v);
end;
const
  kSlices = 128;
  kStacks = 32;
  kVertexCount = kSlices * kStacks;
  kIndexCount = kVertexCount * 2;
var
   ds, dt, s, t: single;
   i,j,k, n: integer;
begin
  setlength(faces, kIndexCount);
  n := 0;
  k := 0;
  for i := 0 to (kSlices-1) do begin
    for j := 0 to (kStacks-1) do begin
        faces[k].x := n + j;
        faces[k].z := n + (j + 1) mod kStacks;
        faces[k].y := (n + j + kStacks) mod kVertexCount;
        k := k + 1;
        faces[k].x := (n + j + kStacks) mod kVertexCount;
        faces[k].z := (n + (j + 1) mod kStacks) mod kVertexCount;
        faces[k].y := (n + (j + 1) mod kStacks + kStacks) mod kVertexCount;
        k := k + 1;
      end;
      n := n + kStacks;
  end;
  setlength(vertices, kVertexCount);
  ds := 1.0 / kSlices;
  dt := 1.0 / kStacks;
  // The upper bounds in these loops are tweaked to reduce the
  // chance of precision error causing an incorrect # of iterations.
  s := 0.0;
  i := 0;
  while (s < 1 - ds / 2) do begin
    s := s + ds;
    t := 0;
    while (t < 1 - dt / 2) do begin
      t := t + dt;
      vertices[i] := EvaluateTrefoil(s, t);
      i := i+1;
    end;
  end;
end; // MakeTrefoil()
{$ENDIF}

procedure TMesh.MakePyramid;
begin
 {$IFDEF TREFOIL}
   MakeTrefoil;
 {$ELSE}
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
   {$ENDIF}
     SetDescriptives;
end; // MakePyramid()

(*function TMesh.AtlasMaxIndex: integer; //returns maximim atlas region, e.g. JHU has 192 regions labelled 1..192, so 192 - returns 0 if not an atlas
var
   num_v, i: integer;
begin
  result := 0;
  num_v := length(vertices);
  if  (num_v < 3) or (length(vertexAtlas) <>  num_v) then exit;
  result := vertexAtlas[0];
  for i := 0 to (num_v -1) do
      if vertexAtlas[i] > result then
         result := vertexAtlas[i];
  if (result < 0) then
     result := 0;
end;*)

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
     OverlappingOverlaysOverwrite := true;
     isVisible := true;
     OpenOverlays := 0;
     OverlayTransparency := 0;
     isRebuildList := true;
     setlength(faces,0);
     setlength(vertices,0);
     setlength(nodes, 0);
     setlength(edges,0,0);
     setlength(vertexRGBA,0);
     setlength(vertexAtlas,0);
     setlength(atlasHideFilter,0);
     setlength(atlasTransparentFilter,0);
     setlength(tempIntensityLUT,0);
     errorString := '';
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
     nodePrefs.isNoNodeWithoutEdge := false;
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
     (*for i := kMinOverlayIndex to kMaxOverlays do begin
         setlength(overlay[i].intensity,0);
         setlength(overlay[i].faces,0);
         setlength(overlay[i].vertices,0);
         setlength(overlay[i].vertexRGBA,0);
     end;*)
     closeOverlaysCore;
end; // Create()

(*
//signature does not distinguish between rare quad files and common area/curv/thickness maps
// one can not use file size to correctly detect, as quad files have a footer filled with random length notes
// examples are lh.qsphere and rh.qshere
// FreeSurfer does not generate .curv or .thick files for these
// in contrast, the triangular lh.sphere and rh.sphere do have corresponding .curv, .thick etc. files
procedure TMesh.LoadFreeSurferQuad(const FileName: string);
//https://cbi.nyu.edu/svn/mrTools/trunk/mrUtilities/File/FreeSurfer/freesurfer_read_surf.m
//https://raw.githubusercontent.com/fieldtrip/fieldtrip/master/external/freesurfer/read_surf.m
const
  kMagic = 16777215;
type
  TV16 = packed record
     X: int16;
     Y: int16;
     Z: int16;
   end;
var
   f: File;
   magic, num_v, num_q, a, b, c, d : uint32;
   FSz, ExpectedFSz,
   i, j: integer;
   v16s: array of TV16;
function freesurfer_fread3(): uint32;
var
   b1,b2,b3: byte;
begin
  blockread(f, b1, 1);
  blockread(f, b2, 1);
  blockread(f, b3, 1);
  {$IFDEF ENDIAN_LITTLE}
  result := b3 + (b2 shl 8) + (b1 shl 16);
  {$ELSE}
  result := b1 + (b2 shl 8) + (b3 shl 16);
  {$ENDIF}
end;
begin
     FSz := FSize(FileName);
     if (FSz < 64) then exit;
     AssignFile(f, FileName);
     FileMode := fmOpenRead;
     Reset(f,1);
     magic := freesurfer_fread3();
      //since these files do not have a file extension, check first 3 bytes "0xFFFFFF00"
     num_v := freesurfer_fread3();
     num_q := freesurfer_fread3();
     if (magic <> kMagic) or (num_q < 1) or (num_v < 4) then begin
        CloseFile(f);
        exit;
     end;
     ExpectedFSz := 9 + (3 * 2 * num_v) + (4 * 3 * num_q);
     if (ExpectedFSz > FSz) then begin
       CloseFile(f);
       showmessage(format('Error reading FreeSurfer Quad file (verts = %d, quads = %d): expected file of %d bytes, not  %d',[num_v, num_q, ExpectedFSz, FSz]));
       exit;
     end;
     setlength(vertices, num_v); //vertices = zeros(num_f, 9);
     setlength(v16s, num_v);
     blockread(f, v16s[0], num_v * sizeof(TV16));
     for i := 0 to (num_v - 1) do begin
         {$IFDEF ENDIAN_LITTLE}
         v16s[i].X := swap(v16s[i].X);
         v16s[i].Y := swap(v16s[i].Y);
         v16s[i].Z := swap(v16s[i].Z);
         {$ENDIF}
         vertices[i].X := v16s[i].X * 0.01;
         vertices[i].Y := v16s[i].Y * 0.01;
         vertices[i].Z := v16s[i].Z * 0.01;
     end;
     v16s := nil;
     //showmessage(format('OK reading FreeSurfer Quad file (verts = %d, quads = %d): expected file of %d bytes,not  %d',[num_v, num_q, ExpectedFSz, FSz]));
     setlength(faces, num_q * 2); //each quadrilateral is 2 triangles
     j := 0;
     for i := 0 to (num_q - 1) do begin
         a := freesurfer_fread3();
         b := freesurfer_fread3();
         c := freesurfer_fread3();
         d := freesurfer_fread3();
         faces[j] := pti(a,b,c);
         j := j + 1;
         faces[j] := pti(a,c,d);
         j := j + 1;
     end;
     CloseFile(f);
end; *)

procedure TMesh.LoadPial(const FileName: string);
//simple format used by Freesurfer  BIG-ENDIAN
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bpial/readPial.m
// http://www.grahamwideman.com/gw/brain/fs/surfacefileformats.htm
{$DEFINE FASTPIAL} //800 -> 16ms
var
   f: File;
   i, sz, num_EOLN: integer;
   b: byte;
   num_v, num_f : LongWord;
   sig : Int64;
   {$IFNDEF FASTPIAL}
   vValues: array [1..3] of single;
   fValues: array [1..3] of LongWord;
   {$ENDIF}
begin
     if (FSize(FileName) < 64) then exit;
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
        //LoadFreeSurferQuad(FileName);
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
     {$IFDEF FASTPIAL}
     blockread(f, vertices[0], num_v * sizeof(TPoint3f));
     for i := 0 to (num_v-1) do begin
         SwapSingle(vertices[i].x);
         SwapSingle(vertices[i].y);
         SwapSingle(vertices[i].z);
     end;
     blockread(f, faces[0], num_f * sizeof(TPoint3i));
     for i := 0 to (num_f-1) do begin
         SwapLongInt(faces[i].x);
         SwapLongInt(faces[i].y);
         SwapLongInt(faces[i].z);
     end;
     {$ELSE}
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
     {$ENDIF}//fast pial
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
  strlst:=TStringList.Create;
  while not EOF(f) do begin
        ReadLn(f, str); //make sure to run CheckMesh after this, as these are indexed from 1!
        if (length(str) > 0) and (str[1] <> '#') then begin
           strlst.DelimitedText := str;
           if strlst.Count < 1 then continue;
           v1 := strtofloatdef(strlst[0],0);
           if (strlst.count > 1) then
              v2 := strtofloatdef(strlst[1],0);
           if (strlst.count > 2) then
              v3 := strtofloatdef(strlst[2],0);
           if (strlst.count > 3) then
              v4 := strtofloatdef(strlst[3],0);
           strlst.free;
           exit;
        end;
  end;
  strlst.free;
end;

procedure TMesh.LoadNV(const FileName: string);
//simple format used by BrainNet Viewer https://www.nitrc.org/projects/bnv/
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bnv/readNv.m
var
   f: TextFile;
   i, num_v, num_f: integer;
   v1,v2,v3, v4: single;
begin
     if (FSize(FileName) < 27) then exit;
     FileMode := fmOpenRead;
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

function TMesh.LoadOffBin(const FileName: string): boolean;
//There seem to be two binary forms: files either begin with "OFF BINARY" or OFF_GENERIC_MAGIC
//This decoder is for the GeomView OFF format
//  http://www.geomview.org/docs/html/Binary-format.html
//  http://netghost.narod.ru/gff/sample/code/off
//  http://www.geomview.org/docs/html/OFF.html
//  For example datasets http://www.pc.rhul.ac.uk/staff/J.Larsson/software/surfrelax/download/download.html
//This is distinct from the "originally developed by WSE" OFF format described in these pages (seems extinct)
//  http://paulbourke.net/dataformats/off/
//  http://netghost.narod.ru/gff/vendspec/off/index.htm
//  #define OFF_GENERIC_MAGIC	0xBEEFBEEFL
label
   666;
var
   str: string;
   f: TFByte;//TextFile;
   FSz, num_v, num_f, i, j: integer;
   binByt: array of byte;
function i32(): uint32;
var v: uint32;
begin
     BlockRead(f,v, sizeof(v));
     {$IFDEF ENDIAN_LITTLE}
     SwapLongWord(v);
     {$ENDIF}
     //showmessage(inttostr(v));
     result := v;
end;
begin
  result := false;
  FSz := FSize(FileName);
  if (FSz < 64) then exit;
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  Reset(f,1);
  str := '';
  ReadLnBin(f, str); //signature
  if (not AnsiContainsText(str, 'OFF')) or (not AnsiContainsText(str, 'BINARY')) then begin
    showmessage('Does not appear to be a OFF BINARY file.');
    goto 666;
  end;
  num_v := i32();
  num_f := i32();
  i := i32();
  if (num_v < 3) or (num_f < 1) then begin
     showmessage('Corrupt file: must have at least 3 vertices and one face');
     goto 666;
  end;
  i := (num_v * 12) + (num_f * 20) + (FilePos(f));
  if (i <> FSz) then begin
     showmessage(format('Only able to read BINARY OFF files that are triangular meshes without colors. Size expected: %d Actual: %d', [i, FSz]));
     goto 666;
  end;
  result := true;
  setlength(vertices, num_v);
  i := num_v * 12; //X,Y,Z * 4bytes
  setlength(binByt, i);
  blockread(f, binByt[0], i);
  j := 0;
  for i := 0 to (num_v - 1) do begin
      Move(binByt[j], vertices[i].X, 4);
      j := j + 4;
      Move(binByt[j], vertices[i].Y, 4);
      j := j + 4;
      Move(binByt[j], vertices[i].Z, 4);
      j := j + 4;
      {$IFDEF ENDIAN_LITTLE}
      SwapSingle(vertices[i].X);
      SwapSingle(vertices[i].Y);
      SwapSingle(vertices[i].Z);
      {$ENDIF}
  end;
  setlength(faces, num_f);
  i := num_f * 20; //X,Y,Z * 4bytes
  setlength(binByt, i);
  blockread(f, binByt[0], i);
  j := 0;
  for i := 0 to (num_f - 1) do begin
    j := j + 4;
    Move(binByt[j], faces[i].X, 4);
    j := j + 4;
    Move(binByt[j], faces[i].Y, 4);
    j := j + 4;
    Move(binByt[j], faces[i].Z, 4);
    j := j + 8;
    {$IFDEF ENDIAN_LITTLE}
    SwapLongInt(faces[i].X);
    SwapLongInt(faces[i].Y);
    SwapLongInt(faces[i].Z);
    {$ENDIF}
  end;
  binByt := nil;
  result := true;
  666:
  closefile(f);
  if result then exit;
  faces := nil;
  vertices := nil;
end; // LoadOffBin()

//{$DEFINE OFFSIMPLE} //Simple reader is faster, but only handles triangular meshes
{$IFDEF OFFSIMPLE}
function TMesh.LoadOff(const FileName: string): boolean;
//http://paulbourke.net/dataformats/off/
var
   f: TextFile;
   i, num_v, num_f: integer;
   s: string;
   v1,v2,v3, v4: single;
begin
     result := false;
     if (FSize(FileName) < 64) then exit;
     FileMode := fmOpenRead;
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
     result := true;
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
{$ELSE}


function TMesh.LoadOff(const FileName: string): boolean;
//http://paulbourke.net/dataformats/off/
//For examples where faces have more then 3 vertices see
// http://www.cs.princeton.edu/courses/archive/spr08/cos426/assn2/formats.html
// http://people.sc.fsu.edu/~jburkardt/data/off/abstr.off
// http://www.holmes3d.net/graphics/offfiles/
label
   666;
var
   f: TextFile;
   i, j, k, indxPrev, num_v, num_f, newF: integer;
   s: string;
   indx1: integer;
   s1, s2, s3, s4: single;
   strlst : TStringList;
begin
     result := false;
     if (FSize(FileName) < 64) then exit;
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
     Reset(f);
     Readln(f,s);
     while (not EOF(f)) and ((length(s) < 1) or (s[1]='#')) do
           Readln(f,s); //files can begin with comments: http://www.geomview.org/docs/oogltour.html
     if (pos('OFF', s) <> 1) and (pos('NOFF', s) <> 1) and (pos('COFF', s) <> 1) then begin
        showmessage('Corrupt file: OFF files must begin with "OFF", "COFF" or "NOFF"');
        CloseFile(f);
        exit;
     end;
     if AnsiContainsText(s, 'BINARY') then begin
       CloseFile(f);
       exit(LoadOffBin(FileName));
     end;
     ReadlnSafe(f, s1,s2,s3, s4);
     num_v := round(s1);
     num_f := round(s2);
     if (num_v < 3) or (num_f < 1) then begin  //e.g. faces on multiple lines, see https://github.com/libigl/libigl
        showmessage('Corrupt file: must have at least 3 vertices and one face');
        CloseFile(f);
        exit;
     end;
     setlength(vertices, num_v);
     for i := 0 to (num_v - 1) do
         ReadlnSafe(f, vertices[i].X, vertices[i].Y, vertices[i].Z, s4);
     setlength(faces, num_f);
     i := 0;
     j := 0;
     strlst:=TStringList.Create;
     while (i < (num_f)) and (not EOF(f)) do begin
         Readln(f, s);
         if (length(s) < 7) or (s[1] = '#') then continue; //skip comments
         strlst.DelimitedText := s;
         newF := StrToIntDef(strlst[0],0);
         if newF < 3 then continue;
         if (newF+1) > (strlst.Count) then begin
            showmessage('Unable to read OFF file (perhaps you can open with MeshLab and export as another format');
            goto 666;
         end;
         indx1 := StrToIntDef(strlst[1],0);
         indxPrev := StrToIntDef(strlst[2],0);
         if (j + newF) > length(faces) then
            setlength(faces, j + 4096); //non-triangular meshes will be composed of more triangles than num_f
         for k := 3 to newF do begin
             faces[j].X := indx1;
             faces[j].Y := indxPrev;
             faces[j].Z := StrToIntDef(strlst[k],0);
             indxPrev :=  faces[j].Z;
             j := j + 1;
         end;
         i := i + 1;
     end;
     setlength(faces, j);
     result := true;
     666:
     if not result then begin
        setlength(faces,0);
        setlength(vertices,0);
     end;
     strlst.free;
     CloseFile(f);
end;
{$ENDIF}

function TMesh.LoadASC_Sparse(const FileName: string): boolean;
//Freesurfer ascii format (almost identical to LoadNV)
// see SPM's read_freesurfer_file or http://www.grahamwideman.com/gw/brain/fs/surfacefileformats.htm
//be aware AFNI includes the file "lh.flat.patch.asc" that breaks FreeSurfer
//  https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/demos/SmallInstalls/DemoData.html#suma-demo
//Note, while this reads sparse patch files successfully, it renumbers the vertices
// this may disrupt overlays that assume the full range of vertices
label 123;
const
  kBlockSz = 8192;
var
   f: TextFile;
   i, j, vno, num_v, num_f, maxVno: integer;
   vertexRenumbers: TInts;
   str, strTrim: string;
   strlst : TStringList;
begin
     result := false;
     maxVno := -1;
     if (FSize(FileName) < 64) then exit;
     FileMode := fmOpenRead;
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
       setlength(vertexRenumbers, num_v);
       for i := 0 to (num_v -1) do
           vertexRenumbers[i] := i; //input => output
       strlst:=TStringList.Create;
       i := 0;
       while not eof(f) and (i < num_v) do begin
             ReadLn(f, str);
             if length(str) < 1 then continue;
             if str[1] = '#' then continue;
             j := posex('vno=', str);
             if j > 0 then begin
               strTrim := copy(str, j+4, maxint);
               vno := StrToIntDef(strTrim, -1);
               if vno < 0 then begin
                  showmessage('"'+strTrim+'"Unable to parse AFNI patch file: '+str);
                  goto 123;
               end;
               if vno > maxVno then begin
                 maxVno := vno;
                 if vno >= length(vertexRenumbers) then
                    setlength(vertexRenumbers, vno + kBlockSz);
               end;
               vertexRenumbers[vno] := i;
               continue;
             end;
             strlst.DelimitedText := str;
             if strlst.Count < 3 then continue;
             vertices[i].x := strtofloatdef(strlst[0],0);
             vertices[i].y := strtofloatdef(strlst[1],0);
             vertices[i].z := strtofloatdef(strlst[2],0);
             i := i + 1;
       end;
       if (i < num_v) then
          goto 123;

       setlength(faces, num_f);
       i := 0;
       while not eof(f) and (i < num_f) do begin
             ReadLn(f, str);
             if length(str) < 1 then continue;
             if str[1] = '#' then continue;
             strlst.DelimitedText := str;
             if strlst.Count < 3 then continue;
             faces[i].x := strtointdef(strlst[0],0);
             faces[i].y := strtointdef(strlst[1],0);
             faces[i].z := strtointdef(strlst[2],0);
             if maxVno > 0 then begin
                faces[i].x := vertexRenumbers[faces[i].x];
                faces[i].y := vertexRenumbers[faces[i].y];
                faces[i].z := vertexRenumbers[faces[i].z];
             end;
             i := i + 1;
       end;
       if (i < num_f) then goto 123;
       result := true;
     123:
       CloseFile(f);
       strlst.free;
     if not (result) then begin
       setlength(vertices, 0);
       setlength(faces, 0);
     end;
end; // LoadASC_Sparse()

// ascii format created by FreeSurfers' mris_convert
function TMesh.LoadASC_SRF(const FileName: string): boolean;
//simple Freesurfer ascii format (almost identical to LoadNV) reader
//  complicated sparse "PATCH" format will cause an exception that will use the
//  slower but more robust LoadASC_Sparse reader than can handle patch files
label 123;
var
   f: TextFile;
   i, num_v, num_f: integer;
   str: string;
begin
     result := false;
     if (FSize(FileName) < 64) then exit;
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
       Reset(f);
       Readln(f,str);  //% #!ascii
       if PosEx('#!ascii',str) < 1 then begin
          CloseFile(f);
          exit;
       end;
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
       for i := 0 to (num_v - 1) do begin
           try
              ReadLn(f, vertices[i].X, vertices[i].Y, vertices[i].Z);
           except
             CloseFile(f);
             setlength(vertices, 0);
             setlength(faces, 0);
             result := LoadASC_Sparse(FileName);
             exit;
           end;
       end;
       setlength(faces, num_f);
       for i := 0 to (num_f - 1) do
           ReadLn(f, faces[i].X, faces[i].Y, faces[i].Z); //make sure to run CheckMesh after this, as these are indexed from 1!
       result := true;
     123:
       CloseFile(f);
     if not (result) then begin
       setlength(vertices, 0);
       setlength(faces, 0);
     end;
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
   denom,frac, radius: single;
   lSphere: TMesh;
   cylFace: TFaces;
   cylVert: TVertices;
   isNodeHasEdge: array of boolean;
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
        if (nodePrefs.isNoNodeWithoutEdge) and (not isNodeHasEdge[ii]) then exit;
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
     //find out which edges survive
     nEdgeThresh := 0;
     edgeColorVaries := false;
     if (nodePrefs.isEdgeColorVaries) and (nodePrefs.maxEdge <> nodePrefs.minEdge) then
        edgeColorVaries := true;
     setlength(isNodeHasEdge, nNode);
     for i := 0 to (nNode - 1) do
         isNodeHasEdge[i] := false;
     //thresholdEdgeSize := nodePrefs.threshEdge * nodePrefs.maxEdge;
     if (nodePrefs.scaleEdgeSize > 0) and (length(Edges) > 0) then begin
        for i := 0 to (nNode - 2) do
           for j := (i+1) to (nNode -1) do
               if isEdgeSurvives(i,j) then begin
                  inc(nEdgeThresh);
                  isNodeHasEdge[i] := true;
                  isNodeHasEdge[j] := true;
               end;
     end;
     //find out which nodes survive
     nNodeThresh := 0;
     if nodePrefs.scaleNodeSize > 0 then begin
       for n := 0 to (nNode -1) do
           if isNodeSurvives(n) then //if (nodes[n].Radius >= thresholdNodeSize) then
              inc(nNodeThresh);
     end;
     nodeColorVaries := false;
     if (nodePrefs.isNodeThresholdBySize) and (nodePrefs.isNodeColorVaries) and (nodePrefs.minNodeSize <> nodePrefs.maxNodeSize) then
        nodeColorVaries := true;
     if (not nodePrefs.isNodeThresholdBySize) and (nodePrefs.isNodeColorVaries) and (nodePrefs.minNodeColor <> nodePrefs.maxNodeColor) then
        nodeColorVaries := true;
     if (nEdgeThresh = 0) and (nNodeThresh = 0) then begin
       setlength(self.faces, 0);
       setlength(self.vertices, 0);
       setlength(self.vertexRGBA, 0);
       exit;
     end;
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
     cLUT := UpdateTransferFunction (nodePrefs.NodeLUTindex, false);
     //draw nodes/balls/spheres
     if nNodeThresh > 0 then begin
       for n := 0 to (nNode-1) do begin
           if isNodeSurvives(n) then begin //if (nodes[n].Radius >= thresholdNodeSize) then begin
              if nodeColorVaries then begin
                 if nodePrefs.isNodeThresholdBySize then
                    clr := cLUT[round(255 * Value2Frac(nodes[n].Radius, nodePrefs.minNodeThresh, nodePrefs.maxNodeThresh) )]
                 else
                    clr := cLUT[round(255 * Value2Frac(nodes[n].Clr, nodePrefs.minNodeThresh, nodePrefs.maxNodeThresh) )]
                 (*if nodePrefs.isNodeThresholdBySize then
                    clr := cLUT[round(255 * Value2Frac(nodes[n].Clr, nodePrefs.minNodeColor, nodePrefs.maxNodeColor) )]
                 else
                    clr := cLUT[round(255 * Value2Frac(nodes[n].Clr, nodePrefs.minNodeThresh, nodePrefs.maxNodeThresh) )] *)
              end else
                  clr := cLUT[192];
              lSphere.MakeSphere; //new unit sphere
             //showmessage(format('%g %g %g %g',[ nodes[n].Radius, nodes[n].X, nodes[n].Y, nodes[n].Z]));
             if (nodePrefs.minNodeThresh = nodePrefs.maxNodeThresh) and (nodePrefs.isNodeThresholdBySize) then
                radius := 1
             else
                radius := nodes[n].Radius;
             ScaleTranslate(lSphere, radius * nodePrefs.scaleNodeSize, nodes[n].X, nodes[n].Y, nodes[n].Z);
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
     // draw sticks/cylinders
     if nEdgeThresh > 0 then begin
       cLUT := UpdateTransferFunction (nodePrefs.edgeLUTindex, false);
       i := nodePrefs.edgeLUTindex + 1;
       cLUTneg := UpdateTransferFunction (i, false);
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
     setlength(isNodeHasEdge,0);
end;

function TMesh.LoadCluster(const FileName: string): boolean;
//https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/Cluster
//Assumes cluster in MNI space
//  cluster --in=spmT --thresh=5 --mm >> max.node
const
	kCache = 100;
var
   f: TextFile;
   str: string;
   num_node: integer;
   strlst : TStringList;
begin
  result := false;
  if (FSize(FileName) < 64) then exit;
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  Reset(f);
  ReadLn(f, str);
  //check signature: "Cluster Index	Voxels	MAX	MAX X (mm)	MAX Y (mm)	MAX Z (mm)	COG X (mm)	COG Y (mm)	COG Z (mm)"
  if PosEx('Cluster',str) <> 1 then begin
     CloseFile(f);
     exit;
  end;
  if PosEx('(mm)',str) < 1 then begin
     CloseFile(f);
     showmessage('Clusters should report peaks in mm not voxels (hint: "cluster --in=spmT --thresh=5 --mm >> max.node" ).');
     exit;
  end;
  strlst:=TStringList.Create;
  num_node := 0;
  while not EOF(f) do begin
	  ReadLn(f, str); //make sure to run CheckMesh after this, as these are indexed from 1!
	  if (length(str) < 9) then continue;
	  strlst.DelimitedText := str;
	  if (strlst.count > 4) then begin
		  if length(nodes) <= num_node then
			  setlength(nodes, num_node + kCache);
		  nodes[num_node].Clr := strtofloat(strlst[2]);
		  nodes[num_node].X := strtofloat(strlst[3]);
		  nodes[num_node].Y := strtofloat(strlst[4]);
		  nodes[num_node].Z := strtofloat(strlst[5]);
		  nodes[num_node].radius := 1;
                  //showmessage(format('%gx%gx%g=%g',[nodes[num_node].X,nodes[num_node].Y,nodes[num_node].Z,nodes[num_node].Clr]));
		  inc(num_node);
	  end;
  end;
  setlength(nodes, num_node);
  CloseFile(f);
  strlst.free;
  result := true;
end;

function TMesh.LoadNodeTxt(const FileName: string): boolean;
label
     123;
var
  num_node, c, i : integer;
  s: string;
  sList: TStringList;
  fp: TextFile;
begin
  result := false;
  num_node := 0; //rows
  c := 0; //cols
  if not Fileexists(FileName) then exit;
  sList := TStringList.Create;
  Filemode := fmOpenRead;
  AssignFile(fp,FileName);
  reset(fp);
  while not EOF(fp) do begin
    readln(fp, s);
    if (length(s) < 1) or (s[1] = '#') then continue;
    sList.DelimitedText := s;
    if c = 0 then c := sList.Count;
    if (c <> 3) then begin
       showmessage('Error reading nodes: number of columns must be 3: '+FileName);
       goto 123;
    end;
    num_node := num_node + 1;
  end;
  if (num_node < 2) or (c <> 3)  then begin
     showmessage(format('Text Nodes unexpected Rows*Columns (%d*%d): node files should be N*3', [num_node, c]));
     goto 123;
  end;
  setlength(self.nodes, num_node);
  num_node := 0;
  reset(fp);
  while not EOF(fp) do begin
    readln(fp, s);
    if (length(s) < 1) or (s[1] = '#') then continue;
    sList.DelimitedText := s;
    if sList.Count <> 3 then continue;
    self.nodes[num_node].X := strtofloatdef(sList[0], 0.0);
    self.nodes[num_node].Y := strtofloatdef(sList[1], 0.0);
    self.nodes[num_node].Z := strtofloatdef(sList[2], 0.0);
    self.nodes[num_node].Clr := 1;
    self.nodes[num_node].radius := 1;
    num_node := num_node + 1;
  end;
  //start - raw copy from LoadNode() - maybe make function
  self.isNode := true;
  nodePrefs.minNodeColor := nodes[0].Clr;
  nodePrefs.maxNodeColor := nodes[0].Clr;
  nodePrefs.minNodeSize := nodes[0].radius;
  nodePrefs.maxNodeSize := nodes[0].radius;
  if num_node > 0 then begin
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
  isRebuildList := true;
  //end copy
  result := true;
  123:
  sList.Free;
  CloseFile(fp);
  Filemode := fmOpenReadWrite;
end;


const
  kEmbeddedEdge = '#ENDNODE'; //signature for node file that contains edge values
procedure TMesh.LoadNode(const FileName: string; out isEmbeddedEdge: boolean);
//BrainNet Node And Edge Connectome Files
//http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0068910
label 555, 666;
var
   f: TextFile;
   str: string;
   num_node,i: integer;
   strlst : TStringList;
begin
     isEmbeddedEdge := false;
     if (FSize(FileName) < 9) then exit;
     num_node := 0;
     if LoadCluster(FileName) then
        goto 555;
     strlst:=TStringList.Create;
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
     Reset(f);
     while not EOF(f) do begin
           ReadLn(f, str); //make sure to run CheckMesh after this, as these are indexed from 1!
           if (length(str) > 0) and (str[1] <> '#') then
               inc(num_node);
     end;
     if (num_node < 1) then goto 666;
     setlength(self.nodes, num_node);
     Reset(f);
     num_node := 0;
     while (not EOF(f)) and (not isEmbeddedEdge)  do begin
           ReadLn(f, str); //make sure to run CheckMesh after this, as these are indexed from 1!
           if (length(str) > 0) and (str[1] <> '#') then begin
              strlst.DelimitedText := str;
              if (strlst.count > 4) then begin
                 self.nodes[num_node].X := strtofloatdef(strlst[0], 0);
                 self.nodes[num_node].Y := strtofloatdef(strlst[1], 0);
                 self.nodes[num_node].Z := strtofloatdef(strlst[2], 0);
                 self.nodes[num_node].Clr := strtofloatdef(strlst[3], 0);
                 self.nodes[num_node].radius := strtofloatdef(strlst[4], 0);
                 inc(num_node);
              end;
           end else if (pos(kEmbeddedEdge, uppercase(str)) > 0)then
                isEmbeddedEdge := true;
     end;
     CloseFile(f);
     strlst.free;
555:
     self.isNode := true;
     nodePrefs.minNodeColor := nodes[0].Clr;
     nodePrefs.maxNodeColor := nodes[0].Clr;
     nodePrefs.minNodeSize := nodes[0].radius;
     nodePrefs.maxNodeSize := nodes[0].radius;
     if num_node > 0 then begin
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
     isRebuildList := true;
     exit;
 666:
  showmessage('Unable to load Nodes');
  CloseFile(f);
  strlst.free;
end; // LoadNode()

function TMesh.LoadEdge(const FileName: string; isEmbeddedEdge: boolean): boolean;
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
  if not FileExistsF(FileName) then exit;
  if (FSize(FileName) < 8) then exit;
  if (length(nodes) < 2) then begin
       showmessage('You must load a NODE file before loading your EDGE file.');
       exit;
  end;
  strlst:=TStringList.Create;
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  Reset(f);
  if isEmbeddedEdge then begin
     str := '';
     while (not EOF(f)) and (pos(kEmbeddedEdge, uppercase(str)) = 0) do
        ReadLn(f, str);
     if (EOF(f)) then begin
        showmessage('Unable to find tag "'+kEmbeddedEdge+'" in '+ FileName);
        goto 666;
     end;
  end;
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
  //2nd pass
  Reset(f);
  if isEmbeddedEdge then begin
     str := '';
     while (not EOF(f)) and (pos(kEmbeddedEdge, uppercase(str)) = 0) do
        ReadLn(f, str);
     if (EOF(f)) then begin
        showmessage('Unable to find tag "'+kEmbeddedEdge+'" in '+ FileName);
        goto 666;
     end;
  end;
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

function TMesh.LoadObjMni(const FileName: string): boolean;
//This is for MNI Obj files, not the WaveFront Obj format
//This code only reads the most popular ASCII polygon mesh that starts with 'P'
//Like AFNI SurfMesh we only handle the ACII variant of this format
// http://www.stat.wisc.edu/~mchung/softwares/mesh/mesh.html
// https://bigbrain.loris.ca/main.php?test_name=brainsurfaces
// http://www.bic.mni.mcgill.ca/users/mishkin/mni_obj_format.pdf
label
   666;
var
   f: TextFile;
   r, g, b, a: single;
   c: char;
   i, j, num_v, num_f, num_c, colour_flag: integer;
   normals: TVertices;
begin
  result := false;
  if (FSize(FileName) < 64) then exit;
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  Reset(f);
  Read(f,c);
  if (c <> 'P') then begin
     CloseFile(f);
     exit;
  end;
  for i := 1 to 5 do Read(f, g);//ignore: 5*surfprop [ambient, diffuse, specular, shininess, transparency)
  Read(f, num_v); //npoints
  if (num_v < 3) then goto 666;
  setlength(vertices, num_v);
  for i := 0 to high(vertices) do
      Read(f, vertices[i].X, vertices[i].Y, vertices[i].Z);
  setlength(normals, num_v);
  for i := 0 to high(vertices) do
      Read(f, normals[i].X, normals[i].Y, normals[i].Z);
  setlength(normals, 0); //ignore
  Read(f, num_f); //nitems
  Read(f, colour_flag); //colour_flag
  if (num_f < 1) or (colour_flag < 0) or (colour_flag > 2) then goto 666;
  //0 = 1 color for all items, 1 = per line, 2 = per vertex
  if colour_flag = 0 then
     num_c := 1
  else if colour_flag = 1 then
     num_c := num_f
  else
      num_c := num_v; //per vertex
  if colour_flag = 2 then begin
     setlength(vertexRGBA, num_v); //per vertex
     for i := 0 to high(vertexRGBA) do begin
       Read(f, r, g, b, a);
       vertexRGBA[i].r := round(255*r);
       vertexRGBA[i].g := round(255*g);
       vertexRGBA[i].b := round(255*b);
       vertexRGBA[i].a := round(255*a);
     end;
  end else begin
    for i := 1 to num_c do
      Read(f, r, g, b, a); //A colour is defined by four floating-point numbers in the interval 0..1
  end;
  for i := 1 to num_f do
      Read(f, j);
  setlength(faces, num_f);
  for i := 0 to high(faces) do
      Read(f, faces[i].X, faces[i].Y, faces[i].Z);
  Result := true;
  666:
  CloseFile(F);
end; // LoadObjMni()

procedure TMesh.LoadObj(const FileName: string);
//WaveFront Obj file used by Blender
// https://en.wikipedia.org/wiki/Wavefront_.obj_file
//6/2016: Add support Obj files with negative indices
//  https://people.cs.clemson.edu/~dhouse/courses/405/docs/brief-obj-file-format.html
//  https://github.com/vistalab/vistasoft/blob/master/fileFilters/OBJ/examples/texturedknot.obj
const
  kBlockSize = 8192;
var
   f: TextFile;
   fsz : int64;
   s : string;
   strlst : TStringList;
   i,j, num_c, num_v, num_f, new_f: integer;
begin
  if LoadObjMni(Filename) then exit; //MNI format, not Wavefront
  fsz := FSize (FileName);
  if fsz < 32 then exit;
  //init values
  num_c := 0;
  num_v := 0;
  num_f := 0;
  strlst:=TStringList.Create;
  setlength(vertices, (fsz div 70)+kBlockSize); //guess number of faces based on filesize to reduce reallocation frequencey
  setlength(vertexRGBA, (fsz div 70)+kBlockSize); //guess number of faces based on filesize to reduce reallocation frequencey
  setlength(faces, (fsz div 35)+kBlockSize); //guess number of vertices based on filesize to reduce reallocation frequencey
  //load faces and vertices
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  Reset(f);
  DefaultFormatSettings.DecimalSeparator := '.';
  while not EOF(f) do begin
    readln(f,s);
    if length(s) < 7 then continue;
    if (s[1] <> 'v') and (s[1] <> 'f') then continue; //only read 'f'ace and 'v'ertex lines
    if (s[2] = 'p') or (s[2] = 'n') or (s[2] = 't') then continue; //ignore vp/vn/vt data: avoid delimiting text yields 20% faster loads
    strlst.DelimitedText := s;
    if (strlst.count > 3) and ( (strlst[0]) = 'f') then begin
       //warning: need to handle "f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3"
       //warning: face could be triangle, quad, or more vertices!
       new_f := strlst.count - 3;
       if ((num_f+new_f) >= length(faces)) then
          setlength(faces, length(faces)+new_f+kBlockSize);
       for i := 1 to (strlst.count-1) do
           if (pos('/', strlst[i]) > 1) then // "f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3" -> f v1 v2 v3
              strlst[i] := Copy(strlst[i], 1, pos('/', strlst[i])-1);
       for j := 1 to (new_f) do begin
           faces[num_f].X := strtointDef(strlst[1], 1);
           faces[num_f].Y := strtointDef(strlst[j+1], 1);
           faces[num_f].Z := strtointDef(strlst[j+2], 1);
           //showmessage(format('%d:%d= %d %d %d', [num_v, num_f, faces[num_f].x, faces[num_f].y, faces[num_f].z]));
       //20:6 -4 -3 -2
         if faces[num_f].X < 0 then
              faces[num_f].X := 1 + num_v + faces[num_f].X;
           if faces[num_f].Y < 0 then
              faces[num_f].Y := 1 + num_v + faces[num_f].Y;
           if faces[num_f].Z < 0 then
              faces[num_f].Z := 1 + num_v + faces[num_f].Z;
           faces[num_f] := vectorAdd(faces[num_f],-1);//-1 since "A valid vertex index starts from 1"
           inc(num_f);
       end;
    end;
    if (strlst.count > 3) and ( (strlst[0]) = 'v') then begin
       if ((num_v+1) >= length(vertices)) then
          setlength(vertices, length(vertices)+kBlockSize);
       vertices[num_v].X := strtofloatDef(strlst[1], 0);
       vertices[num_v].Y := strtofloatDef(strlst[2], 0);
       vertices[num_v].Z := strtofloatDef(strlst[3], 0);
       inc(num_v);
       if  (strlst.count < 7) then continue;
       if ((num_c+1) >= length(vertexRGBA)) then
          setlength(vertexRGBA, length(vertexRGBA)+kBlockSize);
       vertexRGBA[num_c].r := round(strtofloatDef(strlst[4], 0)*255);
       vertexRGBA[num_c].g := round(strtofloatDef(strlst[5], 0)*255);
       vertexRGBA[num_c].b := round(strtofloatDef(strlst[6], 0)*255);
       vertexRGBA[num_c].a := 128;
       inc(num_c);
    end;
  end;
  //showmessage(format('%d %g %g %g',[num_v, vertices[num_v-1].X, vertices[num_v-1].Y, vertices[num_v-1].Z]));
  CloseFile(f);
  strlst.free;
  setlength(faces, num_f);
  setlength(vertices, num_v);
  if (num_c = num_v) then
  	setlength(vertexRGBA, num_c)
  else
  	setlength(vertexRGBA, 0);
  if (num_v < 3) then
     showmessage('Not a Wavefront or MNI format OBJ mesh (perhaps proprietary Mayo Clinic Analyze format)');
  if (num_f < 1) and (num_v > 1) then
    showmessage('Not a face-based OBJ file (perhaps lines, try opening in MeshLab)');
end; // LoadObj()

procedure TMesh.LoadPly(const FileName: string);
// https://en.wikipedia.org/wiki/PLY_(file_format)
// http://paulbourke.net/dataformats/ply/
var
   fb: file;
   f: TextFile;
   isSkipUchar, isSwap, isVertexSection, isAscii, isLittleEndian, isUint32 : boolean;
   fsz, redOffset, greenOffset, blueOffset, AlphaOffset,
   hdrSz, sz, i, j,  num_v, num_f, num_vx, num_header_lines, vertexOffset, indexSectionExtraBytes: integer;
   str: string;
   byt: byte;
   flt: single;
   strlst : TStringList;
   i32: array [1..3] of longword;
   i16: array [1..3] of word;
   binByt: array of byte;
begin
  fsz := FSize(FileName);
  if (fsz < 64) then exit;
  FileMode := fmOpenRead;
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
  num_f := 0;
  num_v := 0;
  isAscii := false;
  isLittleEndian := false;
  isUint32 := true; //assume uint32 not short int16
  isVertexSection := false;
  isSkipUchar := false;
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
     if pos('property uchar intensity',str) = 1 then
        isSkipUchar := true;
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
    showmessage('Not a mesh-based PLY file (perhaps point based, try opening in MeshLab)');
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
      if isSkipUchar then
         read(f, num_vx);  //AFNI ConvertSurface adds an unused dummy value "intensity" at the start of each line
      readln(f, num_vx, faces[i].X, faces[i].Y, faces[i].Z);
      if num_vx < 3 then begin
            showmessage('File does not have the expected number of triangle-based faces '+ FileName);
            closefile(f);
            exit;
      end;
      if num_vx > 3 then begin
          showmessage('Only able to read triangle-based PLY files. (Hint: open with MeshLab and export as CTM format) ');
          closefile(f);
          exit;
      end;
    end;
    closefile(f);
  end else begin //if ASCII else Binary
    closefile(f);
    {$DEFINE FASTPLY}
    {$IFDEF FASTPLY}setlength(binByt,fsz); {$ENDIF}
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
       {$IFNDEF FASTPLY}setlength(binByt, vertexOffset);{$ENDIF}
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
    if (fsz-filepos(fb)) = (num_f * 13) then begin //all triangles, all faces uint32
       //accelerate most common binary ply: triangular mesh, faces described as uint32s
       {$IFNDEF FASTPLY}setlength(binByt, num_f * 13);{$ENDIF}
       blockread(fb, binByt[0], num_f * 13);
       j := 0;
       if isSwap then begin
         for i := 0 to (num_f -1) do begin
             j := j + 1; //skip first byte "3" indicates triangular mesh
             Move(binByt[j], i32[1], 12);
             j := j + 12;
             SwapLongWord(i32[1]);
             SwapLongWord(i32[2]);
             SwapLongWord(i32[3]);
             faces[i] := pti(i32[1], i32[2], i32[3]);
         end;
       end else begin
         for i := 0 to (num_f -1) do begin
             j := j + 1; //skip first byte "3" indicates triangular mesh
             Move(binByt[j], i32[1], 12);
             j := j + 12;
             faces[i] := pti(i32[1], i32[2], i32[3]);
         end;
       end;
    end else begin
      for i := 0 to (num_f -1) do begin
          {$IFNDEF FASTPLY}setlength(binByt, indexSectionExtraBytes);{$ENDIF}
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
    end;
    binByt := nil;
    closefile(fb);
   end; //if ascii else binary
   strlst.Free;
end; // LoadPly()

function jsStr(substr, str: string; out nItems: integer): string;
var
   s, e, i: integer;
begin
  nItems := 0;
  result := '';
  s := PosEx(substr,str);
  if s < 1 then exit; //substr not in str
  s := PosEx('[',str,s);
  if s < 1 then exit; //substr not in str
  e := PosEx(']',str,s);
  if e < 3 then exit; //substr not in str
  result := Copy(str, s+1, e-s-1);
    //count v
  nItems := 1;
  for i := 1 to Length(result) do
    if result[i] = ',' then
      inc(nItems);
end;

function TMesh.LoadJson(const FileName: string): boolean;
//supported, "indices" and "vertices"
//  https://github.com/kchapelier/PRWM/tree/master/models/json
//unsupported, "faces" and "vertices": need to handle type bitmask
//  https://github.com/mrdoob/three.js/wiki/JSON-Model-format-3
//there seem to be many formats...
// also "cells" and "positions": {"cells":[[1825,1637,3],[1825...
var
   f: TextFile;
   s1, s, fstr, vstr: string;
   i,num_v, num_f: integer;
begin
  result := false;
  if (FSize(FileName) < 64) then exit;
  fstr := '';
  vstr := '';
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  Reset(f);
  s := '';
  while not EOF(f) do begin
      Readln(f, s1);
      if s1 = '' then continue;
      s := s+' '+s1;
  end;
  CloseFile(f);
  vstr := jsStr('"vertices"', s, num_v);
  //if vstr = '' then
  //   vstr := jsStr('"positions"', s, num_v);
  if (vstr = '') or (num_v < 3) then begin
     showmessage('Unable to find "vertices" or "positions"');
     exit;
  end;
  //showmessage(vstr);
  fstr := jsStr('"indices"', s, num_f);
  //if fstr = '' then
  // fstr := jsStr('"cells"', s, num_f);
  if fstr = '' then begin
     fstr := jsStr('"faces"', s, num_f);
     if fstr <> '' then begin
       showmessage('Wrong JSON format, expected "indices" or "cells" not "faces"');
       exit;
     end;
     showmessage('Unable to find "indices"');
     exit;
  end;

  if (num_f < 1) or (num_v < 3) then begin
    showmessage('Expected at leat 3 vertices and 3 indices');
    exit;
  end;
  setlength(vertices, num_v div 3); //this format does not report number of faces/vertices, so we need to guess
  setlength(faces, num_f div 3);
  //set v
  num_v := 0;
  s := '';
  for i := 1 to Length(vstr) do begin
    if (vstr[i] <> ',') then
        s := s + vstr[i];
    if (vstr[i] = ',') or (i = Length(vstr)) then begin
      if num_v mod 3 = 0 then
         vertices[num_v div 3].X := strtofloatdef(s,0)
      else if num_v mod 3 = 1 then
         vertices[num_v div 3].Y := strtofloatdef(s,0)
      else
         vertices[num_v div 3].Z := strtofloatdef(s,0);
      inc(num_v);
      s := '';
    end;
  end;
  //set f
  num_f := 0;
  s := '';
  for i := 1 to Length(fstr) do begin
    if (fstr[i] <> ',') then
       s := s + fstr[i];
    if (fstr[i] = ',') or (i = Length(fstr)) then begin
      if num_f mod 3 = 0 then
         faces[num_f div 3].X := strtointdef(s,0)
      else if num_f mod 3 = 1 then
         faces[num_f div 3].Y := strtointdef(s,0)
      else
         faces[num_f div 3].Z := strtointdef(s,0);
      inc(num_f);
      s := '';
    end;
  end;
  UnifyVertices(faces, vertices); //see faceted-nefertiti.json
end; // LoadJson()

function TMesh.LoadPly2(const FileName: string): boolean;
//http://www.riken.jp/brict/Yoshizawa/Research/PLYformat/PLYformat.html
label
   666;
var
   f: TextFile;
   v, t, i3, num_v, num_f: integer;
begin
     result := false;
     if (FSize(FileName) < 64) then exit;
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
     Reset(f);
     Read(f, num_v);
     Read(f, num_f);
     if (num_v < 3) or (num_f < 1) then
        goto 666;
     setlength(vertices, num_v); //this format does not report number of faces/vertices, so we need to guess
     setlength(faces, num_f);
     for v := 0 to (num_v - 1) do begin
         read(f, vertices[v].X);
         read(f, vertices[v].Y);
         read(f, vertices[v].Z);
     end;
     for t := 0 to (num_f - 1) do begin
         read(f, i3);
         if i3 <> 3 then goto 666;
         read(f, faces[t].X);
         read(f, faces[t].Y);
         read(f, faces[t].Z);
     end;
     result := true;
 666:
     if not result then showmessage('Unable to load PLY2 file '+filename);
     CloseFile(f);
end;

function KeyString(key, str: string): string;
// KeyStringInt('ExternalFileName', 'ExternalFileName="beta_0002.dat"') returns beta_0002.dat
var
   s, e: integer;
begin
     result := '';
     s := pos(key, Str);
     if s < 1 then exit;
     s := posEx('"', Str, s);
     if s < 1 then exit;
     e := posEx('"', Str, s+1);
     if (e < 1) then exit;
     result := Copy(Str, s+1, e-s-1);
end;  // KeyStringInt()

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
     e := posEx('"', Str, s+1);
     if (e < 1) then exit;
     if (e-s) = 1 then exit; // ExternalFileOffset="" -> 0
     txt := Copy(Str, s+1, e-s-1);
     result := StrToIntDef(txt,0);
end;  // KeyStringInt()

function MemoryStreamAsString(vms: TMemoryStream): string;
//binary contents as ASCII string: http://forum.lazarus.freepascal.org/index.php?topic=15622.5;wap2
begin
   SetString(Result, vms.Memory, vms.Size)
end; // MemoryStreamAsString()

procedure SavePly(const FileName: string; Faces: TFaces; Vertices: TVertices; vertexRGBA: TVertexRGBA);
//procedure TMesh.SavePly(const FileName: string);
type
 TRGB = packed record //Next: analyze Format Header structure
    R,G,B : byte;
  end;
const
  kEOLN = chr($0A);
var
   i: integer;
   isRGB: boolean;
   s,clrStr: string;
   f: file;
   byt: byte;
   w: array[0..2] of word;
   FileNamePly: string;
   rgb: TRGB;
begin
  if (length(faces) < 1) or (length(vertices) < 3) then begin
        showmessage('You need to open a mesh before you can save it');
        exit;
  end;
  FileNamePly := changeFileExt(FileName, '.ply');
  {$IFDEF UNIX}
  FileNamePly := ExpandUNCFileNameUTF8(FileNamePly);
  {$ENDIF}
  if length(faces) < 32767 then
     s := 'property list uchar short vertex_indices'
  else
      s := 'property list uchar uint vertex_indices';
  isRGB := length(vertexRGBA) = length(vertices);
  clrStr := '';
  if isRGB then
     clrStr := 'property uchar red'+kEOLN+'property uchar green'+kEOLN+'property uchar blue'+kEOLN;

  s := 'ply'+kEOLN
       {$IFDEF ENDIAN_LITTLE}
       +'format binary_little_endian 1.0'+kEOLN
       {$ELSE}
       +'format binary_big_endian 1.0'+kEOLN
       {$ENDIF}
       +'comment SurfIce'+kEOLN
       +'element vertex '+inttostr(length(vertices))+kEOLN
       +'property float x'+kEOLN
       +'property float y'+kEOLN
       +'property float z'+kEOLN
       + clrStr
       +'element face '+inttostr(length(faces))+kEOLN
       +s+kEOLN
       +'end_header'+kEOLN;
  FileMode := fmOpenWrite;   //FileMode := fmOpenRead;
  AssignFile(f, FileNamePly);
  ReWrite(f, 1);
  BlockWrite(f, s[1], length(s));
  if (isRGB) then begin
     for i := 0 to (length(vertices) -1) do begin
         BlockWrite(f, vertices[i], 3 * 4);
         RGB.R := vertexRGBA[i].R;
         RGB.G := vertexRGBA[i].G;
         RGB.B := vertexRGBA[i].B;
         BlockWrite(f, RGB, 3);
     end;
  end else
  BlockWrite(f, vertices[0], 3 * 4 * length(vertices));
  byt := 3;
  if length(faces) < 32767 then begin
     for i := 0 to (length(faces) -1) do begin
         BlockWrite(f, byt, 1 );
         w[0] := faces[i].X;
         w[1] := faces[i].Y;
         w[2] := faces[i].Z;
         BlockWrite(f, w[0], 3 * 2);
     end;
  end else begin
      for i := 0 to (length(faces) -1) do begin
          BlockWrite(f, byt, 1 );
          BlockWrite(f, faces[i], 3 * 4 );
      end;
  end;
  CloseFile(f);
  FileMode := fmOpenRead;
end; // SavePly()


{$DEFINE BINARYSTL}
{$IFDEF BINARYSTL}

{$IFDEF ENDIAN_BIG}
procedure swapPt(var pt: TPoint3f);
begin
  swapsingle(pt.x);
  swapsingle(pt.y);
  swapsingle(pt.z);
end;
{$ENDIF}

procedure SaveStl(const FileName: string; Faces: TFaces; Vertices: TVertices; vertexRGBA: TVertexRGBA);
const
  kHdrBytes = 80;
  kURL = 'https://en.wikipedia.org/wiki/STL_(file_format)';
type
  TTri = packed record
        normal, v1,v2,v3: TPoint3f;
        AttributeByteCount: Uint16
     end;
var
  hdr: array [1..kHdrBytes] of byte;
  //faces16: array of Tui16;
  i, nFace, nVert: integer;
  nFace32: UINT32;
  mStream : TMemoryStream;
  FileNameStl: string;
  t: TTri;
begin
  if (length(faces) < 1) or (length(vertices) < 3) then begin
     showmessage('You need to open a mesh before you can save it');
     exit;
  end;
  if (length(vertices) < 3) or (length(faces) < 1) then exit;   //if not CheckMesh then exit;
  if (length(vertexRGBA) > 0) then
     showmessage('Warning: STL format does not support vertex colors');
  FileNameStl := changeFileExt(FileName, '.stl');
  {$IFDEF UNIX}
  FileNameStl := ExpandUNCFileNameUTF8(FileNameStl); // ~/tst.pl -> /Users/rorden/home/tst.ply
  {$ENDIF}
  for i := 1 to kHdrBytes do
    hdr[i] := 0;
  for i := 1 to length(kURL) do
    hdr[i] := ord(kURL[i]);
  nFace := length(Faces);
  nVert := length(Vertices);
  mStream := TMemoryStream.Create;
  mStream.Write(hdr,sizeOf(hdr));
  nFace32 := nFace;
  t.normal.x := 0;
  t.normal.y := 0;
  t.normal.z := 0;
  t.AttributeByteCount := 0;
  {$IFDEF ENDIAN_BIG}
  nFace32 := swap(nFace);
  swapPt(t.normal);
  t.AttributeByteCount := swap(t.AttributeByteCount);
  {$ENDIF}
  mStream.Write(nFace32, 4);
  for i := 0 to nFace-1 do begin
    t.v1 := ptf(vertices[faces[i].X].X, vertices[faces[i].X].Y, vertices[faces[i].X].Z);
    t.v2 := ptf(vertices[faces[i].Y].X, vertices[faces[i].Y].Y, vertices[faces[i].Y].Z);
    t.v3 := ptf(vertices[faces[i].Z].X, vertices[faces[i].Z].Y, vertices[faces[i].Z].Z);
    {$IFDEF ENDIAN_BIG}}
    swapPt(t.v1);
    swapPt(t.v2);
    swapPt(t.v3);
    {$ENDIF}
    mStream.Write(t, sizeof(t));
  end;
  mStream.Position := 0;
  FileMode := fmOpenWrite;   //FileMode := fmOpenRead;
  mStream.SaveToFile(FileNameStl);
  mStream.Free;
  FileMode := fmOpenRead;
end;
{$ELSE} //ASCII STL Follows
procedure SaveStl(const FileName: string; Faces: TFaces; Vertices: TVertices; vertexRGBA: TVertexRGBA);
//https://en.wikipedia.org/wiki/STL_(file_format)
var
   f : TextFile;
   FileNameStl: string;
   i : integer;
begin
  if (length(faces) < 1) or (length(vertices) < 3) then begin
     showmessage('You need to open a mesh before you can save it');
     exit;
  end;
  if (length(vertices) < 3) or (length(faces) < 1) then exit;   //if not CheckMesh then exit;
  if (length(vertexRGBA) > 0) then
     showmessage('Warning: STL format does not support vertex colors');
  FileNameStl := changeFileExt(FileName, '.stl');
  {$IFDEF UNIX}
  FileNameStl := ExpandUNCFileNameUTF8(FileNameStl); // ~/tst.pl -> /Users/rorden/home/tst.ply
  {$ENDIF}
  FileMode := fmOpenWrite;
  AssignFile(f, FileNameStl);
  ReWrite(f);
  WriteLn(f, 'solid surfice');
  for i := 0 to (length(faces)-1) do begin
      WriteLn(f, ' facet normal 0 0 0'); //wiki says this is OK
      WriteLn(f, '  outer loop');
      WriteLn(f, format('   vertex %e %e %e', [vertices[faces[i].X].X, vertices[faces[i].X].Y, vertices[faces[i].X].Z]));
      WriteLn(f, format('   vertex %e %e %e', [vertices[faces[i].Y].X, vertices[faces[i].Y].Y, vertices[faces[i].Y].Z]));
      WriteLn(f, format('   vertex %e %e %e', [vertices[faces[i].Z].X, vertices[faces[i].Z].Y, vertices[faces[i].Z].Z]));
      WriteLn(f, '  endloop');
      WriteLn(f, ' endfacet');
  end;
  WriteLn(f, 'endsolid surfice');
  CloseFile(f);
  FileMode := fmOpenRead;
end; // SaveStl()
{$ENDIF}

procedure SaveObj(const FileName: string; Faces: TFaces; Vertices: TVertices; vertexRGBA: TVertexRGBA);
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
  if (length(vertices) < 3) or (length(faces) < 1) then exit;   //if not CheckMesh then exit;
  FileNameObj := changeFileExt(FileName, '.obj');
  {$IFDEF UNIX}
  FileNameObj := ExpandUNCFileNameUTF8(FileNameObj); // ~/tst.pl -> /Users/rorden/home/tst.ply
  {$ENDIF}
  FileMode := fmOpenWrite;
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
  FileMode := fmOpenRead;
end; // SaveObj()

{$IFDEF SwizzleMZ}
Type
TUInt8s = array of uint8;

procedure WriteSwizzle(var mStream : TMemoryStream; Buffer: pointer; nBytes: LongInt);
var
  bIn, bOut: TUint8s;
  n4, n4x2, n4x3, i, j: integer;
begin
     if (nBytes < 8) or ((nBytes mod 4) <> 0) then exit;
     setlength(bOut, nBytes);
     bIn := TUint8s(Buffer);
     n4 := nBytes div 4;
     n4x2 := n4 + n4;
     n4x3 := n4 + n4 + n4;
     i := 0;
     for j := 0 to (n4-1) do begin
         bOut[j] := bIn[i];
         bOut[j+n4] := bIn[i+1];
         bOut[j+n4x2] := bIn[i+2];
         bOut[j+n4x3] := bIn[i+3];
         i := i + 4;
     end;
     mStream.Write(bOut[0], nBytes);
     bOut := nil;
end;

procedure ReadSwizzle(var mStream : TMemoryStream; Buffer: pointer; nBytes: LongInt);
var
  bIn, bOut: TUint8s;
  n4, n4x2, n4x3, i, j: integer;
begin
     if (nBytes < 8) or ((nBytes mod 4) <> 0) then exit;
     setlength(bIn, nBytes);
     bOut := TUint8s(Buffer);
     mStream.Read(bIn[0], nBytes);
     n4 := nBytes div 4;
     n4x2 := n4 + n4;
     n4x3 := n4 + n4 + n4;
     i := 0;
     for j := 0 to (n4-1) do begin
         bout[i] := bIn[j];
         bout[i+1] := bIn[j+n4];
         bout[i+2] := bIn[j+n4x2];
         bout[i+3] := bIn[j+n4x3];
         i := i + 4;
     end;
     bIn := nil;
end;
{$ENDIF}

procedure SaveMz3(const FileName: string; Faces: TFaces; Vertices: TVertices; vertexRGBA: TVertexRGBA; intensity: TFloats; isAOMap: boolean = false);
const
 kMagic =  23117; //"MZ"
var
  {$IFDEF SwizzleMZ}isSwizzle{$ENDIF}
  isFace, isVert, isRGBA, isScalar: boolean;
  Magic, Attr: uint16;
  nFace, nVert, nSkip: uint32;
  mStream : TMemoryStream;
  zStream: TGZFileStream;
  FileNameMz3: string;
begin
  {$IFDEF SwizzleMZ}isSwizzle := true;{$ENDIF}
  nFace := length(Faces);
  nVert := length(Vertices);
  FileNameMz3 := DefaultToHomeDir(FileName);
  FileNameMz3 := changeFileExt(FileNameMz3, '.mz3');
  {$IFDEF UNIX} //I do not think we need to do this!
  FileNameMz3 := ExpandUNCFileNameUTF8(FilenameMz3);
  {$ENDIF}
  isFace := nFace > 0;
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
  end else
      isAOMap := false;
  if (nFace = 0) and (nVert = 0) then exit;
  magic := kMagic;
  Attr := 0;
  if isFace then Attr := Attr + 1;
  if isVert then Attr := Attr + 2;
  if isRGBA then Attr := Attr + 4;
  if isScalar then Attr := Attr + 8;
  //if isDouble then Attr := Attr + 16;
  if isAOMap then Attr := Attr + 32;
  nSkip := 0; //do not pad header with any extra data
  mStream := TMemoryStream.Create;
  mStream.Write(Magic,2);
  mStream.Write(Attr,2);
  mStream.Write(nFace,4);
  mStream.Write(nVert,4);
  mStream.Write(nSkip,4);
  {$IFDEF SwizzleMZ}if isSwizzle then begin
    if isFace then
       WriteSwizzle(mStream, addr(Faces[0]), nFace * sizeof(TPoint3i));
    if isVert  then
       WriteSwizzle(mStream, addr(Vertices[0]), nVert * 3 * sizeof(single));
    if isRGBA then
       WriteSwizzle(mStream, addr(vertexRGBA[0]), nVert * 4 * sizeof(byte));
    if isScalar then
       WriteSwizzle(mStream, addr(intensity[0]), nVert * sizeof(single));
  end else{$ENDIF} begin
    if isFace then
       mStream.Write(Faces[0], nFace * sizeof(TPoint3i));
    if isVert  then
       mStream.Write(Vertices[0], nVert * 3 * sizeof(single));
    if isRGBA then
       mStream.Write(vertexRGBA[0], nVert * 4 * sizeof(byte));
    if isScalar then
       mStream.Write(intensity[0], nVert * sizeof(single));
  end;
  mStream.Position := 0;
  FileMode := fmOpenWrite;   //FileMode := fmOpenRead;
  zStream := TGZFileStream.Create(FileNameMz3, gzopenwrite);
  zStream.CopyFrom(mStream, mStream.Size);
  zStream.Free;
  mStream.Free;
  FileMode := fmOpenRead;
end;

procedure SaveVrml(const FileName: string; Faces: TFaces; Vertices: TVertices; vertexRGBA: TVertexRGBA);
var
  i, nFace, nVert, nRGBA: integer;
  FileNameWrl: string;
  f : TextFile;
begin
  nFace := length(Faces);
  nVert := length(Vertices);
  nRGBA := length(vertexRGBA);
  if (nVert < 3) or (nFace < 1) then exit;
  FileNameWrl := DefaultToHomeDir(FileName);
  FileNameWrl := changeFileExt(FileNameWrl, '.wrl');
  {$IFDEF UNIX} //I do not think we need to do this!
  FileNameWrl := ExpandUNCFileNameUTF8(FileNameWrl);
  {$ENDIF}
  FileMode := fmOpenWrite;
  AssignFile(f, FileNameWrl);
  ReWrite(f);
  WriteLn(f, '#VRML V2.0 utf8');
  WriteLn(f, '# Generated by Surfice\n');
  WriteLn(f, 'NavigationInfo {');
  WriteLn(f, '	type [ "EXAMINE", "ANY" ]');
  WriteLn(f, '}');
  WriteLn(f, 'Transform {');
  WriteLn(f, '  scale 1 1 1');
  WriteLn(f, '  translation 0 0 0');
  WriteLn(f, '  children');
  WriteLn(f, '  [');
  WriteLn(f, '    Shape');
  WriteLn(f, '    {');
  WriteLn(f, '      geometry IndexedFaceSet');
  WriteLn(f, '      {');
  WriteLn(f, '        creaseAngle .5');
  WriteLn(f, '        solid FALSE');
  WriteLn(f, '        coord Coordinate');
  WriteLn(f, '        {');
  WriteLn(f, '          point');
  WriteLn(f, '          [');
  Write(f,'            ');
  for i := 0 to (nVert -1) do begin
    if (i > 0) then Write(f,', ');
    Write(f,format('%g %g %g', [Vertices[i].X, Vertices[i].Y, Vertices[i].Z]));
  end;
  WriteLn(f,'');
  WriteLn(f, '          ]');
  WriteLn(f, '        }');
  if (nRGBA = nVert) then begin
    WriteLn(f, '        color Color');
    WriteLn(f, '        {');
    WriteLn(f, '          color');
    WriteLn(f, '          [');
    Write(f,'            ');
    for i := 0 to (nRGBA -1) do begin
        if (i > 0) then Write(f,', ');
        Write(f,format('%g %g %g', [vertexRGBA[i].R/255, vertexRGBA[i].G/255, vertexRGBA[i].B/255]));
    end;
    WriteLn(f,'');
    WriteLn(f, '          ]');
    WriteLn(f, '        }');
  end;
  WriteLn(f, '        coordIndex');
  WriteLn(f, '        [');
  //nFace := length(Faces);

  Write(f,'            ');
  for i := 0 to (nFace -1) do begin
    if (i > 0) then Write(f,', ');
    Write(f,format('%d,%d,%d,-1', [Faces[i].X, Faces[i].Y, Faces[i].Z]));
  end;
  WriteLn(f,'');
  WriteLn(f, '        ]');
  WriteLn(f, '      }');
  WriteLn(f, '      appearance Appearance');
  WriteLn(f, '      {');
  WriteLn(f, '        material Material');
  WriteLn(f, '        {');
  WriteLn(f, '	       ambientIntensity 0.2');
  WriteLn(f, '	       diffuseColor 0.9 0.9 0.9');
  WriteLn(f, '	       specularColor .1 .1 .1');
  WriteLn(f, '	       shininess .5');
  WriteLn(f, '        }');
  WriteLn(f, '      }');
  WriteLn(f, '    }');
  WriteLn(f, '  ]');
  WriteLn(f, '}');
  //for i := 0 to (length(vertices)-1) do
  //    WriteLn(f, 'v ' + floattostr(vertices[i].X)+' '+floattostr(vertices[i].Y)+' '+ floattostr(vertices[i].Z));
  //for i := 0 to (length(faces)-1) do
  //    WriteLn(f, 'f ' + inttostr(faces[i].X+1)+' '+inttostr(faces[i].Y+1)+' '+ inttostr(faces[i].Z+1)); //+1 since "A valid vertex index starts from 1 "
  //fprintf(fid, '# WaveFront Object format image created with MRIcroS\n');
  //fprintf(fid, 'v %.12g %.12g %.12g\n', vertex');
  //fprintf(fid, 'f %d %d %d\n', (face)');
  CloseFile(f);
  FileMode := fmOpenRead;

end;

function ParseStartEnd(key : string; var Str: string; out keyEnd: integer): integer;
begin
  keyEnd := -1;
  result := posEx(key, Str);
  if result < 1 then exit();
  result := result + length(key)+1;
  result := posEx('[', Str, result);
  if result < 1 then exit;
  result := result +1;
  keyEnd := posEx(']', Str, result);
  keyEnd := keyEnd - 1;
  if keyEnd < 1 then exit(-1);
end;

function TMesh.LoadVrml(const FileName: string): boolean; //VRML wireframe
//this is a very basic VRML reader
// probably only able toread VRML files created by MeshLab and Surfice
label
   555, 666;
const
   kBlockSz = 4096;
   kNaN : single = 1/0;
var
   f: file;
   sList: TStringList;
   Str: string;
   isQuad : boolean;
   fl: single;
   i,j, n,tStart, tEnd, szRead, nFace, nVert: integer;
begin
     result := false;
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
     Reset(f,1);
     szRead := FileSize(f);
     SetLength(Str, szRead);
     BlockRead(f, Str[1],szRead);
     CloseFile(f);
     FileMode := fmOpenRead;
     tStart := posEx('IndexedFaceSet', Str);
     sList := TStringList.Create;
     if tStart < 1 then goto 666;
     //read faces
     tStart := ParseStartEnd('coordIndex', Str, tEnd);
     if (tStart < 1) then goto 666;
     sList.DelimitedText := copy(Str,tStart, tEnd-tStart+1);
     if (sList.Count) < 3 then goto 666;
     n := 0;
     nFace := 0;
     isQuad := false;
     setlength(Faces, kBlockSz);
     for i := 0 to (sList.Count-1) do begin
         j := StrToIntDef(sList[i],-1);
         if j < 0 then begin
            n := 0;
            continue;
         end;
         if n = 0 then Faces[nFace].X:=j;
         if n = 1 then Faces[nFace].Y:=j;
         if n = 2 then begin
           Faces[nFace].Z:=j;
           nFace := nFace + 1;
           if (nFace >= length(Faces)) then
              setLength(Faces, nFace+ kBlockSz);
         end;
         if n > 2 then
            isQuad := true;
         n := n + 1;
     end;
     if isQuad then showmessage('Only designed to read triangular VRML meshes.');
     setLength(Faces, nFace);
     //read vertices
     tStart := ParseStartEnd('point', Str, tEnd);
     if (tStart < 1) then goto 666;
     sList.DelimitedText := copy(Str,tStart, tEnd-tStart+1);
     if (sList.Count) < 3 then goto 666;
     n := 0;
     nVert := 0;
     setlength(Vertices, kBlockSz);
     for i := 0 to (sList.Count-1) do begin
         fl := StrToFloatDef(sList[i],kNaN);
         if specialsingle(fl) then
           continue;
         if n = 0 then Vertices[nVert].X:=fl;
         if n = 1 then Vertices[nVert].Y:=fl;
         if n = 2 then begin
           Vertices[nVert].Z:=fl;
           nVert := nVert + 1;
           if (nVert >= length(Vertices)) then
              setLength(Vertices, nVert+ kBlockSz);
           n := -1;
         end;
         n := n + 1;
     end;
     setLength(Vertices, nVert);
     //read colors
     tStart := ParseStartEnd('color', Str, tEnd);
     if (tStart < 1) then goto 555;
     sList.DelimitedText := copy(Str,tStart, tEnd-tStart+1);
     if (sList.Count) < (3 * nVert)  then goto 555;
     n := 0;
     setlength(vertexRGBA, nVert);
     nVert := 0;
     for i := 0 to (sList.Count-1) do begin
         fl := StrToFloatDef(sList[i],kNaN);
         if specialsingle(fl) then
           continue;
         j := round(fl * 255);
         if (j < 0) or (j > 255) then continue;
         if n = 0 then vertexRGBA[nVert].R:=j;
         if n = 1 then vertexRGBA[nVert].G:=j;
         if n = 2 then begin
           vertexRGBA[nVert].B:=j;
           vertexRGBA[nVert].A:=255;
           nVert := nVert + 1;
           n := -1;
         end;
         n := n + 1;
     end;
     555: //no color
     result := true;
     666:
     sList.free;
     if not result then begin
        showmessage('Unable to parse VRML image. You may want to convert this to a simpler format.');
     end;
end;

procedure TMesh.SaveOverlay(const FileName: string; OverlayIndex: integer);
var
  f: TFaces;
  v: TVertices;
  c: TVertexRGBA;
begin
  if OverlayIndex > OpenOverlays then exit;
  //setlength(i,0);
  setlength(c,0);
  if (length(overlay[OverlayIndex].faces) > 0) and (length(overlay[OverlayIndex].vertices) > 0) then
     SaveMz3(FileName, overlay[OverlayIndex].Faces, overlay[OverlayIndex].Vertices, c, overlay[OverlayIndex].intensity)
  else if length(overlay[OverlayIndex].intensity) > 1 then begin
     setlength(f,0);
     setlength(v,0);
     SaveMz3(FileName, f,v,c, overlay[OverlayIndex].intensity);
  end;
end;

procedure SaveGii(const FileName: string; Faces: TFaces; Vertices: TVertices; vertexRGBA: TVertexRGBA);
var
   f : TextFile;
   FileNameGii : string;
   //{$DEFINE GIIGZ}
   {$IFDEF GIIGZ}
   comp : TGZFileStream;
   //comp: TCompressionstream;
   {$ELSE}
    comp: TCompressionstream;
   {$ENDIF}
   outStream    : TMemoryStream;
   nBytes : integer;
   base64: string;
begin
  FileNameGii := changeFileExt(FileName, '.gii');
  {$IFDEF UNIX}
  FileNameGii := ExpandUNCFileNameUTF8(FilenameGii);
  {$ENDIF}
  if (length(faces) < 1) or (length(vertices) < 3) then begin
     showmessage('You need to open a mesh before you can save it');
     exit;
  end; //if not CheckMesh then exit;
  FileMode := fmOpenWrite;   //FileMode := fmOpenRead;
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
(*      zStream := TGZFileStream.Create(FileNameMz3, gzopenwrite);
  zStream.CopyFrom(mStream, mStream.Size);
  zStream.Free;
  mStream.Free;*)
    {$IFDEF GIIGZ}
    comp := TGZFileStream.Create(clMax, outStream);
    {$ELSE}
    comp := TCompressionStream.Create(clMax, outStream);
    {$ENDIF}
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
    {$IFDEF GIIGZ}
    comp := TGZFileStream.Create(clMax, outStream);
    {$ELSE}
    comp := TCompressionStream.Create(clMax, outStream);
    {$ENDIF}
    comp.Write(vertices[0], nBytes);
    comp.Free;
    base64 :=  EncodeStringBase64(MemoryStreamAsString(outStream));
    //showmessage('vertex compression: '+inttostr(nBytes)+' -> '+inttostr(outStream.Size)+' -> '+inttostr(length(base64)));
    outStream.Free;
  WriteLn(f, '      <Data>',base64, '</Data>');
  WriteLn(f, '   </DataArray>');
  WriteLn(f, '</GIFTI>');
  CloseFile(f);
  FileMode := fmOpenRead;
end;

//Next: GIfTI specific types
type
    TLongIntArray = array of LongInt;
TGiiLabel = record
   index: Longint;
   R,G,B,A: byte;
 end;
TGiiLabelArray = array of TGiiLabel;

procedure TransposeDat (var v: TLongIntArray; numRows: integer);
var
   n, i,j, nr, nc, rInc, r: integer;
   vIn: TLongIntArray;
begin
     if numRows = 1 then exit;
     n :=  Length(v);
     vIn := Copy(v, Low(v), n);
     nr := numRows;
     nc := n div nr;
     j := 0;
     for i := 0 to (nc-1) do begin
       rInc := 0;
       for r := 0 to (nr-1) do begin
        v[j] := vIn[i+rInc];
        inc(j);
        rInc := rInc + nc;
       end;
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

function ReadNextInt(s: string; var ddStart, ddEnd: integer): integer;
var
   v: string = '';
begin
  while ddStart < ddEnd do begin
        if s[ddStart] in ['0'..'9','-','+'] then
           v := v + s[ddStart]
        else if v <> '' then
               exit(strtointdef(v,0));
        ddStart := ddStart + 1;
  end;
  result := 0;
end;

function ReadNextFloat(s: string; var ddStart, ddEnd: integer): single;
var
   v: string = '';
begin
  while ddStart < ddEnd do begin
        if s[ddStart] in ['0'..'9','.','-','+','e'] then
           v := v + s[ddStart]
        else if v <> '' then
               exit(strtofloatdef(v,0.0));
        ddStart := ddStart + 1;
  end;
  result := 0;
end;

function TMesh.LoadGii(const FileName: string; lOverlayIndex, lOverlayItem: integer; var HasOverlays: boolean): integer;
label
   666;
var
  {$IFNDEF FASTGZ}
  decomp :  Tdecompressionstream;
  {$ENDIF}
  gz:  TMemoryStream;
  mat: TMat44;
  f: file;
  labelTable : TGiiLabelArray;
  fx : File Of byte;
  surfaceID, Str, Hdr, debase64,externalFileName, externalFilePath: string;
  dat : TLongIntArray;
  s: single;
  externalFileOffset, volInc, i, j, k, szExpected, szRead, matStart, matEnd,  daStart, dhEnd, daEnd, ddStart,ddEnd, Dim0, Dim1, nOverlays: integer;
  isCustomMat, isBinary, isRGBA, isOverlay, isAscii, isVertColor, isInt32, isFloat32,
  isBase64, isBase64Gz, isLEndian, isFace, isVert, isTransposed: boolean;
begin
  if (lOverlayIndex = 0) then begin
     setlength(faces, 0);
     setlength(vertices, 0);
  end;
  result := 0;
  if (FSize(FileName) < 64) then exit;
  debase64 := '';
  nOverlays := 0;
  HasOverlays := false;
  gz := TMemoryStream.Create;
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  Reset(f,1);
  szRead := FileSize(f);
  SetLength(Str, szRead);
  BlockRead(f, Str[1],szRead);
  CloseFile(f);
  labelTable := readLabelTable(Str);
  daStart := pos('<DataArray', Str); //read first data array
  isLEndian := true;
  matEnd := -1;
  mat := matrixEye();
  isCustomMat := false;
  surfaceID := '';
  ddStart := pos('<Name><![CDATA[SurfaceID]]></Name>', str);
  if ddStart > 0 then ddStart := pos('<Value><![CDATA[', str);
  if (ddStart > 0) then begin
     ddStart := ddStart + 17;
     ddEnd := posEx(']', Str, ddStart); // data end
     if ddEnd > ddStart then
        surfaceID := Copy(Str, ddStart, ddEnd-ddStart);
  end;
  while daStart > 0 do begin //read each dataArray
    isRGBA := false;
    dhEnd := posEx('>', Str, daStart); // header end
    daEnd := posEx('</DataArray>', Str, daStart); // data array end
    ddStart := posEx('<Data>', Str, daStart) + 6; // data start
    ddEnd := posEx('</Data>', Str, daStart); // data end
    matStart := posEx('<MatrixData>', Str, daStart); //matrix start
    matEnd := posEx('</MatrixData>', Str, daStart); //matrix end
    if (matStart > 0) and (matEnd > matStart) then begin
       for i := 1 to 4 do
           for j := 1 to 4 do begin
               mat[i,j] := ReadNextFloat(Str, matStart, matEnd);
               if (i = j) and (mat[i,j] <> 1) then isCustomMat := true;
               if (i <> j) and (mat[i,j] <> 0) then isCustomMat := true;

           end;
       matEnd := 0;
       //CustomMat is set to true if matrix is provided and the matrix is not the identity matrix
       //if isCustomMat then showmessage(format('m=[%g %g %g %g; %g %g %g %g; %g %g %g %g; 0 0 0 1]',[mat[1,1],mat[1,2],mat[1,3],mat[1,4], mat[2,1],mat[2,2],mat[2,3],mat[2,4], mat[3,1],mat[3,2],mat[3,3],mat[3,4]]));
    end;
    if (dhEnd < 1) or (daEnd < 1) then goto 666;
    Hdr := Copy(Str, daStart, dhEnd-daStart+1);
    //fix for surf_gifti bug https://github.com/nno/surfing/blob/master/python/surf_gifti.py#L222
    isLEndian :=  (pos('Endian="LittleEndian"', Hdr) > 0) or (pos('Endian="GIFTI_ENDIAN_LITTLE"', Hdr) > 0);
    isAscii :=  pos('Encoding="ASCII"', Hdr) > 0;
    isBase64Gz :=  pos('Encoding="GZipBase64Binary"', Hdr) > 0;
    isBase64 :=  pos('Encoding="Base64Binary"', Hdr) > 0;
    isBinary := pos('Encoding="ExternalFileBinary"', Hdr) > 0;
    Dim0 := KeyStringInt('Dim0', Hdr);
    Dim1 := KeyStringInt('Dim1', Hdr);
    externalFileName := '';
    externalFileName := KeyString('ExternalFileName', Hdr);
    externalFileOffset := KeyStringInt('ExternalFileOffset', Hdr);
    isInt32 :=  pos('DataType="NIFTI_TYPE_INT32"', Hdr) > 0;
    if not isInt32 then
       isInt32 :=  pos('DataType="NIFTI_TYPE_UINT32"', Hdr) > 0;
    isFloat32 :=  pos('DataType="NIFTI_TYPE_FLOAT32"', Hdr) > 0;
    isFace := pos('Intent="NIFTI_INTENT_TRIANGLE"', Hdr) > 0; //faces
    isVert := pos('Intent="NIFTI_INTENT_POINTSET"', Hdr) > 0; //vertices
    isVertColor := pos('Intent="NIFTI_INTENT_LABEL"', Hdr) > 0; //vertex colors
    isOverlay :=  pos('Intent="NIFTI_INTENT_NONE"', Hdr) > 0; //??? Lets hope this is a fMRI statistical map
    if (pos('NIFTI_INTENT_RGBA_VECTOR"', Hdr) > 0) then begin
       isVertColor := true;
       isOverlay := true;
       isRGBA := true;
       isVert := true;
    end;
    if not isOverlay then
       isOverlay :=  pos('Intent="NIFTI_INTENT_SHAPE"', Hdr) > 0; //curvature map
    if not isOverlay then
       isOverlay :=  pos('Intent="NIFTI_INTENT_TTEST"', Hdr) > 0; //curvature map
    (*if (isOverlay) and  (Dim1 = 3) then begin
       isVertColor := true;
       isOverlay := false;
    end;*)
    if isOverlay then
       nOverlays := nOverlays + 1;
    isTransposed := pos('ColumnMajorOrder"', Hdr) > 0;
    if (Dim1 = 0) then Dim1 := 1;
    if (isOverlay) and (lOverlayIndex = 0) and ((length(vertices) > 0) or (length(faces) > 0)) then begin
       HasOverlays := true;
      isOverlay := false;
      //load background image first, load on future pass....
    end;
    (*if (isOverlay) and (lOverlayIndex = 0) then begin
       Showmessage('Please load GIfTI mesh (using File/Open) BEFORE loading the GIFTI overlay (using Overlay/Add)');
       goto 666;
    end;*)
    //if (isOverlay) then showmessage(format('%d %d', [length(vertices), Dim0]));
    if (isOverlay) and (Dim0 <> length(vertices)) and (length(vertices) > 0) then begin
       Showmessage(format('GIFTI overlay has a different number of vertices than the background mesh (%d vs %d)',[Dim0 , length(vertices)]));
       if surfaceID <> '' then
          Showmessage('Hint: first open the background mesh '+ surfaceID);
       goto 666;
    end;
    if (isAscii) and ((isVert) or (isOverlay)  or (isVertColor) or (isFace)) then begin
      //cuban
      {$IFDEF NOTXT}
       Showmessage('Unable to read GIFTI files with ASCII data. Solution: convert to more efficient BINARY data');
       {$ELSE}
       if (lOverlayIndex > 0) and (ddEnd > ddStart) then begin
          Showmessage('Unable to read GIFTI overlays with ASCII data. Solution: convert to more efficient BINARY data');
       end;
       if (isVertColor) and (ddEnd > ddStart) then begin
          Showmessage('Unable to read colored GIFTI with ASCII data. Solution: convert to more efficient BINARY data');
       end;
       if (isFace) and (ddStart > 6) and (ddEnd > ddStart) then begin
          //showmessage('f'+inttostr(ddStart)+'..'+inttostr(ddEnd)+ '  '+inttostr(Dim0));
          setlength(faces, Dim0);
          if isTransposed then begin
             j := Dim0-1;
             for i := 0 to j do
               faces[i].X := ReadNextInt(Str, ddStart, ddEnd);
             for i := 0 to j do
               faces[i].Y := ReadNextInt(Str, ddStart, ddEnd);
             for i := 0 to j do
               faces[i].Z := ReadNextInt(Str, ddStart, ddEnd);
          end else begin
            for i := 0 to (dim0 -1) do begin
                faces[i].X := ReadNextInt(Str, ddStart, ddEnd);
                faces[i].Y := ReadNextInt(Str, ddStart, ddEnd);
                faces[i].Z := ReadNextInt(Str, ddStart, ddEnd);
                //if i < 3 then showmessage(format('%g %g %g',[vertices[i].X, vertices[i].Y, vertices[i].Z]));
            end;
          end;
       end;
       if (isVert) and (ddStart > 6) and (ddEnd > ddStart) then begin
          setlength(vertices, Dim0);
          if isTransposed then begin
            j := Dim0-1;
            for i := 0 to j do
              vertices[i].X := ReadNextFloat(Str, ddStart, ddEnd);
            for i := 0 to j do
              vertices[i].Y := ReadNextFloat(Str, ddStart, ddEnd);
            for i := 0 to j do
              vertices[i].Z := ReadNextFloat(Str, ddStart, ddEnd);

          end else begin
            for i := 0 to (dim0 -1) do begin
                vertices[i].X := ReadNextFloat(Str, ddStart, ddEnd);
                vertices[i].Y := ReadNextFloat(Str, ddStart, ddEnd);
                vertices[i].Z := ReadNextFloat(Str, ddStart, ddEnd);
            end;
          end;
       end;
       {$ENDIF}
       //goto 666;
    end; //ascii
    if ((isVert) and (isFloat32) and (Dim1 = 3)) or ((lOverlayIndex <> 0) and (lOverlayItem = nOverlays) and (isOverlay) and (isFloat32) and (Dim1 >= 1))
      or ((isVertColor) and (isFloat32) and ((Dim1 = 4) or (Dim1 = 3)))  or ((isVertColor) and (isInt32) and (Dim1 = 1)) or ((isFace) and (isInt32) and (Dim1 = 3)) then begin
       if  ((externalFileName <> '') or (isBase64) or (isBase64Gz))  and (Dim0 > 0) and ((externalFileName <> '') or ((ddStart > 6) and (ddEnd > ddStart))) then begin
         if externalFileName = '' then begin
           debase64 :=  DecodeStringBase64(Copy(Str, ddStart, ddEnd-ddStart)); //raw GZ binary, see  http://lazarus-ccr.sourceforge.net/docs/fcl/base64/decodestringbase64.html
           if (not isRGBA) and (Dim1 <> 3) and (isVertColor) and (length(labelTable) < 1) then begin
             showmessage('Error found Intent="NIFTI_INTENT_LABEL" without a "<LabelTable>"');
             goto 666;
          end;
          if (length(debase64) < 4)  then begin
             showmessage('Impossibly small deflated stream');
             goto 666;
          end;
       end;
       szExpected :=  4 * Dim0 * Dim1;
       if externalFileName <> '' then begin
          if not (isBinary) then begin
              showmessage('Unknown encoding style for external GIfTI file');
              goto 666;
          end;
          externalFilePath := externalFileName;
          if not fileexists(externalFilePath) then
             externalFilePath := extractfilepath(FileName)+ externalFileName;
          if not fileexists(externalFilePath) then
             externalFilePath := extractfilepath(FileName)+ extractfilename(externalFileName);
          if not fileexists(externalFilePath) then begin
            showmessage('Unable to find '+externalFilePath);
            goto 666;
          end;
          szRead := Dim0 * Dim1 * 4;
          setlength(dat, Dim0 * Dim1); //check
          Assign (fx, externalFilePath);
          Reset (fx);
          seek(fx,externalFileOffset);
          blockread(fx, dat[0], Dim0 * Dim1 * 4 );
          CloseFile(fx);
           //ExternalFileName
        end else if isBase64Gz then begin
          if (length(debase64) < 1) or (ord(debase64[1]) <> $78) then begin
            showmessage('Deflate compressed stream should begin with 0x78, not '+inttohex(ord(debase64[1]), 2));
            goto 666;
          end;
          {$IFDEF FASTGZ}
          UnCompressStream(@debase64[1], length(debase64), gz, nil, true);
          debase64 := ''; // free memory
          setlength(dat, Dim0 * Dim1); //check
          gz.Position := 0;
          szRead := gz.Read(dat[0], szExpected);
          {$ELSE}
          gz.Write(debase64[1], length(debase64));
          debase64 := ''; // free memory
          gz.Position := 0;
          decomp := Tdecompressionstream.Create(gz);
          setlength(dat, Dim0 * Dim1); //check
          szRead := decomp.Read(dat[0], szExpected);
          decomp.Free;
          {$ENDIF}
          gz.Clear;
        end else begin
            szRead := length(debase64);
            setlength(dat,Dim0 * Dim1); //check
            Move(debase64[1], dat[0], szExpected);
        end;
        {$IFDEF ENDIAN_LITTLE}
        if not isLEndian then
           for i := 0 to (dim0 * Dim1) -1 do
               SwapLongInt(dat[i]);
        {$ELSE}
        if not isLEndian then
           for i := 0 to (dim0 * Dim1) -1 do
               SwapLongInt(dat[i]);
           big endian not tested - please check!!!!
       {$ENDIF}
          if szExpected <> szRead then begin
             showmessage(format('decompressed size incorrect %d != %d',[szExpected, szRead]));
             goto 666;
          end;
          if (isTransposed) and (Dim1 > 1) then
             TransposeDat(dat,Dim1);
          //if (isTransposed) and (Dim1 = 4) then
          //  TransposeDat4(dat);
          if isVertColor then begin
             setlength(vertexRGBA,Dim0);
             if (isRGBA) and (Dim1 = 4) then begin
                s := asSingle(dat[0]);
                for j := 0 to ((Dim0*4)-1) do
                    if asSingle(dat[j]) > s then
                       s := asSingle(dat[j]);
                if s > 1 then
                   s := 1
                else
                    s := 255;
                //if isTransposed then showmessage('x');
                j := 0;
                for i := 0 to (Dim0-1) do begin
                    vertexRGBA[i].R := round(s *asSingle(dat[j])); inc(j);
                    vertexRGBA[i].G := round(s *asSingle(dat[j])); inc(j);
                    vertexRGBA[i].B := round(s *asSingle(dat[j])); inc(j);
                    vertexRGBA[i].A := round(s *asSingle(dat[j])); inc(j);
                end;
             end else if (Dim1 = 3) then begin
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
          end else if (isOverlay) then begin
              //{$DEFINE ONLY1VOL}
              {$IFDEF ONLY1VOL}
              setlength(overlay[lOverlayIndex].intensity, length(vertices));
              j := 0;
                for i := 0 to (Dim0-1) do begin
                    overlay[lOverlayIndex].intensity[i] := asSingle(dat[j]);
                    j := j + Dim1;
                end;

              {$ELSE}
              overlay[lOverlayIndex].volumes := Dim1;
              overlay[lOverlayIndex].currentVolume := 1;
              setlength(overlay[lOverlayIndex].intensity,  Dim1*length(vertices));
              for k := 0 to (Dim1-1) do begin
                j := k;
                volInc := k * Dim0;
                for i := 0 to (Dim0-1) do begin
                    overlay[lOverlayIndex].intensity[i+volInc] := asSingle(dat[j]);
                    //if (k = 1) then
                    //   overlay[lOverlayIndex].intensity[i+volInc] :=  random(100)/100;
                    j := j + Dim1;
                end;

              end;
              {$ENDIF}
       end else if isFace then begin
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
    if (isCustomMat) and (length(vertices) > 0) then begin
       for i := 0 to (length(vertices) - 1) do
           vectorTransform(vertices[i],mat);
       //showmessage('t');
    end;
    isCustomMat := false;
    daStart := posEx('<DataArray', Str, daEnd); //attempt to read next dataArray
  end; //while true
  str := ''; //free
  gz.Free;
  if ((nOverlays > 0) or (HasOverlays) or (length(vertexRGBA) > 0)) and (length(vertices) = 0) then begin
     showmessage('Please load GIfTI mesh (using File/Open) BEFORE loading the GIFTI overlay (using Overlay/Add)');
     setlength(vertexRGBA,0);
     goto 666;
  end;
  if (length(vertexRGBA) > 0) and (length(vertexRGBA) <> length(vertices)) then begin
     showmessage(format('Number of vertices in NIFTI_INTENT_LABEL (%d) does not match number of vertices in NIFTI_INTENT_POINTSET (%d). Use File/Open to load the correct background image before loading your overlay.', [length(vertexRGBA), length(vertices)]));
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
end; //LoadGii()

procedure TMesh.LoadCtm(const FileName: string);
begin
  setlength(faces, 0); //just in case faces were read but not vertices
  setlength(vertices, 0);
  setlength(vertexRGBA,0);
  if (FSize(FileName) < 64) then exit;
  {$IFDEF CTM} readCTM(FileName, Faces, Vertices, vertexRGBA); {$ENDIF}
 //readCTM(const FileName: string; var Faces: TFaces;  var Verts: TVertices; var vertexRGBA : TVertexRGBA): boolean;
end;

(*function TMesh.LoadVbo(const FileName: string): boolean;
//fast native format - very similar to uncompressed mz3
//https://n-e-r-v-o-u-s.com/blog/?p=2738
//https://n-e-r-v-o-u-s.com/blog/wp-content/uploads/interstice_s.vbo
label
   666;
type
Tvbo_vertex = packed record
      vx, vy, vz, nx, ny, nz: single;
   end;
Tvbo_face = packed record
      v1,v2,v3: uint16;
   end;
var
   f: file;
   vert: Tvbo_vertex;
   face: Tvbo_face;
   szX, sz, i: integer;
   numVerts, numFaces: int32;
begin
  result := false;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  sz := FileSize(f);
  if sz < 64 then goto 666;
  blockread(f, numVerts, SizeOf(numVerts) );
  blockread(f, numFaces, SizeOf(numFaces) );
  SwapLongInt(numVerts);
  SwapLongInt(numFaces);
  if (numVerts < 3) or (numFaces < 1) then goto 666; //signature incorrect
  szX := 8+ (numVerts*4*6) + (numFaces * 2 * 3);
  if sz <> szX then
     goto 666;
  setlength(vertices, numVerts);
  for i := 0 to (numVerts - 1) do begin
    blockread(f, vert, SizeOf(vert) );
    vertices[i].X := vert.vx;
    vertices[i].Y := vert.vy;
    vertices[i].Z := vert.vz;
  end;
  setlength(faces, numFaces);
  for i := 0 to (numFaces - 1) do begin
    blockread(f, face, SizeOf(face) );
    faces[i].X := face.v1;
    faces[i].Y := face.v2;
    faces[i].Z := face.v3;
  end;
  result := true;
  666 :
  CloseFile(f);
  if not result then showmessage('Unable to decode VBO '+ FileName);
end; //LoadVbo()  *)


function TMesh.LoadPrwm(const FileName: string): boolean;
//https://github.com/kchapelier/PRWM/blob/master/specifications/prwm.md
//https://github.com/mrdoob/three.js/blob/def6a359df36307a6d32596683bfb1511f8ac3b5/examples/js/loaders/PRWMLoader.js
label
   666;
type
Tui32 = packed record
      x,y,z: uint32;
   end;
Tui16 = packed record
      x,y,z: uint16;
   end;
Tpr_hdr = packed record
      vers,flags,
      nVal1,nVal2,nVal3,
      nIdx1,nIdx2,nIdx3: byte;
   end;
var
   f: file;
   ui16: Tui16;
   ui32: Tui32;
   f32: TPoint3f;
   pos, sz, a,i, pad, aByteCount: integer;
   hdr: Tpr_hdr;
   valuesNumber,indicesNumber,indicesType,numVerts, numFaces: int32;
   indexedGeometry, bigEndian, swapEndian: boolean;
   aType, aNormalized, aCardinality, aEncoding, aflag, attributesNumber: byte;
   ch: char;
   eStr,attributeName : string;
function i24to32(v1,v2,v3: byte): integer;
begin
  if swapEndian then
     result := v3 + (v2 shl 8) + (v1 shl 16)
  else
     result := v1 + (v2 shl 8) + (v3 shl 16);

end;
begin
  result := false;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  sz := FileSize(f);
  numVerts := 0;
  eStr := 'File too small';
  if sz < 64 then goto 666;
  blockread(f, hdr, SizeOf(hdr) );
  indexedGeometry := (hdr.flags and 128) > 0;
  indicesType := (hdr.flags shr 6) and 1; ;
  bigEndian := (hdr.flags and 32) > 0;
  {$IFDEF ENDIAN_LITTLE}
  swapEndian := bigEndian;
  {$ELSE}
  swapEndian := not bigEndian;
  {$ENDIF}
  attributesNumber := (hdr.flags and 31);
  eStr := 'Unupported header';
  if (hdr.vers <> 1) or (attributesNumber = 0) then goto 666; //signature incorrect
  valuesNumber := i24to32(hdr.nVal1, hdr.nVal2, hdr.nVal3);
  indicesNumber := i24to32(hdr.nIdx1, hdr.nIdx2, hdr.nIdx3);
  if ( not indexedGeometry ) then begin
     eStr := 'Error with non-indexed geometries';
  	if ( indicesType <> 0) then
                goto 666 //( 'PRWM decoder: Indices type must be set to 0 for non-indexed geometries' );
        else if ( indicesNumber <> 0 ) then
             goto 666; //( 'PRWM decoder: Number of indices must be set to 0 for non-indexed geometries' );
  end;
  //showmessage(format('%d %d %d %d -> %d %d',[x(indexedGeometry), x(indicesType), x(bigEndian), attributesNumber, valuesNumber, indicesNumber]));
  for a := 1 to attributesNumber do begin
    attributeName := '';
    i := 1;
    pad := 1;
    while (i = 1) do begin
          blockread(f, ch, sizeOf(ch), i);
          pad := pad + 1;
          if (ch = chr(0)) then break;
          ch := UpCase(ch);
          if ch in ['A'..'Z'] then
             attributeName := attributeName + ch;
    end;
    blockread(f, aflag, sizeof(aflag));
    aType := (aflag shr 7) and 1;
    aNormalized := (aflag shr 6) and 1;
    aCardinality := ((aflag shr 4) and 3)+1; //2 bits (0 to 3)
    aEncoding := (aflag and 15); //4 bits (0 to 15)
    seek(f, ((filepos(f)+3) div 4) * 4);//pad
    case aEncoding of
         1,6,10: aByteCount := 4;
         3,7: aByteCount := 1;
         4,8: aByteCount := 2;
         else aByteCount := 0;
    end;
    pos := filepos(f) + (aByteCount * aCardinality * valuesNumber);
    if (attributeName = 'POSITION') then begin
       eStr := 'File too small for POSITION';
       if (pos +  (numVerts*3*4)) > sz  then goto 666;
       if ((aType = 0) and (aEncoding = 1) and (aCardinality = 3)) then begin
         numVerts :=valuesNumber;
         setlength(vertices, numVerts);
         for i := 0 to (numVerts - 1) do begin
             blockread(f, f32, SizeOf(f32) );
             if swapEndian then begin
                SwapSingle(f32.x);
                SwapSingle(f32.y);
                SwapSingle(f32.z);
             end;
             vertices[i].X := f32.x;
             vertices[i].Y := f32.y;
             vertices[i].Z := f32.z;
         end;
      end else begin
          eStr := format('Unsupported POSITION type %d encoding %d or cardinality %d norm %d ',[aType, aEncoding, aCardinality, aNormalized]);
          goto 666;
      end;
    end;
    seek(f,pos);
    seek(f, ((filepos(f)+3) div 4) * 4);//pad
  end; //for each set of attributes
  eStr := 'No POSITION attribute';
  if numVerts < 3 then goto 666;
  if ( indexedGeometry ) then begin
     pos := filepos(f);
     eStr := format('File too small for INDICES %d > %d', [pos +  (indicesNumber*2), sz]);
     if (pos +  (indicesNumber*2)) > sz  then goto 666;
     eStr := format('File too small for INDICES %d > %d', [pos +  (indicesNumber*4), sz]);
     if (indicesType <> 0) and ((pos +  (indicesNumber*4)) > sz) then goto 666;

     numFaces := indicesNumber div 3;
     setlength(faces, numFaces);
     if indicesType = 0 then begin //ui16
       for i := 0 to (numFaces - 1) do begin
           blockread(f, ui16, SizeOf(ui16) );
           if swapEndian then begin
              ui16.x := Swap(ui16.x);
              ui16.y := Swap(ui16.y);
              ui16.z := Swap(ui16.z);
           end;
           faces[i].X := ui16.x;
           faces[i].Y := ui16.y;
           faces[i].Z := ui16.z;
       end;
     end else begin //ui32
       for i := 0 to (numFaces - 1) do begin
           blockread(f, ui32, SizeOf(ui32));
           if swapEndian then begin
              SwapLongWord(ui32.x);
              SwapLongWord(ui32.y);
              SwapLongWord(ui32.z);
           end;
           faces[i].X := ui32.x;
           faces[i].Y := ui32.y;
           faces[i].Z := ui32.z;
       end;
     end;
  end else begin
      numFaces := numVerts div 3;
      setlength(faces, numFaces);
      a := 0;
      for i := 0 to (numFaces - 1) do begin
        faces[i].X := a; inc(a);
        faces[i].Y := a; inc(a);
        faces[i].Z := a; inc(a);
      end;
      UnifyVertices(faces, vertices);
  end;
  result := true;
  666 :
  CloseFile(f);
  if not result then showmessage('Error '+eStr+ ': '+FileName);
end; //LoadPrwm()

function LoadMz3Core(const FileName: string; var Faces: TFaces; var Vertices: TVertices; var vertexRGBA: TVertexRGBA; var intensity: TFloats; var vertexAtlas: TInts; var atlasMaxIndex: integer; out isAOMap: boolean; nVertBackground: integer = 0): boolean;
const
 kMagic =  23117; //"MZ"
 //kChunkSize = 16384;
label 666;
var
  i, j: integer;
  //s: single;
  Magic, Attr: uint16;
  nFace, nVert, nSkip: uint32;
  intensityDoubles: TDoubles;
  isFace, isVert, isRGBA, isScalar, isDoubleScalar: boolean;
  mStream : TMemoryStream;
begin
     result := false;
     isAOMap := false;
     atlasMaxIndex := 0;
     setlength(Faces,0);
     setlength(Vertices,0);
     setlength(vertexRGBA,0);
     setlength(intensity,0);
     if not fileexistsF(Filename) then exit;
     mStream := TMemoryStream.Create;
     ExtractGz(FileName,mStream, kMagic);
     if mStream.Size < 28 then begin
       //showmessage('MZ3 file too small'+inttostr(mStream.Size));
       exit; //16 byte header, 3 vertices, single 4-byte scalar per vertex vertices
     end;
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
     isDoubleScalar := (Attr and 16) > 0;
     isAOMap := (Attr and 32) > 0;
     if (Attr > 63) then begin
        showmessage('Unsupported future format '+ inttostr(Attr));
        goto 666;
     end;
     if (isScalar and isDoubleScalar) then begin
        showmessage('Broken or Unsupported future format '+ inttostr(Attr));
        goto 666;
     end;
     if (nFace = 0) and (isFace) then goto 666;
     if (not isVert) and (nVert = 0) then
        nVert := nVertBackground;
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
        if isScalar then
           for i := 1 to (nVert -1) do
               vertexRGBA[i].A := 255;
     end;
     if isScalar then begin
       j := (mStream.Size - mStream.Position) div (nVert * sizeof(single));
       if j >= 1 then begin
          setlength(intensity, j * nVert);
          mStream.Read(intensity[0], j * nVert * sizeof(single));
       end;
     end;
     if isDoubleScalar then begin
       j := (mStream.Size - mStream.Position) div (nVert * sizeof(double));
       if j >= 1 then begin
          setlength(intensity, j * nVert);
          setlength(intensityDoubles, j * nVert);
          mStream.Read(intensityDoubles[0], j * nVert * sizeof(double));
          for i := 0 to ((j * nVert)-1) do
              intensity[i] := intensityDoubles[i];
          intensityDoubles := nil;
       end;
     end;
     if (isRGBA) and (isScalar) then begin //atlas template the float "intensity" stores integer of index
        setlength(vertexAtlas, nVert);
        for i := 0 to (nVert-1) do
            vertexAtlas[i] := round(intensity[i]);
        setlength(intensity,0);
        atlasMaxIndex := vertexAtlas[0];
        for i := 0 to (nVert -1) do
            if vertexAtlas[i] > atlasMaxIndex then
               atlasMaxIndex := vertexAtlas[i];
        (*atlasMinxIndex := vertexAtlas[0];
        for i := 0 to (nVert -1) do
            if vertexAtlas[i] < atlasMinxIndex then
               atlasMinxIndex := vertexAtlas[i];
        GLForm1.Caption := format('%d..%d',[atlasMinxIndex,atlasMaxIndex]);*)
     end;
     result := true;
   666 :
     mStream.Free;
end; // LoadMz3Core()

function TMesh.LoadMz3(const FileName: string; lOverlayIndex : integer): boolean;
var
  floats: TFloats;
  isAOMap: boolean;
  i: integer;
begin
  if lOverlayIndex < 1 then begin //save main image
     result := LoadMz3Core(Filename, Faces,Vertices,vertexRGBA,floats, vertexAtlas, atlasMaxIndex, isAOMap);
     if not result then exit;
     if (length(Faces) < 1) and (length(floats) > 0) then
        Showmessage('Please load mesh (using File/Open) BEFORE loading the overlay (using Overlay/Add).')
     else if  (length(floats) >=  length(Vertices)) then begin
          if OpenOverlays < kMaxOverlays then begin
            OpenOverlays := OpenOverlays + 1;
            setlength(Overlay[OpenOverlays].intensity, length(floats));
            Overlay[OpenOverlays].intensity := copy(floats, 0, maxint);
            Overlay[OpenOverlays].LUTindex := SetLutIndex(OpenOverlays);
            Overlay[OpenOverlays].filename  := ExtractFilename(FileName);
            Overlay[OpenOverlays].volumes := length(floats) div length(Vertices);
            Overlay[OpenOverlays].CurrentVolume := 1;
            Overlay[OpenOverlays].OpacityPercent:= kLUTopaque;
            SetOverlayDescriptives(OpenOverlays);
            Overlay[OpenOverlays].aoMap := isAOMap;
            if isAOMap then  Overlay[OpenOverlays].LUTindex := 0;
            Overlay[OpenOverlays].LUT := UpdateTransferFunction (Overlay[OpenOverlays].LUTindex, Overlay[OpenOverlays].LUTinvert);
            //Showmessage('>>>'+inttostr(OpenOverlays));
          end else //will never happen: we close all overlays when we open a mesh
             Showmessage('Too many overlays open to show the intensity map.');
     end;
     setlength(floats,0);
     exit;
  end;
  //otherwise, load overlay
  result := LoadMz3Core(Filename, Overlay[lOverlayIndex].Faces,Overlay[lOverlayIndex].Vertices,Overlay[lOverlayIndex].vertexRGBA,Overlay[lOverlayIndex].intensity,Overlay[lOverlayIndex].vertexAtlas, overlay[lOverlayIndex].atlasMaxIndex, isAOMap, length(Vertices));
  if (length(Overlay[lOverlayIndex].vertexRGBA) > 0) and (length(Overlay[lOverlayIndex].intensity) > 0) then //template with BOTH intensity and RGBA: give precedence to RGBA
     setlength(Overlay[lOverlayIndex].intensity,0);
  if not result then exit;
  i := 0;
  if (length(overlay[lOverlayIndex].intensity)  >= length(Vertices)) and ((length(overlay[lOverlayIndex].intensity) mod length(Vertices)) = 0) then
     i := length(overlay[lOverlayIndex].intensity) div length(Vertices);
  if (length(Overlay[lOverlayIndex].Vertices) < 3) and (length(Overlay[lOverlayIndex].intensity) > 0) and (i < 1) then begin
     showmessage(format('This overlay has a different number of vertices (%d) than the background mesh (%d).', [length(overlay[lOverlayIndex].intensity), length(Vertices) ]));
     setlength(overlay[lOverlayIndex].intensity, 0);
     result := false;
  end;
  if i > 0 then begin
     overlay[lOverlayIndex].CurrentVolume:= 1;
     overlay[lOverlayIndex].volumes:= i;
     overlay[lOverlayIndex].aoMap := isAOMap;
  end;
end; // LoadMz3()

function item2Int(s: string; item: integer): integer;
// 2nd item of 'refs 3' is 3
var
     sList: TStringList;
begin
sList := TStringList.Create;
sList.DelimitedText := s;
result := 0;
if sList.Count >= item then
   result := StrToIntDef(sList[item-1],0);
sList.free;
end; //item2ToInt()

function TMesh.Load3Do(const FileName: string): boolean;
//http://wiki.xentax.com/index.php/Shaiya_Online/Model
//assume this is always little-endian?
label
   666;
type
  T3Do_vertex = packed record
     vx, vy, vz, nx, ny, nz, tu, tv: single;
  end;
  T3Do_face = packed record
    v1, v2, v3: uint16;
  end;
var
   f: file;
   sz, i: integer;
   vert: T3Do_vertex;
   face: T3Do_face;
   null, numVerts, numFaces: uint32;
begin
  result := false;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  sz := FileSize(f);
  if sz < 64 then goto 666;
  blockread(f, null, SizeOf(null) );
  blockread(f, numVerts, SizeOf(numVerts) );
  if (null <> 0) or (numVerts < 3) then goto 666; //signature incorrect
  setlength(vertices, numVerts);
  for i := 0 to (numVerts - 1) do begin
    blockread(f, vert, SizeOf(vert) );
    vertices[i].X := vert.vx;
    vertices[i].Y := vert.vy;
    vertices[i].Z := vert.vz;
  end;
  blockread(f, numFaces, SizeOf(numFaces));
  if numFaces < 1 then goto 666;
  setlength(faces, numFaces);
  for i := 0 to (numFaces - 1) do begin
    blockread(f, face, SizeOf(face) );
    faces[i].X := face.v1;
    faces[i].Y := face.v2;
    faces[i].Z := face.v3;
  end;
  result := true;
  666 :
  CloseFile(f);
  if not result then showmessage('Unable to decode Shaiya_Online/Model '+ FileName);
  //UnifyVertices(faces, vertices);
end; // Load3Do()

function TMesh.LoadByu(const FileName: string): boolean;
//http://www.eg-models.de/formats/Format_Byu.html
//http://paulbourke.net/dataformats/moviebyu/
//https://www.tomovision.com/BabySliceO_Help/index.htm?context=800
// used as export from ITKsnap (though not precisely to original spec)
label
   666;
const
  kBlockSz = 4096;
var
   f: TextFile;
   fCurrent, fFirst,fPrev, num_f, nPos,i, nparts, npoints, nconnects: integer;
begin
  result := false;
  if (FSize(FileName) < 64) then exit;
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  Reset(f);
  Readln(f,nparts, npoints, i, nconnects);
  if (nparts < 1) or (npoints < 3) then goto 666;
  for i := 1 to nparts do
      Readln(f);
  setlength(vertices, npoints);
  result := true;
  //showmessage(format('%d', [npoints]));
  //this code reads modern BYU files - it can fail with original files that have no space between values that take precisely 12 digits
  for i := 0 to (npoints-1) do begin
      try
         read(f,vertices[i].X);
         read(f,vertices[i].Y);
         read(f,vertices[i].Z);
      except
        result := false;
      end;
      if not result then goto 666;
  end;
  //showmessage(format('%g %g %g',[vertices[npoints-1].X, vertices[npoints-1].Y, vertices[npoints-1].Z]));
  setlength(faces, kBlockSz);
  num_f := 0;
  nPos := 0;
  fFirst := 1;
  fPrev := 1;
  //showmessage(inttostr(nconnects));
  //ITK also differs from the spec, which demanded 16 indices per line
  if (nconnects < 3) then nconnects := maxint; //afni ConvertSurface sets as -1!
  i := 0;
  while (i < nconnects) and (not EOF(f))do begin
      read(f, fCurrent);
      inc(nPos);
      //showmessage(format('%d %d',[nPos, fCurrent]));
      if nPos = 1 then fFirst := abs(fCurrent);
      if nPos > 2 then begin//new face
        if (num_f) >= length(faces) then
           setlength(faces, num_f + kBlockSz); //non-triangular meshes will be composed of more triangles than num_f
        faces[num_f] := pti(fFirst, fPrev, abs(fCurrent));
         //showmessage(format('%d %d %d',[faces[num_f].X, faces[num_f].Y, faces[num_f].Z]));
         num_f := num_f + 1;
      end;
      fPrev := abs(fCurrent);
      if fCurrent < 0 then nPos := 0;
      i := i + 1;
  end;
  setlength(faces, num_f);
  //showmessage(format('%d %d %d',[faces[num_f-1].X, faces[num_f-1].Y, faces[num_f-1].Z]));
  666:
  closefile(f);
  if not result then
     showmessage('Unable to read BYU file (decoder assumes ITK data and can fail when vertices are packed with twelve digits and no spaces)'+fileName)
  else if num_f < 1 then begin
     showmessage('Only able to read triangular BYU files: maybe this is points or lines');
     result := false;
  end;
end;

function TMesh.LoadAc(const FileName: string): boolean;
//http://www.inivis.com/ac3d/man/ac3dfileformat.html
//https://en.wikipedia.org/wiki/AC3D
label
   666;
const
  kBlockSz = 4096;
var
   f: TextFile;
   new_f, num_v, num_f, i, indxPrev, indx1: integer;
   s: string;
   v4: single;
begin
     result := false;
     if (FSize(FileName) < 64) then exit;
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
     Reset(f);
     Readln(f,s);
     s := trim(s);
     if (pos('AC3Db', s) < 1) then begin
        showmessage('Corrupt file: AC files must begin with AC3Db not "'+s+'"');
        CloseFile(f);
        exit;
     end;
     while not EOF(f) and (pos('NUMVERT', upcase(s)) <> 1) do   //numvert 4
           Readln(f, s);
     if EOF(f) then goto 666;
     num_v := item2Int(s,2);
     if (num_v < 1) then exit;
     setlength(vertices, num_v);
     for i := 0 to (num_v - 1) do
         ReadlnSafe(f, vertices[i].X, vertices[i].Y, vertices[i].Z, v4);
     //read faces
     setlength(faces, kBlockSz);
     num_f := 0;
     while not EOF(f) do begin
           if (pos('REFS', upcase(s)) > 0) then begin  //refs 3
              new_f := item2Int(s,2);
              if new_f < 3 then goto 666;
              if not EOF(f) then readln(f,s);
              indx1 := item2Int(s,1);
              if not EOF(f) then readln(f,s);
              indxPrev := item2Int(s,1);
              if (num_f + (new_f - 2)) > length(faces) then
                 setlength(faces, num_f + kBlockSz); //non-triangular meshes will be composed of more triangles than num_f
              for i := 3 to new_f do begin
                  faces[num_f].X := indx1;
                  faces[num_f].Y := indxPrev;
                  if not EOF(f) then readln(f,s);
                  faces[num_f].Z := item2Int(s,1);
                  indxPrev :=  faces[num_f].Z;
                  num_f := num_f + 1;
              end;
           end;
           Readln(f, s);

     end;
     result := true;
     setlength(faces,num_f);
     UnifyVertices(Faces, Vertices);
  666:
     CloseFile(f);
     if not result then
        showmessage('Unable to read AC file '+fileName);
end;

function TMesh.LoadDae(const FileName: string): boolean;
//this loader only reads simple collada images
// http://codeflow.org/entries/2011/nov/18/parsing-3d-file-formats/
//Collada is a XML format: All of an XML document is case-sensitive
label
  123, 666;
const
  kBlockSz = 4096;
var
  f : TextFile;
  s, pSource, vCount, currentsource, p, inputSemantic: string;
  floatListVal,floatListID: TStringList;
  vertexOffset, maxOffset, offset, nF, nV, i, j, k, newV, newF, indxPrev, indx1, nVstart : integer;
  indx: array of integer;
  function GetTag (s: string): string;
  // semantic="VERTEX" returns VERTEX
  var
    i: integer;
  begin
       result := '';
       i := Pos('="', s);
       if i < 1 then exit;
       result := s;
       delete(result, 1, i+1);
       i := Pos('"', result);
       if i < 1 then exit;
       delete(result, i, maxint);
  end; //nested GetTag()
  function getID(s: string): string;
  // <float_array id="Cube-mesh-positions-array" count="24"> returns  Cube-mesh-positions-array
  var
     sList: TStringList;
     j: integer;
  begin
      result := '"';//impossible value
      if length(s) < 1 then exit;
      sList := TStringList.Create;
      sList.DelimitedText := s;
      for j := 0 to (sList.Count-1) do begin
          if (Pos('id="', sList[j]) = 1) then begin
             result := GetTag(sList[j]);
             exit;

          end;
      end; //for j, each item
      sList.free;
  end; //nested getID()

 function isInputVertex(var semantic: string) : integer;
//returns offset
//<input semantic="VERTEX" source="#Cube-mesh-vertices" offset="0" />
 var
    j: integer;
    inputSource: string;
    sList: TStringList;
  begin
       result := 0;
       semantic := '';
       inputSource := '';//impossible
       if (Pos('<input', s) <> 1) then exit;
       sList := TStringList.Create;
       sList.DelimitedText := s;
       for j := 0 to (sList.Count-1) do begin
           if (Pos('semantic="', sList[j]) = 1) then
              semantic := GetTag(sList[j]);
           if (Pos('source="', sList[j]) = 1) then
              inputSource := GetTag(sList[j]);
           if (Pos('offset="', sList[j]) = 1) then
              result := StrToIntDef(GetTag(sList[j]),0);
       end;
       if (length(inputSource) > 0) and (inputSource[1] = '#') then
          delete(inputSource, 1,1);
       if semantic = 'POSITION' then
          pSource := inputSource;
       sList.free;
  end; //nested isInputVertex()
  function trimXML(s: string): string;
  // "<vcount>2 3 41</vcount>" -> "2 3 41"
  begin
       result := '';
       i := Pos('>', s);
       if i < 1 then exit;
       result := s;
       delete(result, 1, i);
       if length(result) < 1 then begin
         ReadLn(f, s);
         result := trim(s);
         exit;
       end;
       i := Pos('<', result);
       if i < 1 then exit;
       delete(result, i, maxInt);
  end; //nested trimXML()
begin
  result := false;
  nF := 0;
  nV := 0;
  if (FSize(FileName) < 64) then exit;
  SetLength(Faces,kBlockSz);
  SetLength(Vertices,kBlockSz);
  floatListVal := TStringList.Create;
  floatListID := TStringList.Create;
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  Reset(f);
  //1: skip all lines before <mesh>
  s := '';
123:
  while (not Eof(f)) and (Pos('<mesh>', s) < 1) do begin
      ReadLn(f, s);
      s := trim(s);
  end;
  if EOF(f) then goto 666;
  maxOffset := 0;
  vertexOffset := 0;
  p := '';
  vcount := '';
  psource := '';
  currentsource := '';
  floatListVal.Clear;
  floatListID.Clear;
  while (not Eof(f)) and (Pos('</mesh>', s) < 1) do begin
      ReadLn(f, s);
      s := trim(s);
      isInputVertex(inputSemantic);
      if (Pos('<source', s) = 1) then
         currentsource := getID(s);
      if (Pos('<float_array', s) = 1) then begin
         floatListID.Add(currentsource);
         floatListVal.Add(trimXML(s));
      end; //<float_array
      if (Pos('<polylist', s) = 1) or (Pos('<triangles', s) = 1)  then begin   //<triangles
        while (not Eof(f)) and (Pos('</polylist>', s) < 1) and (Pos('</triangles>', s) < 1) do begin
            if (Pos('<input', s) > 0) then begin
               offset := isInputVertex(inputSemantic);
               if inputSemantic = 'VERTEX' then
                  vertexOffset := offset;
               if offset > maxOffset then
                  maxOffset := offset;
            end;

            if (Pos('<vcount>', s) > 0) then
               vcount := trimXML(s);
            if (Pos('<p>', s) > 0) then
               p := trimXML(s);
            ReadLn(f, s); //read floats
            s := trim(s);
        end;
        //sList.DelimitedText := s;
        //Memo1.Lines.add(inttostr(sList.count)+' ' + s);
      end; //<polylist

      //Memo1.Lines.add(inttostr(Pos('</mesh', s)) +' '+s);
  end;  //not EOF
  //Memo1.Lines.add('psource ' + psource+' -> ' + inttostr(floatListID.Count));
  nVstart := nV;
  if (psource = '') then goto 666; //we need vertex indices and positions!
  for i := 0 to (floatListID.Count -1) do begin  //decode vertices aka "POSITIONS"
      if trim(floatListID[i]) = psource then begin
        floatListID.Clear;
        floatListID.DelimitedText := floatListVal[i];
        if (floatListID.Count mod 3) <> 0 then goto 666;
        newV := floatListID.Count div 3;
        if length(vertices) < (nV + newV) then
           setlength(vertices, length(vertices)+newV);
        k := 0;
        for j := 0 to (newV - 1) do begin
            vertices[nV + j].X := StrToFloatDef(floatListID[k], 0.0);
            k := k + 1;
            vertices[nV + j].Y := StrToFloatDef(floatListID[k], 0.0);
            k := k + 1;
            vertices[nV + j].Z := StrToFloatDef(floatListID[k], 0.0);
            k := k + 1;
        end;
        nV := nV + newV;
      end; //if float list is POSITIONS
  end; //for i - number of floatLists
  //decode faces
  if (p = '')  then goto 666;
  floatListID.Clear;
  floatListID.DelimitedText := p; //p 0 1 2 3 4 7 6 5 0 4 5 1 1 5 6 2 2 6 7 3 4 0 3 7
  setlength(indx, floatListID.count);
  for i := 0 to (floatListID.count - 1) do
      indx[i] := StrToIntDef(floatListID[i], 0);
  maxOffset := maxOffset + 1; //e.g. if offsets 0 1, then there are 2 offsets!
  if vcount = '' then begin //triangles, every three indices are one triangle
     if (length(indx) mod (3* maxOffset)) <> 0 then goto 666;
     newF := length(indx) div (3*maxOffset);
     if (nF + newF) > length(faces) then
           setlength(faces, nF +newF + kBlockSz);
     k := vertexOffset;
     for j := 0 to newF do begin
         faces[nF + j].X := indx[k]+nVstart;
         k := k + maxOffset;
         faces[nF + j].Y := indx[k]+nVstart;
         k := k + maxOffset;
         faces[nF + j].Z := indx[k]+nVstart;
         k := k + maxOffset;
         //showmessage(inttostr(faces[nF + j].X)+' '+inttostr(faces[nF + j].Y)+' '+inttostr(faces[nF + j].Z))
     end;
     nF := nF + newF;
  end else begin
    floatListID.Clear;
    floatListID.DelimitedText := vcount; //vcount 4 4 4 4 4 4
    k := 0;
    for i := 0 to (floatListID.count - 1) do begin
        newF := StrToIntDef(floatListID[i], 0) - 2;//e.g. 3 indices = 1 tri, 4 = 2, 5 = 3
        if newF < 1 then goto 666;
        if (nF + newF) > length(faces) then
              setlength(faces, nF +newF + kBlockSz);
        indx1 := indx[k]+nVstart;
        k := k + maxOffset;
        indxPrev :=  indx[k]+nVstart;
        k := k + maxOffset;
        for j := 0 to (newF-1) do begin
               faces[nF + j].X := indx1;
               faces[nF + j].Y := indxPrev;
               faces[nF + j].Z := indx[k]+nVstart;
               k := k + maxOffset;
               indxPrev :=  faces[nF + j].Z;
        end;
        nF := newF + nF;
    end;
    setlength(indx, 0);
  end;
  result := true;
goto 123;
 666:
  // Close the file for the last time
  CloseFile(f);
  floatListVal.free;
  floatListID.free;
  if (not result) or (nF < 1) or (nV < 1) then begin
     result := false;
     nF := 0;
     nV := 0;
  end;
  setlength(Faces, nF);
  setlength(Vertices, nV);
  UnifyVertices(Faces, Vertices);
end; // LoadDae()

function TMesh.LoadGts(const FileName: string): boolean;
//http://gts.sourceforge.net/samples.html
// edges ignore winding: https://sourceforge.net/p/gts/mailman/message/3574977/
// each sample has internally consistent face winding, but winding differs between samples!
// Meshlab 1.3.3 only saves half the triangles specified in first line!
label
   666;
var
   f: TextFile;
   nV, nF, nE, i: integer;
   face: TPoint3i;
   uedges : array of TPoint3i;
begin
     result := false;
     if (FSize(FileName) < 64) then exit;
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
     Reset(f);
     Readln(f, nV, nE, nF);
     if (nV < 3) or (nE < 3) or (nF < 1) then goto 666;
     setlength(vertices, nV);
     setlength(uedges, nE);
     setlength(faces, nF);
     for i := 0 to (nV-1) do begin
         if EOF(f) then goto 666; //MeshLab 1.3.3 patch
         Readln(f, vertices[i].X, vertices[i].Y, vertices[i].Z);
     end;
     for i := 0 to (nE-1) do begin
         if EOF(f) then goto 666; //MeshLab 1.3.3 patch
         Readln(f, uedges[i].X, uedges[i].Y);
         uedges[i] := vectorAdd(uedges[i],-1); //we index arrays from 0, not 1
     end;
     for i := 0 to (nF-1) do begin
        if EOF(f) then goto 666; //MeshLab 1.3.3 patch
        Readln(f, face.X, face.Y, face.Z);
        face := vectorAdd(face,-1); //we index arrays from 0, not 1
        //a lot of conditionals for CONSISTENT winding, ugly but it works
        if (uedges[face.X].X = uedges[face.Z].X) or (uedges[face.X].X = uedges[face.Z].Y) then
           faces[i].X := uedges[face.X].Y
        else
            faces[i].X := uedges[face.X].X;
        if (uedges[face.Y].X = faces[i].X) then
           faces[i].Y := uedges[face.Y].Y
        else
            faces[i].Y := uedges[face.Y].X;
        if (uedges[face.Z].X = faces[i].X) or (uedges[face.Z].X = faces[i].Y) then
           faces[i].Z := uedges[face.Z].Y
        else
            faces[i].Z := uedges[face.Z].X;
     end;
     result := true;
666:
     CloseFile(f);
     if not result then begin
        setlength(vertices, 0);
        setlength(faces, 0);
        showmessage('Unable to import as GTS file '+ FileName);
     end;
     UnifyVertices(faces, vertices);
end; // LoadGts()

function TMesh.LoadDxf(const FileName: string): boolean;
//http://paulbourke.net/dataformats/dxf/min3d.html
//Reads DXF files where faces specified with 10,20,30/11,21,31/12,22,32/13,23,33
//Does not read all files, see wuson.dxf
//  https://github.com/assimp/assimp/tree/master/test/models/DXF
const kBlockSz = 32768; //must be >2, larger is faster (fewer memory reallocations)
var
   f: TextFile;
   nV, nF, groupCode: integer;
   groupValue: single;
   groupCodeStr, groupValueStr: string;
   v : array[0..3] of TPoint3f;
procedure AddTri (var v1, v2, v3: TPoint3f);
begin
  if nF >= length(faces) then setlength(faces, length(faces) + kBlockSz);
  if (nV + 2) >= length(vertices) then setlength(vertices, length(vertices) + kBlockSz);
  faces[nF].X := nV;
  faces[nF].Y := nV + 1;
  faces[nF].Z := nV + 2;
  vertices[nV] := v1;
  vertices[nV + 1] := v2;
  vertices[nV + 2] := v3;
  nF := nF + 1;
  nV := nV + 3;
end;
begin
     result := false;
     if (FSize(FileName) < 64) then exit;
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
     Reset(f);
     nF := 0;
     nV := 0;
     setlength(vertices, kBlockSz); //avoid constantly resizing arrays
     setlength(faces, kBlockSz);
     while not EOF(f) do begin
           Readln(f, groupCodeStr);
           if EOF(f) then break;
           Readln(f, groupValueStr);
           groupCode := StrToIntDef (groupCodeStr, 0);
           if (groupCode < 10) or (groupCode > 33) then continue;
           if (groupCode mod 10) > 3 then continue;//last digit should be in range [0,1,2,3]
           groupValue := StrToFloatDef(groupValueStr, 0.0);
           if groupCode = 10 then v[0].X := groupValue;
           if groupCode = 20 then v[0].Y := groupValue;
           if groupCode = 30 then v[0].Z := groupValue;
           if groupCode = 11 then v[1].X := groupValue;
           if groupCode = 21 then v[1].Y := groupValue;
           if groupCode = 31 then v[1].Z := groupValue;
           if groupCode = 12 then v[2].X := groupValue;
           if groupCode = 22 then v[2].Y := groupValue;
           if groupCode = 32 then v[2].Z := groupValue;
           if groupCode = 13 then v[3].X := groupValue;
           if groupCode = 23 then v[3].Y := groupValue;
           if groupCode = 33 then begin
              v[3].Z := groupValue;
              AddTri (v[0], v[1], v[2]);
              //if (vectorLength(v[2],v[3]) > 0) then //if 4th point is different from 3rd: generate quad
              if not vectorSame(v[2], v[3]) then //if 4th point is different from 3rd: generate quad
                   AddTri (v[0], v[2], v[3]);
           end;
     end;
     CloseFile(f);
     setlength(vertices, nV); //avoid constantly resizing arrays
     setlength(faces, nF);
     if (nF > 0) and (nV > 2) then result := true;
     if not result then
        showmessage('Unable to import DXF file (perhaps not a 3D mesh) '+ FileName);
     UnifyVertices(faces, vertices);
end; // LoadDxf()

function TMesh.CullUnusedVertices(): boolean;
//https://www.nitrc.org/forum/message.php?msg_id=27399
type
    TVtx = packed record
         nUsed, newIdx: integer;
    end;
var
   vs: array of TVtx;
   oldRGBA : TVertexRGBA;//array of TRGBA;
   oldVertices : TVertices;
   nf, nvIn, nvOut, i: integer;
begin
     result := false;
     nvIn := length(vertices);
     nf := length(faces);
     if nf < 1 then exit;
     if nvIn < 3 then exit;
     if not CheckMesh then exit; //make sure faces indexed from 0
     setlength(vs, nvIn);
     for i := 0 to (nvIn-1) do
         vs[i].nUsed:= 0;
     //check if all vertices are used
     for i := 0 to (nf - 1) do begin
         inc(vs[faces[i].x].nUsed);
         inc(vs[faces[i].y].nUsed);
         inc(vs[faces[i].z].nUsed);
     end;
     nvOut := 0;
     for i := 0 to (nvIn-1) do begin
         if (vs[i].nUsed = 0) then continue;
         vs[i].newIdx := nvOut;
         inc(nvOut);
     end;
     if (nVin  = nVout) or (nVout = 0) then begin //nothing to do
        vs := nil;
        exit;
     end;
     {$IFDEF UNIX}
     writeln(format('Unused vertices culled %d -> %d',[nvIn, nvOut]));
     {$ENDIF}
     setlength(oldVertices, nVin);
     oldVertices := Copy(vertices, 0, MaxInt);
     setlength(Vertices, nvOut);
     for i := 0 to (nvIn-1) do begin
         if (vs[i].nUsed = 0) then continue;
         Vertices[vs[i].newIdx]:= oldVertices[i];
     end;
     oldVertices := nil;
     if length(vertexRGBA) = nvIn then begin
       setlength(oldRGBA, nvIn);
       oldRGBA := Copy(vertexRGBA, 0, MaxInt);
       setlength(vertexRGBA, nvOut);
       for i := 0 to (nvIn-1) do begin
           if (vs[i].nUsed = 0) then continue;
           vertexRGBA[vs[i].newIdx]:= oldRGBA[i];
       end;
       oldRGBA := nil;
     end;

     for i := 0 to (nf - 1) do begin
       faces[i].x := vs[faces[i].x].newIdx;
       faces[i].y := vs[faces[i].y].newIdx;
       faces[i].z := vs[faces[i].z].newIdx;
     end;
     vs := nil;
     CheckMesh;
end;

function TMesh.LoadSrf(const FileName: string): boolean;
//ALWAYS little-endian, n.b. despite docs the 'reserve' may not be zero: NeuroElf refers to this as "ExtendedNeighbors"
// http://support.brainvoyager.com/automation-aamp-development/23-file-formats/382-developer-guide-26-file-formats-overview.html
// http://support.brainvoyager.com/automation-aamp-development/23-file-formats/375-users-guide-23-the-format-of-srf-files.html
// https://support.brainvoyager.com/brainvoyager/automation-development/84-file-formats/344-users-guide-2-3-the-format-of-srf-files
type
  THdr = packed record
     vers: single;
     reserve, nVertices, nTriangles: int32;
     meshCenterX, meshCenterY, meshCenterZ: single;
  end;
label
   666;
var
   f: file;
   hdr : THdr;
   sz: int64;
   i, j: integer;
   nNeighbors,neighbor: int32;
   floats: array of single;
   ints: array of int32;
   rgbaf: array [0..1] of TPoint4f;
   rgbab: array [0..2] of  TRGBA;
begin
  result := false;
  if (FSize(FileName) < 64) then exit;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  sz := FileSize(f);
  if sz < 64 then goto 666;
  blockread(f, hdr, SizeOf(hdr) );
  {$IFNDEF ENDIAN_LITTLE} need to byte-swap SRF files {$ENDIF}
  if (specialsingle(hdr.vers)) or (hdr.reserve <> 0) or (hdr.vers < 0) or (hdr.vers > 32) or (hdr.nTriangles < 1) or (hdr.nVertices < 3) then begin //exit without error - perhaps this is a FreeSurfer SRF
     CloseFile(f);
     exit;
  end;
  //showmessage(format('%d %d', [hdr.nTriangles,hdr.nVertices]));
  //showmessage(format('%d %d',[(sizeof(hdr) + (int64(hdr.nVertices) * 3 * 4)+ (int64(hdr.nTriangles) * 3 * 4)), sz]));
  //szX = expected size: header + vertices*4(float32)*3
  //  header size
  //  6*nV*4 = each vertex has 6 coordinates (x,y,z) 3 normals (x,y,z), each as 32-bit float
  //  8*4 = concave/convex colors 8 * 32 bit float
  //  nV * 4 = mesh color
  //  nV * 4 + ? Nearest neighbors
  if (sizeof(hdr) + (int64(hdr.nVertices) * 6 * 4)+ (int64(hdr.nTriangles) * 3 * 4)) > sz then goto 666;
  setlength(vertices, hdr.nVertices);
  setlength(floats, hdr.nVertices);
  //showmessage(format('%g %g %g',[hdr.meshCenterX, hdr.meshCenterY, hdr.meshCenterZ]));
  blockread(f, floats[0],  int64(hdr.nVertices) * sizeof(single)); //coord X
  for i := 0 to high(floats) do
      vertices[i].Y := -(floats[i]-hdr.meshCenterX);
  blockread(f, floats[0],  int64(hdr.nVertices) * sizeof(single)); //coord Y
  for i := 0 to high(floats) do
      vertices[i].Z := -(floats[i]-hdr.meshCenterY);
  blockread(f, floats[0],  int64(hdr.nVertices) * sizeof(single)); //coord Z
  for i := 0 to high(floats) do
      vertices[i].X := -(floats[i]-hdr.meshCenterZ);
  blockread(f, floats[0],  int64(hdr.nVertices) * sizeof(single)); //normal X
  blockread(f, floats[0],  int64(hdr.nVertices) * sizeof(single)); //normal Y
  blockread(f, floats[0],  int64(hdr.nVertices) * sizeof(single)); //normal Z
  setlength(floats, 0);
  blockread(f, rgbaf[0],  sizeof(TPoint4f));
  blockread(f, rgbaf[1],  sizeof(TPoint4f));
  for i := 0 to 1 do begin
      rgbab[i].R := round(255 * rgbaf[i].X);
      rgbab[i].G := round(255 * rgbaf[i].Y);
      rgbab[i].B := round(255 * rgbaf[i].Z);
      rgbab[i].A := round(255 * rgbaf[i].W);
  end;
  rgbab[2].R := 128;
  rgbab[2].G := 128;
  rgbab[2].B := 128;
  rgbab[2].A := 0; //transparent
  setlength(ints, hdr.nVertices);
  blockread(f, ints[0],  int64(hdr.nVertices) * sizeof(int32)); //MeshColor, sequence of color indices, one for each vertex
  setlength(vertexRGBA, hdr.nVertices);
  for i := 0 to high(vertices) do begin //BrainSuite triangle winding opposite of convention
      if ints[i] = 0 then
         vertexRGBA[i] := rgbab[0]
      else if ints[i] = 1 then
           vertexRGBA[i] := rgbab[1]
      else if ints[i] >= 1056964608 then begin
           vertexRGBA[i].R := ints[i] and 255;
           vertexRGBA[i].G := (ints[i] shr 8) and 255;
           vertexRGBA[i].B := (ints[i] shr 16) and 255;
           vertexRGBA[i].A := 255; //Assume opaque
      end else
          vertexRGBA[i] := rgbab[2];
  end;
  //yikes: we have to read the neighbor group: no alternative since no offset to faces and file ends with a variable length string!
  for i := 1 to hdr.nVertices do begin
      blockread(f, nNeighbors,  sizeof(int32));
      if nNeighbors < 0 then goto 666;
      if nNeighbors < 1 then continue;
      for j := 1 to nNeighbors do
          blockread(f, neighbor,  sizeof(int32));
  end;
  if (sz - filepos(f)) < (int64(hdr.nTriangles) * 3 * 4) then goto 666;
  setlength(faces, hdr.nTriangles);
  blockread(f, faces[0],  int64(hdr.nTriangles) * 3 * 4);
  result := true;
  666:
  CloseFile(f);
  if not result then begin
       showmessage('Unable to decode BrainVoyager SRF file '+ FileName);
       faces := nil;
       vertexRGBA := nil;
       vertices := nil;
  end;
  CullUnusedVertices();
  UnifyVertices2(faces, vertices,vertexRGBA); //<- this could further enhance vertexRGBA
end; //LoadSrf()

procedure SwapPt3f(var p: TPoint3f);
begin
     SwapSingle(p.x);
     SwapSingle(p.y);
     SwapSingle(p.z);
end;

procedure SwapXZ(var p: TPoint3i);
var
   s: LongInt;
begin
  s := p.X;
  p.X := p.Z;
  p.Z := s;
end;

procedure SwapPt3i(var p: TPoint3i);
begin
     SwapLongInt(p.x);
     SwapLongInt(p.y);
     SwapLongInt(p.z);
end;

(*function TMesh.LoadMeshAscii(const FileName: string): boolean;
//https://www.rocq.inria.fr/gamma/gamma/ghs3d/file.mesh.pdf
// https://people.sc.fsu.edu/~jburkardt/examples/medit/medit.html
// see 7.2.1 of http://www.ann.jussieu.fr/~frey/publications/RT-0253.pdf
label
   666;
var
   f: Textfile;
   str: string;
   num_v, num_f, i: integer;
   f1,f2,f3,f4: single;
function ReadVal: integer;
//read value on current line or next
var
   strlst : TStringList;
begin
     strlst:=TStringList.Create;
     strlst.DelimitedText := str;
     if (strlst.count > 1) then
        result := strtointdef(strlst[1],0)
     else
         read(f,result);
     strlst.free;
end; //ReadVal
begin
     result := false;
     if (FSize(FileName) < 64) then exit;
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
     Reset(f);
     num_v := 0;

     while (not EOF(f)) and (num_v < 1) do begin
           readln(f,str);
           if (length(str) < 1) or (str[1] = '#') then continue;
           str := uppercase(str);
           if pos('MESHVERSIONFORMATTED', str) = 1 then
              ReadVal()//read(f, str)
           else if pos('DIMENSION', str) = 1 then
              ReadVal()//read(f, str)
           else if pos('VERTICES', str) = 1 then
              num_v := ReadVal();//read(f, num_v);
     end;
     if (num_v < 3) then goto 666;
     setlength(vertices, num_v);
     for i := 0 to (num_v - 1) do
         ReadlnSafe(f, vertices[i].X, vertices[i].Y,  vertices[i].Z, f4);
     i := num_v - 1;
     num_f := 0;
     while (not EOF(f)) and (num_f < 1) do begin
           readln(f,str);
           if (length(str) < 1) or (str[1] = '#') then continue;
           str := uppercase(str);
           if pos('TRIANGLES', str) = 1 then
              num_f := ReadVal();//read(f, num_v);
     end;
     if (num_f < 3) then goto 666;
     setlength(faces, num_f);
     for i := 0 to (num_f -1) do begin
         ReadlnSafe(f, f1, f2, f3, f4);
         faces[i].X := round(f1);
         faces[i].Y := round(f2);
         faces[i].Z := round(f3);
     end;
     //i :=  num_f -1; showmessage(format('%d %d %d',[faces[i].X, faces[i].Y, faces[i].Z]));
     result := true;
     666:
     CloseFile(f);
end;  *)

function ReadNum(var f: TextFile): string; //read next ASCII number in ascii file
var
   ch : Char;
begin
     result := '';
     while (not  EOF(f)) do begin
           Read(f,ch);
           if ch in ['-','.','E','e','0'..'9'] then
              result := result + ch
           else if length(result) > 0 then
              exit;
     end;
end; //ReadNum()

function TMesh.LoadMeshAscii(const FileName: string): boolean;
label 666;
var
   s: string;
   f: TextFile;
   i, num_v, num_f, num_n: integer;
begin
  result := false;
  if (FSize(FileName) < 64) then exit;
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  Reset(f);
  Readln(f,s);  //ascii
  if pos('ASCII', UpperCase(s)) < 1 then begin
    showmessage('Not an ASCII BrainVisa file');
    goto 666;
  end;
  Readln(f,s);  //VOID  textureType
  i :=strtointdef(ReadNum(f),0); //3
  if i <> 3 then begin
     showmessage('Only able to read triangulated BrainVisa files');
     goto 666;
  end;
  Readln(f,s);  //1 numberOfTimeSteps
  Readln(f,s);  //0 timestep
  //next vertices 30176 (80.35473,...
  num_v := strtointdef(ReadNum(f),0); //number of vertices
  if num_v < 3 then goto 666;
  setlength(vertices, num_v);
  for i := 0 to (num_v -1) do begin
      vertices[i].X := strtofloatdef(ReadNum(f),0);
      vertices[i].Y := strtofloatdef(ReadNum(f),0);
      vertices[i].Z := strtofloatdef(ReadNum(f),0);
  end;
  //next normals 30176  (0.10198036,-0.089818045,
  num_n := strtointdef(ReadNum(f),0); //number of vertices
  if num_n > 0 then begin
     for i := 0 to (num_n -1) do begin
         ReadNum(f);
         ReadNum(f);
         ReadNum(f);
     end;
  end; //num_n
  ReadNum(f); //u32
  num_f := strtointdef(ReadNum(f),0); //number of vertices
  if num_f < 1 then goto 666;
  setlength(faces, num_f);
  for i := 0 to (num_f -1) do begin
      faces[i].X := strtointdef(ReadNum(f),0);
      faces[i].Y := strtointdef(ReadNum(f),0);
      faces[i].Z := strtointdef(ReadNum(f),0);
  end;
  for i := 0 to high(vertices) do
      vectorNegate(vertices[i]);
  for i := 0 to high(faces) do
      SwapXZ(faces[i]);
  result := true;
  666:
  closefile(f);
end;

function TMesh.LoadTri(const FileName: string): boolean;
//see BrainStorm's in_tess_tri.m
//     ASCII file with four blocks:
// - VHeader  : one line ("- nbVertices")
// - Vertices : nbVertices lines ("x y z x2 y2 z2")
//     - FHeader  : one line ("- nbFaces nbFaces nbFaces")
//     - Faces    : nbFaces lines ("vertex1 vertex2 vertex3")
label 666;
var
   strlst: TStringList;
   s: string;
   f: TextFile;
   i, num_v, num_f: integer;
begin
  result := false;
  if (FSize(FileName) < 64) then exit;
  strlst:=TStringList.Create;
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  Reset(f);
  Readln(f,s);  //ascii
  if pos('-', UpperCase(s)) <> 1 then begin
    showmessage('Not an ASCII TRI BrainVisa file');
    goto 666;
  end;
  s := copy(s, 2, length(s)-1);
  num_v := strtointdef(s,0);
  if num_v < 3 then goto 666;
  setlength(vertices, num_v);
  for i := 0 to (num_v -1) do begin
    Readln(f,s);  //ascii
    strlst.DelimitedText := s;
    vertices[i].X := strtofloatdef(strlst[0],0);
    vertices[i].Y := strtofloatdef(strlst[1],0);
    vertices[i].Z := strtofloatdef(strlst[2],0);
  end;
  Readln(f,s);
  strlst.DelimitedText := s;
  if (strlst.Count < 4) or (strlst[0] <> '-') then begin
    showmessage('Fatal error reading TRI');
    goto 666;
  end;
  num_f := strtointdef(strlst[1],0);
  if num_f < 1 then goto 666;
  setlength(faces, num_f);
  for i := 0 to (num_f -1) do begin
      Readln(f,s);  //ascii
      strlst.DelimitedText := s;
      faces[i].X := strtointdef(strlst[2],0);
      faces[i].Y := strtointdef(strlst[1],0);
      faces[i].Z := strtointdef(strlst[0],0);
  end;
  result := true;
  666:
  closefile(f);
  strlst.free;
end; //LoadTri()

function TMesh.LoadMesh(const FileName: string): boolean;
// http://brainvisa.info/aimsdata-4.5/user_doc/formats.html
type
  THdr = packed record
     sig: array [1..5] of char; //"binar" or "ascii"
     endian: uint32; //DCBA or ABCD = 1094861636 or 1145258561
     texLen: uint32;
     texTxt: array[1..4] of char; //VOID
     vertex_per_face,
     mesh_time,
     mesh_step,
     vertex_number: uint32;
  end;
const
  kSwapEnd = 1094861636;
  kNativeEnd = 1145258561;
label
   666;
var
   f: file;
   hdr : THdr;
   i: integer;
   normals: array of TPoint3f;
   faces_number, sz: uint32;
   swapEnd: boolean;
begin
  result := false;
  if (FSize(FileName) < 64) then exit;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  if FileSize(f) < 64 then goto 666;
  blockread(f, hdr, SizeOf(hdr) );
  if (uppercase(hdr.sig) = 'ASCII') then begin
     CloseFile(f);
     result := LoadMeshAscii(FileName);
     //showmessage('This is a ASCII format mesh: only able to read binary meshes '+filename);
     exit;
  end;
  if (uppercase(hdr.sig) <> 'BINAR') or (uppercase(hdr.texTxt) <> 'VOID') or (hdr.texLen <> 4) or ( (hdr.endian <> kNativeEnd) and (hdr.endian <> kSwapEnd))  then begin
     showmessage(format('sig=%s tex=%s endian=%d',[uppercase(hdr.sig), uppercase(hdr.texTxt), hdr.endian]));
     goto 666;
  end;
  swapEnd := hdr.endian = kNativeEnd;
  if swapEnd then begin
     SwapLongWord(hdr.vertex_per_face);
     SwapLongWord(hdr.vertex_number);
  end;
  if (hdr.vertex_per_face <> 3) then begin
     showmessage('Only able to read triangular BrainVisa meshes');
     goto 666;
  end;
  setlength(vertices, hdr.vertex_number);
  blockread(f, vertices[0],  int64(hdr.vertex_number) * 3 * 4);
  blockread(f, sz, sizeof(uint32));
  if swapEnd then
    SwapLongWord(sz);
  if (sz > 0) then begin
     setlength(normals, hdr.vertex_number);
     blockread(f, normals[0],  int64(hdr.vertex_number) * 3 * 4);
     setlength(normals, 0);
  end;
  blockread(f, sz, sizeof(uint32));
  blockread(f, faces_number, sizeof(uint32));
  if swapEnd then begin
    SwapLongWord(sz);
    SwapLongWord(faces_number);
     for i := 0 to high(vertices) do
         swapPt3f(vertices[i]);
  end;
  setlength(faces, faces_number);
  blockread(f, faces[0],  int64(faces_number) * 3 * 4);
  if swapEnd then begin
     for i := 0 to high(faces) do
         swapPt3i(faces[i]);
  end;
  for i := 0 to high(vertices) do
      vectorNegate(vertices[i]);
  for i := 0 to high(faces) do
      SwapXZ(faces[i]);
  result := true;
  666:
  CloseFile(f);
  if not result then
       showmessage('Unable to decode BrainVisa file '+ FileName);
end; //LoadMesh()

function TMesh.LoadDfs(const FileName: string): boolean;
// http://brainsuite.org/formats/dfs/
// http://brainsuite.org/processing/additional-tools/
// see also in_tess_dfs.m
// Note: implementations disagree if magic is "DUFFSURF" or "DFS_BE v2.0\0 on big-endian machines, DFS_LEv1.0\0"
//HEADER:
// char magic[8];		// Magic number (DUFFSURF on little-endian machines or byte-swapped equivalent on other architectures)
// char version[4];		// A number in the format 1.1.1.1
// int32 hdrsize;		// Size of complete header (i.e., offset of first data element)
// int32 mdoffset;		// Start of metadata.
// int32 pdoffset;		// Start of patient data header.
// int32 nTriangles;		// Number of triangles
// int32 nVertices;		// Number of vertices
// int32 nStrips;		// Number of triangle strips
// int32 stripSize;		// size of strip data
// int32 normals;		// 4	Int32	<normals>	Start of vertex normal data (0 if not in file)
// int32 uvStart;		// Start of surface parameterization data (0 if not in file)
// int32 vcoffset;		// vertex color:  per vertex color data in (r,g,b) format in 32-bit floating point ([0-1])
// uint8 precision;		// Vertex Precision -- usually float32 or float64
// uint8 pad[3];			// padding
// float64 orientation[4][4]; //4x4 matrix, affine transformation to world coordinates*)
type
  THdr = packed record
     magic: array [1 .. 8] of char; // Magic number (DFS_BE v2.0\0 on big-endian machines, DFS_LE v1.0\0  _OR_ DUFFSURF on little-endian machines or byte-swapped equivalent on other architectures)
     version: array [1 .. 4] of char; // A number in the format 1.1.1.1
     hdrsize, mdoffset,pdoffset, nTriangles, nVertices, nStrips, stripSize, normals, uvStart, vcoffset : int32; // -1 = no bone
     precision: byte;
     pad: array [1..3] of char;
     orientation: array[1..16] of double; //4x4 matrix, affine transformation to world coordinates
  end;
label
   666;
var
   f: file;
   hdr : THdr;
   sz: int64;
   i: integer;
   swp: longint;
   err: string;
begin
  result := false;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  sz := FileSize(f);
  err := 'file too small';
  if sz < 64 then goto 666;
  blockread(f, hdr, SizeOf(hdr) );
  {$IFNDEF ENDIAN_LITTLE} need to byte-swap DFS files {$ENDIF}
  err := 'unable to read big-endian data, save image with BrainStorm''s out_tess';
  if (uppercase(hdr.magic) =  'DFS_BE V') then  //in theory could be big-endian, though BrainSuite only currently available for little-endian. out_tess ALWAYS saves data as little-endian
     goto 666;
  err := 'incorrect magic signature';
  if (uppercase(hdr.magic) <> 'DUFFSURF') and (uppercase(hdr.magic) <>  'DFS_LE V') then goto 666; //oops
  err := 'file too small';
  if (hdr.nTriangles < 1) or (hdr.nVertices < 3) then goto 666;
  setlength(faces, hdr.nTriangles);
  setlength(vertices, hdr.nVertices);
  if (hdr.vcoffset > 0) then begin //per vertex color data in (r,g,b) format in 32-bit floating point ([0-1])
     if (hdr.vcoffset + (int64(hdr.nVertices) * 3 * 4)) > sz then goto 666;
     Seek(f, hdr.vcoffset);
     blockread(f, vertices[0], int64(hdr.nVertices) * 3 * 4);
     setlength(vertexRGBA, hdr.nVertices);
     for i := 0 to high(vertices) do begin //BrainSuite triangle winding opposite of convention
         vertexRGBA[i].R := round(vertices[i].X * 255);
         vertexRGBA[i].G := round(vertices[i].Y * 255);
         vertexRGBA[i].B := round(vertices[i].Z * 255);
         vertexRGBA[i].A := 255; //Assume opaque
     end;
  end;
  if (hdr.hdrsize + (int64(hdr.nVertices) * 3 * 4)+ (int64(hdr.nTriangles) * 3 * 4)) > sz then goto 666;
  Seek(f, hdr.hdrsize);
  blockread(f, faces[0],  int64(hdr.nTriangles) * 3 * 4);
  blockread(f, vertices[0], int64(hdr.nVertices) * 3 * 4);
  for i := 0 to (length(faces)-1) do begin //BrainSuite triangle winding opposite of convention
      swp := faces[i].X;
      faces[i].X := faces[i].Z;
      faces[i].Z := swp;
  end;
  result := true;
  666:
  CloseFile(f);
  if not result then
       showmessage('Unable to decode BrainSuite DFS file ('+err+') '+ FileName);
end; // LoadDfs()

function TMesh.LoadLwo(const FileName: string): boolean;
//LWO2 http://static.lightwave3d.com/sdk/11-6/html/filefmts/lwo2ex/lwo2ex.html
// this is NOT LWOB described by http://www.martinreddy.net/gfx/3d/LWOB.txt https://botb.club/~edlinfan/textfiles/faqsys/formats/lwo.txt
label
   666;
type
  TChunk = packed record
     ID: array [1 .. 4] of char; //header
     Size: Longint; // -1 = no bone
  end;
var
   f: file;
   temp: single;
   nV,nF,nVnew, nFnew, i, polyNodes, chunkStart, sz, maxFCount, bytesLeft: integer;
   chunk: TChunk;
   id : string;
   idx: TPoint3i;
function ReadU2: word;
begin
     blockread(f, result, sizeof(word));
     {$IFDEF ENDIAN_LITTLE} result := Swap(result); {$ENDIF}
     bytesLeft := bytesLeft - 2;
end; //nested ReadU2()
function ReadVX:longint;
begin
     result := ReadU2;
     if (result >= $0000ff00) then
        result := ((result and 255) shl 16) or ReadU2;
end; //nested ReadVX()
begin
  result := false;
  chunk.ID := 'ERRO';
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  sz := FileSize(f);
  if sz < 64 then goto 666;
  blockread(f, chunk, SizeOf(chunk) );
  if (uppercase(chunk.ID) <> 'FORM') then goto 666; //Not a valid LWO file (missing FORM chunk)
  blockread(f, chunk.ID, SizeOf(chunk.ID) );
  if (uppercase(chunk.ID) <> 'LWO2') then goto 666; //Not a valid LWO file (not LWO2 format)
  nV := 0;
  nVnew := 0;
  nF := 0;
  while (not EOF(f)) do begin
        blockread(f, chunk, SizeOf(chunk) );
        {$IFDEF ENDIAN_LITTLE} SwapLongInt(chunk.Size); {$ENDIF}
        id := uppercase(chunk.ID);
        chunkStart := filepos(f);
        if (id = 'PNTS') then begin
           nV := nV + nVnew;
           nVnew := chunk.Size div 12;
           if (nVnew * 12) <> chunk.Size then goto 666;
           setlength(vertices, nVnew + nV);
           blockread(f, vertices[nV], chunk.Size);
           {$IFDEF ENDIAN_LITTLE}
           for i := 0 to (nVnew - 1) do begin
               SwapSingle(vertices[i + nV].X);
               SwapSingle(vertices[i + nV].Y);
               SwapSingle(vertices[i + nV].Z);
           end;
           {$ENDIF}
           for i := 0 to (nVnew - 1) do begin
               temp := vertices[i + nV].Z;
               vertices[i + nV].Z := vertices[i + nV].Y;
               vertices[i + nV].Y := temp;
           end;
        end;
        if (id = 'POLS') then begin
           blockread(f, chunk.ID, SizeOf(chunk.ID) );
           id := uppercase(chunk.ID);
           if (id = 'FACE') or (id = 'PTCH') then begin
              maxFCount := (chunk.Size - 10) div 2; // Perpare for worst case triangle count (a single poly with only 16-bit indices)
              setlength(faces, maxFCount + nF);
              nFnew := 0;
              bytesLeft := chunk.Size - 4;
              while (bytesLeft > 0) do begin
                polyNodes := ReadU2;
                if (polyNodes >= 3) then begin
                   idx.X := ReadVX;
                   idx.Y := ReadVX;
                   idx.Z := ReadVX;
                   polyNodes := polyNodes - 3;
                   while((polyNodes >= 0) and (bytesLeft >= 0)) do begin
                     faces[nFnew + nF].X := idx.X + nV;
                     faces[nFnew + nF].Y := idx.Y + nV;
                     faces[nFnew + nF].Z := idx.Z + nV;
                     nFnew := nFnew + 1;
                     if(polyNodes > 0) then begin
                       idx.Y := idx.Z;
                       idx.Z := ReadVX;
                     end;
                     polyNodes := polyNodes - 1;
                   end;
                end else
                    for i := 1 to polyNodes do
                        ReadVX;
              end; //while bytes left
              nF := nF + nFnew;
              setlength(faces, nF); //shrink array if nFnew <> maxFCount
           end;
        end;
        seek(f,chunkStart+chunk.Size);
  end;
  result := true;
  666 :
  CloseFile(f);
  if not result then begin
     if (uppercase(chunk.ID) <> 'LWOB') then
        showmessage('Unable to decode LightWave LWO2 file '+ FileName)
     else
         showmessage('Expected LWO2 format, not LWOB format '+ FileName);
  end;
  UnifyVertices(faces, vertices);
end; // LoadLwo()

function TMesh.LoadMs3d(const FileName: string): boolean;
//http://sappersblog.blogspot.com/2014/08/milkshape-3d-185-ms3d-file-format.html
//http://paulbourke.net/dataformats/ms3d/ms3dspec.h
label
   666;
type
  Tms3d_vertex = packed record
     flags: byte; // SELECTED | SELECTED2 | HIDDEN
     v: TPoint3f;
     boneId,referenceCount: byte; // -1 = no bone
  end;
  Tms3d_triangle = packed record
    flags: word; // SELECTED | SELECTED2 | HIDDEN
    indexX, indexY, indexZ: word;
    vertexNormals: array [1..3] of TPoint3f;
    s,t: TPoint3f;
    smoothingGroup,groupIndex: byte;
  end;
var
   txt10: array [1 .. 10] of char; //header
   f: file;
   sz, i: integer;
   version: LongInt;
   nNumVertices, nNumTriangles: word;
   vert: Tms3d_vertex;
   face: Tms3d_triangle;
begin
  result := false;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  sz := FileSize(f);
  if sz < 64 then goto 666;
  blockread(f, txt10, SizeOf(txt10) );
  if pos('MS3D000000', UpperCase(txt10)) <> 1 then goto 666; //signature
  blockread(f, version, SizeOf(version) );
  blockread(f, nNumVertices, SizeOf(nNumVertices) );
  setlength(vertices, nNumVertices);
  for i := 0 to (nNumVertices - 1) do begin
    blockread(f, vert, SizeOf(vert) );
    vertices[i] := vert.v;
  end;
  blockread(f, nNumTriangles, SizeOf(nNumTriangles));
  setlength(faces, nNumTriangles);
  for i := 0 to (nNumTriangles - 1) do begin
    blockread(f, face, SizeOf(face) );
    faces[i].X := face.indexX;
    faces[i].Y := face.indexY;
    faces[i].Z := face.indexZ;
  end;
  result := true;
  666 :
  CloseFile(f);
  if not result then showmessage('Unable to decode MilkShape file '+ FileName);
  UnifyVertices(faces, vertices);
end; // LoadMs3d()

function TMesh.Load3ds(const FileName: string): boolean;
//http://www.spacesimulator.net/tutorials/3ds_loader_tutorial.html
//https://en.wikipedia.org/wiki/.3ds
//http://help-site.com/local/3DS.TXT
type
 TFaceShort = packed record
   X,Y,Z,FaceFlag: word;
 end;
label
   666;
var
   f: file;
   sz, nV,nF,nVnew, i: integer;
   ch: char;
   verts : array of TPoint3f;
   facesShort : array of TFaceShort;
   chunk_id, ushort: word; //uint16
   chunk_length : LongWord; //uint32
   nam: string;
begin
  result := false;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  sz := FileSize(f);
  if sz < 64 then goto 666;
  //files should start with $4D4D chunk
  blockread(f, chunk_id, 2);
  blockread(f, chunk_length, 4);
  if  chunk_id <> $4D4D then goto 666;
  //next: read data
  nVnew := 0;
  nV := 0;
  nF := 0;
  while (not EOF(f)) do begin
      blockread(f, chunk_id, 2);
      blockread(f, chunk_length, 4);
      case chunk_id of
           $3D3D:; //read 3D EDITOR CHUNK
           $4000: begin //read OBJECT BLOCK
             nam := '';
             blockread(f, ch, 1);
             while ch <> chr(0) do begin
               nam := nam + ch;
               blockread(f, ch, 1);
             end;
           end;
           $4100:; //read TRIANGULAR MESH
           $4110 : begin //read VERTICES LIST
                blockread(f, ushort, 2);
                setlength(verts,ushort);
                blockread(f, verts[0], ushort * sizeof(TPoint3f) ); //for each vertex, 3 (x,y,z) 4-byte floats
                setlength(vertices, nV+ushort);
                for i := 0 to (ushort -1) do begin
                    vertices[i+nV].X :=  verts[i].X;
                    vertices[i+nV].Y :=  verts[i].Y;
                    vertices[i+nV].Z :=  verts[i].Z;
                 end;
                verts := nil; //free
                nVnew := ushort;
           end;
           $4120 : begin //read FACES DESCRIPTION
             blockread(f, ushort, 2);
             setlength(facesShort, ushort);
             blockread(f, facesShort[0], ushort * sizeof(TFaceShort)); //for each vertex, 3 (x,y,z,faceflag) 2-byte unsigned short ints
             setlength(faces, ushort+nF);
             for i := 0 to (ushort -1) do begin
                 faces[i+nF].X :=  facesShort[i].X+nV;
                 faces[i+nF].Y :=  facesShort[i].Y+nV;
                 faces[i+nF].Z :=  facesShort[i].Z+nV;
             end;
             nF := nF + ushort;
             nV := nV + nVnew;
             facesShort := nil; //free
             result := true;
           end
           else
             seek(f,filepos(f)+chunk_length-6);
      end;
  end;
 666:
  CloseFile(f);
  if not result then showmessage('Unable to decode 3DS file '+ FileName);
  UnifyVertices(faces, vertices);
end; // Load3ds()

function TMesh.LoadSurf(const FileName: string): boolean;
// Mango SURF format http://ric.uthscsa.edu/mango/mango_surface_spec.html
type
  THdr = packed record
     magic: array [1 .. 5] of char; // Magic  "mango"
     endian, versMajor, versMinor: char; //"l" or "b" for little or big endian
     previewSz, nSurf : uint32;
     mat: array[1..16] of double;
     imgDim: array [1..3] of uint32;
     voxDim, origin: array [1..3] of single;
     thresh: single
  end;

  TSurf = packed record
     nam: array [1 .. 64] of char;
     r,g,b: single;
     nParts, nPoint : uint32;
  end;
label
   666;
var
   f: file;
   hdr : THdr;
   surf: TSurf;
   sz: int64;
   num_v,  i: integer;
   err: string;
   b: array of byte;
   num_parts,num_norm, num_f: uint32;
   //swp: int32;
begin
  result := false;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  sz := FileSize(f);
  err := 'file too small';
  if sz < 64 then goto 666;
  blockread(f, hdr, SizeOf(hdr) );
  {$IFNDEF ENDIAN_LITTLE} need to byte-swap SURF files {$ENDIF}
  err := 'unable to read big-endian data, save image with BrainStorm''s out_tess';
  if (uppercase(hdr.endian) =  'B') then
     goto 666;
  err := 'incorrect magic signature';
  if (uppercase(hdr.magic) <> 'MANGO') then goto 666; //oops
  err := 'no surfaces in file';
  if (hdr.nSurf < 1) then goto 666;
  if hdr.previewSz > 0 then begin //skip preview
     setlength(b, hdr.previewSz);
     blockread(f, b[0], hdr.previewSz);
     setlength(b,0);
  end;
  blockread(f, surf, SizeOf(surf) );
  err := inttostr(surf.nPoint)+' points not divisible by 3';
  if (surf.nPoint mod 3) <> 0 then goto 666;
  num_v := surf.nPoint div 3;
  setlength(vertices, num_v);
  blockread(f, vertices[0], num_v * sizeof(TPoint3f));
  blockread(f, num_parts, 4 );
  blockread(f, num_norm, 4 );
  if num_norm > 0 then begin
     setlength(b, num_norm * 4);
     blockread(f, b[0], num_norm * 4);
     setlength(b,0);
  end;
  blockread(f, num_parts, 4 );
  blockread(f, num_f, 4 );
  err := inttostr(num_f)+' faces not divisible by 3';
  if (num_f mod 3) <> 0 then goto 666;
  num_f := num_f div 3;
  setlength(faces, num_f);
  blockread(f, faces[0], num_f * sizeof(TPoint3i));
  result := true;
  //showmessage(format('%g %g %g %g',[hdr.mat[1], hdr.mat[ 2], hdr.mat[ 3], hdr.mat[ 4]] ));
  //showmessage(format('%g %g %g %g',[hdr.mat[5], hdr.mat[ 6], hdr.mat[ 7], hdr.mat[ 8]] ));
  //showmessage(format('%g %g %g %g',[hdr.mat[9], hdr.mat[10], hdr.mat[11], hdr.mat[12]] ));
  for i := 0 to (num_v - 1) do begin
    vertices[i].X := -vertices[i].X;
    vertices[i].Y := -vertices[i].Y;
  end;
  (*for i := 0 to (length(faces)-1) do begin //Mango triangle winding opposite of convention
      swp := faces[i].X;
      faces[i].X := faces[i].Z;
      faces[i].Z := swp;
  end;*)
  666:
  CloseFile(f);
  if not result then
       showmessage('Unable to decode Mango SURF file ('+err+') '+ FileName);
end; // LoadSurf()

procedure TMesh.LoadStlAscii(const FileName: string);
//Read ASCII STL
// https://en.wikipedia.org/wiki/STL_(file_format)
label
   666;
const
  kBlockSize = 8192;
var
   strlst: TStringList;
   s: string;
   f: TextFile;
   i, num_v, num_f: integer;
begin
  if (FSize(FileName) < 64) then exit;
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  Reset(f);
  Readln(f,s);
  if pos('SOLID', UpperCase(s)) < 1 then begin
    showmessage('Not an ASCII STL file');
    closefile(f);
    exit;
  end;
  strlst:=TStringList.Create;
  num_v := 0;
  while not EOF(f) do begin
        Readln(f,s);
        s := trim(s);
        if pos('VERTEX', UpperCase(s)) <> 1 then continue; //e.g. "vertex   7.708280e-01 -2.937950e+00  7.751060e-01"
        strlst.DelimitedText := s;
        if strlst.Count < 4 then continue;
        if (num_v+1) > length(vertices) then
           setlength(vertices, length(vertices)+kBlockSize);
        vertices[num_v].X := StrToFloatDef(strlst[1],0);
        vertices[num_v].Y := StrToFloatDef(strlst[2],0);
        vertices[num_v].Z := StrToFloatDef(strlst[3],0);
        num_v := num_v + 1;
  end;
  setlength(vertices, num_v);
  if num_v < 3 then goto 666;
  setlength(faces, num_v div 3);
  i := 0;
  for num_f := 0 to ((num_v div 3) -1) do begin
      faces[num_f].X := i;
      i := i + 1;
      faces[num_f].Y := i;
      i := i + 1;
      faces[num_f].Z := i;
      i := i + 1;
  end;
666:
   closefile(f);
   strlst.free;
   CheckMesh; //Since STL requires vertex unification, make sure vertices and faces are valid
   UnifyVertices(faces, vertices);
end; // LoadStlAscii()

procedure TMesh.LoadStl(const FileName: string);
//Read STL mesh
// https://en.wikipedia.org/wiki/STL_(file_format)
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bstl/readStl.m
// similar to Doron Harlev http://www.mathworks.com/matlabcentral/fileexchange/6678-stlread
{$DEFINE FASTSTL}

{$IFDEF FASTSTL}
type
    TSTL = packed record
       norm, v0,v1,v2: TPoint3f;
       clr: uint16;
    end;
{$ENDIF}
var
   f: file;
   txt80: array [1 .. 80] of char; //header
   num_f : LongWord; //uint32
   i, v, sz, min_sz: integer;
   {$IFDEF TIMER}startTime : TDateTime;{$ENDIF}
   {$IFDEF FASTSTL}
   vs : array of TSTL;
   {$ELSE}
   triValues: array [1..9] of single;//float32
   colorBytes: word; //uint16
   {$ENDIF}
begin
     if (FSize(FileName) < 64) then exit;
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
        //Showmessage(format('Error: only able to read binary STL files (not ASCII text). Please convert your image: %s', [FileName]));
        CloseFile(f);
        LoadStlAscii(FileName);
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
    {$IFDEF FASTSTL}
    setlength(vs, num_f);
    blockread(f, vs[0], num_f * sizeof(TSTL));
    for i := 0 to (num_f-1) do begin
        vertices[v+0] := vs[i].V0;
        vertices[v+1] := vs[i].V1;
        vertices[v+2] := vs[i].V2;
        v := v + 3;
    end;
    vs := nil;
    {$ELSE}
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
    {$ENDIF}
    setlength(faces, num_f);
    v := 0;
    for i := 0 to (num_f-1) do begin
        faces[i] := pti(v,v+1,v+2);
        v := v + 3;
    end;
    CloseFile(f);
    CheckMesh; //Since STL requires vertex unification, make sure vertices and faces are valid
    {$IFDEF TIMER}startTime := Now;{$ENDIF}
    UnifyVertices(faces, vertices);
    {$IFDEF TIMER}GLForm1.ShaderBox.Caption :=  inttostr(MilliSecondsBetween(Now,StartTime)); {$ENDIF}

end; // LoadStl()

function TMesh.LoadIdtf(const FileName: string): boolean;
label
   666;
var
  f: TextFile;
  str: string;
  strlst: TStringList;
  num_v, num_f, i, n, nMax: integer;
begin
    result := false;
    if (FSize(FileName) < 64) then exit;
    strlst:=TStringList.Create;
    FileMode := fmOpenRead;
    AssignFile(f, FileName);
    Reset(f);
    Readln(f,str);
    if pos('"IDTF"', UpperCase(str)) = 0 then begin
      showmessage('Not a IDTF file');
      goto 666;
    end;
    num_f := 0;
    num_v := 0;
    faces := nil;
    vertices := nil;
    while not EOF(f) do begin
          ReadLn(f, str);
          if pos('FACE_COUNT', UpperCase(str)) <> 0 then begin
              strlst.DelimitedText := str;
              num_f := StrToIntDef(strlst[1],0);
              //showmessage('f'+inttostr(num_f));
          end;
          if pos('MODEL_POSITION_COUNT', UpperCase(str)) <> 0 then begin
              strlst.DelimitedText := str;
              num_v := StrToIntDef(strlst[1],0);
              //showmessage('v'+inttostr(num_v));
          end;
          if (num_f >0) and (pos('MESH_FACE_POSITION_LIST', UpperCase(str)) <> 0) then begin
            setlength(faces, num_f);
            for i := 0 to (num_f-1) do
                Readln(f,faces[i].X, faces[i].Y, faces[i].Z);
          end;
          if (num_v > 2) and (pos('MODEL_POSITION_LIST', UpperCase(str)) <> 0) then begin
            setlength(vertices, num_v);
            for i := 0 to (num_v-1) do
                Readln(f,vertices[i].X, vertices[i].Y, vertices[i].Z);
          end;
    end; //while not EOF
    result := (length(vertices) > 0) and (length(faces) > 0);
  666:
    if not result then begin
       setlength(faces,0);
       setlength(vertices,0);
    end;
    strlst.free;
    CloseFile(f);
end; //LoaIdtfTxt()

function ReadLnVtk(var f: TextFile; out str: string): boolean;
begin
     str := '';
     while (str = '') or (str = ' ') do begin
           if EOF(f) then exit(false);
           Readln(f,str);
     end;
     result := true;
end;

function TMesh.LoadVtkTxt(const FileName: string): boolean;
label
   666;
const
  kBlockSz = 4096;
var
  f: TextFile;
  str: string;
  strlst: TStringList;
  num_v, num_f, i, n, sz, nx, num_fx: integer;
  face: TPoint3i;
begin
    result := false;
    if (FSize(FileName) < 64) then exit;
    strlst:=TStringList.Create;
    FileMode := fmOpenRead;
    AssignFile(f, FileName);
    Reset(f);
    Readln(f,str);
    if pos('VTK', UpperCase(str)) <> 3 then begin
      showmessage('Not a VTK file');
      goto 666;
    end;
    ReadLn(f, str); //comment: 'Comment: created with MRIcroS'
    ReadLn(f, str); //kind: 'BINARY' or 'ASCII'
    if pos('BINARY', UpperCase(str)) <> 0 then begin
       showmessage('Use LoadVtk() to read binary files');
       goto 666;
    end else if pos('ASCII', UpperCase(str)) = 0 then begin
       showmessage('VTK data should be ASCII or BINARY, not "'+str+'"');
       goto 666;
    end;
    ReadLn(f, str); // kind, e.g. "DATASET POLYDATA" or "DATASET STRUCTURED_ POINTS"
    while (str='') and (not eof(f)) do ReadLn(f, str);
    if pos('POLYDATA', UpperCase(str)) = 0 then begin
       showmessage('Only able to read VTK images saved as POLYDATA, not '+ str);
       goto 666;
    end;
    i := 0;
    while (pos('POINTS', UpperCase(str)) <> 1 ) and (i < 65) and (not eof(f)) do begin
            ReadLn(f, str); // number of vertices, e.g. "POINTS 685462 float"
            i := i + 1;
    end;
    if pos('POINTS', UpperCase(str)) <> 1 then begin
       showmessage('Expected header to report "POINTS" not '+ str);
       goto 666;
    end;
    strlst.DelimitedText := str;
    num_v := StrToIntDef(strlst[1],0);
    if (num_v < 3) then begin
      showmessage('Expected at least 3 vertices '+ str);
      goto 666;
    end;
    setlength(vertices, num_v);
    for i := 0 to (num_v-1) do begin
        //see Honolulu.vtk where 6 vertices are saved on each line! https://vtk.org/vtk-textbook-examples-and-data/
           //Readln(f,vertices[i].X, vertices[i].Y, vertices[i].Z);
           Read(f,vertices[i].X, vertices[i].Y, vertices[i].Z);
    end;
    ReadLnVtk(f,str);
    if pos('POLYGONS', UpperCase(str)) <> 1 then begin
       showmessage('Expected header to report "POLYGONS" (hint: open with Slicer and save as conventional format) "'+ str+'"');
       goto 666;
    end;
    strlst.DelimitedText := str;
    num_f := StrToIntDef(strlst[1],0);
    sz := StrToIntDef(strlst[2],0);
    //showmessage(inttostr());
    if (num_f < 1) then begin
       showmessage('Expected at least 1 polygon not '+ str);
       goto 666;
    end;
    setlength(faces, num_f);
    if (sz = (num_f * 4)) then begin //special case: all triangles
      //unlike binary data, there is virtually no benefit in detecting triangular ASCII data
      for i := 0 to (num_f-1) do begin
          Readln(f,n,faces[i].X, faces[i].Y, faces[i].Z);
          if n <> 3 then goto 666; //not all triangles
      end;
    end else begin //if all triangles else some ngons
      num_fx := 0;
      for i := 0 to (num_f-1) do begin
          Read(f,n,face.X, face.Y);
          if n < 3 then goto 666; //minimum n-gon is a triangle
          nx := n - 2; //e.g. if "3", one triangle, if "4" then quad = 2 triangles
          if (num_fx + nx) > length(faces) then
             setlength(faces, num_fx+nx+kBlockSz);
          for n := 1 to nx do begin
              Read(f, face.Z);
              faces[num_fx] := face;
              //face.X := face.Y;
              face.Y := face.Z;
              num_fx := num_fx + 1;
          end;
          //num_fx := num_fx + nx; //e.g.
      end;
      setlength(faces, num_fx);
    end;
    result := true;
  666:
    if not result then begin
       setlength(faces,0);
       setlength(vertices,0);
    end;
    strlst.free;
    CloseFile(f);
end; //LoadVtkTxt()

function ReadLnBinVTK(var f: TFByte; var s: string; skipEmptyLines: boolean = true): boolean;
const
  kEOLN = $0A;
  kCR = $0D;
var
   bt : Byte;
begin
     s := '';
     if EOF(f) then exit(false);
     while (not  EOF(f)) do begin
           Read(f,bt);
           if (bt = kCR) then continue;
           if (bt = kEOLN) and ((not skipEmptyLines) or (s <> '' )) then exit(true);
           if (bt = kEOLN) then continue;
           s := s + Chr(bt);
     end;
     exit(true);
end;

{$DEFINE NEWVTK}
{$IFDEF NEWVTK}
procedure TMesh.LoadVtk(const FileName: string);
//Read VTK mesh
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bvtk/readVtk.m
// http://www.ifb.ethz.ch/education/statisticalphysics/file-formats.pdf
// ftp://ftp.tuwien.ac.at/visual/vtk/www/FileFormats.pdf
//  "The VTK data files described here are written in big endian form"
//n.b. ASCII reading is slow - strlst.DelimitedText much slower than readln!
label
   666;
const
  kBlockSz = 4096;
type
  TPoint4i = packed record
  N: longint;
  X: longint; //ensure 32-bit for simple GIfTI writing
  Y: longint;
  Z: longint;
end;

var
   f: TFByte;//TextFile;
   strlst: TStringList;
   str: string;
   strips: array of int32;
   c, i, j, k, num_v, num_f, num_f_allocated, num_strip, cnt: integer;
   //nV: LongInt;
   faces4: array of TPoint4i;
   ok: boolean;
   isBinary: boolean = true;
begin
  if (FSize(FileName) < 64) then exit;
  strlst:=TStringList.Create;
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  num_f := 0;
  Reset(f,1);
  ReadLnBinVTK(f, str); //signature: '# vtk DataFile'
  if pos('VTK', UpperCase(str)) <> 3 then begin
    showmessage('Not a VTK file');
    goto 666;
  end;
  ReadLnBinVTK(f, str); //comment: 'Comment: created with MRIcroS'
  ReadLnBinVTK(f, str); //kind: 'BINARY' or 'ASCII'
  if pos('BINARY', UpperCase(str)) <> 0 then
     isBinary := true
  else if pos('ASCII', UpperCase(str)) <> 0 then begin
     isBinary := false;
     //next lines optional but faster!
     closefile(f);
     strlst.free;
     LoadVtkTxt(FileName);
     exit;
  end else begin  // '# vtk DataFile'
     showmessage('VTK data should be ASCII or binary, not "'+str+'"');
     goto 666;
  end;
  ok := ReadLnBinVTK(f, str); // kind, e.g. "DATASET POLYDATA" or "DATASET STRUCTURED_ POINTS"
  if not ok then goto 666;
  while (str='') and (not eof(f)) do ReadLnBinVTK(f, str);
  if pos('POLYDATA', UpperCase(str)) = 0 then begin
    showmessage('Only able to read VTK images saved as POLYDATA, not '+ str);
    goto 666;
  end;
  ReadLnBin(f, str); // number of vertices, e.g. "POINTS 685462 float"
  if pos('POINTS', UpperCase(str)) <> 1 then begin
    showmessage('Expected header to report "POINTS" not '+ str);
    goto 666;
  end;
  num_v := 0;
  strlst.DelimitedText := str;
  num_v := StrToIntDef(strlst[1],0);
  if (num_v < 1) or (pos('FLOAT', UpperCase(strlst[2])) <> 1) then begin
    showmessage('Expected at least 1 point of type FLOAT, not '+ str);
    goto 666;
  end;
  setlength(vertices, num_v); //vertices = zeros(num_f, 9);
  if isBinary then begin
     blockread(f, vertices[0], 3 * 4 * num_v);
     {$IFDEF ENDIAN_LITTLE} // VTK is ALWAYS big endian!
     for i := 0 to (num_v -1) do begin
            SwapSingle(vertices[i].X);
            SwapSingle(vertices[i].Y);
            SwapSingle(vertices[i].Z);
     end;
     {$ENDIF}
  end else begin //if binary else ASCII
       for i := 0 to (num_v-1) do begin
         //n.b. ParaView and Mango pack multiple vertices per line, so we can not use ReadLnBin(f, str);
           vertices[i].X := StrToFloatDef(ReadNumBin(f),0);
           vertices[i].Y := StrToFloatDef(ReadNumBin(f),0);
           vertices[i].Z := StrToFloatDef(ReadNumBin(f),0);
       end;
  end;
  ReadLnBin(f, str); // number of vertices, e.g. "POLYGONS 1380 5520"
  while (str = '') and (not EOF(f)) do ReadLnBin(f, str);
  if pos('LINES', UpperCase(str)) > 0 then begin
    showmessage('This is a fiber file: rename with a ".fib" extension and use Tracks/Open to view: '+ str);
    goto 666;
  end;
  if pos('METADATA', UpperCase(str)) = 1 then begin //ParaView inserts a metadata block
    //http://www.vtk.org/doc/nightly/html/IOLegacyInformationFormat.html
    while (str <> '') and (not EOF(f)) do ReadLnBin(f, str); // The metadata block is terminated by an empty line
    ReadLnBin(f, str);
  end;
  if pos('TRIANGLE_STRIPS', UpperCase(str)) = 1 then begin
     strlst.DelimitedText := str;
     num_strip := StrToIntDef(strlst[1],0);
     cnt := StrToIntDef(strlst[2],0);
     num_f_allocated := cnt;
     (*if isBinary then begin
        showmessage(format('Unable to read VTK binary "TRIANGLE_STRIPS" %d %d', [num_strip, cnt]));
        goto 666;
     end;*)
     setlength(faces, num_f_allocated); //not sure how many triangles, lets guess
     num_f := 0;
     if isBinary then begin
       //Harvard Surgical Planning Laboratory MRB files are zip files with binary VTK Triangle_Strips
       c := cnt;
       setlength(strips, cnt); //vertices = zeros(num_f, 9);
       blockread(f, strips[0], 4 * cnt);
       {$IFDEF ENDIAN_LITTLE} // VTK is ALWAYS big endian!
       for i := 0 to (cnt -1) do
           SwapLongInt(strips[i]);
       {$ENDIF}
       k := 0;
       for i := 0 to (num_strip -1) do begin
           cnt := strips[k];
           k := k + 1;
           if ((cnt < 3) or (k > c)) then begin
              setlength(faces,0);
              num_f := 0;
              goto 666;
           end;
           if ((num_f+cnt) > num_f_allocated) then begin //need to allocate more faces
              num_f_allocated := num_f_allocated + cnt + kBlockSz;
              setlength(faces, num_f_allocated);
           end;
            faces[num_f].X := strips[k];
            faces[num_f].Y := strips[k+1];
            faces[num_f].Z := strips[k+2];
            k := k + 3;
            num_f := num_f + 1;
            if (cnt > 3) then begin
               for j := 4 to cnt do begin
                   //http://ogldev.atspace.co.uk/www/tutorial27/tutorial27.html
                   //  winding order is reversed on the odd triangles
                   if odd(j) then begin //ITK snap triangle winding reverses with each new point
                      faces[num_f].X := strips[k-2];
                      faces[num_f].Y := strips[k-1];
                   end else begin
                     faces[num_f].X := strips[k-1];
                     faces[num_f].Y := strips[k-2];
                   end;
                 faces[num_f].Z := strips[k];
                 k := k + 1;
                 num_f := num_f + 1;
              end; //for j: each additional strip point
           end; //cnt > 3
       end; //for i: each strip
     end else begin
       for i := 0 to (num_strip -1) do begin
           ReadLnBinVTK(f, str);
           strlst.DelimitedText := str;
           cnt := StrToIntDef(strlst[0],0);
           if (cnt < 3) then begin
              setlength(faces,0);
              num_f := 0;
              goto 666;
           end;
           if ((num_f+cnt) > num_f_allocated) then begin //need to allocate more faces
              num_f_allocated := num_f_allocated + cnt + kBlockSz;
              setlength(faces, num_f_allocated);
           end;
           faces[num_f].Y := StrToIntDef(strlst[1],0);
           faces[num_f].X := StrToIntDef(strlst[2],0);
           faces[num_f].Z := StrToIntDef(strlst[3],0);
           num_f := num_f + 1;
           if (cnt > 3) then begin
              for j := 4 to cnt do begin
                  //http://ogldev.atspace.co.uk/www/tutorial27/tutorial27.html
                  //  winding order is reversed on the odd triangles
                  if odd(j) then begin //ITK snap triangle winding reverses with each new point
                     faces[num_f].X := StrToIntDef(strlst[j-1],0);
                     faces[num_f].Y := StrToIntDef(strlst[j-2],0);
                  end else begin
                    faces[num_f].X := StrToIntDef(strlst[j-2],0);
                    faces[num_f].Y := StrToIntDef(strlst[j-1],0);
                  end;
                faces[num_f].Z := StrToIntDef(strlst[j],0);
                  num_f := num_f + 1;
              end; //for j: each additional strip point
           end; //cnt > 3
       end; //for i: each strip
     end; //if binary else ASCII
     setlength(faces, num_f);
     goto 666;

  end;
  if pos('POLYGONS', UpperCase(str)) <> 1 then begin
    showmessage('Expected header to report "TRIANGLE_STRIPS" or "POLYGONS" not '+ str);
    goto 666;
  end;
  strlst.DelimitedText := str;
  num_f := StrToIntDef(strlst[1],0);
  cnt := StrToIntDef(strlst[2],0);
  if cnt <> (num_f * 4) then begin
     showmessage('Only able to read triangular meshes, not '+ str);
     goto 666;
  end;
  setlength(faces, num_f);
  if isBinary then begin
    {$DEFINE FASTVTK}
    {$IFDEF FASTVTK}
    setlength(faces4, num_f);
    blockread(f, faces4[0], num_f*sizeof(TPoint4i));
    for i := 0 to (num_f -1) do begin
        faces[i].X := faces4[i].X;
        faces[i].Y := faces4[i].Y;
        faces[i].Z := faces4[i].Z;
    end;
    faces4 := nil;
    {$ELSE}
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
    {$ENDIF}
    {$IFDEF ENDIAN_LITTLE} // VTK is ALWAYS big endian!
    for i := 0 to (num_f -1) do begin
           SwapLongInt(faces[i].X);
           SwapLongInt(faces[i].Y);
           SwapLongInt(faces[i].Z);
    end;
    (*for i := 0 to (num_v -1) do begin
           SwapSingle(vertices[i].X);
           SwapSingle(vertices[i].Y);
           SwapSingle(vertices[i].Z);
    end;*)
    {$ENDIF}
  end else begin //if binary else ASCII - indexed from 0
      for i := 0 to (num_f -1) do begin
          ReadLnBinVTK(f, str);
           strlst.DelimitedText := str;
           faces[i].X := StrToIntDef(strlst[1],0);
           faces[i].Y := StrToIntDef(strlst[2],0);
           faces[i].Z := StrToIntDef(strlst[3],0);
      end;
  end; //if binary else ASCII
666:
  {$DEFINE FSLvtk} //FSL first - triangle winding reversed?
  {$IFDEF FSLvtk}  //Slicer appears to use conventional triangle winding, while FSL is reversed
  (*if num_f > 0 then begin
    for i := 0 to (num_f -1) do begin
      j := faces[i].Y;
      faces[i].Y := faces[i].X;
      faces[i].X := j;
    end;
  end;*)
  {$ENDIF}
  closefile(f);
  strlst.free;
end; // LoadVtk()
{$ELSE}
procedure TMesh.LoadVtk(const FileName: string);
//Read VTK mesh
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bvtk/readVtk.m
// http://www.ifb.ethz.ch/education/statisticalphysics/file-formats.pdf
// ftp://ftp.tuwien.ac.at/visual/vtk/www/FileFormats.pdf
//  "The VTK data files described here are written in big endian form"
//n.b. ASCII reading is slow - strlst.DelimitedText much slower than readln!
label
   666;
const
  kBlockSz = 4096;
type
  TPoint4i = packed record
  N: longint;
  X: longint; //ensure 32-bit for simple GIfTI writing
  Y: longint;
  Z: longint;
end;

var
   f: TFByte;//TextFile;
   strlst: TStringList;
   str: string;
   strips: array of int32;
   c, i, j, k, num_v, num_f, num_f_allocated, num_strip, cnt: integer;
   nV: LongInt;
   faces4: array of TPoint4i;
   isBinary: boolean = true;
begin
  if (FSize(FileName) < 64) then exit;
  strlst:=TStringList.Create;
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  num_f := 0;
  Reset(f,1);
  ReadLnBin(f, str); //signature: '# vtk DataFile'
  if pos('VTK', UpperCase(str)) <> 3 then begin
    showmessage('Not a VTK file');
    goto 666;
  end;
  ReadLnBin(f, str); //comment: 'Comment: created with MRIcroS'
  ReadLnBin(f, str); //kind: 'BINARY' or 'ASCII'
  if pos('BINARY', UpperCase(str)) <> 0 then
     isBinary := true
  else if pos('ASCII', UpperCase(str)) <> 0 then begin
     isBinary := false;
     //next lines optional but faster!
     closefile(f);
     strlst.free;
     LoadVtkTxt(FileName); exit;
     exit;
  end else begin  // '# vtk DataFile'
     showmessage('VTK data should be ASCII or binary, not '+str);
     goto 666;
  end;
  ReadLnBin(f, str); // kind, e.g. "DATASET POLYDATA" or "DATASET STRUCTURED_ POINTS"
  while (str='') and (not eof(f)) do ReadLnBin(f, str);
  if pos('POLYDATA', UpperCase(str)) = 0 then begin
    showmessage('Only able to read VTK images saved as POLYDATA, not '+ str);
    goto 666;
  end;
  c := 0;
  while (pos('POINTS', UpperCase(str)) <> 1 ) and (c < 65) and (not eof(f)) do begin
          ReadLnBin(f, str); // number of vertices, e.g. "POINTS 685462 float"
          c := c + 1;
  end;
  //ReadLnBin(f, str); // number of vertices, e.g. "POINTS 685462 float"
  if pos('POINTS', UpperCase(str)) <> 1 then begin
    showmessage('Expected header to report "POINTS" not '+ str);
    goto 666;
  end;
  num_v := 0;
  strlst.DelimitedText := str;
  num_v := StrToIntDef(strlst[1],0);
  if (num_v < 1) or (pos('FLOAT', UpperCase(strlst[2])) <> 1) then begin
    showmessage('Expected at least 1 point of type FLOAT, not '+ str);
    goto 666;
  end;
  setlength(vertices, num_v); //vertices = zeros(num_f, 9);
  if isBinary then begin
    showmessage(inttostr(filepos(f)));
     blockread(f, vertices[0], 3 * 4 * num_v);
     {$IFDEF ENDIAN_LITTLE} // VTK is ALWAYS big endian!
     for i := 0 to (num_v -1) do begin
            SwapSingle(vertices[i].X);
            SwapSingle(vertices[i].Y);
            SwapSingle(vertices[i].Z);
     end;
     {$ENDIF}
  end
  else begin //if binary else ASCII
       for i := 0 to (num_v-1) do begin
         //n.b. ParaView and Mango pack multiple vertices per line, so we can not use ReadLnBin(f, str);
           vertices[i].X := StrToFloatDef(ReadNumBin(f),0);
           vertices[i].Y := StrToFloatDef(ReadNumBin(f),0);
           vertices[i].Z := StrToFloatDef(ReadNumBin(f),0);
       end;
  end;
  ReadLnBin(f, str); // number of vertices, e.g. "POLYGONS 1380 5520"
  while (str = '') and (not EOF(f)) do ReadLnBin(f, str);
  if pos('LINES', UpperCase(str)) > 0 then begin
    showmessage('This is a fiber file: rename with a ".fib" extension and use Tracks/Open to view: '+ str);
    goto 666;
  end;
  if pos('METADATA', UpperCase(str)) = 1 then begin //ParaView inserts a metadata block
    //http://www.vtk.org/doc/nightly/html/IOLegacyInformationFormat.html
    while (str <> '') and (not EOF(f)) do ReadLnBin(f, str); // The metadata block is terminated by an empty line
    ReadLnBin(f, str);
  end;
  if pos('TRIANGLE_STRIPS', UpperCase(str)) = 1 then begin
     strlst.DelimitedText := str;
     num_strip := StrToIntDef(strlst[1],0);
     cnt := StrToIntDef(strlst[2],0);
     num_f_allocated := cnt;
     (*if isBinary then begin
        showmessage(format('Unable to read VTK binary "TRIANGLE_STRIPS" %d %d', [num_strip, cnt]));
        goto 666;
     end;*)
     setlength(faces, num_f_allocated); //not sure how many triangles, lets guess
     num_f := 0;
     if isBinary then begin
       //Harvard Surgical Planning Laboratory MRB files are zip files with binary VTK Triangle_Strips
       c := cnt;
       setlength(strips, cnt); //vertices = zeros(num_f, 9);
       blockread(f, strips[0], 4 * cnt);
       {$IFDEF ENDIAN_LITTLE} // VTK is ALWAYS big endian!
       for i := 0 to (cnt -1) do
           SwapLongInt(strips[i]);
       {$ENDIF}
       k := 0;
       for i := 0 to (num_strip -1) do begin
           cnt := strips[k];
           k := k + 1;
           if ((cnt < 3) or (k > c)) then begin
              setlength(faces,0);
              num_f := 0;
              goto 666;
           end;
           if ((num_f+cnt) > num_f_allocated) then begin //need to allocate more faces
              num_f_allocated := num_f_allocated + cnt + kBlockSz;
              setlength(faces, num_f_allocated);
           end;
            faces[num_f].X := strips[k];
            faces[num_f].Y := strips[k+1];
            faces[num_f].Z := strips[k+2];
            k := k + 3;
            num_f := num_f + 1;
            if (cnt > 3) then begin
               for j := 4 to cnt do begin
                   //http://ogldev.atspace.co.uk/www/tutorial27/tutorial27.html
                   //  winding order is reversed on the odd triangles
                   if odd(j) then begin //ITK snap triangle winding reverses with each new point
                      faces[num_f].X := strips[k-2];
                      faces[num_f].Y := strips[k-1];
                   end else begin
                     faces[num_f].X := strips[k-1];
                     faces[num_f].Y := strips[k-2];
                   end;
                 faces[num_f].Z := strips[k];
                 k := k + 1;
                 num_f := num_f + 1;
              end; //for j: each additional strip point
           end; //cnt > 3
       end; //for i: each strip
     end else begin
       for i := 0 to (num_strip -1) do begin
           ReadLnBin(f, str);
           strlst.DelimitedText := str;
           cnt := StrToIntDef(strlst[0],0);
           if (cnt < 3) then begin
              setlength(faces,0);
              num_f := 0;
              goto 666;
           end;
           if ((num_f+cnt) > num_f_allocated) then begin //need to allocate more faces
              num_f_allocated := num_f_allocated + cnt + kBlockSz;
              setlength(faces, num_f_allocated);
           end;
           faces[num_f].Y := StrToIntDef(strlst[1],0);
           faces[num_f].X := StrToIntDef(strlst[2],0);
           faces[num_f].Z := StrToIntDef(strlst[3],0);
           num_f := num_f + 1;
           if (cnt > 3) then begin
              for j := 4 to cnt do begin
                  //http://ogldev.atspace.co.uk/www/tutorial27/tutorial27.html
                  //  winding order is reversed on the odd triangles
                  if odd(j) then begin //ITK snap triangle winding reverses with each new point
                     faces[num_f].X := StrToIntDef(strlst[j-1],0);
                     faces[num_f].Y := StrToIntDef(strlst[j-2],0);
                  end else begin
                    faces[num_f].X := StrToIntDef(strlst[j-2],0);
                    faces[num_f].Y := StrToIntDef(strlst[j-1],0);
                  end;
                faces[num_f].Z := StrToIntDef(strlst[j],0);
                  num_f := num_f + 1;
              end; //for j: each additional strip point
           end; //cnt > 3
       end; //for i: each strip
     end; //if binary else ASCII
     setlength(faces, num_f);
     goto 666;

  end;
  if pos('POLYGONS', UpperCase(str)) <> 1 then begin
    showmessage('Expected header to report "TRIANGLE_STRIPS" or "POLYGONS" not '+ str);
    goto 666;
  end;
  strlst.DelimitedText := str;
  num_f := StrToIntDef(strlst[1],0);
  cnt := StrToIntDef(strlst[2],0);
  if cnt <> (num_f * 4) then begin
     showmessage('Only able to read triangular meshes, not '+ str);
     goto 666;
  end;
  setlength(faces, num_f);
  if isBinary then begin
    {$DEFINE FASTVTK}
    {$IFDEF FASTVTK}
    setlength(faces4, num_f);
    blockread(f, faces4[0], num_f*sizeof(TPoint4i));
    for i := 0 to (num_f -1) do begin
        faces[i].X := faces4[i].X;
        faces[i].Y := faces4[i].Y;
        faces[i].Z := faces4[i].Z;
    end;
    faces4 := nil;
    {$ELSE}
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
    {$ENDIF}
    {$IFDEF ENDIAN_LITTLE} // VTK is ALWAYS big endian!
    for i := 0 to (num_f -1) do begin
           SwapLongInt(faces[i].X);
           SwapLongInt(faces[i].Y);
           SwapLongInt(faces[i].Z);
    end;
    (*for i := 0 to (num_v -1) do begin
           SwapSingle(vertices[i].X);
           SwapSingle(vertices[i].Y);
           SwapSingle(vertices[i].Z);
    end;*)
    {$ENDIF}
  end else begin //if binary else ASCII - indexed from 0
      for i := 0 to (num_f -1) do begin
          ReadLnBin(f, str);
           strlst.DelimitedText := str;
           faces[i].X := StrToIntDef(strlst[1],0);
           faces[i].Y := StrToIntDef(strlst[2],0);
           faces[i].Z := StrToIntDef(strlst[3],0);
      end;
  end; //if binary else ASCII
666:
  {$DEFINE FSLvtk} //FSL first - triangle winding reversed?
  {$IFDEF FSLvtk}  //Slicer appears to use conventional triangle winding, while FSL is reversed
  (*if num_f > 0 then begin
    for i := 0 to (num_f -1) do begin
      j := faces[i].Y;
      faces[i].Y := faces[i].X;
      faces[i].X := j;
    end;
  end;*)
  {$ENDIF}
  closefile(f);
  strlst.free;
end; // LoadVtk()


{$ENDIF}

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
     if ((mx - mn)+1) > length(vertices) then begin
        showmessage(format('Range of vertex indices (%d..%d) exceeds number of vertices (%d).',[mn,mx,length(vertices)]));
        result := false;
     end;
     if (result) and (mn <> 0) and (mx >= length(vertices)) then begin //incase faces indexed from 1, not 0!
        //Only do this if required - see Skull.ply which has unused vertex[0] http://people.sc.fsu.edu/~jburkardt/data/ply/ply.html
        for i := 0 to (length(faces)-1) do begin
            faces[i].X := faces[i].X - mn;
            faces[i].Y := faces[i].Y - mn;
            faces[i].Z := faces[i].Z - mn;
        end;
        mx := mx - mn;
        mn := mn - mn;
     end;
     if (result) and ((mn < 0) or (mx >= length(vertices))) then begin
        showmessage('Error: mesh does not make sense: '+inttostr(length(vertices))+' vertices but '+inttostr(mx-mn+1)+' indices ('+inttostr(mn)+'..'+inttostr(mx)+')');
        result := false;
     end;
     isNan := false;
     for i := 0 to (length(vertices) -1) do
         if specialsingle( vertices[i].X) or specialsingle( vertices[i].Y) or specialsingle( vertices[i].Z) then
            isNan := true;
     if result and isNan then begin
        showmessage('Vertices are corrupted (infinity of NaN values) '+inttostr(length(vertices)));
        result := false;
     end;
     if not result then
        MakePyramid;
     //result := true;
end; //CheckMesh()

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

function thicknessName(FileName: string): string;
// e.g. filename "lh.sphere.reg.T1fs_conform.gii" ->  lh.thickness.T1fs_conform
var
   pth, nam, prefix: string;
   i : integer;
begin
     result := '';
     nam := ExtractFileName(FileName);
     if (posex('lh.', nam) <> 1) and (posex('rh.', nam) <> 1) then exit;
     prefix := copy(nam, 1, 3);
     pth := extractfilepath(FileName);
     //https://www.nitrc.org/forum/message.php?msg_id=25643
     result := pth +prefix+'thickness';
     if fileexists(result)then exit;
     result := result + '.cr';
     if fileexists(result)then exit;
     // simnibs convention  lh.sphere.reg.T1fs_conform.gii ->  lh.thickness.T1fs_conform
     nam := copy(nam, 4, length(nam)-7); // "lh.*.gii' -> "*"
     i := posex('.', nam);
     if i < 1 then exit;
     nam := copy(nam, i, maxint);
     result := pth +prefix+'thickness'+nam;
     if fileexists(result)then exit;
     result := result + '.cr';
     if fileexists(result)then exit;
     result := '';
end;

function TMesh.LoadFromFile(const FileName: string): boolean;
var
   ext, fnm: string;
   isEmbeddedEdge: boolean;
   HasOverlays : boolean = false;
   {$IFDEF TIMER}startTime : TDateTime;{$ENDIF}
begin
	result := false;
        {$IFDEF TIMER}startTime := Now;{$ENDIF}
	isEmbeddedEdge := false;
	isNode := false;
	isFreeSurferMesh := false;
        if (FileName = '-') then begin
  	   isRebuildList := true;
  	   CloseOverlays;
  	   self.Close;
           MakePyramid;
        end;
        if not FileExistsF(FileName) then exit;
        FileMode := fmOpenRead;
	if (FSize(FileName) < 9) then exit;
	isBusy := true;
	ext := UpperCase(ExtractFileExt(Filename));
        //ext2 := UpCaseExt2(FileName); // "file.gii.dset" -> ".GII.DSET"
        isRebuildList := true;
	CloseOverlays;
	self.Close;
	if (ext = '.3DO') then
	   Load3Do(Filename);
	if (ext = '.3DS') then
	   Load3ds(Filename);
	if (ext = '.AC') then
	   LoadAc(Filename);
	if (ext = '.BYU') then
	   LoadByu(Filename);
	if (ext = '.DAE') then
	   LoadDae(Filename);
	if (ext = '.GTS') then
	   LoadGts(Filename);
	if (ext = '.DFS') then
	   LoadDfs(FileName);
	if (ext = '.DXF') then
	   LoadDxf(Filename);
        if (ext = '.IDTF') then
           LoadIdtf(Filename);
        if ((ext = '.JS') or (ext = '.JSON')) then
	   LoadJson(Filename);
	if (ext = '.LWO') then
	   LoadLwo(Filename);
	if (ext = '.MS3D') then
	   LoadMs3d(Filename);
	if (ext = '.MZ3') then
	   LoadMz3(Filename, 0);
	//if (ext = '.VBO') then
	//   LoadVbo(Filename);
	if (ext = '.MESH') then
	LoadMesh(Filename);
	if (ext = '.CTM') then
	 LoadCtm(Filename);
	if (ext = '.NV') then
	 LoadNv(Filename);
	if (ext = '.OFF') or (ext = '.NOFF') then
	 LoadOff(Filename);
	if (ext = '.PLY') then
	 LoadPly(Filename);
        if (ext = '.PLY2') then
	 LoadPly2(Filename);
	if (ext = '.PRWM') then
	LoadPrwm(Filename);
	if (ext = '.GII') then
	 LoadGii(Filename,0, 1, HasOverlays);
	if (ext = '.VTK') then
	 LoadVtk(Filename);
	if (ext = '.STL') then
	 LoadStl(Filename);
	if (ext = '.SURF') then
	LoadSurf(Filename);
        if (ext = '.TXT') then
           LoadNodeTxt(Filename);
	if (ext = '.NODE') or (ext = '.NODZ')then begin
	 LoadNode(Filename, isEmbeddedEdge);
	 if isEmbeddedEdge then
		LoadEdge(Filename, isEmbeddedEdge);
	end;
        if (ext = '.SRF') and (not LoadSrf(Filename)) then
	   LoadAsc_Srf(Filename);
	if (ext = '.TRI') then
	   LoadTri(Filename);
        if (ext = '.ASC') then  //https://brainder.org/category/neuroinformatics/file-types/
	   LoadAsc_Srf(Filename);
	if (ext = '.OBJ') then
	 LoadObj(Filename);
        if (ext = '.WRL') then
           LoadVrml(Filename);
	if (ext = '.WFR') then
	   LoadWfr(Filename);
	if length(faces) < 1 then begin//not yet loaded - see if it is freesurfer format (often saved without filename extension)
          LoadPial(Filename);
          //maybe someday read FreeSurfer patch files - a point cloud file without triangle indice
          //  p = read_patch('rh.cortex.patch.3d')
	   if length(faces) > 0 then
	      isFreeSurferMesh := true;
	end;
        if length(faces) < 1 then //error loading file
	   MakePyramid
	else
	    result := true;
        {$IFDEF TIMER}GLForm1.ClipBox.Caption :=  inttostr(MilliSecondsBetween(Now,StartTime)); {$ENDIF}
        //MakePyramid;
        if not CheckMesh then
           result := false; //note: prior "MakePyramid" will pass CheckMesh but should still return "false"
        SetDescriptives;
	if not isZDimIsUp then
	   SwapYZ;
	//showmessage(Filename+'  '+ inttostr(length(faces))+' '+inttostr(length(vertices)) );
	//NormalizeSize;
	isBusy := false;
        if HasOverlays then
           LoadOverlay(FileName, true);
        if (ext = '.GII') then begin
           fnm := thicknessName(FileName);
           if fileexists(fnm) then
              LoadOverlay(fnm, true);
        end;
        //https://www.nitrc.org/forum/message.php?msg_id=25643
        // simnibs convention  lh.sphere.reg.T1fs_conform.gii ->  lh.thickness.T1fs_conform
        (*if (ext = '.GII') and AnsiContainsText(FileName,'rh.central') then begin
           fnm := extractfilepath(FileName)+'rh.thickness.cr';
           if fileexists(fnm) then
              LoadOverlay(fnm, true);
        end;
        if (ext = '.GII') and AnsiContainsText(FileName,'lh.central') then begin
           fnm := extractfilepath(FileName)+'lh.thickness.cr';
           if fileexists(fnm) then
              LoadOverlay(fnm, true);
        end; *)
end; // LoadFromFile()

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
end; // LoadGcs()

function asInt(i : TRGBA): longint;
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
  result := inguy^.Lng;
end; // asRGBA()

function asIntA0(i : TRGBA): longint;
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
  inguy^.rgba.a := 0;
  result := inguy^.Lng;
end; // asRGBA()

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

procedure printf(s: string);
begin
     {$IFDEF UNIX}writeln(s);{$ENDIF}
end;

function TMesh.LoadNiml(const FileName: string): boolean;
label
   123;
const
  kDT_4FLOAT_1INT_1STR = 6;
  kFORM_TXT = 0;
  kFORM_LSB = 1;
  kFORM_MSB = 2;
type
  TAFNI = record
        ni_form, ni_dimen, ni_type, ni_bytesPerElement: integer;
   end;
function clearAfni: TAfni;
begin
     result.ni_dimen := 0;
     result.ni_type := 0;
     result.ni_bytesPerElement := 0;
     result.ni_form := kFORM_TXT;
end;
var
   st: string;
   i, nVert: integer;
   FSz: int64;
   f: TFByte;
   afni: TAFNI;
   rgbas: TVertexRGBA;
   floats: TFloats;
procedure ReadLnBinNiml(var f: TFByte; out s: string);
//niml follows binary immediately after ">" without EOLN!
const
  kEOLN = $0A;
  kEnd = ord('>');
var
   bt : Byte;
begin
     s := '';
     while (not  EOF(f)) do begin
           Read(f,bt);
           if bt = kEOLN then exit;
           s := s + Chr(bt);
           if bt = kEnd then exit;
     end;
end;
function parseBetweenQuotes(s: string):string; //'ni_type="int"'->'int'
var
   b,e: integer;
begin
     result := '';
     b := PosEx('"',s);
     if b < 1 then exit;
     e := PosEx('"',s, b+1);
     if e <= b then exit;
     result := copy(s, b+1, e-b-1);
end;
procedure parseAfni();
begin
    //writeln('-->', st);
    if AnsiContainsText(st, 'ni_type="4*float,int,String"') then
       afni.ni_type := kDT_4FLOAT_1INT_1STR;
    if AnsiContainsText(st, 'ni_type="float"') then begin
      afni.ni_type := kDT_FLOAT32;
      afni.ni_bytesPerElement := 4;
    end;
    if AnsiContainsText(st, 'ni_type="int"') then begin
      afni.ni_type := kDT_INT32;
      afni.ni_bytesPerElement := 4;
    end;
    if AnsiContainsText(st, 'ni_form="binary.lsbfirst"') then
       afni.ni_form := kFORM_LSB;
    if AnsiContainsText(st, 'ni_form="binary.msbfirst"') then
       afni.ni_form := kFORM_MSB;
    if AnsiContainsText(st, 'ni_dimen="') then
       afni.ni_dimen := strtointdef( parseBetweenQuotes(st), -1);
    if AnsiContainsText(st, 'data_type="LabelTableObject_data"') then
       afni.ni_bytesPerElement := 6;
end;
function readObjectHeader(): boolean;
begin
  afni := clearAfni();
  result := false;
  while not eof(f) and (not AnsiContainsText(st, '>')) do begin
      ReadLnBinNiml(f, st);
      parseAfni();
  end;
  if (afni.ni_type = kDT_4FLOAT_1INT_1STR) then begin
     result := true;
     exit;
     //LabelTableObject_data
  end;
  if not afni.ni_form = kFORM_MSB then begin
   {$IFDEF UNIX}writeln('NIML MSB not supported'); {$ENDIF}
   exit;
  end;
  if (afni.ni_type <> kDT_INT32) and (afni.ni_type <> kDT_FLOAT32) then begin
   {$IFDEF UNIX}writeln(format('Only support "int" (%d) not %d', [kDT_INT32, afni.ni_type])); {$ENDIF}
   exit;
  end;
  if afni.ni_dimen <> length(vertices) then begin //num_v := length(vertices)
   {$IFDEF UNIX}writeln(format('Expected %d labels but found %d',[length(vertices), afni.ni_dimen])); {$ENDIF}
   exit;
  end;
  if (filepos(f)+(afni.ni_bytesPerElement*afni.ni_dimen)) > FSz then begin
    {$IFDEF UNIX}writeln(format('File size too small for data %d',[FSz])); {$ENDIF}
    exit;
  end;
  ////assume sorted_node_def="Yes"!
  result := true;
end; //readObjectHeader()

procedure parseLabel(out r,g,b,a: byte; out idx: integer);
var
   strlst:TStringList;
begin
  idx := -1;
  strlst:=TStringList.Create;
  strlst.DelimitedText := st;
  if (strlst.count < 5) then exit;  //X Y Z A Index "StringLabel"
  idx := strtointdef(strlst[4],-1);
  if idx < 1 then begin
     strlst.free;
     exit;
  end;
  r := round(strtofloatdef(strlst[0],0)*255);
  g := round(strtofloatdef(strlst[1],0)*255);
  b := round(strtofloatdef(strlst[2],0)*255);
  a := round(strtofloatdef(strlst[3],0)*255);
  strlst.free;
end;

function readLabels(): boolean;
var
   i, idx, maxidx: integer;
   r,g,b,a: byte;

begin
  result := true;
  i := filepos(f);
  maxidx := -1;
  //first pass: max index e.g. there may be 360 regions with indices 1..1180
  while not eof(f) and (not AnsiContainsText(st, '</')) do begin
      ReadLnBinNiml(f, st);
      parseLabel(r,g,b,a,idx);
      if idx > maxidx then maxidx := idx;
  end;
  //writeln(maxidx);
  if maxidx < 1 then exit;
  //second pass
  Seek(f,i);
  st := '.';
  setlength(rgbas, maxidx+1);
  for i := 0 to atlasMaxIndex do
      rgbas[i] := RGBA(random(255), random(255), random(255), 0);
  while not eof(f) and (not AnsiContainsText(st, '</')) do begin
      ReadLnBinNiml(f, st);
      parseLabel(r,g,b,a,idx);
      if idx < 1 then continue;
      rgbas[idx] := RGBA(r,g,b,a);
      //writeln(format('%d = %d %d %d %d', [idx, r, g, b, a]));
  end;
end; //readObjectHeader()
begin
  result := false;
  atlasMaxIndex := -1;
  rgbas := nil;
  nVert := -1;
  FSz := FSize(FileName);
  if (FSz < 64) then exit;
  FileMode := fmOpenRead;
  AssignFile(f, FileName);
  Reset(f,1);
  afni := clearAfni();
  ReadLnBin(f, st); //signature
  if (not AnsiContainsText(st, '<AFNI_dataset'))  then begin
    {$IFDEF UNIX}writeln('Does not appear to be a AFNI dataset');{$ENDIF}
    //showmessage('Does not appear to be a AFNI dataset.');
    goto 123;
  end;
  while not eof(f) do begin
    ReadLnBin(f, st);
    if AnsiContainsText(st, '<INDEX_LIST') then begin
      //LabelTableObject_data
      result := readObjectHeader();
      if not result then goto 123;
      seek(f, filepos(f)+(afni.ni_bytesPerElement*afni.ni_dimen));
    end;
    // LabelTableObject_data
    if AnsiContainsText(st, '<SPARSE_DATA') then begin
      result := readObjectHeader();
      if not result then goto 123;
      if (afni.ni_bytesPerElement = 6) and (afni.ni_type = kDT_4FLOAT_1INT_1STR) then begin //LabelTableObject_data
         readLabels();
         continue;
      end;
      nVert := length(vertices);
      if nVert <> afni.ni_dimen then begin
        printf(format('NIML expected %d not %d vertices', [nVert, afni.ni_dimen]));
        goto 123;
      end;
      if afni.ni_form = kFORM_TXT then begin
         //OpenOverlays := OpenOverlays + 1;
         setlength(overlay[OpenOverlays].intensity, nVert);
         // (not AnsiContainsText(st, '</')) do
         i := 0;
         while i < nVert do begin
               ReadLnBin(f, st);
               if (length(st) < 1) then continue;
               if (AnsiContainsText(st, '</')) then goto 123;
               overlay[OpenOverlays].intensity[i] := strtofloatdef(st, 0);
               i := i + 1;
         end;
         result := true;
         goto 123;
      end;
      setlength(vertexAtlas, nVert);
      if afni.ni_type = kDT_INT32 then begin
         blockread(f, vertexAtlas[0],  nVert * sizeof(int32));
      end else if afni.ni_type = kDT_FLOAT32 then begin
          setlength(floats, nVert);
          blockread(f, floats[0],  nVert * sizeof(single));
          for i := 0 to (nVert-1) do
            vertexAtlas[i] := round(floats[i]);
          floats := nil;
      end else begin
          writeln('Unsupported datatype');
          goto 123;
      end;
      atlasMaxIndex := vertexAtlas[0];
      for i := 0 to (nVert -1) do
          if vertexAtlas[i] > atlasMaxIndex then
             atlasMaxIndex := vertexAtlas[i];
      //debug
      (*atlasMinIndex := vertexAtlas[0];
      for i := 0 to (nVert -1) do
          if vertexAtlas[i] < atlasMinIndex then
             atlasMinIndex := vertexAtlas[i];
      writeln(format(':::%d..%d',[atlasMinIndex, atlasMaxIndex]));
      //
      setlength(rgbas, atlasMaxIndex+1);
      for i := 0 to atlasMaxIndex do
          //rgbas[i] := RGBA(random(255), random(255), random(255), 128);
          rgbas[i] := RGBA(random(255), 255, random(255), 128);
      setlength(vertexRGBA, nVert);

      for i := 0 to (nVert -1) do
          vertexRGBA[i] := rgbas[(vertexAtlas[i])]; *)
      result := true;
      //goto 123;
    end;
  end; //while not eof(f)
  if (result) and (atlasMaxIndex > 0) and (nVert > 1) and (length(rgbas) > atlasMaxIndex) then begin
     //writeln(format('>>:::%d..%d %d',[length(rgbas), atlasMaxIndex, nVert]));
     setlength(vertexRGBA, nVert);
     for i := 0 to (nVert -1) do
          vertexRGBA[i] := rgbas[(vertexAtlas[i])];

  end;
  //result := true;
 123:
 closefile(f);
end;

{$DEFINE ANNOTasTEMPLATE}
{$IFDEF ANNOTasTEMPLATE}
function TMesh.LoadAnnot(const FileName: string): boolean;
//freesurfer Annotation file provides vertex colors
//  https://surfer.nmr.mgh.harvard.edu/fswiki/LabelsClutsAnnotationFiles
label
   666;
var
   f: File;
   v,i,j,sz: integer;
   nVert: LongWord;
   labelRGBA: TRGBA;
   num_entries, tag, ctabversion, maxstruc, labelNum, len, r,g,b,a: int32;
   idx: array of LongInt;
begin
  result := false;
  AssignFile(f, FileName);
  FileMode := fmOpenRead;
  Reset(f,1);
  sz := FileSize(f);
  if (sz < 20) or (length(vertices) < 3) then  //not a valid annot file
        goto 666;
  blockread(f, nVert, 4);
  {$IFDEF ENDIAN_LITTLE}  //FreeSurfer files ALWAYS big endian
  SwapLongWord(nVert);
  {$ENDIF}
  if (nVert <> length(vertices)) then begin
     showmessage('Annot file does not match currently loaded image: it describes '+inttostr(nVert)+' vertices but the mesh has '+inttostr(length(vertices)));
     goto 666;
  end;
  if (sz < (4+ (nVert * 8) )) then begin //too small
     showmessage('Annot file corrupted: file is smaller than expected');
     goto 666;
  end;
  setlength(idx, 2*nVert);
  blockread(f, idx[0], 2*4*nVert);
  {$IFDEF ENDIAN_LITTLE}  //FreeSurfer files ALWAYS big endian
  for i := 0 to ((2*nVert)-1) do
      SwapLongInt(idx[i]);
  {$ENDIF}
  setlength(vertexRGBA, nVert);
  j := 0;
  for i := 0 to (nVert-1) do begin
      v := idx[j];
      if (v < 0) or (v >= nVert) then begin
         showmessage(inttostr(i)+'/'+inttostr(nVert)+'Index out of range '+inttostr(v));
         goto 666;
      end;
      vertexRGBA[v] := asRGBA(idx[j+1]);
      j := j + 2;
  end;
  //next ATLAS data
  //GLForm1.Caption := (format('%d %d', [filepos(f),sz]));
  if ((filepos(f)+128)  < sz) then begin
    blockread(f, tag, 4);
    blockread(f, ctabversion, 4);
    blockread(f, maxstruc, 4);
    blockread(f, len, 4);
    {$IFDEF ENDIAN_LITTLE}
    SwapLongInt(ctabversion);
    SwapLongInt(maxstruc);
    SwapLongInt(len);
    {$ENDIF}
    //GLForm1.Caption := (format('%d %d %d', [sz, filepos(f),len]));
    if (ctabversion >= 0) or ((filepos(f)+len+8)  > sz) then begin
      showmessage('Undocumented old ctabversion not supported');
      goto 666;
    end;
    seek(f, filepos(f)+len);
    blockread(f, num_entries, 4);
    {$IFDEF ENDIAN_LITTLE}
    SwapLongInt(num_entries);
    {$ENDIF}
    if (num_entries < 1) then begin
      showmessage('Annot file does not make sense');
      goto 666;
    end;
    //GLForm1.Caption := (format('%d %d %d', [num_entries, filepos(f),len]));
    setlength(vertexAtlas, nVert);
    for v := 0 to (nVert -1) do
        vertexAtlas[v] := -1;
    for i := 0 to (num_entries-1) do begin
      blockread(f, labelNum, 4);
      blockread(f, len, 4);
      {$IFDEF ENDIAN_LITTLE}
      SwapLongInt(labelNum);
      SwapLongInt(len);
      {$ENDIF}
      seek(f, filepos(f)+len);
      blockread(f, r, 4);
      blockread(f, g, 4);
      blockread(f, b, 4);
      blockread(f, a, 4);
      {$IFDEF ENDIAN_LITTLE}
      SwapLongInt(r);
      SwapLongInt(g);
      SwapLongInt(b);
      SwapLongInt(a);
      {$ENDIF}
      labelNum := labelNum + 1; //index from 1 not zero!
      //GLForm1.ScriptOutputMemo.Lines.Add(format('%d rgba %d %d %d %d',[labelNum, r, g, b, a]));
      if i = 0 then atlasMaxIndex := labelNum;
      if labelNum > atlasMaxIndex then atlasMaxIndex := labelNum;
      labelRGBA := RGBA(r,g,b,a);
      //we need to use the RGB not RGBA value to infer label number!
      // mri_annotation2label can use a different alpha for label and vertex!
      for v := 0 to (nVert -1) do
          if asIntA0(labelRGBA) = asIntA0(vertexRGBA[v]) then
             vertexAtlas[v] := labelNum;
    end;
  end;
  for v := 0 to (nVert -1) do
      if (vertexAtlas[v] < 0) then begin
        GLForm1.ScriptOutputMemo.Lines.Add(format('annot file ERROR %d rgba %d %d %d %d',[666, vertexRGBA[v].r, vertexRGBA[v].g, vertexRGBA[v].b, vertexRGBA[v].a]));
        break;
      end;
  result := true;
  CloseFile(f);
  exit;
 666:
   CloseFile(f);
 setlength(vertexRGBA, 0);
end; // LoadAnnot()
{$ELSE}
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
      //vertexRGBA[v] := swizzleRGBA(vertexRGBA[v]);
      j := j + 2;
  end;
  result := true;
  CloseFile(f);
  exit;
 666:
   CloseFile(f);
 setlength(vertexRGBA, 0);
end; // LoadAnnot()
{$ENDIF}

procedure RemoveNaNs(var intensity: TFloats; var faces : TFaces; var vertices: TVertices; kUndefined: single);
//remove all faces and vertices where intensity = kUndefined
var
   i, num_v, num_ok: integer;
   outFace: TPoint3i;
   outIdx: TInts;
   inIntensity: TFloats;
   inFaces : TFaces;
   inVertices: TVertices;
begin
 num_v := length(vertices);
 if (num_v < 3) or (length(intensity) <> num_v) then exit;
 num_ok := 0;
 setlength(outIdx, num_v);
 for i := 0 to (num_v -1) do begin
     outIdx[i] := -1;
     if intensity[i] = kUndefined then continue;
     outIdx[i] := num_ok;
     num_ok := num_ok + 1;
 end;
 if num_ok = 0 then begin
    setlength(outIdx, 0);
    exit;
 end;
 inFaces := Copy(faces, 0, MaxInt);
 inVertices := Copy(vertices, 0, MaxInt);
 inIntensity := Copy(intensity, 0, MaxInt);
 setlength(intensity, num_ok);
 setlength(vertices, num_ok);
 num_ok := 0;
 //collapse vertices and scalar intensity
 for i := 0 to (num_v -1) do begin
     if outIdx[i] < 0 then continue;
     vertices[num_ok] := inVertices[i];
     intensity[num_ok] := inIntensity[i];
     num_ok := num_ok + 1;
 end;
 //collapse faces
 num_ok := 0;
 for i := 0 to (length(faces) -1) do begin
     outFace.x := outIdx[inFaces[i].X];
     outFace.y := outIdx[inFaces[i].Y];
     outFace.z := outIdx[inFaces[i].Z];
     if (outFace.x < 0) or (outFace.y < 0) or (outFace.z < 0)  then continue;
     faces[num_ok] := outFace;
     num_ok := num_ok + 1;
 end;
 setlength(faces, num_ok);
 setlength(inFaces, 0);
 setlength(inVertices, 0);
 setlength(inIntensity, 0);
 //showmessage(inttostr(num_ok));
 setlength(outIdx, 0);
end;

const
  kUndefined : single = 1/0; //NaN - used to indicate unused region - faces connected to these vertices will be discarded

function TMesh.LoadAtlasMapCore(lOverlayIndex: integer; intensityLUT: TFloats): string;
var
   maxROI, num_v, i, idx : integer;
   inten,minInten, maxInten: single;
begin
     num_v := length(vertices);
     maxROI := length(intensityLUT) - 1; // 0..maxROI
     if (num_v < 3) or (lOverlayIndex < kMinOverlayIndex) or (lOverlayIndex > kMaxOverlays) or (maxROI < 1) then exit('Corrupt LoadAtlasMapCore');
     setlength(overlay[lOverlayIndex].intensity, num_v); //vertices = zeros(num_f, 9);
     minInten := 0;
     maxInten := 0;
     for i := 0 to (num_v -1) do begin
         idx :=  vertexAtlas[i];
         if (idx > maxROI) then
            inten := kUndefined
         else
             inten := intensityLUT[idx];
         if inten < minInten then minInten := inten;
         if inten > maxInten then maxInten := inten;
         overlay[lOverlayIndex].intensity[i] := inten;
     end;
     //optional - create mesh overlay rather than scalars
     overlay[lOverlayIndex].faces := Copy(faces, 0, MaxInt);
     overlay[lOverlayIndex].vertices := Copy(vertices, 0, MaxInt);
     overlay[lOverlayIndex].minIntensity := minInten;
     overlay[lOverlayIndex].maxIntensity := maxInten;
     overlay[lOverlayIndex].windowScaledMin:= minInten;
     overlay[lOverlayIndex].windowScaledMax := maxInten;
     RemoveNaNs(overlay[lOverlayIndex].intensity, overlay[lOverlayIndex].faces, overlay[lOverlayIndex].vertices, kUndefined);
     //end optional
     result := '';
end;

function TMesh.LoadAtlasMap(const FileName: string; lOverlayIndex: integer): boolean;
//function TMesh.LoadAltasMap(const FileName: string; lOverlayIndex: integer): boolean;
label
   666;
var
   intensityLUT : TFloats;
   maxROI, num_v, i: integer;
   f: TextFile;
   inten: single;
   str: string;
   strlst : TStringList;
   ok : boolean = false;
begin
     result := false;
     num_v := length(vertices);
     if (num_v < 3) or (length(vertexAtlas) <> num_v) then begin
        showmessage('ROX format requires labelled mesh.');
        exit;
     end;
     maxROI := AtlasMaxIndex;
     if (maxROI < 1) then begin
        showmessage('ROX format requires labelled mesh with varying indices.');
        exit;
     end;
     setlength(intensityLUT, maxROI+1);
     for i := 0 to maxROI do
         intensityLUT[i] := kUndefined;
     //if (lOverlayIndex < kMinOverlayIndex) or (lOverlayIndex > kMaxOverlays) then exit;
     strlst:=TStringList.Create;
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
     Reset(f);
     while (not EOF(f)) do begin
           ReadLn(f, str); //make sure to run CheckMesh after this, as these are indexed from 1!
           if (length(str) > 0) and (str[1] <> '#') then begin
              strlst.DelimitedText := str;
              if (strlst.count > 1) then begin
                 i := strtointdef(strlst[0],-1);
                 inten := strtofloatdef(strlst[1],0);
         		if (i < 0) or (i > maxROI) then begin
         			showmessage('ROX file does not appear to match background mesh: expected indices 1..'+inttostr(maxROI)+' not '+inttostr(i));
         			goto 666;
         		end;
         		intensityLUT[i] := inten;
         		ok := true;
              end;
           end;
     end;
     if not ok then
        goto 666;
     LoadAtlasMapCore(lOverlayIndex, intensityLUT);
     if (length(overlay[lOverlayIndex].vertices) < 3) then
        showmessage('Unable to find any matching indices in atlas')
     else
         result := true;
   666:
     CloseFile(f);
     setlength(intensityLUT,0);
     strlst.free;
end;

function TMesh.LoadVoxel2Vertex(const FileName: string; lOverlayIndex: integer): boolean;
//some tools, e.g. PALM use voxel image formats (e.g. MGH) to store scalar data
// for Surfice, both voxel-based and vertex-based can be loaded.
// here we assume that if the number of voxels is precisely the number of vertices, the data represents vertex scalars
//load NIfTI,MGH,NRRD files where Dim[1] is equal to number of vertices
var
   fnm: string;
   hdr: TNIFTIhdr;
   i, gzFlag, x64, vol, nVal: int64;
   swapEndian: boolean;
   decomp: TGZFileStream;
   f: File;
   imgBytes: array of byte;
   i16s: TInt16s;
   ui16s: TUInt16s;
   i32s: TInt32s;
   f32s: TFloats;
   f64s: TDoubles;
begin
     result := false;
     if length(vertices) < 1 then exit;
     fnm := FileName;
     //if not readMGHHeader (fnm, hdr, gzFlag, swapEndian, x64) then exit;
     if not readVoxHeader (fnm, hdr, gzFlag, swapEndian, x64) then exit;
     if (x64 <> length(vertices)) then begin
        //showmessage(format('Expected %d vertices, not %d.', [length(vertices), x64]));
        exit;
     end;
     if (gzFlag = K_gzBytes_onlyImageCompressed) then begin
       showmessage('Unable to read files with uncompressed headers and compressed images.');
       exit;
     end;
     vol := hdr.dim[2];
     if (vol < 1) then vol := 1;
     overlay[lOverlayIndex].volumes := vol;
     overlay[lOverlayIndex].currentVolume := 1;
     setlength(overlay[lOverlayIndex].intensity,  vol*length(vertices));
     nVal := vol*length(vertices) ;
     if gzFlag = K_gzBytes_headerAndImageCompressed then begin
        decomp := TGZFileStream.create(FileName, gzopenread);
        setlength(imgBytes, round(hdr.vox_offset));
        decomp.Read(imgBytes[0], round(hdr.vox_offset));
        setlength(imgBytes, nVal * (hdr.bitpix div 8));
        decomp.Read(imgBytes[0], nVal * sizeof(single));
        decomp.free;
     end else begin
       setlength(imgBytes, nVal * (hdr.bitpix div 8));
       AssignFile(f, FileName);
       FileMode := fmOpenRead;
       Reset(f,1);
       Seek(f,round(hdr.vox_offset));
       BlockRead(f, imgBytes[0],nVal * sizeof(single));
       CloseFile(f);
     end;
     if (hdr.datatype = kDT_UINT8) then begin
        for i := 0 to (nVal-1) do
            overlay[lOverlayIndex].intensity[i] := imgBytes[i];
     end else if (hdr.datatype = kDT_INT16) then begin
         i16s := TInt16s(imgBytes);
         for i := 0 to (nVal-1) do begin
             if (swapEndian) then //data always stored big endian
                Swap(i16s[i]);
             overlay[lOverlayIndex].intensity[i] := i16s[i];
         end;
     end else if (hdr.datatype = kDT_UINT16) then begin
         ui16s := TUInt16s(imgBytes);
         for i := 0 to (nVal-1) do begin
             if (swapEndian) then //data always stored big endian
                Swap(ui16s[i]);
             overlay[lOverlayIndex].intensity[i] := ui16s[i];
         end;
     end else if (hdr.datatype = kDT_INT32) then begin
         i32s := TInt32s(imgBytes);
         for i := 0 to (nVal-1) do begin
             if (swapEndian) then //data always stored big endian
                Swap(i32s[i]);
             overlay[lOverlayIndex].intensity[i] := i32s[i];
         end;
     end else if (hdr.datatype = kDT_FLOAT32) then begin
        f32s := TFloats(imgBytes);
        for i := 0 to (nVal-1) do begin
          if (swapEndian) then //data always stored big endian
             SwapSingle(f32s[i]);
          overlay[lOverlayIndex].intensity[i] := f32s[i];
        end;
     end else if (hdr.datatype = kDT_DOUBLE) then begin
        f64s := TDoubles(imgBytes);
        for i := 0 to (nVal-1) do begin
          if (swapEndian) then
             SwapDouble(f64s[i]);
          overlay[lOverlayIndex].intensity[i] := f64s[i];
        end;
     end else
         showmessage(format('Unsupported scalar datatype %d', [hdr.datatype]));
     imgBytes := nil;
     result := true;
end;


function TMesh.LoadCol(const FileName: string; lOverlayIndex, lOverlayItem: integer): integer; //AFNI https://afni.nimh.nih.gov/afni/community/board/read.php?1,44391,44410
//https://afni.nimh.nih.gov/afni/community/board/read.php?1,44391,44410
label
   666;
var
   num_v, i, nOK: integer;
   f: TextFile;
   inten: single;
   strlst : TStringList;
   str: string;

begin
     result := 0;
     if (lOverlayIndex < kMinOverlayIndex) or (lOverlayIndex > kMaxOverlays) then exit;
     num_v := length(vertices);
     setlength(overlay[lOverlayIndex].intensity, num_v);
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
     strlst:=TStringList.Create;
     nOK := 0;
     Reset(f);
     ReadLn(f, str); //make sure to run CheckMesh after this, as these are indexed from 1!
     strlst.DelimitedText := str;
     //showmessage(format('%d', [strlst.Count]));
     result := strlst.Count;
     if result < 1 then goto 666;
     if lOverlayItem < result then lOverlayItem := result;
     //GLForm1.Caption := inttostr(strlst.Count);
     Reset(f);
     for i := 0 to (num_v-1) do begin
       try
          //ReadLn(f, inten);
          ReadLn(f, str); //make sure to run CheckMesh after this, as these are indexed from 1!
          strlst.DelimitedText := str;
          //inten := strtofloat(strlst[lOverlayItem-1]);
          inten := strtofloat(strlst[lOverlayItem-1]);
       except
         continue;
       end;
       overlay[lOverlayIndex].intensity[nOK] := inten;
       nOK := nOK + 1;
     end;
   666:
     CloseFile(f);
   strlst.Free;
   if (nOK <> num_v) or (result < 1) then begin

      showmessage('COL file does not appear to match background mesh: expected '+inttostr(num_v)+' vertices not '+inttostr(nOK));
      //GLForm1.Caption := ('COL file does not appear to match background mesh: expected '+inttostr(num_v)+' vertices not '+inttostr(nOK));

      result := 0;
      overlay[lOverlayIndex].intensity := nil;
   end;
   //GLForm1.Caption := ('COL file does not appear to match background mesh: expected '+inttostr(num_v)+' vertices not '+inttostr(nOK));

end;

function TMesh.Load1D(const FileName: string; lOverlayIndex: integer): boolean;
//ConvertDset -o_1D -input lh.aparc.a2009s.annot.niml.dset -prefix lh.aparc
//
//# <SPARSE_DATA
//#  ni_type = "int"
//#  ni_dimen = "168098"
//#  data_type = "Node_Label_data"
//# >
// 3937420
// 3937420
// 3937420
// 3937420
label
   666;
const
     kNaN : single = 1/0;
var
   num_v: integer;
   f: TextFile;
   v: single;
   str: string;
   i, idx: integer;
   strlst : TStringList;

begin
     result := false;
     if (lOverlayIndex < kMinOverlayIndex) or (lOverlayIndex > kMaxOverlays) then exit;
     num_v := length(vertices);
     setlength(overlay[lOverlayIndex].intensity, num_v);
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
     Reset(f);
     idx := 0;
     while not EOF(f) do begin
         try
            ReadLn(f, str);
         except
           continue;
         end;
         if (length(str) < 1) then continue;
         if (str[1] = '#') then continue;
         if AnsiContainsText(str, 'Using average radius') then begin
           printf('Unsupported type of AFNI 1D dataset');
           goto 666; //"std.141.FT_rh.MI.1D" does not comment this line with #
         end;
         v := strtofloatdef(str, kNaN);

         if (v = kNaN) then continue;
         if specialsingle(v) then continue;
         if (idx >= num_v) then continue;
         overlay[lOverlayIndex].intensity[idx] := v;
         idx := idx + 1;
     end;
     if (idx = 0) and (length(str) >= (2*num_v)) and (str[1] <> '#') then begin
        //?transposed file created with ConvertDset -o_1Dpt -input lh.aparc.a2009s.annot.niml.dset -prefix lh.pt
        strlst:=TStringList.Create;
        strlst.DelimitedText := str;
        if strlst.Count = num_v then begin
           idx := 0;
           for i := 0 to (num_v-1) do begin
              v := strtofloatdef(strlst[i], kNaN);
              if (v = kNaN) then continue;
              overlay[lOverlayIndex].intensity[idx] := v;
              idx := idx + 1;
           end;
        end;
        strlst.free;
     end;
     if (idx <> num_v) then begin //when fewer vertices than background image
        str := format('1D file reports %d vertices but background mesh has %d',[idx, num_v]);
        printf(str);
        showmessage(str)
     end else
         result := true;
   666:
     CloseFile(f);
    if not result then
       setlength(overlay[lOverlayIndex].intensity, 0);
end;


function TMesh.LoadDpv(const FileName: string; lOverlayIndex: integer): boolean;
//https://brainder.org/2011/09/25/braindering-with-ascii-files/
label
   666;
var
   num_v, iMax, iMin: integer;
   f: TextFile;
   idxMaxInten: single;
   idxf, v1, v2, v3, inten: single;
   idx: integer;
begin
     result := false;
     idxMaxInten := 0;
     if (lOverlayIndex < kMinOverlayIndex) or (lOverlayIndex > kMaxOverlays) then exit;
     num_v := length(vertices);
     setlength(overlay[lOverlayIndex].intensity, num_v); //vertices = zeros(num_f, 9);
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
     Reset(f);
     iMax := -1;
     iMin := maxint;
     for idx := 0 to (num_v-1) do
         overlay[lOverlayIndex].intensity[idx] := 0;
     idx := 0;
     while not EOF(f) do begin
         try
            ReadLn(f, idxf, v1, v2, v3, inten);
         except
           continue;
         end;
         idx := round(idxf);
         if (idx < 0) or (idx > num_v) then begin //when more vertices than background image
           showmessage('DPV file does not appear to match background mesh: expected indices 1..'+inttostr(num_v)+' not '+inttostr(idx));
           goto 666;
         end;
         if idx > iMax then iMax := idx;
         if idx < iMin then iMin := idx;
         if idx >= num_v then begin
         	idxMaxInten := inten;
         end else
         	overlay[lOverlayIndex].intensity[idx] := inten;
     end;
     if iMax = num_v then begin //prior to 11/2018 PALM indexed from 0 not 1
       for idx := 1 to (num_v-1) do
     		overlay[lOverlayIndex].intensity[idx-1] := overlay[lOverlayIndex].intensity[idx];
       overlay[lOverlayIndex].intensity[num_v-1] := idxMaxInten;
     end;
     idx := iMax-iMin+1;
     if (idx <> num_v) then //when fewer vertices than background image
        showmessage(format('DPV file reports %d vertices but background mesh has %d (%d..%d)',[idx, num_v, iMin, iMax]));
     result := true;
   666:
     CloseFile(f);
end;

function IsCurv(const FileName: string): boolean;
var
   sig : array [1..3] of byte;
   f: File;
   aValsPerVertex, anum_v, anum_f: LongWord;
   sz: integer;
begin
     result := false;
     AssignFile(f, FileName);
     FileMode := fmOpenRead;
     Reset(f,1);
     sz := FileSize(f);
     if (sz < 20) then begin
        CloseFile(f);
        exit;
     end;
     blockread(f, sig, 3 ); //since these files do not have a file extension, check first 3 bytes "0xFFFFFF"
     blockread(f, anum_v, 4 ); //uint32
     blockread(f, anum_f, 4 ); //uint32
     blockread(f, aValsPerVertex, 4 ); //uint32
     {$IFDEF ENDIAN_LITTLE}
     SwapLongWord(anum_v);
     SwapLongWord(anum_f);
     SwapLongWord(aValsPerVertex);
     {$ENDIF}
     CloseFile(f);
     if aValsPerVertex <> 1 then exit;
     if (sig[1] <> $FF) or (sig[2] <> $FF) or (sig[3] <> $FF) then //not CURV format
        exit;
     if (sz <> (15 + (anum_v * 4))) then exit;
     result := true;
end;

procedure TMesh.LoadCurv(const FileName: string; lOverlayIndex: integer);
//simple format used by Freesurfer  BIG-ENDIAN
// https://github.com/bonilhamusclab/MRIcroS/blob/master/%2BfileUtils/%2Bpial/readPial.m
// http://www.grahamwideman.com/gw/brain/fs/surfacefileformats.htm
var
   f: File;
   i, sz: integer;
   mn,mx: single;
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
     blockread(f, sig, 3 ); //since these files do not have a file extension, check first 3 bytes "0xFFFFFF"
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
     //normalize intensity
     mn := overlay[lOverlayIndex].intensity[0];
     mx := mn;
     for i := 0 to (num_v-1) do begin
         if (overlay[lOverlayIndex].intensity[i] > mx) then mx := overlay[lOverlayIndex].intensity[i];
         if (overlay[lOverlayIndex].intensity[i] < mn) then mn := overlay[lOverlayIndex].intensity[i];
     end;
     if (mn = mx) or (mx < 0) or (mn > 0) then exit;
     if abs(mn) > mx then mx := abs(mn);
     for i := 0 to (num_v-1) do
         overlay[lOverlayIndex].intensity[i] := overlay[lOverlayIndex].intensity[i] / mx;
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
end; // fread3()

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
         SwapSingle(s); //W files are ALWAYS big endian
         {$ENDIF}
         overlay[lOverlayIndex].intensity[idx] := s;
     end;
     CloseFile(f);
end; // LoadW()

function TMesh.LoadWfr(const FileName: string): boolean; //EMSE wireframe
//http://eeg.sourceforge.net/doc_m2html/bioelectromagnetism/emse_write_wfr.html
//http://eeg.sourceforge.net/doc_m2html/bioelectromagnetism/emse_read_wfr.html
label
   666;
const
  kBlockSize = 16384;
var
   f: TextFile;
   s: string;
   version, file_type, minor_rev, num_v, num_f, meshtype: integer;
   strlst : TStringList;
begin
     result := false;
     strlst:=TStringList.Create;
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
     Reset(f);
     Readln(f, version, file_type);  //'3\t4000\n'
     Readln(f, minor_rev);  //'3\n'
     if ((file_type <> 4000) and (file_type <> 8000)) or (minor_rev <> 3) then begin
       showmessage(format('cannot read WFR file type: %d revision %d',[file_type, minor_rev]));
       goto 666;
     end;
     Readln(f, meshtype); //meshtype - 0=unknown, 40=scalp, 80=outerskull, 100=innerskull, 200=brain
     setlength(vertices, kBlockSize); //this format does not report number of faces/vertices, so we need to guess
     setlength(faces, kBlockSize);
     num_v := 0;
     num_f := 0;
     DefaultFormatSettings.DecimalSeparator := '.';
     while not EOF(f) do begin
           Readln(f, s);  //'v -75.442212 -33.082844 -24.913490' or 't 0 1 2'
           if length(s) < 7 then continue;
           if (upcase(s[1]) <> 'V') and (upcase(s[1]) <> 'T') then continue; //only read 'f'ace and 'v'ertex lines
           strlst.DelimitedText := s;
           if (strlst.count < 4) then continue;
           if upcase(s[1]) = 'V' then begin //vertex
              if ((num_v) >= length(vertices)) then
                 setlength(vertices, length(vertices)+kBlockSize);
              vertices[num_v].X := strtofloatDef(strlst[1], 0);
              vertices[num_v].Y := strtofloatDef(strlst[2], 0);
              vertices[num_v].Z := strtofloatDef(strlst[3], 0);
              inc(num_v);
           end;
           if upcase(s[1]) = 'T' then begin //triangle
              if ((num_f) >= length(faces)) then
                 setlength(faces, length(faces)+kBlockSize);
              faces[num_f].X := strtointDef(strlst[1], 1);
              faces[num_f].Y := strtointDef(strlst[2], 1);
              faces[num_f].Z := strtointDef(strlst[3], 1);
              inc(num_f);
           end;
     end;
     setlength(vertices, num_v); //trim to correct size
     setlength(faces, num_f); //trim to correct size
     result := true;
 666:
     CloseFile(f);
     strlst.free;
end;

procedure TMesh.LoadNii(const FileName: string; lOverlayIndex: integer; lLoadSmooth: boolean);
//Load NIfTI image as overlay
var
   i, num_v: integer;
  nii: TNIFTI;
begin
   nii := TNIfTI.Create;
   if lLoadSmooth then
      nii.LoadFromFile(FileName, kNiftiSmoothMaskZero)
   else
       nii.LoadFromFile(FileName, kNiftiSmoothNone);
   num_v := length(vertices);
   setlength(overlay[lOverlayIndex].intensity, num_v);
   for i := 0 to (num_v-1) do
       overlay[lOverlayIndex].intensity[i] := nii.mm2intensity(vertices[i].X, vertices[i].Y, vertices[i].Z, lLoadSmooth);
   overlay[lOverlayIndex].isBinary := nii.isBinary;
   nii.free;
end; // LoadNii()

procedure TMesh.LoadMeshAsOverlay(const FileName: string; lOverlayIndex: integer);
var
   lMesh: TMesh;
begin
     lMesh := TMesh.Create;
     overlay[lOverlayIndex].isBinary := false;
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

procedure TMesh.MinMaxPct(lOverlayIndex, num_v: integer; var mx, mn: single; isExcludeZero: boolean);
//adjust mn/mx to be top/bottom 1% instead of absolute min/max
const
  kBins = 6000;
  kFrac = 0.01;
var
   i, sum, pct, bin: integer;
   slope, range, mn2, mx2: single;
   cnt: array [0..kBins] of integer;
begin
   range := mx - mn;
   //showmessage(format('%g %g', [mn, mx]));
   if (range <= 0) or (num_v < 2) then exit;
   for i := 0 to kBins do
       cnt[i] := 0;
   slope := kBins * 1/range;
   if isExcludeZero then begin
      sum := 0;
      for i := 0 to num_v do begin
          if overlay[lOverlayIndex].intensity[i] <> 0 then begin
             bin := round((overlay[lOverlayIndex].intensity[i] - mn) * slope);
             IntBound(bin, 0, kBins);
             inc(cnt[bin]);
             inc(sum);
          end;
      end;
      if sum = 0 then exit;
      pct := trunc(kFrac * sum);
   end else begin
       for i := 0 to num_v do begin
         bin := round((overlay[lOverlayIndex].intensity[i] - mn) * slope);
         IntBound(bin, 0, kBins);
         inc(cnt[bin]);
       end;
       pct := trunc(kFrac * num_v);
   end;
   //find new max
   sum := 0;
   i := kBins+1;
   while (sum < pct) and (i > 0) do begin
     i := i - 1;
     sum := sum + cnt[i];
   end;
   mx2 := mn + ((i/kBins) * range);
   //find new min
   sum := 0;
   i := -1;
   while (sum < pct) and (i < kBins) do begin
     i := i + 1;
     sum := sum + cnt[i];
   end;
   mn2 := mn + ((i/kBins) * range);
   if mx2 > mn2 then begin
      mx := mx2;
      mn := mn2;
   end;
end;

procedure TMesh.SetOverlayDescriptives(lOverlayIndex: integer);
const
  k0 : single = 0.0;
  k1 : single = 1.0;
var
   mx, mn, mnNot0, v: single;
   i, num_v, lLog10: integer;
   isBinary : boolean;
begin
  overlay[lOverlayIndex].LUTinvert := false;
  overlay[lOverlayIndex].aoMap := false;
  overlay[lOverlayIndex].PaintMode := kPaintHideDefaultBehavior;
  num_v := length(overlay[lOverlayIndex].intensity);
  if (num_v < 3) then exit;
  mn := overlay[lOverlayIndex].intensity[0];
  mx := mn;
  for i := 0 to (num_v-1) do
      if mx < overlay[lOverlayIndex].intensity[i] then mx := overlay[lOverlayIndex].intensity  [i];
  for i := 0 to (num_v-1) do
      if mn > overlay[lOverlayIndex].intensity[i] then mn := overlay[lOverlayIndex].intensity  [i];
  //2nd pass: histogram for percent...
  MinMaxPct(lOverlayIndex, num_v, mx, mn, false);
  //set binary if mesh is only 0 and 1
  isBinary := true;
  if (mn <> k0) and (mx <> k1) then isBinary := false;
  if overlay[lOverlayIndex].isBinary then isBinary := false; //no need to check while loop, e.g. interpolated voxels
  if isBinary then begin
     i := 0;
     while (i < num_v) and (isBinary) do begin
           if (overlay[lOverlayIndex].intensity[i] <> k0) and (overlay[lOverlayIndex].intensity[i] <> k1) then
              isBinary := false;
           i := i + 1;
     end;
  end;
  isBinary := isBinary or overlay[lOverlayIndex].isBinary;
  //end set binary
  if mn = 0 then begin //find minimal value that is greater than zero
     mnNot0 := mx;
     for i := 0 to (num_v-1) do begin
         v :=  overlay[lOverlayIndex].intensity[i];
         if (v < mnNot0) and (v <> 0)  then mnNot0 := v;
     end;
     if mnNot0 < mx then
        mn := mnNot0;
  end;
  overlay[lOverlayIndex].minIntensity := (mn);
  overlay[lOverlayIndex].maxIntensity := (mx);
  lLog10 := trunc(log10( mx-mn))-1;
  if isBinary then begin
    overlay[lOverlayIndex].windowScaledMin:= 0.5;
    overlay[lOverlayIndex].windowScaledMax:= 0.5;

  end else if (mx > 4) and (mn < -1) then begin
     overlay[lOverlayIndex].windowScaledMin:= 2;
     overlay[lOverlayIndex].windowScaledMax:= roundto(mx,lLog10);
  end else if (mx <= 0) and (mn < -4) then begin
     overlay[lOverlayIndex].windowScaledMin:= -2;
     overlay[lOverlayIndex].windowScaledMax:= roundto(mn,lLog10); ;
  end else begin
      overlay[lOverlayIndex].windowScaledMin:= roundto(mn,lLog10);
      overlay[lOverlayIndex].windowScaledMax := roundto(mx,lLog10);
  end;
  (*if (mn = mx) then begin
     showmessage('Error: no variability in overlay '+floattostr(mn));
     overlay[lOverlayIndex].intensity := nil; //release
     exit;
  end;*)
  //showmessage('overlay'+floattostr(mn)+'  '+floattostr(mx));
end; // SetOverlayDescriptives()

{$Include cifti.inc}

function TMesh.SetLutIndex(layer:integer): integer;
//count other overlays, but exclude curve maps
var
   i,n: integer;
begin
     n := 0;
     i := kMinOverlayIndex;
     while (i < layer) and (i <= kMaxOverlays) do begin
           if not isFreeSurferLUT(overlay[i].LUTindex) then
              n := n + 1;
           i := i + 1;
     end;
     result := n + 1;
     if n > 12 then
        result := 0;
end;

procedure SavePrwm(const FileName: string; Faces: TFaces; Vertices: TVertices; vertexRGBA: TVertexRGBA);
type
Tui16 = packed record
      x,y,z: uint16;
   end;
Tpr_hdr = packed record
      vers,flags,
      nVal1,nVal2,nVal3,
      nIdx1,nIdx2,nIdx3: byte;
   end;
var
  hdr: Tpr_hdr;
  faces16: array of Tui16;
  i, nIdx, nFace, nVert: integer;
  mStream : TMemoryStream;
  FileNamePrwm: string;
  aflag, pad: byte;
begin
  nFace := length(Faces);
  nVert := length(Vertices);
  nIdx := nFace * 3;//each triangular face is 3 indices: indicesNumber
  pad := 0;
  hdr.vers := 1;
  //  indexedGeometry := (hdr.flags and 128) > 0;
  //indicesType := (hdr.flags shr 6) and 1; ;
  //bigEndian := (hdr.flags and 32) > 0;
  hdr.flags := 0;
  hdr.flags := hdr.flags + 128; //indexedGeometry
  if (nIdx >= 65535) then
     hdr.flags := hdr.flags + 64; //indices are 32-bit
  {$IFDEF ENDIAN_BIG}
  hdr.flags := hdr.flags + 32; //big endian
  {$ENDIF}
  hdr.flags := hdr.flags + 1; //only one attribute: "position"
  hdr.nVal1 := (nVert and 255);
  hdr.nVal2 := ((nVert shr 8) and 255);
  hdr.nVal3 := ((nVert shr 16) and 255);
  hdr.nIdx1 := (nIdx and 255);
  hdr.nIdx2 := ((nIdx shr 8) and 255);
  hdr.nIdx3 := ((nIdx shr 16) and 255);
  FileNamePrwm := DefaultToHomeDir(FileName);
  FileNamePrwm := changeFileExt(FileNamePrwm, '.prwm');
  {$IFDEF UNIX} //I do not think we need to do this!
  FileNamePrwm := ExpandUNCFileNameUTF8(FileNamePrwm);
  {$ENDIF}
  mStream := TMemoryStream.Create;
  mStream.Write(hdr,sizeOf(hdr));
  mStream.Write('position', 8);
  mStream.Write(pad, 1); //null terminated string
  aflag := 0; //0=float,not normalized
  aflag := aflag + (2 shl 4); //Cardinality: 3D
  aflag := aflag + 1;// Encoding type: Float
  mStream.Write(aflag, 1); //null terminated string
  mStream.Write(pad, 1); //pad1
  mStream.Write(pad, 1); //pad2
  mStream.Write(Vertices[0], nVert * 3 * sizeof(single));
  if (nIdx < 65535) then begin
      setlength(faces16, nFace);
      for i := 0 to (nFace -1) do begin
          faces16[i].x := Faces[i].x;
          faces16[i].y := Faces[i].y;
          faces16[i].z := Faces[i].z;
      end;
      mStream.Write(faces16[0], nFace * sizeof(Tui16));
      faces16 := nil;
  end else
      mStream.Write(Faces[0], nFace * sizeof(TPoint3i));
  mStream.Position := 0;
  FileMode := fmOpenWrite;   //FileMode := fmOpenRead;
  mStream.SaveToFile(FileNamePrwm);
  mStream.Free;
  FileMode := fmOpenRead;
end;

procedure SaveMeshCore(const FileName: string; Faces: TFaces; Vertices: TVertices; vertexRGBA: TVertexRGBA);
var
  x: string;
  intensities: TFloats;
begin
  x := UpperCase(ExtractFileExt(Filename));
  intensities := nil;
  if (x = '.WRL') then
     SaveVrml(FileName, Faces, Vertices, vertexRGBA)
  else if (x = '.MZ3') then
     SaveMz3(FileName, Faces, Vertices, vertexRGBA, intensities)
  else if (x = '.GII') then
     SaveGii(Filename, Faces, Vertices, vertexRGBA)
  else if (x = '.STL') then
     SaveStl(Filename, Faces, Vertices, vertexRGBA)
  else if (x = '.PLY') then
     SavePly(Filename, Faces, Vertices, vertexRGBA)
  else if (x = '.PRWM') then
     SavePrwm(Filename, Faces, Vertices, vertexRGBA)
  else if (x = '.OBJ') then
     SaveObj(Filename, Faces, Vertices, vertexRGBA)
  else begin
       SaveObj(Filename+'.obj', Faces, Vertices, vertexRGBA);
       //SaveMz3(Filename+'.mz3', Faces, Vertices, vertexRGBA);
  end;
end;

function linearStep (x, edge0, edge1: single): single; inline;
//http://www.shaderific.com/glsl-functions/
begin
     if x <= edge0 then exit(0.0);
     if x >= edge1 then exit(1.0);
     result :=  (x - edge0)/(edge1-edge0);
end;

procedure TMesh.SaveMesh(const FileName: string; MeshColor: TColor = clWhite);
var
  intensities: TFloats;
  i: integer;
  isOverlayPainting : boolean = false;
  vRGBA: TVertexRGBA;
  clr: TRGBA;
  x: string;
begin
  //showmessage(format ('%dx%d', [length(vertexRGBA), Self.OpenOverlays]));
  x := UpperCase(ExtractFileExt(Filename));
  vRGBA := nil;
  if (length(vertexRGBA) > 0) and (x = '.MZ3') and (PosEx('AOMap',Filename) > 0) then begin
     //save RGB as ambient occlusion map
    //showmessage('xx');
     setlength(intensities, length(Vertices));
     for i := 0 to (length(Vertices)-1) do
         intensities[i] := vertexRGBA[i].G / 255.0;
     SaveMz3(FileName, Faces, Vertices, vRGBA, intensities, true);
     intensities := nil;
     exit;
   end;
  if (length(vertexRGBA) > 0) or (Self.OpenOverlays < 1) then begin
     SaveMeshCore(Filename, Faces,Vertices, vertexRGBA);
     exit;
  end;

  (*if (Self.OpenOverlays = 1) and (not isFreeSurferLUT(Overlay[1].LUTindex)) and (x = '.MZ3') and (length(Overlay[1].intensity) = length(Vertices)) then begin
    setlength(intensities, length(Vertices));
    for i := 0 to (length(Vertices)-1) do
       intensities[i] := Overlay[1].intensity[i];
    SaveMz3(FileName, Faces, Vertices, vRGBA, intensities);
    intensities := nil;
    exit;
  end;*)
  if (Self.OpenOverlays = 1) and (isFreeSurferLUT(Overlay[1].LUTindex)) and (x = '.MZ3') and (length(Overlay[1].intensity) = length(Vertices)) then begin
    //save ambient occlusion map
    setlength(intensities, length(Vertices));
    for i := 0 to (length(Vertices)-1) do
       //intensities[i] := Overlay[1].intensity[i];
        intensities[i] := 1.0 - linearStep(Overlay[1].intensity[i], -0.6, 0.6);
    SaveMz3(FileName, Faces, Vertices, vRGBA, intensities);
    intensities := nil;
    exit;
  end;
  for i :=  OpenOverlays downto 1 do
      if (overlay[i].OpacityPercent <> kLUTinvisible) and (length(overlay[i].intensity) >= length(Vertices)) then
           isOverlayPainting := true;
  if (not isOverlayPainting) then begin
     SaveMeshCore(Filename, Faces,Vertices, vertexRGBA);
     exit;
  end;
  clr := asRGBA(MeshColor);
  vRGBA := nil;
  //overlay painting
  BuildListCore(Clr, Faces, Vertices, vRGBA, nil, false);
  SaveMeshCore(Filename, Faces,Vertices, vRGBA);
  vRGBA := nil;
end;

function TMesh.LoadOverlay(const FileName: string; lLoadSmooth: boolean): boolean; //; isSmooth: boolean
var
   i, nOverlays: integer;
   isLUTinvert: boolean = false;
   ext, ext2: string;
   isCiftiNii, HasOverlays: boolean;
begin
  result := false;
  if (not FileExistsF(FileName)) and (length(tempIntensityLUT) < 1) then exit;
  FileMode := fmOpenRead;
  nOverlays := 1;
  isCiftiNii := false;
  ext := UpperCase(ExtractFileExt(Filename));
  ext2 := UpCaseExt2(FileName); // "file.gii.dset" -> ".GII.DSET"
  if (ext = '.NII') then
     isCiftiNii := isCIfTI(FileName);
  if (length(vertices) < 3) then begin
     if isCiftiNii then
       showmessage('Unable to load overlay: load background mesh (.surf.gii) first')
     else
         showmessage('Unable to load overlay: load background mesh first');
     exit; //load background first
  end;
  if (OpenOverlays >= kMaxOverlays) then begin
     showmessage('Unable to add overlay: too many overlays open');
     exit;
  end;
  if (ext = '.ANNOT') then
     if LoadAnnot(FileName) then begin
        result := true;
        isRebuildList := true;
        exit;
     end;
  if (ext = '.GCS') then
     if LoadGcs(FileName) then exit; //not supported - but inform user
  OpenOverlays := OpenOverlays + 1;
  Overlay[OpenOverlays].volumes := 1;
  Overlay[OpenOverlays].CurrentVolume := 1;
  setlength(Overlay[OpenOverlays].intensity,0);
  Overlay[OpenOverlays].OpacityPercent:= kLUTopaque;
  Overlay[OpenOverlays].filename  := ExtractFilename(FileName);
  Overlay[OpenOverlays].LUTindex := SetLutIndex(OpenOverlays);
  Overlay[OpenOverlays].LUT := UpdateTransferFunction (Overlay[OpenOverlays].LUTindex, Overlay[OpenOverlays].LUTinvert);
  if (Filename = '') and (length(tempIntensityLUT) > 0) then begin
     errorString := LoadAtlasMapCore(OpenOverlays, tempIntensityLUT);
     if (errorString <> '') then begin
       OpenOverlays := OpenOverlays - 1;
       setlength(tempIntensityLUT,0);
       exit;
     end;
    setlength(tempIntensityLUT,0);
    Overlay[OpenOverlays].filename:= 'AtlasMap';
  end;
  if (ext = '.ROX') then begin
     if not LoadAtlasMap(FileName, OpenOverlays) then begin
       OpenOverlays := OpenOverlays - 1;
       exit;
     end;
  end;
  if (ext = '.MZ3') then begin
     if not LoadMz3(FileName, OpenOverlays) then begin //unable to open as an overlay - perhaps vertex colors?
       OpenOverlays := OpenOverlays - 1;
        exit;
     end;
  end;
  if (ext = '.COL') then begin
     nOverlays := LoadCol(FileName, OpenOverlays, 1);
     if nOverlays < 1 then begin //unable to open as an overlay - perhaps vertex colors?
       OpenOverlays := OpenOverlays - 1;
        exit;
     end;
     if (nOverlays > 1) and ( (OpenOverlays+nOverlays-1) <= kMaxOverlays) then begin
        for i := 2 to nOverlays do begin //GIfTI files can store multiple overlays - see NITRC Caret GIfTI examples
            SetOverlayDescriptives(OpenOverlays);
            OpenOverlays := OpenOverlays + 1;
            LoadCol(FileName, OpenOverlays, i);
            Overlay[OpenOverlays].filename  := ExtractFilename(FileName)+':'+inttostr(i);
            Overlay[OpenOverlays].LUTindex := SetLutIndex(OpenOverlays);
            Overlay[OpenOverlays].OpacityPercent := kLUTinvisible;
            Overlay[OpenOverlays].LUT := UpdateTransferFunction (Overlay[OpenOverlays].LUTindex, Overlay[OpenOverlays].LUTinvert);
        end;
     end;
  end;
  if (ext = '.DPV') or (ext = '.ASC') then begin
     if not LoadDPV(FileName, OpenOverlays) then begin
       OpenOverlays := OpenOverlays - 1;
       exit;
     end;
  end;
  if (ext2 = '.NIML.DSET') then begin
    if not LoadNiml(FileName) then begin
       printf('NIML variant not supported');
       OpenOverlays := OpenOverlays - 1;
       exit;
    end;
     (*if LoadNiml(FileName) then begin
        result := true;
        SetOverlayDescriptives(OpenOverlays);
        Overlay[OpenOverlays].LUT := UpdateTransferFunction (Overlay[OpenOverlays].LUTindex, Overlay[OpenOverlays].LUTinvert); //set color scheme
        isRebuildList := true;
        exit;
     end;
     {$IFDEF UNIX}writeln('.NIML.DSET format not supported: use ConvertDset to convert to GIfTI.');  {$ENDIF}
     {$IFNDEF Darwin}Showmessage('.NIML.DSET format not supported: use ConvertDset to convert to GIfTI.'); {$ENDIF}
    OpenOverlays := OpenOverlays - 1;
    exit;  *)
  end;
  if (ext2 = '.1D.DSET') or (ext = '.1D') then begin
     if not Load1D(FileName, OpenOverlays) then begin
       OpenOverlays := OpenOverlays - 1;
       exit;
     end;

  end;
  if (ext2 = '.GII.DSET') or (ext = '.GII') or (isCiftiNii) then begin
    // for example of "filename.gii.dset" see http://andysbrainblog.blogspot.com/2012/04/suma-demo.html
     if isCiftiNii then begin
        nOverlays := loadCifti(FileName, OpenOverlays, 1, (origin.X < 0));
        Overlay[OpenOverlays].LUTindex := 1;
     end else
         nOverlays := LoadGii(FileName, OpenOverlays, 1, HasOverlays);
     if ((OpenOverlays+nOverlays+1) > kMaxOverlays) then
        nOverlays := kMaxOverlays - 1 - OpenOverlays;
     if nOverlays > 3 then
        nOverlays := 3; // <- this line constrains loading and is optional
     if (nOverlays > 1) and ( (OpenOverlays+nOverlays-1) <= kMaxOverlays) then begin
        for i := 2 to nOverlays do begin //GIfTI files can store multiple overlays - see NITRC Caret GIfTI examples
            SetOverlayDescriptives(OpenOverlays);
            OpenOverlays := OpenOverlays + 1;
            setlength(Overlay[OpenOverlays].intensity,0);
            if isCiftiNii then
               Overlay[OpenOverlays].OpacityPercent := kLUTinvisible
            else
                Overlay[OpenOverlays].OpacityPercent := kLUTopaque;
            Overlay[OpenOverlays].filename  := ExtractFilename(FileName);
            if isCiftiNii then
               nOverlays := loadCifti(FileName, OpenOverlays, i, (origin.X < 0))
            else
                LoadGii(FileName, OpenOverlays, i, HasOverlays);
            if isCiftiNii then
               Overlay[OpenOverlays].LUTindex := 1
            else if OpenOverlays > 12 then
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
  if not isCiftiNii then begin
    if (ext = '.NII') or (ext = '.IMG') or (ext = '.HDR')  or (ext = '.GZ')  or (ext = '.VOI') or (ext = '.NHDR')
      or (ext = '.NRRD') or (ext = '.HEAD') or (ext = '.MGH')  or (ext = '.MGZ')  or (ext = '.MHA') or (ext = '.MHD') then begin
         if not LoadVoxel2Vertex(FileName, OpenOverlays) then
            LoadNii(FileName, OpenOverlays, lLoadSmooth);
      end;
  end;
  //if (length(overlay[OpenOverlays].intensity) < 1 ) and (PosEx('.thickness.',FileName) > 1) and (IsCurv(FileName))  then begin
  if (length(overlay[OpenOverlays].intensity) < 1 ) and (PosEx('.curv',FileName) > 1) and (IsCurv(FileName))  then begin
      LoadCurv(FileName, OpenOverlays);
      if (length(overlay[OpenOverlays].intensity) > 0 ) then
         Overlay[OpenOverlays].LUTindex := 15;//CURV file
  //end else if (length(overlay[OpenOverlays].intensity) < 1 ) and (PosEx('.thickness',FileName) > 1) and (IsCurv(FileName))  then begin
  end else if (length(overlay[OpenOverlays].intensity) < 1 ) and (IsCurv(FileName))  then begin //thickness, area
     LoadCurv(FileName, OpenOverlays); //CAT12: "rh.thickness.cr" if CURV format file for "rh.central.cr.gii"  http://www.neuro.uni-jena.de/cat/
     //'.sulc'
     if (PosEx('.sulc',FileName) > 1) then begin
        isLUTinvert := true;
        Overlay[OpenOverlays].LUTindex := 0;
     end;

  end;
  if (length(overlay[OpenOverlays].intensity) < 1 )  then
       LoadW(FileName, OpenOverlays);

  if  (length(overlay[OpenOverlays].intensity) < 1 ) then begin
      LoadMeshAsOverlay(FileName, OpenOverlays);
  end else
      SetOverlayDescriptives(OpenOverlays);
  if isFreeSurferLUT(Overlay[OpenOverlays].LUTindex) then begin //CURV file
    Overlay[OpenOverlays].windowScaledMin := -0.6;
    Overlay[OpenOverlays].windowScaledMax := 0.6;
    //Overlay[OpenOverlays].windowScaledMin:= 0.0;//-0.1;
    //Overlay[OpenOverlays].windowScaledMax := 1.0;//0.8;
  end;
  if (isLUTinvert) then Overlay[OpenOverlays].LUTinvert := true;
  Overlay[OpenOverlays].LUT := UpdateTransferFunction (Overlay[OpenOverlays].LUTindex, Overlay[OpenOverlays].LUTinvert); //set color scheme
  isRebuildList := true;
  //showmessage(format('%d %d', [length(Overlay[OpenOverlays].intensity), length(Overlay[OpenOverlays].vertices) ]));
  result :=  (length(Overlay[OpenOverlays].intensity) > 0) or (length(Overlay[OpenOverlays].vertices) > 0);
  if not result then
     OpenOverlays := OpenOverlays - 1;
end; // LoadOverlay()

procedure TMesh.CloseOverlaysCore;
var
   i: integer;
begin
     for i := kMinOverlayIndex to kMaxOverlays do begin
         setlength(overlay[i].intensity,0);
         setlength(overlay[i].faces,0);
         setlength(overlay[i].vertices,0);
         setlength(overlay[i].vertexRGBA,0);
         setlength(overlay[i].vertexAtlas,0);
         overlay[i].atlasMaxIndex := 0;
         setlength(overlay[i].atlasTransparentFilter,0);
         setlength(overlay[i].atlasHideFilter,0); //atlas show these atlas regions, e.g. if [7, 8, 22] then only regions 7,8 and 22 will be visible
     end;
     OpenOverlays := 0;
     setlength(vertexRGBA,0);
end; // CloseOverlaysCore()

procedure TMesh.CloseOverlays;
begin
  CloseOverlaysCore;
  isRebuildList := true;
end; // CloseOverlays()

procedure TMesh.Close;
begin
  CloseOverlays;
  setlength(faces, 0);
  setlength(vertices, 0);
  setlength(nodes,0);
  setlength(edges,0);
  setlength(vertexRGBA,0);
  setlength(vertexAtlas,0);
  AtlasMaxIndex := 0;
  setlength(atlasHideFilter,0);
  setlength(atlasTransparentFilter,0);
  setlength(tempIntensityLUT,0); //only used to load without external files - flushed after LoadOverlay
end; // Close()

destructor TMesh.Destroy;
begin
  self.Close;
  // Release memory, if glcontext obtained
  //would be nice to call
  // glDeleteLists(displayList, 1);
  inherited;
end; // Destroy()

end.
