unit meshify_simplify;

{$Include opts.inc}
{$mode objfpc}{$H+}
// OLD_SIMPLIFY ported from Stan Melax's progmesh.cpp
// https://github.com/melax/sandbox/blob/master/bunnylod/progmesh.cpp
// http://dev.gameres.com/program/visual/3d/PolygonReduction.pdf
//Roughly equivalent to Matlab's "reducepatch(F, V, R)"

interface

uses
  Classes, SysUtils, matmath, dialogs, define_types;

function ReducePatch( var faces: TFaces; var vertices: TVertices; R: single):boolean;
//UnifyVertices/UnifyVertices2/UnifyVertices3 virtually identical, 2 and 3 use a bit less RAM, 2 collapses vertex colors
procedure UnifyVertices2(var faces: TFaces;  var vertices: TVertices; var vRGBA : TVertexRGBA);//merge identical vertices, i.e. ClusterVertex with Radius = 0
procedure UnifyVertices3(var faces: TFaces;  var vertices: TVertices);//merge identical vertices, i.e. ClusterVertex with Radius = 0
procedure UnifyVertices(var faces: TFaces;  var vertices: TVertices); //merge identical vertices, i.e. ClusterVertex with Radius = 0
procedure ClusterVertex( var faces: TFaces; var vertices: TVertices; Radius: single); //merge nearby vertices

implementation

{$IFNDEF OLD_SIMPLIFY}
uses  meshify_simplify_quadric;
{$ENDIF}

(*procedure SmoothVertices (var lMesh: TMesh);
//adjust each vertex to have the average position of all connected vertices
var
  vNorm : array of TPoint3f;
  vNum : array of integer;
  fNorm : TPoint3f;
  i : integer;
begin
  if (length(lMesh.vertices) < 4) or (length(lMesh.faces) < 2) then exit;
  setlength(vNorm, length(lMesh.vertices));
  setlength(vNum, length(lMesh.vertices));
  fNorm := ptf(0,0,0);
  for i := 0 to (length(lMesh.vertices)-1) do begin
    vNorm[i] := fNorm;
    vNum[i] := 0;
  end;
  for i := 0 to (length(lMesh.faces)-1) do begin
    vectorAdd(vNorm[lMesh.faces[i].X] , lMesh.vertices[lMesh.faces[i].X]);
    vectorAdd(vNorm[lMesh.faces[i].X] , lMesh.vertices[lMesh.faces[i].Y]);
    vectorAdd(vNorm[lMesh.faces[i].X] , lMesh.vertices[lMesh.faces[i].Z]);
    inc(vNum[lMesh.faces[i].X],3);

    vectorAdd(vNorm[lMesh.faces[i].Y] , lMesh.vertices[lMesh.faces[i].X]);
    vectorAdd(vNorm[lMesh.faces[i].Y] , lMesh.vertices[lMesh.faces[i].Y]);
    vectorAdd(vNorm[lMesh.faces[i].Y] , lMesh.vertices[lMesh.faces[i].Z]);
    inc(vNum[lMesh.faces[i].Y],3);

    vectorAdd(vNorm[lMesh.faces[i].Z] , lMesh.vertices[lMesh.faces[i].X]);
    vectorAdd(vNorm[lMesh.faces[i].Z] , lMesh.vertices[lMesh.faces[i].Y]);
    vectorAdd(vNorm[lMesh.faces[i].Z] , lMesh.vertices[lMesh.faces[i].Z]);
    inc(vNum[lMesh.faces[i].Z],3);
  end;
  for i := 0 to (length(lMesh.vertices)-1) do begin
    if (vNum[i] > 0) then begin
       lMesh.vertices[i].X := vNorm[i].X / vNum[i];
       lMesh.vertices[i].Y := vNorm[i].Y / vNum[i];
       lMesh.vertices[i].Z := vNorm[i].Z / vNum[i];
    end;
  end;
end;  *)

type
TSortTypeI = uint32; //can be integer, single, double, etc
TSortI = record
   index:  integer;
   value: TSortTypeI;
end;
TSortArrayI = array of TSortI;
TSortType = single; //can be integer, single, double, etc
TSort = record
   index:  integer;
   value: TSortType;
end;
TSortArray = array of TSort;

{$DEFINE QSORT} //QuickSort is fast and simple, but very recursive
{$IFDEF QSORT}
//http://stackoverflow.com/questions/24335585/quicksort-drama

procedure QuickSortI(left, right: integer; var s: TSortArrayI);
// left:      Index des 1. Elements, right: Index des letzten Elements
var
  l, r, lswap: integer;
  pivot: TSortType;
begin
  if (right > left) then begin
    l := left;
    r := right;
    pivot := s[s[(right + left) div 2].index].value;
    while (l < r) do begin
      while s[s[l].index].value < pivot do
        l := l + 1;
      while s[s[r].index].value > pivot do
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
      QuickSortI(left, r, s);
    if (right > l) then
      QuickSortI(l, right, s);
  end;
end;
procedure QuickSort(left, right: integer; var s: TSortArray);
// left:      Index des 1. Elements, right: Index des letzten Elements
var
  l, r, lswap: integer;
  pivot: TSortType;
begin
  if (right > left) then begin
    l := left;
    r := right;
    pivot := s[s[(right + left) div 2].index].value;
    while (l < r) do begin
      while s[s[l].index].value < pivot do
        l := l + 1;
      while s[s[r].index].value > pivot do
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

{$ELSE}
//http://delphi.wikia.com/wiki/Heapsort
procedure HeapSort(var s: TSortArray);
procedure Sink(indx, Arraylength: integer);
var
   item, leftChild, sinkindx, rightChild, parent: integer;
   done: boolean;
begin
   sinkindx := indx;
   item := s[indx].index; // s[s[indx].index].value
   done := False;
   while not done do begin // search sink-path and move up all items
      leftChild := ((sinkindx) * 2) + 1;
      rightChild := ((sinkindx + 1) * 2);
      if rightChild <= Arraylength then begin
         if s[s[leftChild].index].value < s[s[rightChild].index].value then begin
            s[sinkindx].index := s[rightChild].index;  // s[s[leftChild].index].value
            sinkindx := rightChild;
         end else begin
           s[sinkindx].index  := s[leftChild].index;
           sinkindx := leftChild;
         end;
      end else begin
         done := True;

         if leftChild <= Arraylength then begin
            s[sinkindx].index := s[leftChild].index;
            //Data[sinkindx] := Data[leftChild];
            sinkindx := leftChild;
         end;
      end;
   end;
   // move up current Item
   s[sinkindx].index := item;
   done := False;
   while not done do begin
      parent := Trunc((sinkindx - 1) / 2);
      if (s[s[parent].index].value < s[s[sinkindx].index].value) and (parent >= indx) then begin
         item := s[parent].index;
         s[parent].index := s[sinkindx].index;
         s[sinkindx].index := item;
         sinkindx := parent;
      end
      else
         done := True;
   end;
end;

var
x, b: integer;
begin
  // first make it a Heap
  for x := Trunc((High(s) - 1) / 2) downto Low(s) do
     sink(x, High(s));

  // do the ButtomUpHeap sort
  for x := High(s) downto Low(s) + 1 do begin
     b := s[x].index;
     s[x].index := s[Low(s)].index;
     s[Low(s)].index := b;
     sink(Low(s), x - 1);
  end;
end;
{$ENDIF}

procedure SortArrayIndices(var s: TSortArray); //sorts indices, not values!
var
 i : integer;
begin
     if length(s) < 1 then exit;
     for i := 0 to (length(s)-1) do  //set indices
         s[i].index := i;
     {$IFDEF QSORT}
     quicksort(low(s), high(s), s);
     {$ELSE}
     HeapSort(s);
     {$ENDIF}
end;

procedure SortArrayIndicesI(var s: TSortArrayI); //sorts indices, not values!
var
 i : integer;
begin
     if length(s) < 1 then exit;
     for i := 0 to (length(s)-1) do  //set indices
         s[i].index := i;
     quicksortI(low(s), high(s), s);
end;


function RemoveDegenerateTriangles(var faces: TFaces): integer;
var
	nOK, n,i: integer;
	f: TFaces;
begin
	result := 0; //EXIT_SUCCESS - no change
	n := length(faces);
	if n < 1 then exit;
	nOK := 0;
	for i := 0 to (n-1) do
		if (faces[i].x <> faces[i].y) and (faces[i].x <> faces[i].z) and (faces[i].y <> faces[i].z) then
			nOK := nOK + 1;
	//printf(format(' %d degenerate triangles', [n - nOK]));
	if (nOK = n) then exit;
	if (nOK = 0) then exit; //nothing survives!
	result := n - nOK; //report number of faces removed
	setlength(f,n);
	f := Copy(faces, Low(faces), Length(faces));
	setlength(faces,nOK);
	nOK := 0;
	for i := 0 to (n-1) do
		if (faces[i].x <> faces[i].y) and (faces[i].x <> faces[i].z) and (faces[i].y <> faces[i].z) then begin
			faces[nOK] := f[i];
			nOK := nOK + 1;
		end;
end; //end RemoveDegenerateTriangles()

procedure UnifyVertices3(var faces: TFaces;  var vertices: TVertices);
var vRGBA : TVertexRGBA;
begin
     vRGBA := nil;
     UnifyVertices2(faces, vertices, vRGBA);
end;
//{$DEFINE FX}
{$IFDEF FX}
//something is not right here - try STL created by slicer
procedure UnifyVertices2(var faces: TFaces;  var vertices: TVertices;  var vRGBA : TVertexRGBA);//merge identical vertices, i.e. ClusterVertex with Radius = 0
//{$DEFINE HASH3D} //hash3d Teschner http://www.beosil.com/download/CollisionDetectionHashing_VMV03.pdf
//HASH3D slower (820ms vs 480)
label
  666;
var
 oldRGBA : TVertexRGBA;
 s: TSortArrayI;
 j,i, nv,nvPost,nf: integer;
 pt: TPoint3f;
 face: TPoint3i;
 oldVert: TVertices;
 oldFaces: TFaces;
 remap: TInts;
begin
   nv := length(vertices);
   if (nv < 3) then exit;
   setlength(s,nv); //indices sorted by Z-position
   setlength(remap,nv); //maps old index to new index
   for i := 0 to (nv -1) do begin
       {$IFDEF HASH3D}
       s[i].value := uint32(uint32(vertices[i].Z) * 73856093) xor uint32(uint32(vertices[i].Y) * 19349663) xor uint32(uint32(vertices[i].X) * 83492791);
       {$ELSE}
       s[i].value := uint32(vertices[i].Z);//, 19349663, 83492791
       {$ENDIF}
       remap[i] := -1;
   end;
   SortArrayIndicesI(s);
   nvPost := 0;
      for i := 0 to (nv - 1) do begin
         if remap[s[i].index] < 0 then begin
             pt := vertices[s[i].index];
             j := i;
             while (j < nv) and (vertices[s[j].index].Z = pt.Z) do begin  //i.Z==j.Z
                   {$IFDEF HASH3D}
                   if (vertices[s[j].index].X = pt.X) and (vertices[s[j].index].Y = pt.Y) and (vertices[s[j].index].Z = pt.Z) then//i.X==j.X, i.Y==j.Y
                   {$ELSE}
                   if (vertices[s[j].index].X = pt.X) and (vertices[s[j].index].Y = pt.Y) then//i.X==j.X, i.Y==j.Y
                   {$ENDIF}
                      remap[s[j].index] := nvPost;
                   j := j + 1;
             end;
             nvPost := nvPost + 1; //yet another vertex survives
          end; //not yet clustered
      end; //for each vertex
   if nvPost = nv then goto 666; //no clusters - no change!
   //remap vertices
   oldVert := Copy(vertices, Low(vertices), MaxInt);
   for i := 0 to (nv -1) do
       vertices[remap[i]] := oldVert[i];
   setlength(vertices, nvPost);
   oldVert := nil;
   //remap vertex colors
   if length(vRGBA) = nv then begin
      oldRGBA := Copy(vRGBA, 0, MaxInt);
      for i := 0 to (nv-1) do
          vRGBA[remap[i]]:= oldRGBA[i];
      setlength(vRGBA, nvPost);
      oldRGBA := nil;
   end;
   //remap faces to new vertices
   oldFaces := Copy(faces, Low(faces), Length(faces));
   nf := 0;
   for i := 0 to (length(oldFaces) - 1) do begin
        face.X := remap[oldFaces[i].X];
        face.Y := remap[oldFaces[i].Y];
        face.Z := remap[oldFaces[i].Z];
        if (face.X = face.Y) or (face.X = face.Z) or (face.Y = face.Z) then continue;  //exclude degenerate faces
        Faces[nf] :=  face;
        nf := nf + 1;
   end;
   oldFaces := nil;
   setlength(faces, nf);
   //RemoveDegenerateTriangles(faces);  //not required, done as we built new faces
666:
    s  := nil;
    remap := nil;
end;

{$ELSE}
procedure UnifyVertices2(var faces: TFaces;  var vertices: TVertices;  var vRGBA : TVertexRGBA);//merge identical vertices, i.e. ClusterVertex with Radius = 0
label
  666;
var
  oldRGBA : TVertexRGBA;
 s: TSortArray;
 j,i, nv,nvPost,nf: integer;
 pt: TPoint3f;
 face: TPoint3i;
 oldVert: TVertices;
 oldFaces: TFaces;
 remap: TInts;
begin
   nv := length(vertices);
   if (nv < 3) then exit;
   setlength(s,nv); //indices sorted by Z-position
   setlength(remap,nv); //maps old index to new index
   for i := 0 to (nv -1) do begin
       s[i].value := vertices[i].Z;
       remap[i] := -1;
   end;
   SortArrayIndices(s);
   nvPost := 0;
      for i := 0 to (nv - 1) do begin
         if remap[s[i].index] < 0 then begin
             pt := vertices[s[i].index];
             j := i;
             while (j < nv) and (vertices[s[j].index].Z = pt.Z) do begin  //i.Z==j.Z
                   if (vertices[s[j].index].X = pt.X) and (vertices[s[j].index].Y = pt.Y) then//i.X==j.X, i.Y==j.Y
                      remap[s[j].index] := nvPost;
                   j := j + 1;
             end;
             nvPost := nvPost + 1; //yet another vertex survives
          end; //not yet clustered
      end; //for each vertex
   if nvPost = nv then goto 666; //no clusters - no change!
   //remap vertices
   oldVert := Copy(vertices, Low(vertices), MaxInt);
   for i := 0 to (nv -1) do
       vertices[remap[i]] := oldVert[i];
   setlength(vertices, nvPost);
   oldVert := nil;
   //remap vertex colors
   if length(vRGBA) = nv then begin
      oldRGBA := Copy(vRGBA, 0, MaxInt);
      for i := 0 to (nv-1) do
          vRGBA[remap[i]]:= oldRGBA[i];
      setlength(vRGBA, nvPost);
      oldRGBA := nil;
   end;
   //remap faces to new vertices
   oldFaces := Copy(faces, Low(faces), Length(faces));
   nf := 0;
   for i := 0 to (length(oldFaces) - 1) do begin
        face.X := remap[oldFaces[i].X];
        face.Y := remap[oldFaces[i].Y];
        face.Z := remap[oldFaces[i].Z];
        if (face.X = face.Y) or (face.X = face.Z) or (face.Y = face.Z) then continue;  //exclude degenerate faces
        Faces[nf] :=  face;
        nf := nf + 1;
   end;
   oldFaces := nil;
   setlength(faces, nf);
   //RemoveDegenerateTriangles(faces);  //not required, done as we built new faces
666:
    s  := nil;
    remap := nil;
end;
{$ENDIF}

procedure ClusterVertex( var faces: TFaces; var vertices: TVertices; Radius: single);
label
  666;
var
 s: TSortArray;
 j,i, nv,nc,nvPost,nf: integer;
 z, dz, dx: TSortType;
 pt,sum: TPoint3f;
 face: TPoint3i;
 newVert: TVertices;
 oldFaces: TFaces;
 radiusSqr: single;
 cluster, remap: TInts;
begin
   nv := length(vertices);
   if (nv < 3) or (Radius < 0) then exit;
   setlength(s,nv); //indices sorted by Z-position
   setlength(remap,nv); //maps old index to new index
   setlength(cluster,nv); //identifies if vertex has already been matched
   setLength(newVert, nv); //newly ordered vertices
   for i := 0 to (nv -1) do begin
       s[i].value := vertices[i].Z;
       cluster[i] := i;
       remap[i] := -1;
   end;
   SortArrayIndices(s);
   nvPost := 0;
   if Radius <= 0 then begin
      for i := 0 to (nv - 1) do begin
          if cluster[i] = i then begin //not part of previous cluster
             pt := vertices[s[i].index];
             j := i;
             while (j < nv) and (vertices[s[j].index].Z = pt.Z) do begin  //i.Z==j.Z
                   if (vertices[s[j].index].X = pt.X) and (vertices[s[j].index].Y = pt.Y) then begin//i.X==j.X, i.Y==j.Y
                      cluster[j] := nvPost;
                      remap[s[j].index] := nvPost;
                   end;
                   j := j + 1;
             end;
             newVert[nvPost] := pt;
             nvPost := nvPost + 1; //yet another vertex survives
          end; //not yet clustered
      end; //for each vertex
   end else begin //Radius > 0
     radiusSqr := sqr(Radius); //avoids calculating square-root for each comparison
     for i := 0 to (nv - 1) do begin
         if cluster[i] = i then begin //not part of previous cluster
            z := s[s[i].index].value;
            pt := vertices[s[i].index];
            sum := pt;
            dz := 0;
            j := i + 1;
            nc := 1;
            while (dz <= Radius) and (j < nv)  do begin
                  dz := abs(s[s[j].index].value - z);
                  //dx := vectorDistance(pt, vertices[s[j].index]);
                  dx := sqr(pt.X-vertices[s[j].index].X)+ sqr(pt.Y-vertices[s[j].index].Y) + sqr(pt.Z-vertices[s[j].index].Z);
                  if dx <= radiusSqr then begin
                     vectorAdd(sum, vertices[s[j].index]);
                     cluster[j] := nvPost;
                     remap[s[j].index] := nvPost;
                     nc := nc + 1;
                  end;
                  j := j + 1;
            end;
            newVert[nvPost] := vectorScale(sum, 1/nc);
            cluster[i] := nvPost;
            remap[s[i].index] := nvPost;
            nvPost := nvPost + 1; //no neighbors
         end; //not yet clustered
     end; //for each vertex
   end;
   if nvPost = nv then goto 666; //no clusters - no change!
   vertices := Copy(newVert, Low(newVert), nvPost);
   newVert := nil;
   //remap faces to new vertices
   oldFaces := Copy(faces, Low(faces), Length(faces));
   nf := 0;
   for i := 0 to (length(oldFaces) - 1) do begin
        face.X := remap[oldFaces[i].X];
        face.Y := remap[oldFaces[i].Y];
        face.Z := remap[oldFaces[i].Z];
        if (face.X = face.Y) or (face.X = face.Z) or (face.Y = face.Z) then continue;  //exclude degenerate faces
        Faces[nf] :=  face;
        nf := nf + 1;
   end;
   oldFaces := nil;
   setlength(faces, nf);
   //RemoveDegenerateTriangles(faces);  //not required, done as we built new faces
666:
    s  := nil;
    remap := nil;
    cluster := nil;
    newVert := nil;
end;

procedure UnifyVertices(var faces: TFaces;  var vertices: TVertices);
//STL format saves raw vertices, this uses a lot of RAM and makes estimating vertex normals impossible...
// http://www.mathworks.com/matlabcentral/fileexchange/29986-patch-slim--patchslim-m-
begin
     ClusterVertex(faces, vertices, 0);
end;

{$IFDEF OLD_SIMPLIFY}
type
  TPMTriangle = record
     VertexID: array [0..2] of integer;
     normal: TPoint3f;
     deleted: boolean;
   end;
  TPMVertex = record
     position: TPoint3f;
     id: integer; //place in original array
     neighbor: TInts; //adjacent vertices
     face: TInts; //adjacent triangles
     objdist: single;
     collapse: integer; // candidate vertex for collapse
     deleted : boolean;
     //locked: boolean;
  end;
  TPMtriangles = array of TPMTriangle;
  TPMvertices = array of TPMVertex;
var
  gTriangles : TPMtriangles;
  gVertices : TPMvertices;

function minF(A,B: single): single;
begin
  if A < B then
     result := A
  else
      result := B;
end;

function maxF(A,B: single): single;
begin
  if A > B then
     result := A
  else
      result := B;
end;

function ComputeEdgeCollapseCost(var u, v: TPMVertex): single;
  // if we collapse edge uv by moving u to v then how
  // much different will the model change, i.e. how much "error".
  // Texture, vertex normal, and border vertex code was removed
  // to keep this demo as simple as possible.
  // The method of determining cost was designed in order
  // to exploit small and coplanar regions for
  // effective polygon reduction.
  // Is is possible to add some checks here to see if "folds"
  // would be generated.  i.e. normal of a remaining face gets
  // flipped.  I never seemed to run into this problem and
  // therefore never added code to detect this case.
var
  edgelength, curvature, mincurv, dotprod: single;
  sides: TInts;
  j, i: integer;
begin
     edgelength := vectorLength(v.position, u.position);
     //
     curvature :=0;
  	// find the "sides" triangles that are on the edge uv
        setlength(sides, 0);
  	for i := 0 to (length(u.face) -1) do begin
          if (gTriangles[u.face[i]].VertexID[0] = v.id) or (gTriangles[u.face[i]].VertexID[1] = v.id) or (gTriangles[u.face[i]].VertexID[2] = v.id) then begin
                       setlength(sides, length(sides)+1);
                       sides[High(sides)] := u.face[i];
          end;
  	end;
  	// use the triangle facing most away from the sides
  	// to determine our curvature term
  	for i := 0 to (length(u.face)-1) do begin
  		mincurv :=1; // curve for face i and closer side to it
  		for j := 0 to (length(sides) -1) do begin
  			dotprod := vectorDot(gTriangles[u.face[i]].normal , gTriangles[sides[j]].normal);	  // use dot product of face normals.
                        mincurv := minF(mincurv,(1-dotprod)/2.0);
  		end;
  		curvature := maxF(curvature, mincurv);
  	end;
        //curvature := maxF(curvature, 0.01); // <- not in original code
  	// the more coplanar the lower the curvature term
        result := edgelength * curvature;
        //showmessage(format('%d %d %g', [u.id, v.id, result]));
end;

procedure ComputeEdgeCostAtVertex(var v: TPMVertex);
// compute the edge collapse cost for all edges that start
// from vertex v.  Since we are only interested in reducing
// the object by selecting the min cost edge at each step, we
// only cache the cost of the least cost edge at this vertex
// (in member variable collapse) as well as the value of the
// cost (in member variable objdist).
var
  i: integer;
  dist: single;
begin
  if (length(v.neighbor) < 1) or (length(v.face) < 3 ) {or (v.locked)} then begin
	  v.collapse := -1;
	  v.objdist := 1000000;
	  exit;
  end;
  v.objdist := 1000000;
  v.collapse := -1;
  // search all neighboring edges for "least cost" edge
  for i := 0 to (length(v.neighbor) - 1) do begin
    //if not gVertices[v.neighbor[i]].locked then begin
		dist := ComputeEdgeCollapseCost(v,gVertices[v.neighbor[i]]);
		if(dist < v.objdist) then begin
			v.collapse := v.neighbor[i];  // candidate for edge collapse
			v.objdist := dist;             // cost of the collapse
                end;
    //end;
  end;
end;

procedure ComputeAllEdgeCollapseCosts();
// For all the edges, compute the difference it would make
// to the model if it was collapsed.  The least of these
// per vertex is cached in each vertex object.
var
  i: integer;
begin
  for i := 0 to (length(gVertices)-1) do
         ComputeEdgeCostAtVertex(gVertices[i]);
end;

procedure AddNeighbors (var a: TPMVertex; bid, cid, faceID: integer);
var
  i, bpos, cpos: integer;

begin
     bpos := -1;
     cpos := -1;
     if length(a.neighbor) > 0 then
        for i := 0 to (length(a.neighbor)-1) do begin
            if a.neighbor[i] = bid then bpos := i;
            if a.neighbor[i] = cid then cpos := i;
        end;
     if (bpos < 0) then begin//new item
        setlength(a.neighbor, length(a.neighbor)+1);
        a.neighbor[High(a.neighbor)] := bid;
     end;
     if (cpos < 0) then begin//new item
        setlength(a.neighbor, length(a.neighbor)+1);
        a.neighbor[High(a.neighbor)] := cid;
     end;
     if faceID >= 0 then begin
        setlength(a.face, length(a.face)+1);
        a.face[High(a.face)] := faceID;
     end;
end;

procedure CleanNeighbor (n, u, v: integer);  //adjust vertices
var
  i : integer;
begin
if length(gVertices[n].neighbor) < 1 then exit;
for i := 0 to (length(gVertices[n].neighbor)-1) do
    if gVertices[n].neighbor[i]= u then
       gVertices[n].neighbor[i] := v;
end;

procedure recaclVertex (var  v: TPMVertex);  //recompute vertex
var
  i,j,k : integer;
  old: TInts;
begin
     if v.deleted then exit;
     v.deleted := true;
     if length(v.face) < 1 then exit;
     old := Copy(v.face, low(v.face), length(v.face));
     setlength(v.face,0);
     for i := 0 to (length(old)-1) do begin
         if not gTriangles[old[i]].deleted then begin
            setlength(v.face,length(v.face)+1);
            v.face[High(v.face)] := old[i];
         end;
     end;
     setlength(v.neighbor,0);
     if length(v.face) < 1 then exit;
     for i := 0 to (length(v.face)-1) do begin
            if gTriangles[v.face[i]].VertexID[0] = v.id then
               j := gTriangles[v.face[i]].VertexID[1]
            else
               j := gTriangles[v.face[i]].VertexID[0];
            if gTriangles[v.face[i]].VertexID[2] = v.id then
               k := gTriangles[v.face[i]].VertexID[1]
            else
               k := gTriangles[v.face[i]].VertexID[2];
            AddNeighbors(v, j, k, -1);
     end;
     if length(v.neighbor) < 1 then exit;
     v.deleted := false;
     ComputeEdgeCostAtVertex(v);
end;

(*procedure PruneNeighbor (var  n: TPMVertex; u, v: integer);  //adjust vertices
var
  i : integer;
  oldNeighbor: TInts; //adjacent vertices
begin
  if length(n.neighbor) < 1 then exit;
  oldNeighbor := Copy(n.neighbor, low(n.neighbor), length(n.neighbor));
  setlength(n.neighbor, 0);
  for i := 0 to (length(oldNeighbor)-1) do begin
      if (oldNeighbor[i] <> u) and (oldNeighbor[i] <> v) then begin
         setlength(n.neighbor,length(n.neighbor)+1);
         n.neighbor[High(n.neighbor)] := oldNeighbor[i];
      end;
  end;
  if length(n.neighbor) = 0 then
     n.deleted := true;
 ComputeEdgeCostAtVertex(n);
end;    *)


procedure Collapse(var  u, v: TPMVertex);
	// Collapse the edge uv by moving vertex u onto v
	// Actually remove tris on uv, then update tris that
	// have u to have v, and then remove u.
var
  tmp: TInts;
  k,j,i,numU,numV: integer;
begin
  u.deleted := true;
  if (v.deleted) then begin // u is a vertex all by itself so just delete it
       showmessage(format('error deleting %d, as %d already deleted!', [u.id, v.id]) );
       exit;
  end;
  //now clean up faces
  numU := length(u.face);
  numV := length(v.face);
  setlength(tmp,numU+numV);
  for i := 0 to (numU-1) do
      tmp[i] := u.face[i];
  for i := 0 to (numV-1) do
      tmp[i+numU] := v.face[i];
  //clean up neighbors
  setlength(u.neighbor,0);
  setlength(v.neighbor,0);
  setlength(u.face,0);
  setlength(v.face,0);
  for i := 0 to (numV+numU-1) do begin //adjust triangles
      k := 0;
      for j := 0 to 2 do begin
          if gTriangles[tmp[i]].VertexID[j] = u.id then
             gTriangles[tmp[i]].VertexID[j] := v.id;
          if gTriangles[tmp[i]].VertexID[j] = v.id then k := k + 1;
      end;
      if (k > 1) or (gTriangles[tmp[i]].deleted) then //delete this triangle - faces collapsed
         gTriangles[tmp[i]].deleted := true
      else if k = 1 then begin
           setlength(v.face,length(v.face)+1);
           v.face[High(v.face)] := tmp[i];
      end;
  end;
  if length(v.face) < 1 then begin
     v.deleted:= true;
     for i := 0 to (numV+numU-1) do begin //inform neighbors of removed triangle
         for j := 0 to 2 do begin
             k := gTriangles[tmp[i]].VertexID[j];
             if (k <> u.id) and (k <> v.id) then
                recaclVertex(gVertices[k]);//, u.id, v.id);
         end;
     end;
     exit;
  end;
  for i := 0 to (length(v.face)-1) do begin
         if gTriangles[v.face[i]].VertexID[0] = v.id then
            j := gTriangles[v.face[i]].VertexID[1]
         else
            j := gTriangles[v.face[i]].VertexID[0];
         if gTriangles[v.face[i]].VertexID[2] = v.id then
            k := gTriangles[v.face[i]].VertexID[1]
         else
            k := gTriangles[v.face[i]].VertexID[2];
         AddNeighbors(v, j, k, -1);
         CleanNeighbor (j, u.id, v.id);
         CleanNeighbor (k, u.id, v.id);
  end;
  ComputeEdgeCostAtVertex(v);
  for i := 0 to (length(v.neighbor)-1) do begin
      CleanNeighbor (gVertices[v.neighbor[i]].id, u.id, v.id);
      ComputeEdgeCostAtVertex(gVertices[v.neighbor[i]]);
  end;
  // recompute the edge collapse costs for neighboring vertices
end;

function MinimumCostEdge(): integer;
// Find the edge that when collapsed will affect model the least.
// This funtion actually returns a VertexID, the second vertex
// of the edge (collapse candidate) is stored in the vertex data.
// Serious optimization opportunity here: this function currently
// does a sequential search through an unsorted Array :-(
// Our algorithm could be O(n*lg(n)) instead of O(n*n)
var
  mn : single;
  i: integer;
begin
     result := 0;
     mn := 1000000;
     for i:= 0 to (length(gVertices)-1) do
         if (not gVertices[i].deleted) and (gVertices[i].objdist < mn)  and (gVertices[i].collapse >= 0) then begin
         // TO DO : no vertices should link to a deleted vertex ....
         //if (not gVertices[i].deleted) and (gVertices[i].objdist < mn) and (not gVertices[gVertices[i].collapse].deleted) and (gVertices[i].collapse >= 0) then begin
            if gVertices[gVertices[i].collapse].deleted then
               recaclVertex(gVertices[i])  //rare outcome where triangle removal orphaned a vertex
            else begin
                 result := i;
                 mn := gVertices[i].objdist;
                 if mn = 0 then exit; //no need to search further
            end;
         end;
end;

(*NEXT SECTION FOR LOCKED EDGES
type
TSortType = UInt64; //can be integer, single, double, etc
TSortArray = array of TSortType;

procedure HeapSort(var Data: TSortArray);
   procedure Sink(Index, Arraylength: integer);
   var
      item, leftChild, sinkIndex, rightChild, parent: integer;
      done: boolean;
   begin
      sinkIndex := index;
      item := Data[index];
      done := False;
      while not done do begin // search sink-path and move up all items
         leftChild := ((sinkIndex) * 2) + 1;
         rightChild := ((sinkIndex + 1) * 2);
         if rightChild <= Arraylength then begin
            if Data[leftChild] < Data[rightChild] then begin
               Data[sinkIndex] := Data[rightChild];
               sinkIndex := rightChild;
            end
            else begin
               Data[sinkIndex] := Data[leftChild];
               sinkIndex := leftChild;
            end;
         end
         else begin
            done := True;

            if leftChild <= Arraylength then begin
               Data[sinkIndex] := Data[leftChild];
               sinkIndex := leftChild;
            end;
         end;
      end;
      // move up current Item
      Data[sinkIndex] := item;
      done := False;
      while not done do begin
         parent := Trunc((sinkIndex - 1) / 2);
         if (Data[parent] < Data[sinkIndex]) and (parent >= Index) then begin
            item := Data[parent];
            Data[parent] := Data[sinkIndex];
            Data[sinkIndex] := item;
            sinkIndex := parent;
         end
         else
            done := True;
      end;
   end; //sink()
var
   x, b: integer;
begin
   // first make it a Heap
   for x := Trunc((High(Data) - 1) / 2) downto Low(Data) do
      sink(x, High(Data));
   // do the ButtomUpHeap sort
   for x := High(Data) downto Low(Data) + 1 do begin
      b := Data[x];
      Data[x] := Data[Low(Data)];
      Data[Low(Data)] := b;
      sink(Low(Data), x - 1);
   end;
end;  //HeapSort()

function AsUint64(a,b: integer): TSortType;
begin
     if a < b then
       result := (TSortType(a) shl 32) + TSortType(b)
     else
        result := (TSortType(b) shl 32) + TSortType(a);
end;

procedure lockEdge(pos: TSortType);
var
  a,b: TSortType;
begin
     a :=  pos shr 32;
     b := pos and $FFFFFFFF;
     gVertices[a].locked:= true;
     gVertices[b].locked:= true;
end;

procedure lockVertices;
var
  i,j, num_edge: integer;
  edges: TSortArray;
begin
     num_edge := length(gTriangles) * 3;
     setlength(edges, num_edge);
     j := 0;
     for i := 0 to (length(gTriangles) -1) do begin
         edges[j] := asUint64(gTriangles[i].VertexID[0], gTriangles[i].VertexID[1]) ;
         //if edges[j] = 1 then cx(gTriangles[i].VertexID[0], gTriangles[i].VertexID[1], edges[j]);
         j := j + 1;
         edges[j] := asUint64(gTriangles[i].VertexID[1], gTriangles[i].VertexID[2]) ;
         //if edges[j] = 1 then cx(gTriangles[i].VertexID[0], gTriangles[i].VertexID[1], edges[j]);
         j := j + 1;
         edges[j] := asUint64(gTriangles[i].VertexID[2], gTriangles[i].VertexID[1]) ;
         //if edges[j] = 1 then cx(gTriangles[i].VertexID[0], gTriangles[i].VertexID[1], edges[j]);
         j := j + 1;
     end;
     heapsort(edges);
     //now identify unique edges used by only a single triangle
     if edges[0] <> edges[1] then
          lockEdge(edges[0]);
     for i := 1 to (num_edge -2) do begin
         if (edges[i] <> edges[i-1]) and (edges[i] <> edges[i+1]) then
            lockEdge(edges[i]);
     end;
     if edges[num_edge-1] <> edges[num_edge-2] then
        lockEdge(edges[num_edge-1]);

     {j := 0;
     for i := 0 to (length(gVertices) -1) do
         if gVertices[i].locked then inc(j);
     showmessage(format('f=%d v=%d lockedV=%d',[ length(gTriangles), length(gVertices), j]));  }

end;    *)

(*function MinimumCostEdge2(var costSort : TSortArray; lItem: integer; lThresh: single ): integer;
// Find the edge that when collapsed will affect model the least.
var
  mn : single;
  i, ix: integer;
begin
     result := 0;
     mn := 1000000;
     for i:= lItem to (length(gVertices)-1) do
         ix := costSort[i].index;
         if (not gVertices[ix].deleted) and (gVertices[ix].objdist < mn)  and (gVertices[ix].collapse >= 0) then begin
         // TO DO : no vertices should link to a deleted vertex ....
         //if (not gVertices[i].deleted) and (gVertices[i].objdist < mn) and (not gVertices[gVertices[i].collapse].deleted) and (gVertices[i].collapse >= 0) then begin
            if gVertices[gVertices[ix].collapse].deleted then
               recaclVertex(gVertices[ix])  //rare outcome where triangle removal orphaned a vertex
            else begin
                 result := i;
                 mn := gVertices[ix].objdist;
                 if (mn < lThresh) then exit; //no need to search further

            end;
         end;
end;  *)

function ReducePatch( var faces: TFaces; var vertices: TVertices; R: single): boolean;
//note while Matlabs' ReducePatch R refers to number of faces, here we compress number of vertices
// if R is 0.2, 20% of the vertices will be kept.
var
  i,j, mn, nVertIn, nVertRemove: integer;
  //lThresh: single;
  //costSort : TSortArray;
begin
  result := false;
  nVertIn := length(vertices);
  nVertRemove := round(nVertIn * (1.0-R));
  //nVertRemove := 350;
  if (nVertRemove < 1) or ((nVertIn - nVertRemove) < 3) or (length(faces) < 1) then exit;
  //setup
  setlength(gTriangles, length(faces));
  for i := 0 to (length(faces)-1) do begin
      gTriangles[i].VertexID[0] := faces[i].X;
      gTriangles[i].VertexID[1] := faces[i].Y;
      gTriangles[i].VertexID[2] := faces[i].Z;
      gTriangles[i].deleted:= false;
      gTriangles[i].normal := getSurfaceNormal(vertices[faces[i].X], vertices[faces[i].Y], vertices[faces[i].Z]);
      vectorNormalize(gTriangles[i].normal);
  end;
  setlength(gVertices, length(vertices));
  for i := 0 to (length(vertices)-1) do begin
      gVertices[i].position := vertices[i];
      gVertices[i].id := i;
      gVertices[i].deleted := false;
      //gVertices[i].locked := false;
  end;
  for i := 0 to (length(faces)-1) do begin
      AddNeighbors(gVertices[faces[i].X], gVertices[faces[i].Y].id, gVertices[faces[i].Z].id, i);
      AddNeighbors(gVertices[faces[i].Y], gVertices[faces[i].X].id, gVertices[faces[i].Z].id, i);
      AddNeighbors(gVertices[faces[i].Z], gVertices[faces[i].X].id, gVertices[faces[i].Y].id, i);
  end;
  //lockVertices;
  //showmessage(format('Faces %d Vin %d Remove %d Unlocked %d',[length(Faces), nVertIn, nVertRemove, J]));
  ComputeAllEdgeCollapseCosts(); //set collapse and objdist
  (*setlength(costSort,nVertIn);
   for i := 0 to (nVertIn -1) do begin
       costSort[i].value := gVertices[i].objdist;
   end;
   SortArrayIndices(costSort);
   lThresh := costSort[nVertRemove].value;*) //values better than this are acceptable for deletion
  //now remove vertices...
  for i := 1 to nVertRemove do begin
      mn := MinimumCostEdge();
      //mn := MinimumCostEdge2(costSort, i-1, lThresh);
      //showmessage(format('%d = %d', [mn, gVertices[mn].collapse]));
      Collapse(gVertices[mn],gVertices[gVertices[mn].collapse]);
  end;
  j := 0;
  for i:= 0 to (length(gVertices)-1) do
      if not gVertices[i].deleted then
         j := j + 1;
  setlength(vertices, j);
  j := 0;
  for i:= 0 to (length(gVertices)-1) do begin
      gVertices[i].id := -1;
      if not gVertices[i].deleted then begin
         vertices[j] := gVertices[i].position;
         gVertices[i].id := j;
         j := j + 1;
      end;
  end;
  j := 0;
  for i := 0 to (length(gTriangles)-1) do
      if (gVertices[gTriangles[i].VertexID[0]].id >= 0) and (gVertices[gTriangles[i].VertexID[1]].id >= 0) and (gVertices[gTriangles[i].VertexID[2]].id >= 0) and (not gTriangles[i].deleted) then
            j := j + 1;
  setlength(faces, j);
  j := 0;
  for i := 0 to (length(gTriangles)-1) do begin
      if (gVertices[gTriangles[i].VertexID[0]].id >= 0) and (gVertices[gTriangles[i].VertexID[1]].id >= 0) and (gVertices[gTriangles[i].VertexID[2]].id >= 0) and (not gTriangles[i].deleted) then begin
           faces[j].X := gVertices[gTriangles[i].VertexID[0]].id;
           faces[j].Y := gVertices[gTriangles[i].VertexID[1]].id;
           faces[j].Z := gVertices[gTriangles[i].VertexID[2]].id;
           if (faces[j].X < 0) or (faces[j].Y < 0) or (faces[j].Z < 0) then begin
              showmessage(format ('mesh reduction failed %d: %d %d %d', [gTriangles[i].VertexID[0], faces[j].X,faces[j].Y,faces[j].Z]));
           end;
           if (faces[j].X >= length(vertices)) or (faces[j].Y >= length(vertices)) or (faces[j].Z >= length(vertices)) then begin
              showmessage('mesh reduction overage');
              exit;
           end;
           j := j + 1;
      end;
  end;
  result := true;
  //cleanup memory
  for i := 0 to (length(gVertices)-1) do begin
      setlength(gVertices[i].face, 0);
      setlength(gVertices[i].neighbor, 0);
  end;
  setlength(gVertices,0);
  setlength(gTriangles, 0);
end;
{$ELSE}

function ReducePatch( var faces: TFaces; var vertices: TVertices; R: single): boolean;
var
  facesTarget : integer;
begin
  result := false;
  facesTarget := round(length(faces) * R);
  if (facesTarget < 4) then begin
     Showmessage('Error: no mesh will survive such an extreme reduction.');
     exit;
  end;
  UnifyVertices(faces, vertices);  //remove duplicate vertices - see example "duplicated_vertices.obj"
  simplify_mesh(faces, vertices, facesTarget, 3, true);
  simplify_mesh_lossless(faces, vertices);
  result := true;
(*var
  msh: TSimplify;
  i, facesTarget: integer;
begin
     result := false;
 if (length(faces) < 1) or (length(vertices) < 3) then begin
    Showmessage('You need to load a mesh (File/Open) before you can simplify a mesh');
    exit;
 end;
 facesTarget := round(length(faces) * R);
 if (facesTarget < 4) then begin
    Showmessage('Error: no mesh will survive such an extreme reduction.');
    exit;
 end;
 msh := TSimplify.Create;
 setlength(msh.vertices, length(vertices));
 for i := 0 to (length(vertices)-1) do begin
        msh.vertices[i].p.X := vertices[i].X;
        msh.vertices[i].p.Y := vertices[i].Y;
        msh.vertices[i].p.Z := vertices[i].Z;
 end;
 setlength(msh.triangles, length(faces));

 for i := 0 to (length(faces)-1) do begin
        msh.triangles[i].v[0] := faces[i].X;
        msh.triangles[i].v[1] := faces[i].Y;
        msh.triangles[i].v[2] := faces[i].Z;
 end;
 msh.simplify_mesh(facesTarget, 7);
 setlength(vertices, length(msh.vertices));
 for i := 0 to (length(msh.vertices)-1) do begin
     vertices[i].X := msh.vertices[i].p.X;
     vertices[i].Y := msh.vertices[i].p.Y;
     vertices[i].Z := msh.vertices[i].p.Z;
 end;
 setlength(Faces, length(msh.triangles));
 for i := 0 to (length(msh.triangles)-1) do begin
        faces[i].X := msh.triangles[i].v[0];
        faces[i].Y := msh.triangles[i].v[1];
        faces[i].Z := msh.triangles[i].v[2];
 end;
 msh.Free;
 result := true;*)
end;

{$ENDIF}

end.

