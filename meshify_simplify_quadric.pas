unit meshify_simplify_quadric;
{$mode objfpc}{$H+}
interface
/////////////////////////////////////////////
//
// Mesh Simplification Tutorial
//
// (C) by Sven Forstmann in 2014
//
// License : MIT
// http://opensource.org/licenses/MIT
//
// https://github.com/sp4cerat/Fast-Quadric-Mesh-Simplification
// http://www.gamedev.net/topic/656486-high-speed-quadric-mesh-simplification-without-problems-resolved/
// http://voxels.blogspot.com/2014/05/quadric-mesh-simplification-with-source.html
// https://github.com/neurolabusc/Fast-Quadric-Mesh-Simplification-Pascal-
//
// 5/2016: Chris Rorden ported from C++ to FreePascal/Delphi

uses
	Classes, SysUtils, Math;

type
	TFloat = single; //"TFloat = single" is faster "TFloat = double" more precise

	TSymetricMatrix = array [0..9] of TFloat;

	TRef = record
		tid,tvertex: integer;
	end;

	Tvec3f = record
		X: single;
		Y: single;
		Z: single;
	end;

	TVertex = record
		p: Tvec3f;
		border, tstart,tcount: integer;
		q: TSymetricMatrix;
	end;

	TTriangle = record
		v: array[0..2] of integer;
		err: array[0..3] of TFloat;
		dirty, deleted: boolean;
		n: Tvec3f;
	end;

	TBools = array of boolean;

	TSimplify = class
		public
			vertices : array  of TVertex;
			triangles : array  of TTriangle;
			procedure simplify_mesh(target_count: integer; agressiveness : double=7);
		private
			refs : array of TRef;
			nrefs: integer;
			procedure compact_mesh;
			procedure update_mesh(iteration: integer);
			function calculate_error(id_v1, id_v2: integer; var p_result: Tvec3f): TFloat;
			function flipped(p: Tvec3f; i0, i1: integer; var v0,v1: TVertex; var deleted: TBools): boolean;
			procedure update_triangles(i0: integer; var v: TVertex; var deleted: TBools; var deleted_triangles: integer);
	end; // TSimplify

implementation

function symMat(c: TFloat): TSymetricMatrix; overload;
var
	i: integer;
begin
	for i := 0 to 9 do
		result[i] := c;
end; // symMat()

function symMat(a,b,c,d: TFloat): TSymetricMatrix; overload;
begin
	result[0] := a*a;  result[1] := a*b;  result[2] := a*c;  result[3] := a*d;
	result[4] := b*b;  result[5] := b*c; result[6] := b*d;
	result[7] :=c*c; result[8] := c*d;
	result[9] := d*d;
end; // symMat()

function symMat( m11, m12, m13, m14,  m22, m23, m24,  m33, m34, m44: TFloat): TSymetricMatrix; overload;
begin
	result[0] := m11;  result[1] := m12;  result[2] := m13;  result[3] := m14;
	result[4] := m22;  result[5] := m23;  result[6] := m24;
	result[7] := m33;  result[8] := m34;
	result[9] := m44;
end; // symMat()

function symMatAdd(n,m: TSymetricMatrix): TSymetricMatrix;
begin
	result := symMat(n[0]+m[0], n[1]+m[1], n[2]+m[2], n[3]+m[3], n[4]+m[4],
		n[5]+m[5], n[6]+m[6], n[7]+m[7], n[8]+m[8], n[9]+m[9]);
end; // symMatAdd()

function symMatDet(m: TSymetricMatrix; a11, a12, a13, a21, a22, a23, a31, a32, a33: integer): TFloat;
begin
  result := m[a11]*m[a22]*m[a33] + m[a13]*m[a21]*m[a32] + m[a12]*m[a23]*m[a31]
	     - m[a13]*m[a22]*m[a31] - m[a11]*m[a23]*m[a32]- m[a12]*m[a21]*m[a33];
end; // symMatDet()

function ptf(x,y,z: single):Tvec3f;
begin
     result.x := x;
     result.y := y;
     result.z := z;
end; // ptf()

function vCross(v1, v2: Tvec3f): Tvec3f;
begin
     result := ptf(v1.y * v2.z - v1.z * v2.y, v1.z * v2.x - v1.x * v2.z,
     	v1.x * v2.y - v1.y * v2.x);
end; // vCross()

function vSum(a,b: Tvec3f): Tvec3f;
begin
     result.X := a.X+b.X;
     result.Y := a.Y+b.Y;
     result.Z := a.Z+b.Z;
end; // vSum()

function vSubtract (a,b: Tvec3f): Tvec3f;
begin
     result.X := A.X - B.X;
     result.Y := A.Y - B.Y;
     result.Z := A.Z - B.Z;
end; // vSubtract()

procedure vNormalize(var v: Tvec3f);
var
   len: single;
begin
     len := sqrt( (v.X*v.X) + (v.Y*v.Y) + (v.Z*v.Z) );
     if len <= 0 then exit;
     v.X := v.X / len;
     v.Y := v.Y / len;
     v.Z := v.Z / len;
end; // vNormalize()

function vDot (A, B: Tvec3f): single;
begin  //dot product
     result := A.X*B.X + A.Y*B.Y + A.Z*B.Z;
end;  // vDot()

function vMult(a: Tvec3f; v: TFloat):Tvec3f;
begin
     result.X := a.X*v;
     result.Y := a.Y*v;
     result.Z := a.Z*v;
end; // vMult()

// Error between vertex and Quadric
function vertex_error(q: TSymetricMatrix; x,y,z: TFloat): TFloat;
begin
      result := q[0]*x*x + 2*q[1]*x*y + 2*q[2]*x*z + 2*q[3]*x + q[4]*y*y
           + 2*q[5]*y*z + 2*q[6]*y + q[7]*z*z + 2*q[8]*z + q[9];
end; // vertex_error()

// Error for one edge
function TSimplify.calculate_error(id_v1, id_v2: integer; var p_result: Tvec3f): TFloat;
var
  q : TSymetricMatrix;
  border: integer;
  error1,error2,error3,error, det: TFloat;
  p1, p2, p3: Tvec3f;
begin
  // compute interpolated vertex
  q := symMatAdd(vertices[id_v1].q, vertices[id_v2].q);
  border := vertices[id_v1].border + vertices[id_v2].border;
  error := 0;
  det := symMatDet(q, 0, 1, 2, 1, 4, 5, 2, 5, 7);
  if ( det <> 0) and ( border = 0) then begin
    // q_delta is invertible
    p_result.x := -1/det*(symMatDet(q,1, 2, 3, 4, 5, 6, 5, 7 , 8));	// vx = A41/det(q_delta)
    p_result.y :=  1/det*(symMatDet(q,0, 2, 3, 1, 5, 6, 2, 7 , 8));	// vy = A42/det(q_delta)
    p_result.z := -1/det*(symMatDet(q,0, 1, 3, 1, 4, 6, 2, 5,  8));	// vz = A43/det(q_delta)
    error := vertex_error(q, p_result.x, p_result.y, p_result.z);
  end else begin
    // det = 0 -> try to find best result
    p1 := vertices[id_v1].p;
    p2 := vertices[id_v2].p;
    p3 := vMult(vSum(p1, p2), 0.5);
    error1 := vertex_error(q, p1.x,p1.y,p1.z);
    error2 := vertex_error(q, p2.x,p2.y,p2.z);
    error3 := vertex_error(q, p3.x,p3.y,p3.z);
    error := min(error1, min(error2, error3));
    if (error1 = error) then p_result := p1;
    if (error2 = error) then p_result := p2;
    if (error3 = error) then p_result := p3;
  end;
  result := error;
end; // calculate_error()

procedure TSimplify.update_mesh(iteration: integer);
var
  dst, i, j, k, tstart, ofs, id: integer;
  t: ^Ttriangle;
  v: ^TVertex;
  p,n: Tvec3f;
  p3 : array[0..2] of Tvec3f;
  vcount,vids: array of integer;
begin
	if(iteration>0) then begin // compact triangles
		dst := 0;
		for i := 0 to high(triangles) do begin
			if(not triangles[i].deleted) then begin
		  		triangles[dst] := triangles[i];
			  	dst := dst + 1;
			end; //if not deleted
		end; //for each triangle
		setlength(triangles, dst);
	end; //if iteration > 0
	//
	// Init Quadrics by Plane & Edge Errors
	//
	// required at the beginning ( iteration == 0 )
	// recomputing during the simplification is not required,
	// but mostly improves the result for closed meshes
	//
	if( iteration = 0 ) then begin
		for i := 0 to high(vertices) do
		vertices[i].q := symMat(0.0);
		for i := 0 to high(triangles) do begin
			t := @triangles[i];
			for j := 0 to 2 do
				p3[j] := vertices[t^.v[j]].p;
			p3[1] := vSubtract(p3[1],p3[0]);
			p3[2] := vSubtract(p3[2],p3[0]);
			n := vCross(p3[1], p3[2]);
			vNormalize(n);
			t^.n :=n;
			for j := 0 to 2 do
				vertices[t^.v[j]].q := symMatAdd(vertices[t^.v[j]].q, symMat(n.x,n.y,n.z,-vDot(n,p3[0])));
	 	end; //for i: triangles
	 	for i := 0 to high(triangles) do begin
        	// Calc Edge Error
	     	t := @triangles[i];
            p := ptf(0,0,0);
        	for j := 0 to 2 do
        		t^.err[j] := calculate_error(t^.v[j],t^.v[(j+1) mod 3] ,p);
        	t^.err[3] := min(t^.err[0],min(t^.err[1],t^.err[2]));
	 	end; //for i: triangles
	end; //if iteration = 0
	// Init Reference ID list
    for i := 0 to high(vertices) do begin
		vertices[i].tstart := 0;
		vertices[i].tcount := 0;
	end;
	for i := 0 to high(triangles) do
		for j := 0 to 2 do
			vertices[triangles[i].v[j]].tcount := vertices[triangles[i].v[j]].tcount + 1;
	tstart := 0;
	for i := 0 to high(vertices) do begin
		vertices[i].tstart := tstart;
		tstart := tstart + vertices[i].tcount;
		vertices[i].tcount := 0;
	end;
	// Write References
	setlength(refs, length(triangles) * 3);
	nrefs := length(refs);
	for i := 0 to high(triangles) do begin
		t := @triangles[i];
		for j := 0 to 2 do begin
			v := @vertices[t^.v[j]];
			refs[v^.tstart+v^.tcount].tid := i;
			refs[v^.tstart+v^.tcount].tvertex := j;
			v^.tcount := v^.tcount + 1;
		end;
	end; //for i: triangles
	// Identify boundary : vertices[].border=0,1
	if( iteration = 0 ) then begin
		for i := 0 to high(vertices) do
			vertices[i].border := 0;
		for i := 0 to high(vertices) do begin
			v := @vertices[i];
			setlength(vcount, 0);
			setlength(vids, 0);
			for j := 0 to (v^.tcount -1) do begin
				k := refs[v^.tstart+j].tid;
				t := @triangles[k];
				for k := 0 to 2 do begin
					ofs := 0;
					id := t^.v[k];
					while ( ofs< length(vcount) ) do begin
						if (vids[ofs] = id) then break;
						ofs := ofs + 1;
					end;
					if(ofs = length(vcount)) then begin
						setlength(vcount, length(vcount)+1);
						vcount[high(vcount)] := 1;
						setlength(vids, length(vids)+1);
						vids[high(vids)] := id;
					end else
						vcount[ofs] := vcount[ofs] + 1;
				end; // for k
			end; //for j
			for j := 0 to (length(vcount)-1) do
				if vcount[j] = 1 then
					vertices[vids[j]].border := 1;
		end; //for i
	end; // if iteration = 0
end; // update_mesh()

// Finally compact mesh before exiting
procedure TSimplify.compact_mesh;
var
  dst, i, j: integer;
begin
	dst := 0;
	for i := 0 to high(vertices) do
		vertices[i].tcount := 0;
	for i := 0 to high(triangles) do begin
		if (not triangles[i].deleted) then begin
			triangles[dst] := triangles[i];
			dst := dst + 1;
			for j := 0 to 2 do
				vertices[triangles[i].v[j]].tcount := 1;
		end; //if not deleted
	end; //for i
	setlength(triangles, dst);
	dst := 0;
	for i := 0 to high(vertices) do begin
		if vertices[i].tcount <> 0 then begin
			vertices[i].tstart := dst;
			vertices[dst].p := vertices[i].p;
			dst := dst + 1;
		end;
	end; //for i
	for i := 0 to high(triangles) do
		for j := 0 to 2 do
			triangles[i].v[j] := vertices[triangles[i].v[j]].tstart;
	setlength(vertices, dst);
end; // compact_mesh()

function TSimplify.flipped(p: Tvec3f; i0, i1: integer; var v0,v1: TVertex; var deleted: TBools): boolean;
var
   k, bordercount, s, id1, id2: integer;
   t: ^Ttriangle;
   n, d1, d2: Tvec3f;
begin
	bordercount := 0;
	result := true;
	for k := 0 to (v0.tcount -1) do begin
		t := @triangles[refs[v0.tstart+k].tid];
		if(t^.deleted) then continue;
	 	s := refs[v0.tstart+k].tvertex;
		id1 := t^.v[(s+1) mod 3];
		id2 := t^.v[(s+2) mod 3];
	 	if(id1=i1) or (id2=i1) then begin// delete ?
			bordercount := bordercount + 1;
			deleted[k] := true;
			continue;
		end;
		d1 := vSubtract(vertices[id1].p, p);
	 	vNormalize(d1);
		d2 := vSubtract(vertices[id2].p, p);
		vNormalize(d2);
	 	if(abs(vDot(d1, d2))>0.999) then
			exit;
	 	n := vCross(d1, d2);
	 	vNormalize(n);
	 	deleted[k] := false;
		if(vDot(n, t^.n)<0.2) then
			exit;
	end;
	result := false;
end; // flipped()

procedure TSimplify.update_triangles(i0: integer; var v: TVertex; var deleted :TBools; var deleted_triangles: integer);
const
  kBlockSz = 4096; //re-allocate memory in chunks
var
   p: TVec3f;
   k: integer;
   r: ^TRef;
   t: ^TTriangle;
   ref: TRef;
begin
  for k := 0 to (v.tcount-1) do begin
      r := @refs[v.tstart+k];
      t := @triangles[r^.tid];
      if(t^.deleted) then continue;
      if (deleted[k]) then begin
        t^.deleted := true;
        deleted_triangles := deleted_triangles + 1;
        continue;
      end;
      t^.v[r^.tvertex] := i0;
      t^.dirty := true;
      t^.err[0] := calculate_error(t^.v[0],t^.v[1],p);
      t^.err[1] := calculate_error(t^.v[1],t^.v[2],p);
      t^.err[2] := calculate_error(t^.v[2],t^.v[0],p);
      t^.err[3] := min(t^.err[0],min(t^.err[1],t^.err[2]));
      // setlength() is costly, so we do resize the array in chunks
      ref := r^; //<- n.b. setlength() can change address of refs, so copy data to ref prior to resize!
      if (nrefs >= length(refs)) then
         setlength(refs, nrefs + kBlockSz);
      refs[nrefs] := ref;
      nrefs := nrefs + 1;
  end;
end;

procedure TSimplify.simplify_mesh(target_count: integer; agressiveness : double=7);
var
  iteration, i, j, deleted_triangles, triangle_count, i0, i1, tstart, tcount: integer;
  deleted0,deleted1: TBools;
  threshold: TFloat;
  t: ^TTriangle;
  v0, v1: ^TVertex;
  p: Tvec3f;
begin
	for i := 0 to high(triangles) do
		triangles[i].deleted := false;
	// main iteration loop
	deleted_triangles := 0;
	triangle_count := length(triangles);
	for iteration := 0 to (100-1) do begin
		// target number of triangles reached ? Then break
		//printf("iteration %d - triangles %d\n",iteration,triangle_count-deleted_triangles);
		if(triangle_count-deleted_triangles<=target_count) then break;
		// update mesh once in a while
		if(iteration mod 5 =0) then
		  update_mesh(iteration);
		// clear dirty flag
		for i := 0 to high(triangles) do
		triangles[i].dirty := false;
		//
		// All triangles with edges below the threshold will be removed
		//
		// The following numbers works well for most models.
		// If it does not, try to adjust the 3 parameters
		//
		threshold := 0.000000001*power(TFloat(iteration+3),agressiveness);
		// remove vertices & mark deleted triangles
		for i := 0 to high(triangles) do begin
			t := @triangles[i];
			if (t^.err[3]>threshold) then continue;
			if (t^.deleted) then continue;
			if (t^.dirty) then continue;
			for j := 0 to 2 do begin
				if (t^.err[j]<threshold) then begin
					i0 := t^.v[ j];
					v0 := @vertices[i0];
					i1 := t^.v[(j+1) mod 3];
					v1 := @vertices[i1];
					// Border check
					if(v0^.border <> v1^.border) then continue;
					// Compute vertex to collapse to
					calculate_error(i0,i1,p);
					setlength(deleted0, v0^.tcount);
					setlength(deleted1, v1^.tcount);
					// dont remove if flipped
					if( flipped(p,i0,i1,v0^,v1^,deleted0) ) then continue;
					if( flipped(p,i1,i0,v1^,v0^,deleted1) ) then continue;
					// not flipped, so remove edge
					v0^.p := p;
					v0^.q := symMatAdd(v1^.q, v0^.q);
					tstart := nrefs; //length(refs);
					update_triangles(i0,v0^,deleted0,deleted_triangles);
					update_triangles(i0,v1^,deleted1,deleted_triangles);
					tcount := nrefs - tstart;//length(refs)-tstart;
					if(tcount<=v0^.tcount) then begin // save ram
					  if (tcount > 0) then
						move(refs[tstart], refs[v0^.tstart], tcount * sizeof(TRef));  //Move(src,dest,count);
					end else // append
					   v0^.tstart := tstart;
					v0^.tcount := tcount;
					break;
				end; //if <theshold
			end; //loop j
			// done?
			if(triangle_count-deleted_triangles<=target_count) then  break;
		end;//for i :, each triangle
	end; //for iteration
    // clean up mesh
    compact_mesh();
end; // simplify_mesh()

end.

