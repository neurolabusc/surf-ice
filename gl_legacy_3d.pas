unit gl_legacy_3d;
{$Include opts.inc}
{$mode objfpc}{$H+}

interface

uses
 {$IFDEF LEGACY_INDEXING} glext,{$ENDIF}
 {$IFDEF DGL} dglOpenGL, {$ELSE DGL} {$IFDEF COREGL}glcorearb, {$ELSE} gl, {$ENDIF}  {$ENDIF DGL}
    prefs, math, Classes, SysUtils, mesh, matMath, Graphics, define_types, track;

const
  kPrimitiveRestart = 2147483647;

  {$IFDEF LEGACY_INDEXING}
  type
  TVtxNormClr = Packed Record
    vtx   : TPoint3f; //vertex coordinates
    norm   : TPoint3f; //vertex normal
    clr : TRGBA;
  end;
  const  kVert3d = '#version 120'
+#10'attribute vec3 Vert;'
+#10'attribute vec3 Norm;'
+#10'attribute vec4 Clr;'
+#10'varying vec3 vN, vL, vV;'
+#10'varying vec4 vClr, vP;'
+#10'uniform vec3 LightPos = vec3(0.0, 20.0, 30.0); //LR, -DU+, -FN+'
+#10'void main() {'
+#10'    vP = vec4(Vert, 1.0);'
+#10'    vN = normalize(gl_NormalMatrix * Norm);'
+#10'    vV = -vec3(gl_ModelViewMatrix* vP);'
+#10'    vL = normalize(gl_LightSource[0].position.xyz);'
+#10'    gl_Position = gl_ModelViewProjectionMatrix * vP;'
+#10'    vClr = Clr;'
+#10'}';
  {$ELSE}
  const  kVert3d = 'varying vec3 vN, vL, vV;'
  +#10'varying vec4 vP, vClr;'
  +#10'void main() {  '
  +#10'    gl_Position = ftransform();'
  +#10'    vN = normalize(gl_NormalMatrix * gl_Normal);'
  +#10'    vL = normalize(gl_LightSource[0].position.xyz);'
  +#10'    vV = -vec3(gl_ModelViewMatrix*gl_Vertex);'
  +#10'    vClr = gl_Color;'
  +#10'    vP = gl_Vertex;'
  +#10'}';
  {$ENDIF}

 //Blinn/Phong Shader GPLv2 (C) 2007 Dave Griffiths, FLUXUS GLSL library
 kFrag3d = 'uniform vec4 ClipPlane;'
 +#10'varying vec4 vClr;'
 +#10'varying vec3 vN, vL, vV;'
 +#10'void main() {'
 +#10' float Ambient = 0.4;'
 +#10' float Diffuse = 0.7;'
 +#10' float Specular = 0.6;'
 +#10' float Shininess = 60.0;'
 +#10' vec3 l = normalize(vL);'
 +#10' vec3 n = normalize(vN);'
 +#10' vec3 v = normalize(vV);'
 +#10' vec3 h = normalize(vL+v);'
 +#10' float diffuse = dot(vL,n);'
 +#10' vec3 a = gl_FrontMaterial.ambient.rgb;'
 +#10' a = mix(a.rgb, vClr.rgb, vClr.a);'
 +#10' vec3 d = a * Diffuse;'
  +#10' a *= Ambient;'
 +#10' 	float diff = dot(n,l);'
 +#10' 	float spec = pow(max(0.0,dot(n,h)), Shininess);'
 +#10' 	vec3 backcolor = Ambient*vec3(0.1+0.1+0.1) + d*abs(diff);'
 +#10' 	float backface = step(0.00, n.z);'
 +#10' 	gl_FragColor = vec4(mix(backcolor.rgb, a + d*diff + spec*Specular,  backface), 1.0);'
 +#10'}';


  {$IFDEF LEGACY_INDEXING}
 const  kTrackShaderIdxVert = '#version 120'
+#10'attribute vec3 Vert;'
+#10'attribute vec3 Norm;'
+#10'attribute vec4 Clr;'
 +#10'varying vec3 vN;'
 +#10'varying vec4 vClr;'
 +#10'void main() {'
 +#10'    gl_Position = gl_ModelViewProjectionMatrix * vec4(Vert, 1.0);'
 +#10'    vN = normalize(gl_NormalMatrix * Norm);'
 +#10'    vClr = Clr;'
 +#10'}';
 {$ENDIF}
  const kTrackShaderVert = 'varying vec3 vN;'
+#10'varying vec4 vClr;'
+#10'void main()'
+#10'{    '
+#10'    vN = gl_NormalMatrix * gl_Normal;'
+#10'    gl_Position = ftransform();'
+#10'    vClr = gl_Color;'
+#10'}';

const kTrackShaderFrag = 'varying vec3 vN;'
+#10'varying vec4 vClr;'
+#10'void main()'
+#10'{     '
+#10'	vec3 specClr = vec3(0.7, 0.7, 0.7);'
+#10'	vec3 difClr = vClr.rgb * 0.9;'
+#10'	vec3 ambClr = vClr.rgb * 0.1;'
+#10'	vec3 L = vec3(0.707, 0.707, 0.0);'
+#10'    vec3 n = abs(normalize(vN));'
+#10'    float spec = pow(dot(n,L),100.0);'
+#10'    float dif = dot(L,n);'
+#10'    gl_FragColor = vec4(specClr*spec + difClr*dif + ambClr,1.0);'
+#10'}';

procedure SetLighting (var lPrefs: TPrefs);
{$IFDEF LEGACY_INDEXING}
procedure BuildDisplayListIndexed(var faces: TFaces; vertices: TVertices; vRGBA: TVertexRGBA; var index_vbo, vertex_vbo: gluint; Clr: TRGBA);
//procedure SetVertexAttrib(shaderProgram, vertex_vbo: GLuint);
//procedure SetVertexAttribs(vertex_vbo: GLuint);
{$ENDIF}
function BuildDisplayList(var faces: TFaces; vertices: TVertices; vRGBA: TVertexRGBA; Clr: TRGBA): GLuint;
function BuildDisplayListStrip(Indices: TInts; Verts, vNorms: TVertices; vRGBA: TVertexRGBA; LineWidth: integer): GLuint;
procedure DrawScene(w,h: integer; isFlipMeshOverlay, isOverlayClipped, isDrawMesh, isMultiSample: boolean; var lPrefs: TPrefs; origin: TPoint3f; ClipPlane: TPoint4f; scale, distance, elevation, azimuth: single; var lMesh,lNode: TMesh; lTrack: TTrack);

implementation

uses shaderu,  gl_core_matrix;

function BuildDisplayListStrip(Indices: TInts; Verts, vNorms: TVertices; vRGBA: TVertexRGBA; LineWidth: integer): GLuint;
var
  i,j,  n: integer;
begin
     n := length(Indices);
     if (n < 2) or (length(Verts) < 2) or (length(Verts) <> length(vNorms)) or (length(Verts) <> length(vRGBA)) then exit;
     //glDeleteLists(displayList, 1);
     result := glGenLists(1);
     glNewList(result, GL_COMPILE);
     glLineWidth(LineWidth * 2);
     glDisable(GL_LINE_SMOOTH); //back face of lines look terrible with smoothing on Intel - rely on multisampling!
     {$IFDEF TUBES}
     glBegin(GL_LINE_STRIP); //glBegin(GL_LINE_STRIP_ADJACENCY);
     {$ELSE}
     glBegin(GL_LINE_STRIP);
     {$ENDIF}
     for j := 0 to (n-1) do begin
         i := Indices[j];
         if i <> kPrimitiveRestart then begin
            glColor4ub(vRGBA[i].r, vRGBA[i].g, vRGBA[i].b, vRGBA[i].a);
            glNormal3d(vNorms[i].X, vNorms[i].Y, vNorms[i].Z);
            glVertex3f(Verts[i].X, Verts[i].Y, Verts[i].Z);

         end else begin
            glEnd();
            {$IFDEF TUBES}
            glBegin(GL_LINE_STRIP); //glBegin(GL_LINE_STRIP_ADJACENCY);
            {$ELSE}
            glBegin(GL_LINE_STRIP);
            {$ENDIF}
         end;
     end;
     glEnd();
     glEndList();
     glLineWidth(1);
end;

function TColorToF (C: TColor; i: integer): single;
begin
     result := 1;
     if i = 1 then result := red(C)/255;
     if i = 2 then result := green(C)/255;
     if i = 3 then result := blue(C)/255;
end;

procedure SetLighting (var lPrefs: TPrefs);
// http://www.cs.brandeis.edu/~cs155/OpenGL%20Lecture_05_6.pdf
//https://lost-contact.mit.edu/afs/su.se/i386_linux24/pkg/matlab/13/toolbox/matlab/graph3d/material.m
//https://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml
// https://github.com/Psychtoolbox-3/Psychtoolbox-3/blob/master/Psychtoolbox/PsychDemos/OpenGL4MatlabDemos/UtahTeapotDemo.m
var
  kMaterial : array [1..5] of single = (0.3, 0.6, 0.9, 120, 1.0); //shiny
  wcolor : array [1..4] of single = (1, 1, 1, 0.5);  //white
  objcolor : array [1..4] of single = (1, 1, 1, 0.5);  //white
  kaRGB, kdRGB, ksRGB : array [1..4] of single;
  i: integer;
begin
  for i := 1 to 4 do begin
    objcolor[i] := TColorToF(lPrefs.ObjColor,i);
    kaRGB[i] := objColor[i] * kMaterial[1];
    kdRGB[i] := objColor[i] * kMaterial[2];
    ksRGB[i] := (((1-kMaterial[5]) * objColor[i]) +(kMaterial[5] * wcolor[i])) * kMaterial[3];
  end;
  //glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_FALSE);
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GLint(GL_FALSE));
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glLightfv(GL_LIGHT0, GL_AMBIENT, @KaRGB);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @KdRGB);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @KsRGB);
  glShadeModel(GL_SMOOTH);
  glMaterialfv (GL_FRONT_AND_BACK, GL_AMBIENT, @objColor);
  glMaterialfv (GL_FRONT_AND_BACK, GL_DIFFUSE, @objColor);
  glMaterialfv (GL_FRONT_AND_BACK, GL_SPECULAR, @wColor);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, kMaterial[4]);
end;


{$IFNDEF DGL}  //gl.pp does not link to the glu functions, so we will write our own
procedure gluPerspective (fovy, aspect, zNear, zFar: single);
//https://www.opengl.org/sdk/docs/man2/xhtml/gluPerspective.xml
var
  i, j : integer;
  f: single;
  m : array [0..3, 0..3] of single;
begin
   for i := 0 to 3 do
       for j := 0 to 3 do
           m[i,j] := 0;
   f :=  cot(degtorad(fovy)/2);
   m[0,0] := f/aspect;
   m[1,1] := f;
   m[2,2] := (zFar+zNear)/(zNear-zFar) ;
   m[3,2] := (2*zFar*zNear)/(zNear-zFar);
   m[2,3] := -1;
   //glLoadMatrixf(@m[0,0]);
   glMultMatrixf(@m[0,0]);
   //raise an exception if zNear = 0??
end;
{$ENDIF}

procedure SetOrtho (w,h: integer; Distance, MaxDistance: single; isMultiSample, isPerspective: boolean);
const
 kScaleX  = 0.7;
var
   aspectRatio, scaleX: single;
   z: integer;
begin
 if (isMultiSample) then //and (gZoom <= 1) then
   z := 2
 else
   z := 1;
 glViewport( 0, 0, w*z, h*z );
 ScaleX := kScaleX * Distance;
 AspectRatio := w / h;
 if isPerspective then
    gluPerspective(40.0, w/h, 0.01, MaxDistance+1)
  else begin
     (*if AspectRatio > 1 then //Wide window
        glOrtho ( (-ScaleX * AspectRatio)+lPrefs.ScreenPan.X, (ScaleX * AspectRatio)+lPrefs.ScreenPan.X, -ScaleX+lPrefs.ScreenPan.Y, ScaleX+lPrefs.ScreenPan.Y, 0.0, 2.0) //Left, Right, Bottom, Top
     else //Tall window
       glOrtho ((-ScaleX)+lPrefs.ScreenPan.X, ScaleX+lPrefs.ScreenPan.X, (-ScaleX/AspectRatio)+lPrefs.ScreenPan.Y, (ScaleX/AspectRatio)+lPrefs.ScreenPan.Y, 0.0,  2.0); //Left, Right, Bottom, Top
  *)
     if AspectRatio > 1 then //Wide window
        glOrtho ( (-ScaleX * AspectRatio), (ScaleX * AspectRatio), -ScaleX, ScaleX, 0.0, 2.0) //Left, Right, Bottom, Top
     else //Tall window
       glOrtho (-ScaleX, ScaleX, (-ScaleX/AspectRatio), (ScaleX/AspectRatio), 0.0,  2.0); //Left, Right, Bottom, Top

  end;
end;


procedure DrawScene(w,h: integer; isFlipMeshOverlay, isOverlayClipped, isDrawMesh, isMultiSample: boolean; var lPrefs: TPrefs; origin: TPoint3f; ClipPlane: TPoint4f; scale, distance, elevation, azimuth: single; var lMesh,lNode: TMesh; lTrack: TTrack);
var
   clr: TRGBA;
begin
  clr := asRGBA(lPrefs.ObjColor);
   {$IFDEF DGL}
    glDepthMask(TRUE); //GL_TRUE enable writes to Z-buffer
   {$ELSE}
   glDepthMask(GL_TRUE); //GL_TRUE enable writes to Z-buffer
   {$ENDIF}
 glEnable(GL_DEPTH_TEST);
 glDisable(GL_CULL_FACE); // glEnable(GL_CULL_FACE); //check on pyramid
 glEnable(GL_BLEND);
 glClearColor(red(lPrefs.BackColor)/255, green(lPrefs.BackColor)/255, blue(lPrefs.BackColor)/255, 0); //Set background
 //glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
 glClear(GL_COLOR_BUFFER_BIT or  GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT );
 glEnable(GL_NORMALIZE);
 glMatrixMode(GL_PROJECTION);
 glLoadIdentity();
 SetOrtho (w,h,Distance, kMaxDistance, isMultiSample, lPrefs.Perspective);
 glTranslatef(lPrefs.ScreenPan.X, lPrefs.ScreenPan.Y, 0 );
 glMatrixMode (GL_MODELVIEW);
 glLoadIdentity ();
 glLightfv(GL_LIGHT0, GL_POSITION, @gShader.lightpos);
 //caption := floattostr(lightpos[1])+'x'+ floattostr(lightpos[2])+'x'+ floattostr(lightpos[3]);
 //glDisable(GL_CULL_FACE);
 //glEnable(GL_DEPTH_TEST);
 //glEnable(GL_BLEND);
 glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
 //glTranslatef(0,0, -Scale * 8);
 glScalef(0.5/Scale, 0.5/Scale, 0.5/Scale);
 if lPrefs.Perspective then
        glTranslatef(0,0, -Scale*2* Distance )
 else
     glTranslatef(0,0, -Scale*2 );
 glRotatef(90- Elevation,-1,0,0);
 glRotatef(-Azimuth,0,0,1);
 //glScalef(0.5/Scale, 0.5/Scale, 0.5/Scale);
 //origin := GetOrigin;
 glTranslatef(-origin.X, -origin.Y, -origin.Z);
 glShadeModel(GL_SMOOTH);
 setLighting (lPrefs);
 //glEnable(GL_BLEND);
 //glBlendFunc(GL_ONE, GL_ZERO);
 //glEnable(GL_BLEND);
 //glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
 //glLineWidth(0);   // GL_INVALID_VALUE is generated if width is less than or equal to 0.
 if lTrack.n_count > 0 then begin
    if lTrack.isTubes then
         RunMeshGLSL (asPt4f(2,ClipPlane.Y,ClipPlane.Z,ClipPlane.W),  lPrefs.ShaderForBackgroundOnly) //disable clip plane
     else
         RunTrackGLSL(lTrack.LineWidth, w, h, lTrack.isTubes);
   lTrack.DrawGL;
 end;
 if length(lNode.nodes) > 0 then begin
     RunMeshGLSL (asPt4f(2,ClipPlane.Y,ClipPlane.Z,ClipPlane.W),  lPrefs.ShaderForBackgroundOnly); //disable clip plane
   lNode.DrawGL(clr, clipPlane, isFlipMeshOverlay);
 end;
 if  (length(lMesh.faces) > 0) then begin
    lMesh.isVisible := isDrawMesh;
    RunMeshGLSL (ClipPlane,  false);
    if not isOverlayClipped then
       lMesh.DrawGL(clr, asPt4f(2,ClipPlane.Y,ClipPlane.Z,ClipPlane.W),isFlipMeshOverlay )
    else
        lMesh.DrawGL(clr, clipPlane, isFlipMeshOverlay);
    lMesh.isVisible := true;
 end;
end; //DrawScene

{$IFDEF LEGACY_INDEXING}
(*procedure SetVertexAttrib(shaderProgram, vertex_vbo: GLuint);
//for Legacy OpenGL: vertexattribs must be updated if either vertex_vbo or shader is updated.
//  in contrast, Modern OpenGL supports "location" in shader,  so only update when vertex_vbo is updated
var
  vertLoc, normLoc, clrLoc: GLint;
begin
  if (vertex_vbo = 0) or (shaderProgram = 0) then exit;
  glUseProgram(shaderProgram);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_vbo);
  vertLoc := glGetAttribLocation(shaderProgram, 'Vert');
  normLoc := glGetAttribLocation(shaderProgram, 'Norm');
  clrLoc := glGetAttribLocation(shaderProgram, 'Clr');
  glVertexAttribPointer(vertLoc, 3, GL_FLOAT, GL_FALSE, sizeof(TVtxNormClr), PChar(0));
  glVertexAttribPointer(normLoc, 3, GL_FLOAT, GL_TRUE, sizeof(TVtxNormClr), PChar(sizeof(TPoint3f)));
  glVertexAttribPointer(clrLoc, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(TVtxNormClr), PChar(sizeof(TPoint3f) + sizeof(TPoint3f)));
  glEnableVertexAttribArray(vertLoc);
  glEnableVertexAttribArray(normLoc);
  glEnableVertexAttribArray(clrLoc);
end;

procedure SetVertexAttribs(vertex_vbo: GLuint);
begin
     SetVertexAttrib(gShader.program3dx, vertex_vbo);
     SetVertexAttrib(gShader.programDefault, vertex_vbo);
end;*)

procedure BuildDisplayListIndexed(var faces: TFaces; vertices: TVertices; vRGBA: TVertexRGBA; var index_vbo, vertex_vbo: gluint; Clr: TRGBA);
var
  vnc: array of TVtxNormClr;
  vNorm: array of TPoint3f;
  fNorm: TPoint3f;
  i: integer;
begin
  //compute surface normals...
  setlength(vNorm, length(vertices));
  fNorm := ptf(0,0,0);
  for i := 0 to (length(vertices)-1) do
      vNorm[i] := fNorm;
  for i := 0 to (length(faces)-1) do begin //compute the normal for each face
      fNorm := getSurfaceNormal(vertices[faces[i].X], vertices[faces[i].Y], vertices[faces[i].Z]);
      vectorAdd(vNorm[faces[i].X] , fNorm);
      vectorAdd(vNorm[faces[i].Y] , fNorm);
      vectorAdd(vNorm[faces[i].Z] , fNorm);
  end;
  for i := 0 to (length(vertices)-1) do
      vectorNormalize(vNorm[i]);
  //create VBO that combines vertex, normal and color information
  setlength(vnc, length(vertices));
  //set every vertex
  for i := 0 to (length(vertices) -1) do begin
      vnc[i].vtx := vertices[i];
      vnc[i].norm := vNorm[i];
      vnc[i].clr := clr;
      //fNorm := getSurfaceNormal(vertices[faces[i].X], vertices[faces[i].Y], vertices[faces[i].Z]);
  end;
  if length(vRGBA) = length(vertices) then
     for i := 0 to (length(vertices) -1) do
         vnc[i].clr := vRGBA[i];
  //
  if (vertex_vbo <> 0) then
     glDeleteBuffers(1, @vertex_vbo);
  glGenBuffers(1, @vertex_vbo);
  glBindBuffer(GL_ARRAY_BUFFER, vertex_vbo);
  glBufferData(GL_ARRAY_BUFFER, Length(vnc)*SizeOf(TVtxNormClr), @vnc[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  if (index_vbo <> 0) then
     glDeleteBuffers(1, @index_vbo);
  glGenBuffers(1, @index_vbo);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, index_vbo);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(faces)*sizeof(TPoint3i), @faces[0], GL_STATIC_DRAW);
  vnc := nil;
end;
{$ENDIF}

function BuildDisplayList(var faces: TFaces; vertices: TVertices; vRGBA: TVertexRGBA; Clr: TRGBA): GLuint;
var
  i: integer;
  vNorm : TVertices;
  fNorm : TPoint3f;
begin
  //glDeleteLists(mesh.displayList, 1);
  if  (length(vertices) < 3) or (length(faces) < 1) then exit(0);
  setlength(vNorm, length(vertices) );
  //compute vertex normals
  fNorm := ptf(0,0,0);
  for i := 0 to (length(vertices)-1) do
    vNorm[i] := fNorm;
  for i := 0 to (length(faces)-1) do begin //compute the normal for each face
    fNorm := getSurfaceNormal(vertices[faces[i].X], vertices[faces[i].Y], vertices[faces[i].Z]);
    vectorAdd(vNorm[faces[i].X] , fNorm);
    vectorAdd(vNorm[faces[i].Y] , fNorm);
    vectorAdd(vNorm[faces[i].Z] , fNorm);
  end;
  for i := 0 to (length(vertices)-1) do
    vectorNormalize(vNorm[i]);
  //draw vertices
  result := glGenLists(1);
  glNewList(result, GL_COMPILE);
  glBegin(GL_TRIANGLES);
  if  (length(vRGBA) > 0) then begin
    for i := 0 to (length(faces)-1) do begin
       glNormal3d(vNorm[faces[i].X].X, vNorm[faces[i].X].Y, vNorm[faces[i].X].Z);
       glColor4ub(vRGBA[faces[i].X].R,vRGBA[faces[i].X].G,vRGBA[faces[i].X].B,vRGBA[faces[i].X].A);
       glVertex3f(vertices[faces[i].X].X, vertices[faces[i].X].Y, vertices[faces[i].X].Z);
       glNormal3d(vNorm[faces[i].Y].X, vNorm[faces[i].Y].Y, vNorm[faces[i].Y].Z);
       glColor4ub(vRGBA[faces[i].Y].R,vRGBA[faces[i].Y].G,vRGBA[faces[i].Y].B,vRGBA[faces[i].Y].A);
       glVertex3f(vertices[faces[i].Y].X, vertices[faces[i].Y].Y, vertices[faces[i].Y].Z);
       glNormal3d(vNorm[faces[i].Z].X, vNorm[faces[i].Z].Y, vNorm[faces[i].Z].Z);
       glColor4ub(vRGBA[faces[i].Z].R,vRGBA[faces[i].Z].G,vRGBA[faces[i].Z].B,vRGBA[faces[i].Z].A);
       glVertex3f(vertices[faces[i].Z].X, vertices[faces[i].Z].Y, vertices[faces[i].Z].Z);
   end;
  end else begin
    glColor4ub(clr.r, clr.g, clr.b,clr.a);
    for i := 0 to (length(faces)-1) do begin
        glNormal3d(vNorm[faces[i].X].X, vNorm[faces[i].X].Y, vNorm[faces[i].X].Z);
        glVertex3f(vertices[faces[i].X].X, vertices[faces[i].X].Y, vertices[faces[i].X].Z);
        glNormal3d(vNorm[faces[i].Y].X, vNorm[faces[i].Y].Y, vNorm[faces[i].Y].Z);
        glVertex3f(vertices[faces[i].Y].X, vertices[faces[i].Y].Y, vertices[faces[i].Y].Z);
        glNormal3d(vNorm[faces[i].Z].X, vNorm[faces[i].Z].Y, vNorm[faces[i].Z].Z);
        glVertex3f(vertices[faces[i].Z].X, vertices[faces[i].Z].Y, vertices[faces[i].Z].Z);
    end;
  end;
  glEnd();
  glEndList();
end;

end.

