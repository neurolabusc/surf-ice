unit glcube;
//openGL cube
{$Include opts.inc}
{$mode objfpc}{$H+}

interface

uses
  {$IFDEF COREGL}glcorearb,  gl_core_matrix, {$ELSE}gl, glext, {$ENDIF}
  shaderu, Classes, SysUtils, Graphics, OpenGLContext, math, dialogs;

type
  TGLCube = class
  private
         {$IFDEF COREGL}
         uniform_mtx: GLint;
         vbo_face2d, vao_point2d, shaderProgram: GLuint;
         {$ELSE}displayLst: GLuint;{$ENDIF}
         fAzimuth, fElevation,SizeFrac : Single;
         scrnW, scrnH: integer;
         isRedraw, isTopLeft: boolean;
         procedure SetIsTopLeft(f: boolean);
         procedure SetSize(f: single);
         procedure SetAzimuth(f: single);
         procedure SetElevation(f: single);
         procedure  ScreenSize(Width,Height: integer);
         {$IFDEF COREGL}procedure CreateStrips;{$ENDIF}
         procedure CreateCube(sz: single);
  public
    property TopLeft : boolean read isTopLeft write SetIsTopLeft;
    property Azimuth : single read fAzimuth write SetAzimuth;
    property Elevation : single read fElevation write fElevation;
    property Size : single read SizeFrac write SetSize;
    procedure Draw(Width,Height: integer); //must be called while TOpenGLControl is current context
    constructor Create(Ctx: TOpenGLControl);
    Destructor  Destroy; override;
  end;
  {$IFNDEF COREGL}var GLErrorStr : string = '';{$ENDIF}

implementation

{$IFDEF COREGL}
type
  TRGBA = packed record //Next: analyze Format Header structure
   R,G,B,A : byte;
  end;
  TPoint3f = Packed Record
    x,y,z: single;
  end;

TVtxClr = Packed Record
  vtx   : TPoint3f; //vertex coordinates
  clr : TRGBA;
end;

var
    g2Dvnc: array of TVtxClr;
    g2Drgba : TRGBA;
    g2DNew: boolean;
    gnface: integer;

    const
        kBlockSz = 8192;
        kVert2D ='#version 330'
    +#10'layout(location = 0) in vec3 Vert;'
    +#10'layout(location = 3) in vec4 Clr;'
    +#10'out vec4 vClr;'
    +#10'uniform mat4 ModelViewProjectionMatrix;'
    +#10'void main() {'
    +#10'    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);'
    +#10'    vClr = Clr;'
    +#10'}';
        kFrag2D = '#version 330'
    +#10'in vec4 vClr;'
    +#10'out vec4 color;'
    +#10'void main() {'
    +#10'    color = vClr;'
    +#10'}';

procedure  TGLCube.CreateStrips;
const
    kATTRIB_VERT = 0;  //vertex XYZ are positions 0,1,2
    kATTRIB_CLR = 3;   //color RGBA are positions 3,4,5,6
type
  TInts = array of integer;
var
  i: integer;
  faces: TInts;
  vbo_point : GLuint;
  //mvp : TnMat44;
  //mvpMat: GLint;
begin
  //if not isRedraw then exit;
  //nface := Length(g2Dvnc); //each face has 3 vertices
  if gnface < 1 then exit;
  if vao_point2d <> 0 then
     glDeleteVertexArrays(1,@vao_point2d);
  glGenVertexArrays(1, @vao_point2d);
  if (vbo_face2d <> 0) then
        glDeleteBuffers(1, @vbo_face2d);
  glGenBuffers(1, @vbo_face2d);
  vbo_point := 0;
  glGenBuffers(1, @vbo_point);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  glBufferData(GL_ARRAY_BUFFER, Length(g2Dvnc)*SizeOf(TVtxClr), @g2Dvnc[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  // Prepare vertrex array object (VAO)
  glBindVertexArray(vao_point2d);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  //Vertices
  glVertexAttribPointer(kATTRIB_VERT, 3, GL_FLOAT, GL_FALSE, sizeof(TVtxClr), PChar(0));
  glEnableVertexAttribArray(kATTRIB_VERT);
  //Color
  glVertexAttribPointer(kATTRIB_CLR, 4, GL_UNSIGNED_BYTE, GL_TRUE, sizeof(TVtxClr), PChar( sizeof(TPoint3f)));
  glEnableVertexAttribArray(kATTRIB_CLR);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  glDeleteBuffers(1, @vbo_point);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbo_face2d);
  setlength(faces,gnface);
  for i := 0 to (gnface-1) do
         faces[i] := i;
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, gnface*sizeof(uint32), @faces[0], GL_STATIC_DRAW);
  //glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(faces)*sizeof(TPoint3i), @faces[0], GL_STATIC_DRAW);
  setlength(faces, 0 );
  setlength(g2Dvnc,0);
end;

procedure nglBegin(mode: integer);
begin
     g2DNew := true;
end;

procedure nglColor4ub (r,g,b,a: byte);
begin
  g2Drgba.r := round(r );
  g2Drgba.g := round(g );
  g2Drgba.b := round(b );
  g2Drgba.a := round(a );
end;

procedure nglVertex3f(x,y,z: single);
var
  i: integer;
begin
  i := gnface; //array indexed from 0 not 1
  gnface := gnface + 1;
  if (gnface+1) > length(g2Dvnc) then
     setlength(g2Dvnc, length(g2Dvnc)+kBlockSz);
   g2Dvnc[i].vtx.X := x;
   g2Dvnc[i].vtx.Y := y;
   g2Dvnc[i].vtx.Z := z;
   g2Dvnc[i].clr := g2Drgba;
   if not g2DNew then exit;
   g2DNew := false;
   g2Dvnc[gnface] := g2Dvnc[i];
   gnface := gnface + 1;
end;

procedure nglEnd;
var
  i: integer;
begin
     //add tail
     if gnface < 1 then exit;
     i := gnface; //array indexed from 0 not 1
     gnface := gnface + 1;
     if gnface > length(g2Dvnc) then
        setlength(g2Dvnc, length(g2Dvnc)+kBlockSz);
     g2Dvnc[i] := g2Dvnc[i-1];
end;

(*procedure DrawTextCore (lScrnWid, lScrnHt: integer);
begin
  nglMatrixMode(nGL_MODELVIEW);
  nglLoadIdentity;
  nglMatrixMode (nGL_PROJECTION);
  nglLoadIdentity ();
  nglOrtho (0, lScrnWid,0, lScrnHt,-10,10);
end; *)
{$ELSE} //for legacy OpenGL
procedure nglColor4ub (r,g,b,a: byte);
begin
  glColor4ub (r,g,b,a);
end;

procedure nglVertex3f(x,y,z: single);
begin
     glVertex3f(x,y,z);
end;

procedure nglVertex2f(x,y: single);
begin
     glVertex2f(x,y);
end;

procedure nglBegin(mode: integer);
begin
     glBegin(mode);
end;

procedure nglVertex2fr(x,y: single);
begin
  nglVertex3f(round(x),round(y), -1);
end;

procedure nglEnd;
begin
     glEnd();
end;
{$ENDIF}

procedure TGLCube.SetAzimuth(f: single);
begin
     if (f <> fAzimuth) then isRedraw := true;
     fAzimuth := f;
end;

procedure TGLCube.SetElevation(f: single);
begin
     if (f <> fElevation) then isRedraw := true;
     fElevation := f;
end;

procedure TGLCube.SetIsTopLeft(f: boolean);
begin
     if (f <> isTopLeft) then isRedraw := true;
     isTopLeft := f;
end;

procedure  TGLCube.SetSize(f: single);
begin
     if (f <> sizeFrac) then isRedraw := true;
     sizeFrac := f;
     if sizeFrac < 0.005 then sizeFrac := 0.005;
     if sizeFrac > 0.25 then sizeFrac := 0.25;

end;

procedure  TGLCube.ScreenSize(Width,Height: integer);
begin
     if (Width = scrnW) and (Height = scrnH) then exit;
     scrnW := Width;
     scrnH := Height;
     isRedraw := true;
end;

constructor  TGLCube.Create(Ctx: TOpenGLControl);
begin
     scrnH := 0;
     SizeFrac := 0.03;
     isRedraw := true;
     fAzimuth := 30;
     fElevation := -15;
     isTopLeft := false;
     {$IFDEF COREGL}
     vao_point2d := 0;
     vbo_face2d := 0;
     Ctx.MakeCurrent();
     shaderProgram :=  initVertFrag(kVert2D, '', kFrag2D);
     uniform_mtx := glGetUniformLocation(shaderProgram, pAnsiChar('ModelViewProjectionMatrix'));
     //glFinish;
     Ctx.ReleaseContext;
     {$ELSE}
     displayLst := 0;
     {$ENDIF}
end;

procedure MakeCube(sz: single);
//draw a cube of size sz
var
  sz2 : single;
begin
  sz2 := sz;
  {$IFDEF COREGL}
  nglColor4ub(204,204,204,255);
  {$ELSE}
  nglColor4ub(25,25,25,255);
  {$ENDIF}
  nglBegin(GL_TRIANGLE_STRIP); //* Bottom side
	nglVertex3f(-sz, -sz, -sz2);
	nglVertex3f(-sz, sz, -sz2);
	nglVertex3f(sz, -sz, -sz2);
        nglVertex3f(sz, sz, -sz2);
  nglEnd;
  {$IFDEF COREGL}
  nglColor4ub(25,25,25,255);
  {$ELSE}
  nglColor4ub(204,204,204,255);
  {$ENDIF}
  nglBegin(GL_TRIANGLE_STRIP); //* Top side
	nglVertex3f(-sz, -sz, sz2);
	nglVertex3f(sz, -sz, sz2);
        nglVertex3f(-sz, sz, sz2);
        nglVertex3f(sz, sz, sz2);
  nglEnd;
  nglColor4ub(0,0,128,255);
  nglBegin(GL_TRIANGLE_STRIP); //* Front side
    nglVertex3f(-sz, sz, -sz2);
    nglVertex3f(-sz, sz, sz2);
    nglVertex3f(sz, sz, -sz2);
    nglVertex3f(sz, sz, sz2);
  nglEnd;
  nglColor4ub(77,0,77,255);
  nglBegin(GL_TRIANGLE_STRIP);//* Back side
	nglVertex3f(-sz, -sz, -sz2);
	nglVertex3f(sz, -sz, -sz2);
	nglVertex3f(-sz, -sz, sz2);
	nglVertex3f(sz, -sz, sz2);
  nglEnd;
  nglColor4ub(153,0,0,255);
  nglBegin(GL_TRIANGLE_STRIP); //* Left side
	nglVertex3f(-sz, -sz, -sz2);
	nglVertex3f(-sz, -sz, sz2);
	nglVertex3f(-sz, sz, -sz2);
	nglVertex3f(-sz, sz, sz2);
  nglEnd;
  nglColor4ub(0,153,0,255);
  nglBegin(GL_TRIANGLE_STRIP); //* Right side
	nglVertex3f(sz, -sz, -sz2);
	nglVertex3f(sz, sz, -sz2);
	nglVertex3f(sz, -sz, sz2);
	nglVertex3f(sz, sz, sz2);
  nglEnd();
end; //MakeCube()

procedure  TGLCube.CreateCube(sz: single);
begin
  {$IFDEF COREGL}
  gnface := 0;
  setlength(g2Dvnc, 0);
  {$ELSE}
  if displayLst <> 0 then
     glDeleteLists(displayLst, 1);
  displayLst := glGenLists(1);
  glNewList(displayLst, GL_COMPILE);
  {$ENDIF}
  MakeCube(sz);

       {$IFDEF COREGL}
     CreateStrips;
     {$ELSE}
     glEndList();
     {$ENDIF}
     isRedraw := false;
end;

procedure  TGLCube.Draw(Width,Height: integer);
var
  sz: single;
{$IFDEF COREGL}
  mvp : TnMat44;
{$ENDIF}
begin
  ScreenSize(Width,Height);
  sz := ScrnW;
  if  sz > ScrnH then sz := ScrnH;
  if sz < 10 then exit;
  sz := sz * SizeFrac;
  {$IFDEF COREGL}

  nglMatrixMode(nGL_MODELVIEW);
  nglLoadIdentity;
  nglMatrixMode (nGL_PROJECTION);
  nglLoadIdentity ();
  nglOrtho (0, ScrnW,0, ScrnH,-10*sz,10*sz);
  nglTranslatef(0,0,sz*8);
  if isTopLeft then
      nglTranslatef(ScrnW - (1.8*sz), ScrnH-(1.8*sz),0)
  else
      nglTranslatef(1.8*sz,1.8*sz,0);
  nglRotatef(fElevation-90,-1,0,0);
  nglRotatef(-fAzimuth,0,0,1);
  {$ELSE}
  glUseProgram(0);
  glEnable (GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glDisable (GL_LIGHTING);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, ScrnW, 0, ScrnH,0.01,sz*4);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  //glEnable(GL_DEPTH_TEST);
  glTranslatef(0,0,-sz*2);
  if isTopLeft then
      glTranslatef(ScrnW - (1.8*sz), ScrnH-(1.8*sz),0)
  else
      glTranslatef(1.8*sz,1.8*sz,0);
  glRotatef(90-fElevation,-1,0,0);
  glRotatef(-fAzimuth,0,0,1);
  {$ENDIF}
  //glEnable( GL_MULTISAMPLE );
  if isRedraw then
     CreateCube(sz);
  glEnable(GL_CULL_FACE);
  {$IFDEF COREGL}
  glUseProgram(shaderProgram);
  mvp := ngl_ModelViewProjectionMatrix;
  glUniformMatrix4fv(uniform_mtx, 1, GL_FALSE, @mvp[0,0]); // note model not MVP!
  glBindVertexArray(vao_point2d);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbo_face2d);
  glDrawElements(GL_TRIANGLE_STRIP, gnface, GL_UNSIGNED_INT, nil);
  glBindVertexArray(0);
  glUseProgram(0);
 {$ELSE}
 glCallList(displayLst);
 {$ENDIF}
 glDisable(GL_CULL_FACE);
end;

destructor TGLCube.Destroy;
begin
  //call the parent destructor:
  inherited;
end;

end.

