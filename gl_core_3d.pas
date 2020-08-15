unit gl_core_3d;

{$mode objfpc}{$H+}
{$Include opts.inc}
interface

uses
{$IFDEF DGL} dglOpenGL, {$ELSE DGL} {$IFDEF COREGL}glcorearb, {$ELSE} gl, {$ENDIF}  {$ENDIF DGL}
  gl_core_matrix, Classes, SysUtils, mesh, matMath, Graphics, define_types, Prefs, Track, math;

//procedure LoadBufferData (var faces: TFaces; var vertices: TVertices;  var vertexRGBA: TVertexRGBA) ;
//function BuildDisplayList(var faces: TFaces; vertices: TVertices; vRGBA: TVertexRGBA): GLuint;
procedure BuildDisplayList(var faces: TFaces; vertices: TVertices; vRGBA: TVertexRGBA; var vao, vbo: gluint; Clr: TRGBA);
//procedure SetLighting (var lPrefs: TPrefs);
procedure DrawScene(w,h: integer; isFlipMeshOverlay, isOverlayClipped,isDrawMesh, isMultiSample: boolean; var lPrefs: TPrefs; origin : TPoint3f; ClipPlane: TPoint4f; scale, distance, elevation, azimuth: single; var lMesh,lNode: TMesh; lTrack: TTrack);
//procedure SetCoreUniformsX(lProg: Gluint);
procedure SetTrackUniforms (lineWidth, ScreenPixelX, ScreenPixelY: integer);
//procedure BuildDisplayListStrip(Indices: TInts; Verts, vNorms: TVertices; vRGBA: TVertexRGBA; LineWidth: integer; var vao, vbo: gluint);
procedure BuildDisplayListStrip(Indices: TInts; vertices, vNorm: TVertices; vRGBA: TVertexRGBA; vType: TInts; LineWidth: integer; var vao, vbo: gluint);

const
  kPrimitiveRestart = 2147483647;
const
kVert3d = '#version 330'
+#10'layout(location = 0) in vec3 Vert;'
+#10'layout(location = 3) in vec3 Norm;'
+#10'layout(location = 6) in vec4 Clr;'
+#10'out vec3 vN, vL, vV;'
+#10'out vec4 vClr, vP;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'uniform mat4 ModelViewMatrix;'
+#10'uniform mat3 NormalMatrix;'
+#10'uniform vec3 LightPos = vec3(0.0, 20.0, 30.0); //LR, -DU+, -FN+'
+#10'void main() {'
+#10'    vN = normalize((NormalMatrix * Norm));'
+#10'    vP = vec4(Vert, 1.0);'
+#10'    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);'
+#10'    vL = normalize(LightPos);'
+#10'    vV = -vec3(ModelViewMatrix*vec4(Vert,1.0));'
+#10'    vClr = Clr;'
+#10'}';

kFrag3d = '#version 330'
+#10' in vec4 vClr, vP;'
+#10' in vec3 vN, vL, vV;'
+#10' out vec4 color;'
+#10' uniform float Ambient = 0.5;'
+#10' uniform float Diffuse = 0.7;'
+#10' uniform float Specular = 0.2;'
+#10' uniform float Shininess = 60.0;'
+#10' uniform vec4 ClipPlane = vec4(2.0, 0.0, 0.0, 0.0);'
+#10'void main() {'
+#10' if ((ClipPlane[0] < 1.5) && (dot( ClipPlane, vP) > 0.0)) discard;'
+#10'  vec3 l = normalize(vL);'
+#10'  vec3 n = normalize(vN);'
+#10'  vec3 h = normalize(l+normalize(vV));'
+#10'  vec3 a = vClr.rgb;'
+#10'  vec3 backcolor = Ambient*vec3(0.1+0.1+0.1) + a*abs(dot(n,l))*Diffuse;'
+#10'  vec3 d = a * dot(n,l) * Diffuse;'
+#10'  a *= Ambient;'
+#10'  float s = pow(max(0.0,dot(n,h)), Shininess) * Specular;'
+#10'  float backface = step(0.00, n.z);'
+#10'  color = vec4(mix(backcolor.rgb, a + d + s,  backface), 1.0);'
+#10'}';
(*
//Blinn-Phong Shader GPLv2 (C) 2007 Dave Griffiths, FLUXUS GLSL library   - permission to distribute with the BSD project granted by author in 2015
kFrag3d = '#version 330'
+#10'in vec4 vClr, vP;'
+#10'in vec3 vN, vL, vV;'
+#10'out vec4 color;'
+#10'uniform float Ambient = 0.4;'
+#10'uniform float Diffuse = 0.7;'
+#10'uniform float Specular = 0.6;'
+#10'uniform float Roughness = 0.1;'
+#10'uniform vec4 ClipPlane = vec4(2.0, 0.0, 0.0, 0.0);'
+#10'vec3 desaturate(vec3 color, float amount) {'
+#10'    vec3 gray = vec3(dot(vec3(0.2126,0.7152,0.0722), color));'
+#10'    return vec3(mix(color, gray, amount));'
+#10'}'
+#10'void main() {'
+#10' if ((ClipPlane[0] < 1.5) && (dot( ClipPlane, vP) > 0.0)) discard;'
+#10' vec3 n = normalize(vN);'
+#10' vec3 v = normalize(vV);'
+#10' vec3 h = normalize(vL+v);'
+#10' float diffuse = dot(vL,n);'
+#10' vec3 AmbientColor = vClr.rgb;'
+#10' vec3 DiffuseColor = vClr.rgb;'
+#10' if (n.z < 0.0) { //treat backfaces differently'
+#10' 	vec3 backsurface = desaturate(AmbientColor*Ambient * 0.75 +'
+#10'          DiffuseColor*abs(diffuse)*Diffuse * 0.75, 0.5);'
+#10'  color = vec4(backsurface, 1.0);'
+#10'  return;'
+#10' }'
+#10' vec3 SpecularColor = vec3(1.0, 1.0, 1.0);'
+#10' float specular =  pow(max(0.0,dot(n,h)),1.0/(Roughness * Roughness));'
+#10' color = vec4(AmbientColor*Ambient + DiffuseColor*diffuse*Diffuse +SpecularColor*specular* Specular, 1.0);'
+#10'}';
  *)

//{$DEFINE SIMPLE_TRACK_GLSL}  //SIMPLE GLSL is fast, but CORE limits line widths to 1 pixel! the slow version fixes this
{$IFDEF SIMPLE_TRACK_GLSL}
const kTrackShaderVert = kVert3d;
const kTrackShaderGeom = '';

const kTrackShaderFrag = '#version 330'
+#10'in vec4 vClr;'
+#10'in vec3 vN;'
+#10'out vec4 color;'
+#10'void main()'
+#10'{     '
+#10'	vec3 specClr = vec3(0.7, 0.7, 0.7);'
+#10'	vec3 difClr = vClr.rgb * 0.9;'
+#10'	vec3 ambClr = vClr.rgb * 0.1;'
+#10'	vec3 L = vec3(0.707, 0.707, 0.0);'
+#10'    vec3 n = abs(normalize(vN));'
+#10'   //vec3 n = normalize(vN);'
+#10'    float spec = pow(dot(n,L),100.0);'
+#10'    float dif = dot(L,n);'
+#10'    color = vec4(specClr*spec + difClr*dif + ambClr,1.0);'
+#10'}';
{$ELSE}
 const kTrackShaderVert = '#version 330'
+#10'#define M_PI 3.1415926535897932384626433832795'
+#10'layout(location = 0) in vec3 Vert;'
+#10'layout(location = 3) in vec4 Norm;'
+#10'layout(location = 6) in vec4 Clr;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'uniform mat4 ModelViewMatrix;'
+#10'uniform mat4 ProjectionMatrix;'
+#10'uniform float Radius;'
+#10'uniform mat3 NormalMatrix;'
+#10'uniform vec3 LightPos = vec3(0.0, 20.0, 30.0); //LR, -DU+, -FN+'
+#10'out vec4 vClr;'
+#10'out vec4 vP;'
+#10'out vec3 vN;'
+#10'out float TextCordCylinder;'
+#10'out vec2 TextCordEnd;'
+#10'out float fType;'
+#10'out vec3 vLightPos;'
+#10'out mat3 Rot;'
+#10'out float RotInversion;'
+#10'out vec3 SimN;'
+#10'mat3 AlignVectors(vec3 FromVector,vec3 ToVector, inout float Inversion){'
+#10'	vec3 v = cross(FromVector,ToVector);'
+#10'	float E = M_PI/2.0;'
+#10'	float s = length(v);'
+#10'	float c = dot(FromVector,ToVector);'
+#10'	float ang = acos(c);'
+#10'	if((M_PI-ang)<E){'
+#10'		Inversion=-1.0;'
+#10'	}'
+#10''
+#10'	mat3 vX;'
+#10'	vX[0] = vec3(0,v[2],-v[1]);'
+#10'	vX[1] = vec3(-v[2],0,v[0]);'
+#10'	vX[2] = vec3(v[1],-v[0],0);'
+#10''
+#10'	mat3 final = mat3(1.0)+vX+vX*vX*(1-c)/s*s*s*s;'
+#10'	return final;'
+#10'}'
+#10'float angle(vec3 a,vec3 b){'
+#10'	float c = dot(a,b);'
+#10'	return acos(c);'
+#10'}'
+#10'void main() {'
+#10'	float rScale = 0.001;'
+#10'	float vType = Norm.a;'
+#10'	vec3 VectorPoints = normalize(NormalMatrix*Norm.xyz);'
+#10'	vec4 VertexPosition = ModelViewMatrix*vec4(Vert, 1.0);'
+#10'	vec3 vecCameraPoint = vec3(0,0,1);'
+#10'	vec4 VectorBillboard = normalize(vec4(cross(VectorPoints, vecCameraPoint),0.0));'
+#10'	float kind = mod(gl_VertexID,2);'
+#10'	float inversion = 1.0;'
+#10'	if(vType==0.0){'
+#10'		VectorBillboard = VectorBillboard*Radius*rScale;'
+#10'		Rot=AlignVectors(vec3(0.0,-1.0,0.0),VectorPoints,inversion);'
+#10'		if(kind==0.0){'
+#10'			VertexPosition = VertexPosition-VectorBillboard;'
+#10'			TextCordCylinder = 0.0;'
+#10'			SimN=normalize(-VectorBillboard.xyz);'
+#10'		}else{'
+#10'			VertexPosition = VertexPosition+VectorBillboard;'
+#10'			TextCordCylinder = 1.0;'
+#10'			SimN=normalize(VectorBillboard.xyz);'
+#10'		}'
+#10'	}else if(vType==1.0){'
+#10'		vec4 VectorEndBillboard = normalize(vec4(cross(VectorBillboard.xyz,VectorPoints.xyz),0.0))*Radius*rScale;'
+#10'		Rot=AlignVectors(vec3(0.0,1.0,0.0),VectorBillboard.xyz,inversion);'
+#10'		SimN=normalize(VectorPoints.xyz);'
+#10'		VectorBillboard = VectorBillboard*Radius*rScale;'
+#10'		if(kind==0.0){'
+#10'			VertexPosition = VertexPosition-VectorBillboard+VectorEndBillboard;'
+#10'			TextCordEnd = vec2(1.0,0.0);'
+#10'		}else{'
+#10'			VertexPosition = VertexPosition+VectorBillboard+VectorEndBillboard;'
+#10'			TextCordEnd = vec2(1.0,1.0);'
+#10'		}'
+#10'	}else if(vType==-2.0){'
+#10'		vec4 VectorEndBillboard = normalize(vec4(cross(VectorBillboard.xyz,VectorPoints.xyz),0.0))*Radius*rScale;'
+#10''
+#10'		Rot=AlignVectors(vec3(0.0,1.0,0.0),VectorBillboard.xyz,inversion);'
+#10'		SimN=normalize(VectorPoints.xyz);'
+#10''
+#10'		VectorBillboard = VectorBillboard*Radius*rScale;'
+#10'		if(kind==0.0){'
+#10'			VertexPosition = VertexPosition-VectorBillboard-VectorEndBillboard;'
+#10'			TextCordEnd = vec2(0.0,0.0);'
+#10'		}else{'
+#10'			VertexPosition = VertexPosition+VectorBillboard-VectorEndBillboard;'
+#10'			TextCordEnd = vec2(0.0,1.0);'
+#10'		}'
+#10'	}else{'
+#10'		vClr=vec4(0.0,0.0,0.0,1.0);'
+#10'	}'
+#10'	RotInversion = inversion;'
+#10'	fType = vType;'
+#10'	vClr = Clr;'
+#10'	vP = ProjectionMatrix*VertexPosition;'
+#10'	gl_Position = vP;'
+#10'    vN = normalize((NormalMatrix * Norm.xyz));'
+#10'	vLightPos=LightPos;'
+#10'}';

 const  kTrackShaderGeom = '';

 const kTrackShaderFrag = '#version 330'
+#10'in vec4 vClr;'
+#10'in vec3 vN;'
+#10'in vec4 vP;'
+#10'in float TextCordCylinder;'
+#10'in float fType;'
+#10'in vec2 TextCordEnd;'
+#10'in vec3 vLightPos;'
+#10'in mat3 Rot;'
+#10'in vec3 SimN;'
+#10'in float RotInversion;'
+#10'uniform sampler1D normalmaptexture;'
+#10'uniform sampler2D normalmaptexturesphere;'
+#10'uniform float Ambient = 0.2;'
+#10'uniform float Diffuse = 0.8;'
+#10'uniform float Specular = 0.5;'
+#10'uniform float Shininess = 60.0;'
+#10'uniform float Edge = 0.5;'
+#10'uniform float SpecularRough = 0.05;'
+#10'out vec4 color;'
+#10'void main() {'
+#10'	vec4 N;'
+#10'	vec3 NN;'
+#10'	if(fType==0.0){'
+#10'		N = texture(normalmaptexture  , TextCordCylinder);'
+#10'		NN =  Rot*(2.0*N.xyz-1.0);'
+#10'	}else{'
+#10'		N = texture(normalmaptexturesphere  , TextCordEnd);'
+#10'		if(N.a==0.0)discard;'
+#10'		NN =  Rot*(2.0*N.xyz-1.0);'
+#10'	}'
+#10'	vec3 SupX = Rot*vec3(0.0,0.0,N.z);'
+#10'	vec3 SupY = RotInversion*Rot*vec3(0.0,0.0,N.z);'
+#10'	NN.x = SupX.x+SimN.x;'
+#10'	NN.y = max(0.0,SupY.y) + SimN.y;'
+#10'	NN.z = NN.z;'
+#10'	vec3 difClr = vClr.rgb;'
+#10'	vec3 ambClr = vClr.rgb;'
+#10'	vec3 L = normalize(vLightPos);'
+#10'	vec3 n = NN;'
+#10'	float NormalSpec = pow(max(0.0,dot(vN,L)),Shininess);'
+#10'	float dif = max(0.0,dot(n,L)) * Diffuse;'
+#10'	float s = pow(max(0.0,dot(n,L)), 10.0) * Specular;'
+#10'	color = vec4(s + difClr*dif + Ambient*ambClr , 1.0);'
+#10'}';

(*const kTrackShaderVert = '#version 330'
+#10'layout(location = 0) in vec3 Vert;'
+#10'layout(location = 3) in vec3 Norm;'
+#10'layout(location = 6) in vec4 Clr;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'uniform mat3 NormalMatrix;'
+#10'out vec4 vClr;'
+#10'out vec4 vP;'
+#10'out vec3 vN;'
+#10'void main() {'
+#10'    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);'
+#10'    vP = gl_Position;'
+#10'    vClr = Clr;'
+#10'    vN = normalize((NormalMatrix * Norm));'
+#10'}';

const kTrackShaderGeom ='#version 330'
+#10'layout (triangle_strip, max_vertices = 5) out;'
+#10'layout (lines_adjacency) in;'
+#10'in vec4 vP[4];'
+#10'in vec4 vClr[4];'
+#10'in vec3 vN[4];'
+#10'uniform float Radius = 1.0;'
+#10'float THICKNESS = Radius;'
+#10'uniform vec2 ScreenPixels = vec2(1600,1600);'
+#10'out vec4 gClr;'
+#10'out vec3 gN;'
+#10'vec2 screen_space(vec4 vertex) {'
+#10'	return vec2( vertex.xy / vertex.w ) * ScreenPixels;'
+#10'}'
+#10'void main(void) {'
+#10'  vec2 p0 = screen_space( vP[0] );'
+#10'  vec2 p1 = screen_space( vP[1] );'
+#10'  vec2 p2 = screen_space( vP[2] );'
+#10'  vec2 p3 = screen_space( vP[3] );'
+#10'  vec2 v0 = normalize(p1-p0);'
+#10'  vec2 v1 = normalize(p2-p1);'
+#10'  vec2 v2 = normalize(p3-p2);'
+#10'  vec2 n0 = vec2(-v0.y, v0.x);'
+#10'  vec2 n1 = vec2(-v1.y, v1.x);'
+#10'  vec2 n2 = vec2(-v2.y, v2.x);'
+#10'  vec2 miter_a = normalize(n0 + n1);'
+#10'  vec2 miter_b = normalize(n1 + n2);'
+#10'  float kEps = 0.1;'
+#10'  float length_a = 0.0;'
+#10'  float length_b = 0.0;'
+#10'  if ( abs(dot(miter_a, n1)) > kEps)'
+#10'  	length_a = THICKNESS / dot(miter_a, n1);'
+#10'  if ( abs(dot(miter_b, n1)) > kEps)'
+#10'  	length_b = THICKNESS / dot(miter_b, n1);'
+#10'  gN = normalize(vN[1] + vN[2]);'
+#10'  gClr = vClr[1];'
+#10'  if( dot(v0,n1) > 0 ) {'
+#10'	gl_Position = vec4( (p1 - length_a * miter_a) / ScreenPixels, vP[1].z, 1.0 );'
+#10'	EmitVertex();'
+#10'    gl_Position = vec4( (p1 + THICKNESS * n1) / ScreenPixels, vP[1].z, 1.0 );'
+#10'	EmitVertex();'
+#10' } else {'
+#10'    gl_Position = vec4( (p1 - THICKNESS * n1) / ScreenPixels, vP[1].z, 1.0 );'
+#10'	EmitVertex();'
+#10'	gl_Position = vec4( (p1 + length_a * miter_a) / ScreenPixels, vP[1].z, 1.0 );'
+#10'	EmitVertex();'
+#10'  }'
+#10'  gClr = vClr[2];'
+#10'  if( dot(v2,n1) < 0 ) {'
+#10'    gl_Position = vec4( (p2 - length_b * miter_b) / ScreenPixels, vP[2].z, 1.0 );'
+#10'	EmitVertex();'
+#10'    gl_Position = vec4( (p2 + THICKNESS * n1) / ScreenPixels, vP[2].z, 1.0 );'
+#10'	EmitVertex();'
+#10'    gl_Position = vec4( (p2 + THICKNESS * n2) / ScreenPixels, vP[2].z, 1.0 );'
+#10'	EmitVertex();'
+#10'  } else {'
+#10'    gl_Position = vec4( (p2 - THICKNESS * n1) / ScreenPixels, vP[2].z, 1.0 );'
+#10'	EmitVertex();'
+#10'	gl_Position = vec4( (p2 + length_b * miter_b) / ScreenPixels, vP[2].z, 1.0 );'
+#10'	EmitVertex();'
+#10'	gl_Position = vec4( (p2 - THICKNESS * n2) / ScreenPixels, vP[2].z, 1.0 );'
+#10'	EmitVertex();'
+#10'  }'
+#10'  EndPrimitive();'
+#10'}';

const kTrackShaderFrag = '#version 330'
+#10'in vec4 gClr;'
+#10'in vec3 gN;'
+#10'out vec4 color;'
+#10'void main() {'
+#10'	vec3 specClr = vec3(0.7, 0.7, 0.7);'
+#10'	vec3 difClr = gClr.rgb * 0.9;'
+#10'	vec3 ambClr = gClr.rgb * 0.1;'
+#10'	vec3 L = vec3(0.707, 0.707, 0.0);'
+#10'    vec3 n = abs(normalize(gN));'
+#10'   	float spec = pow(dot(n,L),100.0);'
+#10'    float dif = dot(L,n);'
+#10'	color = vec4(specClr*spec + difClr*dif + ambClr,1.0);'
+#10'}';   *)

{$ENDIF}



implementation

uses shaderu;

procedure SetTrackUniforms(lineWidth, ScreenPixelX, ScreenPixelY: integer);
 var
  p , mv, mvp : TnMat44;
  n : TnMat33;
  mvpMat, mvMat, normMat, pMat, lp: GLint;
  px: array [0..1] of single;
begin
  glUseProgram(gShader.programTrackID);

  p := ngl_ProjectionMatrix;
  pMat := glGetUniformLocation(gShader.programTrackID, pAnsiChar('ProjectionMatrix'));
  glUniformMatrix4fv(pMat, 1, kGL_FALSE, @p[0,0]); // note model not MVP!
  glUniform1f(glGetUniformLocation(gShader.programTrackID, pAnsiChar('Radius')), lineWidth) ;

  mvp := ngl_ModelViewProjectionMatrix;
  mv := ngl_ModelViewMatrix;
  n :=  ngl_NormalMatrix;

  mvpMat := glGetUniformLocation(gShader.programTrackID, pAnsiChar('ModelViewProjectionMatrix'));
  mvMat := glGetUniformLocation(gShader.programTrackID, pAnsiChar('ModelViewMatrix'));
  normMat := glGetUniformLocation(gShader.programTrackID, pAnsiChar('NormalMatrix'));

  glUniformMatrix4fv(mvpMat, 1, kGL_FALSE, @mvp[0,0]);
  glUniformMatrix4fv(mvMat, 1, kGL_FALSE, @mv[0,0]);
  glUniformMatrix3fv(normMat, 1, kGL_FALSE, @n[0,0]);

  px[0] := ScreenPixelX;
  px[1] := ScreenPixelY;
  glUniform2fv(glGetUniformLocation(gShader.programTrackID, pAnsiChar('ScreenPixels')), 1, @px[0]);
  glPrimitiveRestartIndex(kPrimitiveRestart);
  glEnable(GL_PRIMITIVE_RESTART);
  {$IFNDEF TUBES}
  glUniform3f(glGetUniformLocation(gShader.programTrackID, pAnsiChar('LightPos')), gShader.lightPos.X, gShader.lightPos.Y, gShader.lightPos.Z);
  glUniform1f(glGetUniformLocation(gShader.programTrackID, pAnsiChar('Ambient')), gShader.TrackAmbient) ;
  glUniform1f(glGetUniformLocation(gShader.programTrackID, pAnsiChar('Diffuse')), gShader.TrackDiffuse) ;
  //glUniform1f(glGetUniformLocation(gShader.programTrackID, pAnsiChar('Specular')), gShader.TrackSpecular * 0.75) ; //<<<<<<<<<
  glUniform1f(glGetUniformLocation(gShader.programTrackID, pAnsiChar('Specular')), gShader.TrackSpecular) ; //full glisten

  {$ENDIF}

end;

(*procedure SetCoreUniforms(lProg: GLuint);
var
  mv, mvp : TnMat44;
  n : TnMat33;
  mvpMat, mvMat, normMat: GLint;
begin
  glUseProgram(lProg);
  //AdjustShaders(gShader);
  //uniform4f('ClipPlane',cp1,cp2,cp3,cp4)
  mvp := ngl_ModelViewProjectionMatrix;
  mv := ngl_ModelViewMatrix;
  n :=  ngl_NormalMatrix;
  mvpMat := glGetUniformLocation(lProg, pAnsiChar('ModelViewProjectionMatrix'));
  mvMat := glGetUniformLocation(lProg, pAnsiChar('ModelViewMatrix'));
  normMat := glGetUniformLocation(lProg, pAnsiChar('NormalMatrix'))
  glUniformMatrix4fv(mvpMat, 1, kGL_FALSE, @mvp[0,0]); // note model not MVP!
  glUniformMatrix4fv(mvMat, 1, kGL_FALSE, @mv[0,0]);
  glUniformMatrix3fv(normMat, 1, kGL_FALSE, @n[0,0]);
end; *)

type
TVtxNormClr = Packed Record
  vtx   : TPoint3f; //vertex coordinates
  //norm   : TPoint3f; //vertex normal
  norm : int32;
  clr : TRGBA;
end;

function Float2Int16(f: single): int16;
begin
     if f > 1 then
        exit(32767);
     if f < -1 then
        exit(-32768);
     if f > 0 then
        result := round(f * 32767)
     else
         result := round(f * 32768);
end;
(*function Float2Int16(fv: single): int16;
var
   f: single;
begin
     f := fv;
     if f > 1 then
        f := 1;
     if f < -1 then
        f := -1;
     if f > 0 then
        result := round(f * 32767)
     else
         result := round(f * 32768);
end;*)
function AsGL_INT_2_10_10_10_REV(f: TPoint3f): int32;
//pack 3 32-bit floats as 10 bit signed integers, assumes floats normalized to -1..1
var
   x,y,z: int32;
begin
     x := (Float2Int16(f.X)) shr 6;
     y := (Float2Int16(f.Y)) shr 6;
     z := (Float2Int16(f.Z)) shr 6;
     result := (z shl 20)+ (y shl 10) + (x shl 0);
end;
(*function AsGL_INT_2_10_10_10_REV(f: TPoint3f): int32;
//pack 3 32-bit floats as 10 bit signed integers, assumes floats normalized to -1..1
var
   x,y,z: uint16;
begin
     x := uint16(Float2Int16(f.X)) shr 6;
     y := uint16(Float2Int16(f.Y)) shr 6;
     z := uint16(Float2Int16(f.Z)) shr 6;
     result := (z shl 20)+ (y shl 10) + (x shl 0);
end; *)

function AsGL_INT_2_10_10_10_REV_T(f: TPoint3f; g: uint16): int32;
//pack 3 32-bit floats as 10 bit signed integers, assumes floats normalized to -1..1 and uses the 2bit to a int between 0 and 3
var
   a,x,y,z: uint16;
begin
     x := uint16(Float2Int16(f.X)) shr 6;
     y := uint16(Float2Int16(f.Y)) shr 6;
     z := uint16(Float2Int16(f.Z)) shr 6;
     a := g;
     result := (a shl 30)+ (z shl 20)+ (y shl 10) + (x shl 0);
end;


procedure BuildDisplayList(var faces: TFaces; vertices: TVertices; vRGBA: TVertexRGBA; var vao, vbo: gluint; Clr: TRGBA);
const
    kATTRIB_VERT = 0;  //vertex XYZ are positions 0,1,2
    kATTRIB_NORM = 3;  //normal XYZ are positions 3,4,5
    kATTRIB_CLR = 6;   //color RGBA are positions 6,7,8,9
var
  vnc: array of TVtxNormClr;
  vNorm: array of TPoint3f;
  vbo_point : GLuint;
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
      vnc[i].norm :=  AsGL_INT_2_10_10_10_REV(vNorm[i]);
      vnc[i].clr := clr;
      //fNorm := getSurfaceNormal(vertices[faces[i].X], vertices[faces[i].Y], vertices[faces[i].Z]);
  end;
  if length(vRGBA) = length(vertices) then
     for i := 0 to (length(vertices) -1) do
         vnc[i].clr := vRGBA[i];
  vbo_point := 0;
  glGenBuffers(1, @vbo_point);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  glBufferData(GL_ARRAY_BUFFER, Length(vnc)*SizeOf(TVtxNormClr), @vnc[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  // Prepare vertrex array object (VAO)
  //if vao <> 0 then
  //   glDeleteVertexArrays(1,@vao);
  glGenVertexArrays(1, @vao);
  glBindVertexArray(vao);
  //glBindBuffer(GL_ARRAY_BUFFER, gShader.vbo_point3d);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  //Vertices
  glVertexAttribPointer(kATTRIB_VERT, 3, GL_FLOAT, kGL_FALSE, sizeof(TVtxNormClr), PChar(0));

  glEnableVertexAttribArray(kATTRIB_VERT);
  //Normals typically stored as 3*32 bit floats (96 bytes), but we will pack them as 10-bit integers in a single 32-bit value with GL_INT_2_10_10_10_REV
  //  https://www.opengl.org/wiki/Vertex_Specification_Best_Practices

  glVertexAttribPointer(kATTRIB_NORM, 4, GL_INT_2_10_10_10_REV, kGL_FALSE, sizeof(TVtxNormClr), PChar(sizeof(TPoint3f)));
  glEnableVertexAttribArray(kATTRIB_NORM);
  //Color
  glVertexAttribPointer(kATTRIB_CLR, 4, GL_UNSIGNED_BYTE, kGL_TRUE, sizeof(TVtxNormClr), PChar(sizeof(int32)+ sizeof(TPoint3f)));
  glEnableVertexAttribArray(kATTRIB_CLR);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  glGenBuffers(1, @vbo);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbo);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(faces)*sizeof(TPoint3i), @faces[0], GL_STATIC_DRAW);
  glDeleteBuffers(1, @vbo_point);
end;

procedure BuildDisplayListStrip(Indices: TInts; vertices, vNorm: TVertices; vRGBA: TVertexRGBA; vType: TInts; LineWidth: integer; var vao, vbo: gluint);
const
    kATTRIB_VERT = 0;  //vertex XYZ are positions 0,1,2
    kATTRIB_NORM = 3;  //normal XYZ are positions 3,4,5
    kATTRIB_CLR = 6;   //color RGBA are positions 6,7,8,9
const
     kMx = (256*3+2);
     kMx2 = (4*256*256+4*256+3);
var
  vnc: array of TVtxNormClr;
  vbo_point : GLuint;
  i,j,k: integer;
  DataCylinder: array[0..256,0..2] of single;
  FinalDataCylinder: array[0..kMx] of UInt8;
  DataSphere: array[0..256,0..256,0..3] of single;
  //FinalDataSphere: array[0..kMx2] of Int8;
  FinalDataSphere: array[0..kMx2] of UInt8;
  RenderTexture: array [0..1] of GLuint;
  angle, rad,tx,tz,d: single;
  fx: single;
begin
  //create VBO that combines vertex, normal and color information
  if length(vRGBA) <> length(vertices) then
     exit;
  setlength(vnc, length(vertices));
  //set every vertex
  for i := 0 to (length(vertices) -1) do begin
      vnc[i].vtx := vertices[i];
      vnc[i].norm :=  AsGL_INT_2_10_10_10_REV_T(vNorm[i],vType[i]);
      //vnc[i].norm :=  AsGL_INT_2_10_10_10_REV(vNorm[i]);
      vnc[i].clr := vRGBA[i];;
  end;
  //Generating Cylinder Normal Map
  for i := 0 to 256 do begin
     angle := 180/256.0*i;
     rad := angle * (Pi/180);
     tx := Cos(rad);
     tz := Sin(rad);
     DataCylinder[i][0] := tx;
     DataCylinder[i][1] := 0;
     DataCylinder[i][2] := tz;
  end;
  for i:=0 to 256 do begin
      d:=0.0;
      for k:=0 to 2 do begin
          d+=DataCylinder[i][k]*DataCylinder[i][k];
      end;
      d:=Sqrt(d);
      for k:=0 to 2 do begin
          DataCylinder[i][k] := 0.5*DataCylinder[i][k]/d+0.5;
      end;
  end;
  //DataCylinder: array[0..256,0..2] of real;
  for i:=0 to 256 do begin
      for k:=0 to 2 do begin
             fx := 255 * DataCylinder[i][k];
             fx := min(max(fx, 0), 255);
             FinalDataCylinder[3*i+k]:=round(fx);
      end;
  end;
  glGenTextures(2, @RenderTexture);
  glActiveTexture(GL_TEXTURE6);
  glBindTexture(GL_TEXTURE_1D,  RenderTexture[0]);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexImage1D(GL_TEXTURE_1D, 0, GL_RGBA8, 256, 0, GL_RGB, GL_UNSIGNED_BYTE, @FinalDataCylinder);
  glUniform1i(glGetUniformLocation(gShader.programTrackID, pAnsiChar('normalmaptexture')), 6) ;
  //Generating Sphere Normal Map
  for i := 0 to 256 do begin
     angle := 180/256.0*i;
     rad := angle * (Pi/180);
     tx := Cos(rad);
     tz := Sin(rad);
     for j:= 0 to 256 do begin
        if((i-256.0/2.0)*(i-256.0/2.0)+(j-256.0/2.0)*(j-256.0/2.0)>(256.0/2.0)*(256.0/2.0)) then begin
             DataSphere[i][j][0] := 0.0;
             DataSphere[i][j][1] := 0.0;
             DataSphere[i][j][2] := 0.0;
             DataSphere[i][j][3] := 0.0;
        end
        else begin
             DataSphere[i][j][0] := tx;
             DataSphere[i][j][1] := 0;
             DataSphere[i][j][2] := tz;
             DataSphere[i][j][3] := 1.0;
        end;
     end;
  end;
  for i:=0 to 256 do begin
      for j:=0 to 256 do begin
        d:=0.0;
        for k:=0 to 2 do begin
            d+=DataSphere[i][j][k]*DataSphere[i][j][k];
        end;
        d:=Sqrt(d);
        for k:=0 to 2 do begin
            DataSphere[i][j][k] := 0.5*DataSphere[i][j][k]/d+0.5;
        end;
      end;
  end;
  for i:=0 to 256 do begin
    for j:=0 to 256 do begin
      for k:=0 to 3 do begin
          fx :=DataSphere[i][j][k] * 255;
          //fx := min(max(fx, -128), 127);
          fx := min(max(fx, 0), 255);
          FinalDataSphere[4*256*i+4*j+k]:=round(fx);
      end;
    end;
  end;
  glActiveTexture(GL_TEXTURE7);
  glBindTexture(GL_TEXTURE_2D,  RenderTexture[1]);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 256,256, 0, GL_RGBA, GL_UNSIGNED_BYTE, @FinalDataSphere);
  glUniform1i(glGetUniformLocation(gShader.programTrackID, pAnsiChar('normalmaptexturesphere')), 7) ;
  glActiveTexture(GL_TEXTURE8);  //guarantee that anywhere else use TEXTURE7
  vbo_point := 0;
  glGenBuffers(1, @vbo_point);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  glBufferData(GL_ARRAY_BUFFER, Length(vnc)*SizeOf(TVtxNormClr), @vnc[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  // Prepare vertrex array object (VAO)
  //if vao <> 0 then
  //   glDeleteVertexArrays(1,@vao);
  glGenVertexArrays(1, @vao);
  glBindVertexArray(vao);
  //glBindBuffer(GL_ARRAY_BUFFER, gShader.vbo_point3d);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  //Vertices
  glVertexAttribPointer(kATTRIB_VERT, 3, GL_FLOAT, kGL_FALSE, sizeof(TVtxNormClr), PChar(0));
  glEnableVertexAttribArray(kATTRIB_VERT);
  //Normals typically stored as 3*32 bit floats (96 bytes), but we will pack them as 10-bit integers in a single 32-bit value with GL_INT_2_10_10_10_REV
  //  https://www.opengl.org/wiki/Vertex_Specification_Best_Practices
  glVertexAttribPointer(kATTRIB_NORM, 4, GL_INT_2_10_10_10_REV, kGL_FALSE, sizeof(TVtxNormClr), PChar(sizeof(TPoint3f)));
  glEnableVertexAttribArray(kATTRIB_NORM);
  //Color
  glVertexAttribPointer(kATTRIB_CLR, 4, GL_UNSIGNED_BYTE, kGL_TRUE, sizeof(TVtxNormClr), PChar(sizeof(int32)+ sizeof(TPoint3f)));
  glEnableVertexAttribArray(kATTRIB_CLR);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  glGenBuffers(1, @vbo);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbo);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(Indices)*sizeof(int32), @Indices[0], GL_STATIC_DRAW);
  glDeleteBuffers(1, @vbo_point);
end;

procedure SetLighting (var lPrefs: TPrefs);
begin
  //Done by shader
end;

procedure nSetOrtho (w,h: integer; Distance, MaxDistance: single; isMultiSample, isPerspective: boolean);
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
    ngluPerspective(40.0, w/h, 0.01, MaxDistance+1)
  else begin
     if AspectRatio > 1 then //Wide window                                           xxx
        nglOrtho (-ScaleX * AspectRatio, ScaleX * AspectRatio, -ScaleX, ScaleX, 0.0, 2.0) //Left, Right, Bottom, Top
     else //Tall window
       nglOrtho (-ScaleX, ScaleX, -ScaleX/AspectRatio, ScaleX/AspectRatio, 0.0,  2.0); //Left, Right, Bottom, Top
  end;
end;

procedure DrawScene(w,h: integer; isFlipMeshOverlay, isOverlayClipped, isDrawMesh, isMultiSample: boolean; var lPrefs: TPrefs; origin : TPoint3f; ClipPlane: TPoint4f; scale, distance, elevation, azimuth: single; var lMesh,lNode: TMesh; lTrack: TTrack);
//procedure DrawScene(w,h: integer; isDrawMesh, isMultiSample: boolean; var lPrefs: TPrefs; origin : TPoint3f; ClipPlane: TPoint4f; scale, distance, elevation, azimuth: single; var lMesh,lNode: TMesh; lTrack: TTrack);
var
   clr: TRGBA;
begin
  clr := asRGBA(lPrefs.ObjColor);
  //glClearColor( Red(lPrefs.backColor)/255, Green(lPrefs.backColor)/255, Blue(lPrefs.backColor)/255, 1.0); //Set blue background
  glClearColor(red(lPrefs.BackColor)/255, green(lPrefs.BackColor)/255, blue(lPrefs.BackColor)/255, 0); //Set background
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glEnable(GL_DEPTH_TEST);

  nglMatrixMode(nGL_PROJECTION);
  nglLoadIdentity();
  nSetOrtho(w, h, Distance, kMaxDistance, isMultiSample, lPrefs.Perspective);
  nglTranslatef(lPrefs.ScreenPan.X, lPrefs.ScreenPan.Y, 0 );
  nglMatrixMode (nGL_MODELVIEW);
  nglLoadIdentity ();
  //object size normalized to be -1...+1 in largest dimension.
  //closest/furthest possible vertex is therefore -1.73..+1.73 (e.g. cube where corner is sqrt(1+1+1) from origin)
  nglScalef(0.5/Scale, 0.5/Scale, 0.5/Scale);
  if lPrefs.Perspective then
      nglTranslatef(0,0, -Scale*2*Distance )
  else
     nglTranslatef(0,0,  -Scale*2 );
  nglRotatef(90-Elevation,-1,0,0);
  nglRotatef(-Azimuth,0,0,1);
  nglTranslatef(-origin.X, -origin.Y, -origin.Z);
   if lTrack.n_count > 0 then begin
     if lTrack.isTubes then
         RunMeshGLSL (asPt4f(2,ClipPlane.Y,ClipPlane.Z,ClipPlane.W),  lPrefs.ShaderForBackgroundOnly) //disable clip plane
     else begin
        if lPrefs.CoreTrackDisableDepth then begin
           glEnable(GL_DEPTH_TEST);
           glDisable(GL_BLEND);
        end;
       if isMultiSample then
        RunTrackGLSL(lTrack.LineWidth, w * 2, h * 2)
       else
         RunTrackGLSL(lTrack.LineWidth, w, h);

       if lPrefs.CoreTrackDisableDepth then
          glEnable(GL_BLEND);
        //glBlendEquation(GL_FUNC_ADD);
        //glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
     end;
   lTrack.DrawGL;
 end;
 if length(lNode.nodes) > 0 then begin
     RunMeshGLSL (asPt4f(2,ClipPlane.Y,ClipPlane.Z,ClipPlane.W), lPrefs.ShaderForBackgroundOnly); //disable clip plane
   lNode.DrawGL(clr, clipPlane, isFlipMeshOverlay);
 end;
 if  (length(lMesh.faces) > 0) then begin
    lMesh.isVisible := isDrawMesh;
    RunMeshGLSL (clipPlane, false);
    if not isOverlayClipped then
       lMesh.DrawGL(clr, asPt4f(2,ClipPlane.Y,ClipPlane.Z,ClipPlane.W), isFlipMeshOverlay )
    else
        lMesh.DrawGL(clr, clipPlane, isFlipMeshOverlay);
    lMesh.isVisible := true;
 end;

end;

end.

