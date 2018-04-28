unit shaderu;
{$Include opts.inc}
{$IFDEF FPC}{$mode objfpc}{$H+}{$ENDIF}
{$D-,O+,Q-,R-,S-}
interface
uses
 {$IFDEF DGL} dglOpenGL, {$ELSE DGL} {$IFDEF COREGL}glcorearb, {$ELSE} gl, glext, {$IFDEF GEOM_GLEXT}glext2, {$ENDIF} {$ENDIF}  {$ENDIF DGL}
  {$IFDEF COREGL} gl_core_3d, {$ELSE} gl_legacy_3d, {$ENDIF}
  sysutils,dialogs, define_types,  userdir, StrUtils;
const
{$IFDEF DGL}
   kGL_FALSE = FALSE;
   kGL_TRUE = TRUE;
   //kGL_TRUE = 1;
   //kGL_FALSE = 0;
{$ELSE}
   kGL_FALSE = GL_FALSE;
   kGL_TRUE = GL_TRUE;
{$ENDIF}
kMinDistance = 0.1;
kMaxDistance = 10;
  kMaxUniform = 10;
  kError = 666;
  kNote = 777;
  kBool = 0;
  kInt = 1;
  kFloat = 2;
  kSet = 3;
type
  TUniform = record
    Name: string;
    Widget: integer;
    Min,DefaultV,Max: single;
    Bool: boolean;
  end;
  TFrameBuffer = record
    {$IFDEF HEMISSAO} tex1, {$ENDIF} depthBuf,frameBuf, tex: GLUint;
    w, h: integer;
  end;
  TShader = record
    vao_point2d, vbo_face2d, program2d, program3dx, programDefault, programTrackID, programAoID: GLuint;
    OverlayVolume,nUniform: integer;
    f1, f2,  fScreenShot: TFrameBuffer;
    lightPos : TPoint3f;
    {$IFDEF COREGL} TrackAmbient, TrackDiffuse, TrackSpecular : single; {$ENDIF}
    isGeometryShaderSupported: boolean;
    AOradiusU: integer;
    FragmentProgram,VertexProgram, GeometryProgram, Note, Vendor: AnsiString;
    Uniform: array [1..kMaxUniform] of TUniform;
  end;

var
  gShader: TShader;
  function  initVertFrag(vert, geom, frag: string): GLuint;
function LoadShader(lFilename: string; var Shader: TShader): boolean;
function InitGLSL (isStartUp: boolean): boolean;
procedure RunOverlayGLSL(clipPlane: TPoint4f);
procedure RunMeshGLSL (clipPlane: TPoint4f;  UseDefaultShader: boolean);
procedure RunTrackGLSL (lineWidth, ScreenPixelX, ScreenPixelY: integer);
procedure RunAoGLSL (var f1, f2 : TFrameBuffer; zoom : integer; alpha1, blend1, fracAO, distance: single);
function setFrame (wid, ht: integer; var f : TFrameBuffer; isMultiSample: boolean; var isOK: boolean) : boolean; //returns true if multi-sampling
procedure initFrame(var f : TFrameBuffer);
procedure releaseFrame;
procedure freeFrame (var f : TFrameBuffer);
//procedure Set2DDraw (w,h: integer; r,g,b: byte);

implementation
uses mainunit;

{$IFDEF COREGL}
const kVert2D ='#version 330'
+#10'layout(location = 0) in vec3 Vert;'
+#10'layout(location = 3) in vec4 Clr;'
+#10'out vec4 vClr;'
+#10'uniform mat4 ModelViewProjectionMatrix;'
+#10'void main() {'
+#10'    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);'
+#10'    //gl_Position.z = gl_Position.z - 0.7;'
+#10'    vClr = Clr;'
+#10'}';
    const kFrag2D = '#version 330'
+#10'in vec4 vClr;'
+#10'out vec4 color;'
+#10'void main() {'
+#10'    color = vClr;'
+#10'}';

const kAoShaderVert = '#version 330'
+#10'layout(location = 0) in vec3 Vert;'
+#10'layout(location = 3) in vec2 Coord;'
+#10'smooth out vec2 texCoord;'
+#10'void main () {'
+#10'  gl_Position = vec4 (Vert, 1.0);'
+#10'  texCoord = Coord;'
+#10'}';

  {$IFDEF HEMISSAO}
  //https://gist.github.com/fisch0920/6770363
     //http://blog.evoserv.at/index.php/2012/12/hemispherical-screen-space-ambient-occlusion-ssao-for-deferred-renderers-using-openglglsl/
    const kAoShaderFrag = '#version 330'
+#10'uniform sampler2D tex1, tex2, depth_texture1, depth_texture2, norm1;'
+#10'uniform float blend1, alpha1, fracAO, aoRadius;'
+#10'uniform vec2 texture_size;'
+#10'#define PI    3.14159265'
+#10'float width = texture_size.x; //texture width'
+#10'float height = texture_size.y; //texture height'
+#10'smooth in vec2 texCoord;'
+#10'out vec4 color;'
+#10'vec3 viewNormal;'
+#10'//------------------------------------------'
+#10'//general stuff'
+#10'//make sure that these two values are the same for your camera, otherwise distances will be wrong.'
+#10'//user variables'
+#10'int samples = 64; //ao sample count'
+#10'float aoclamp = 0.25; //depth clamp - reduces haloing at screen edges'
+#10'bool noise = true; //use noise instead of pattern for sample dithering'
+#10'float noiseamount = 0.0002; //dithering amount'
+#10'float diffarea = 0.4; //self-shadowing reduction'
+#10'float gdisplace = 0.4; //gauss bell center'
+#10'//float near = 1.0; //Z-near;'
+#10'//float far = 3.0; //Z-far'
+#10'//--------------------------------------------------------'
+#10'vec2 rand(vec2 coord) {'
+#10'	float noiseX = ((fract(1.0-coord.s*(width/2.0))*0.25)+(fract(coord.t*(height/2.0))*0.75))*2.0-1.0;'
+#10'	float noiseY = ((fract(1.0-coord.s*(width/2.0))*0.75)+(fract(coord.t*(height/2.0))*0.25))*2.0-1.0;'
+#10'	if (noise) {'
+#10'	   noiseX = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233))) * 43758.5453),0.0,1.0)*2.0-1.0;'
+#10'	   noiseY = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233)*2.0)) * 43758.5453),0.0,1.0)*2.0-1.0;'
+#10'	}'
+#10'	return vec2(noiseX,noiseY)*noiseamount;'
+#10'}'
+#10'float readDepth(in vec2 coord) {'
+#10'   return texture(depth_texture1, coord).x;'
+#10'}'
+#10'float calAO(float depth, float dw, float dh, float mx) {'
+#10'	//float dd = (1.0-depth)*aoRadius;'
+#10'	float dd = aoRadius;'
+#10'	//float dd = aoRadius * 0.1;'
+#10'	float coordw = texCoord.x + dw*dd;'
+#10'	float coordh = texCoord.y + dh*dd;'
+#10'	float d = readDepth(vec2(coordw, coordh));'
+#10'	//if (d > dd) return 0.0;'
+#10'	vec3 sampleDir = vec3(dw*dd, dh*dd, depth-d);'
+#10'   sampleDir = normalize(sampleDir);'
+#10'   float NdotS = max(dot(viewNormal, sampleDir), 0);'
+#10'   //float rangeCheck  = 1.0 - step(mx, abs(depth-d));'
+#10'	return NdotS;// * rangeCheck;'
+#10'}'
+#10'float getAO(void) {'
+#10'   viewNormal = texture(norm1, texCoord).xyz;'
+#10'	vec2 noise = rand(texCoord); '
+#10'	float depth = readDepth(texCoord);'
+#10'	float w = (1.0 / width)+(noise.x*(1.0-noise.x));'
+#10'	float h = (1.0 / height)+(noise.y*(1.0-noise.y));'
+#10'	float mx = max(w,h) * aoRadius * 4.0;'
+#10'	//float w = (1.0 / width)/clamp(depth,aoclamp,1.0)+(noise.x*(1.0-noise.x));'
+#10'	//float h = (1.0 / height)/clamp(depth,aoclamp,1.0)+(noise.y*(1.0-noise.y));'
+#10'	float pw;'
+#10'	float ph;'
+#10'	float ao = 0.0;'
+#10'	float dl = PI*(3.0-sqrt(5.0));'
+#10'	float dz = 1.0/float(samples);'
+#10'	float l = 0.0;'
+#10'	float z = 1.0 - dz/2.0;'
+#10'	for (int i = 0; i <= samples; i ++) {'
+#10'		float r = sqrt(1.0-z);'
+#10'		pw = cos(l)*r;'
+#10'		ph = sin(l)*r;'
+#10'		ao += calAO(depth,pw*w,ph*h, mx);'
+#10'		z = z - dz;'
+#10'		l = l + dl;'
+#10'	}'
+#10'	ao /= float(samples);'
+#10'   //ao = clamp(ao, 0.0, 1.0);'
+#10'   ao = (clamp(ao, 0.0, 0.7) ) * 1.428; //threshold then LERP 0..1'
+#10'   //ao = pow(ao, 0.2);'
+#10'   ao = smoothstep(0.0, 1.0, ao);'
+#10'   //ao = pow(ao, 0.2);'
+#10'   //ao = (1.0 - ao);// * fracAO;'
+#10'   return ao * fracAO;'
+#10'}'
+#10'void main(void) {'
+#10'  //color = vec4(1.0, 0.0, 0.0, 1.0); return;'
+#10'  vec4 t1 = texture(tex1, texCoord);'
+#10'  if (t1.a == 0.0) discard;'
+#10'  vec4 t2 = texture(tex2, texCoord);'
+#10'  float ao = getAO();'
+#10'  //float ao = fracAO  * getAO();'
+#10'  t1.rgb = clamp(t1.rgb-ao, 0.0, 1.0);'
+#10'  //if (fracAO > 0.0)'
+#10'  //  t1.rgb = clamp(t1.rgb-getAO(), 0.0, 1.0);'
+#10'  t1.rgb = mix(t2.rgb,t1.rgb, alpha1);'
+#10'  float depth = 1.0 - (3.0 * (texture(depth_texture2, texCoord).x - texture(depth_texture1, texCoord).x));'
+#10'  //t2.a *= clamp(depth, 0.0, 1.0);'
+#10'  depth = clamp(depth, 0.0, 1.0);'
+#10'  color = mix(t1, t2, blend1 * depth);'
+#10'  //color.rgb = texture(norm1, texCoord).rgb;'
+#10'  //color.rgb = vec3(1.0 - ao);'
+#10'}';

  {$ELSE}
(*//this next shader is nice, but AO changes as image is zoomed closer/further
const kAoShaderFragToon = '#version 330'
+#10'uniform sampler2D tex1, tex2, depth_texture1, depth_texture2;'
+#10'uniform float blend1, alpha1, fracAO;'
+#10'uniform vec2 texture_size;'
+#10'smooth in vec2 texCoord;'
+#10'out vec4 color;'
+#10'float getAO(void) {'
+#10'	vec2 px = vec2(1.0 / texture_size.x, 1.0 / texture_size.y) * 1.4;'
+#10'	vec2 nXY = texCoord;'
+#10'	vec2 x1 = vec2(px.x, 0.0);'
+#10'	vec2 y1 = vec2(0.0, px.y);'
+#10'	float dx =  0.125 * (texture(depth_texture1, nXY-x1-y1).x+texture(depth_texture1, nXY-y1).x+texture(depth_texture1, nXY+x1-y1).x+texture(depth_texture1, nXY-x1).x+texture(depth_texture1, nXY+x1).x+texture(depth_texture1, nXY-x1+y1).x+texture(depth_texture1, nXY+y1).x+texture(depth_texture1, nXY+x1+y1).x);'
+#10'	dx = abs(texture(depth_texture1, nXY).x - dx);'
+#10'	dx = dx * 5000.0;'
+#10'	dx = smoothstep(1.0-fracAO, 1.0, dx);'
+#10'	return dx;'
+#10'}'
+#10'void main(void) {'
+#10'  vec4 t1 = texture(tex1, texCoord);'
+#10'  if (t1.a == 0.0) discard;'
+#10'  vec4 t2 = texture(tex2, texCoord);'
+#10'  if (fracAO > 0.0)'
+#10'    t1.rgb = clamp(t1.rgb-getAO(), 0.0, 1.0);'
+#10'  t1.rgb = mix(t2.rgb,t1.rgb, alpha1);'
+#10'  float depth = 1.0 - (3.0 * (texture(depth_texture2, texCoord).x - texture(depth_texture1, texCoord).x));'
+#10'  depth = clamp(depth, 0.0, 1.0);'
+#10'  color = mix(t1, t2, blend1 * depth);'
+#10'}';   *)

const kAoShaderFrag = '#version 330'
 +#10'uniform sampler2D tex1, tex2, depth_texture1, depth_texture2;'
+#10'uniform float blend1, alpha1, fracAO, aoRadius;'
+#10'uniform vec2 texture_size;'
+#10'#define PI    3.14159265'
+#10'smooth in vec2 texCoord;'
+#10'out vec4 color;'
+#10'//general stuff'
+#10'int samples = 32; //ao sample count'
+#10'float aoclamp = 0.25; //depth clamp - reduces haloing at screen edges'
+#10'bool noise = true; //use noise instead of pattern for sample dithering'
+#10'float noiseamount = 0.0002; //dithering amount'
+#10'float diffarea = 0.5; //self-shadowing reduction'
+#10'float gdisplace = 0.4; //gauss bell center'
+#10'vec2 rand(vec2 coord) {'
+#10'	float noiseX = ((fract(1.0-coord.s*(texture_size.x/2.0))*0.25)+(fract(coord.t*(texture_size.y/2.0))*0.75))*2.0-1.0;'
+#10'	float noiseY = ((fract(1.0-coord.s*(texture_size.x/2.0))*0.75)+(fract(coord.t*(texture_size.y/2.0))*0.25))*2.0-1.0;'
+#10'	if (noise) {'
+#10'	   noiseX = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233))) * 43758.5453),0.0,1.0)*2.0-1.0;'
+#10'	   noiseY = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233)*2.0)) * 43758.5453),0.0,1.0)*2.0-1.0;'
+#10'	}'
+#10'	return vec2(noiseX,noiseY)*noiseamount;'
+#10'}'
+#10'float readDepth(in vec2 coord) {'
+#10'   return 1.0 -  ((texture(depth_texture1, coord).x + 0.87) * 0.534);'
+#10'}'
+#10'float compareDepths(in float depth1, in float depth2,inout int far) {'
+#10'	float garea = 2.0; //gauss bell width'
+#10'	float diff = (depth1 - depth2)*100.0; //depth difference (0-100)'
+#10'	if (diff<gdisplace) //reduce left bell width to avoid self-shadowing'
+#10'	  garea = diffarea;'
+#10'	else'
+#10'	  far = 1;'
+#10'	float gauss = pow(2.7182,-2.0*(diff-gdisplace)*(diff-gdisplace)/(garea*garea));'
+#10'	return gauss;'
+#10'}'
+#10'float calAO(float depth, vec2 coordwh) {'
+#10'	int far = 0;'
+#10'	float temp = compareDepths(depth, readDepth(texCoord+coordwh),far);'
+#10'	if (far > 0) {'
+#10'		float temp2 = compareDepths(readDepth(texCoord-coordwh),depth,far);'
+#10'		temp += (1.0-temp)*temp2;'
+#10'	}'
+#10'	return temp;'
+#10'}'
+#10'float getAO(void) {'
+#10'	vec2 noise = rand(texCoord);'
+#10'	float depth = readDepth(texCoord);'
+#10'	float dd = (1.0-depth)*aoRadius;'
+#10'	float w = (1.0 / texture_size.x)/clamp(depth,aoclamp,1.0)+(noise.x*(1.0-noise.x));'
+#10'	float h = (1.0 / texture_size.y)/clamp(depth,aoclamp,1.0)+(noise.y*(1.0-noise.y));'
+#10'	float ao = 0.0;'
+#10'	float dl = PI*(3.0-sqrt(5.0));'
+#10'	float dz = 1.0/float(samples);'
+#10'	float l = 0.0;'
+#10'	float z = 1.0 - dz/2.0;'
+#10'	for (int i = 0; i <= samples; i ++) {'
+#10'		float r = sqrt(1.0-z);'
+#10'		ao += calAO(depth, vec2(cos(l)*r*w*dd,sin(l)*r*h*dd));'
+#10'		z = z - dz;'
+#10'		l = l + dl;'
+#10'	}'
+#10'	ao /= float(samples);'
+#10'	ao = clamp(ao, 0.0, 0.4) * 2.5; //threshold then LERP 0..1'
+#10'	ao = smoothstep(0.0, 1.0, ao);'
+#10'	ao = (1.0 - ao) * fracAO;'
+#10'	return ao;'
+#10'}'
+#10'void main(void) {'
+#10'  vec4 t1 = texture(tex1, texCoord);'
+#10'  if (t1.a == 0.0) discard;'
+#10'  vec4 t2 = texture(tex2, texCoord);'
+#10'  //float ao = 1.0 - getAO(); color = vec4(ao, ao, ao, 1.0); return;'
+#10'  if (fracAO > 0.0)'
+#10'    t1.rgb = clamp(t1.rgb-getAO(), 0.0, 1.0);'
+#10'  t1.rgb = mix(t2.rgb,t1.rgb, alpha1);'
+#10'  float depth = 1.0 - (3.0 * (texture(depth_texture2, texCoord).x - texture(depth_texture1, texCoord).x));'
+#10'  depth = clamp(depth, 0.0, 1.0);'
+#10'  color = mix(t1, t2, blend1 * depth);'
+#10'}';

      {$ENDIF}

 {$ELSE}
    const kVert2D ='varying vec4 vClr;'
    +#10'void main() {'
    +#10'    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;'
    +#10'    vClr = gl_Color;'
    +#10'}';
        const kFrag2D = 'varying vec4 vClr;'
    +#10'void main() {'
    +#10'    gl_FragColor = vClr;'
    +#10'}';

    const kAoShaderFrag = 'uniform sampler2D tex1, tex2, depth_texture1, depth_texture2;'
+#10'uniform float blend1, alpha1, fracAO, aoRadius;'
+#10'uniform vec2 texture_size;'
+#10'#define PI    3.14159265'
+#10'vec2 texCoord = gl_TexCoord[0].xy;'
+#10'//general stuff'
+#10'int samples = 32; //ao sample count'
+#10'float aoclamp = 0.25; //depth clamp - reduces haloing at screen edges'
+#10'bool noise = true; //use noise instead of pattern for sample dithering'
+#10'float noiseamount = 0.0002; //dithering amount'
+#10'float diffarea = 0.5; //self-shadowing reduction'
+#10'float gdisplace = 0.4; //gauss bell center'
+#10'vec2 rand(vec2 coord) {'
+#10'	float noiseX = ((fract(1.0-coord.s*(texture_size.x/2.0))*0.25)+(fract(coord.t*(texture_size.y/2.0))*0.75))*2.0-1.0;'
+#10'	float noiseY = ((fract(1.0-coord.s*(texture_size.x/2.0))*0.75)+(fract(coord.t*(texture_size.y/2.0))*0.25))*2.0-1.0;'
+#10'	if (noise) {'
+#10'	   noiseX = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233))) * 43758.5453),0.0,1.0)*2.0-1.0;'
+#10'	   noiseY = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233)*2.0)) * 43758.5453),0.0,1.0)*2.0-1.0;'
+#10'	}'
+#10'	return vec2(noiseX,noiseY)*noiseamount;'
+#10'}'
+#10'float readDepth(in vec2 coord) {'
+#10'   return 1.0 -  ((texture2D(depth_texture1, coord).x + 0.87) * 0.534);'
+#10'}'
+#10'float compareDepths(in float depth1, in float depth2,inout int far) {'
+#10'	float garea = 2.0; //gauss bell width'
+#10'	float diff = (depth1 - depth2)*100.0; //depth difference (0-100)'
+#10'	if (diff<gdisplace) //reduce left bell width to avoid self-shadowing'
+#10'	  garea = diffarea;'
+#10'	else'
+#10'	  far = 1;'
+#10'	float gauss = pow(2.7182,-2.0*(diff-gdisplace)*(diff-gdisplace)/(garea*garea));'
+#10'	return gauss;'
+#10'}'
+#10'float calAO(float depth, vec2 coordwh) {'
+#10'	int far = 0;'
+#10'	float temp = compareDepths(depth, readDepth(texCoord+coordwh),far);'
+#10'	if (far > 0) {'
+#10'		float temp2 = compareDepths(readDepth(texCoord-coordwh),depth,far);'
+#10'		temp += (1.0-temp)*temp2;'
+#10'	}'
+#10'	return temp;'
+#10'}'
+#10'float getAO(void) {'
+#10'	vec2 noise = rand(texCoord);'
+#10'	float depth = readDepth(texCoord);'
+#10'	float dd = (1.0-depth)*aoRadius;'
+#10'	float w = (1.0 / texture_size.x)/clamp(depth,aoclamp,1.0)+(noise.x*(1.0-noise.x));'
+#10'	float h = (1.0 / texture_size.y)/clamp(depth,aoclamp,1.0)+(noise.y*(1.0-noise.y));'
+#10'	float ao = 0.0;'
+#10'	float dl = PI*(3.0-sqrt(5.0));'
+#10'	float dz = 1.0/float(samples);'
+#10'	float l = 0.0;'
+#10'	float z = 1.0 - dz/2.0;'
+#10'	for (int i = 0; i <= samples; i ++) {'
+#10'		float r = sqrt(1.0-z);'
+#10'		ao += calAO(depth, vec2(cos(l)*r*w*dd,sin(l)*r*h*dd));'
+#10'		z = z - dz;'
+#10'		l = l + dl;'
+#10'	}'
+#10'	ao /= float(samples);'
+#10'	ao = clamp(ao, 0.0, 0.4) * 2.5; //threshold then LERP 0..1'
+#10'	ao = smoothstep(0.0, 1.0, ao);'
+#10'	ao = (1.0 - ao) * fracAO;'
+#10'	return ao;'
+#10'}'
+#10'void main(void) {'
+#10'  vec4 t1 = texture2D(tex1, texCoord);'
+#10'  if (t1.a == 0.0) discard;'
+#10'  vec4 t2 = texture2D(tex2, texCoord);'
+#10'  //float ao = 1.0 - getAO(); gl_FragColor = vec4(ao, ao, ao, 1.0); return;'
+#10'  if (fracAO > 0.0)'
+#10'    t1.rgb = clamp(t1.rgb-getAO(), 0.0, 1.0);'
+#10'  t1.rgb = mix(t2.rgb,t1.rgb, alpha1);'
+#10'  float depth = 1.0 - (3.0 * (texture2D(depth_texture2, texCoord).x - texture2D(depth_texture1, texCoord).x));'
+#10'  depth = clamp(depth, 0.0, 1.0);'
+#10'  gl_FragColor = mix(t1, t2, blend1 * depth);'
+#10'}';

{$ENDIF}

(*moved to gl_2d procedure Set2DDraw (w,h: integer; r,g,b: byte);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho (0, 1, 0, 1, -6, 12);
  glMatrixMode (GL_MODELVIEW);
  glLoadIdentity ();
  glDepthMask(kGL_TRUE); // enable writes to Z-buffer
  glEnable(GL_DEPTH_TEST);
  glDisable(GL_CULL_FACE); // glEnable(GL_CULL_FACE); //check on pyramid
  glEnable(GL_BLEND);
  glEnable(GL_NORMALIZE);
  glClearColor(r/255, g/255, b/255, 0.0); //Set background
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
  glViewport( 0, 0, w, h); //required when bitmap zoom <> 1
end; *)

procedure uniform1ix(prog: GLint; name: AnsiString; value: integer);
begin
    glUniform1i(glGetUniformLocation(prog, pAnsiChar(Name)), value) ;
end;

procedure uniform1fx(prog: GLint;  name: AnsiString; value: single );
begin
  glUniform1f(glGetUniformLocation(prog, pAnsiChar(Name)), value) ;
end;
procedure uniform2fx(prog: GLint;  name: AnsiString; v1, v2: single );
begin
  glUniform2f(glGetUniformLocation(prog, pAnsiChar(Name)), v1, v2) ;
end;

procedure uniform4fx(prog: GLint;   name: AnsiString; v: TPoint4f);
begin
  glUniform4f(glGetUniformLocation(prog, pAnsiChar(Name)), v.X,v.Y,v.Z, v.W) ;
end;

procedure uniform3fx(prog: GLint;   name: AnsiString; v1,v2,v3: single);
begin
  glUniform3f(glGetUniformLocation(prog, pAnsiChar(Name)), v1,v2,v3) ;
end;

procedure uniform1i( name: AnsiString; value: integer);
begin
  glUniform1i(glGetUniformLocation(gShader.program3dx, pAnsiChar(Name)), value) ;
end;

procedure uniform1f( name: AnsiString; value: single );
begin
  glUniform1f(glGetUniformLocation(gShader.program3dx, pAnsiChar(Name)), value) ;
end;

procedure uniform4f( name: AnsiString; v1,v2,v3, v4: single);
begin
  glUniform4f(glGetUniformLocation(gShader.program3dx, pAnsiChar(Name)), v1,v2,v3, v4) ;
end;

procedure uniform3f( name: AnsiString; v1,v2,v3: single);
begin
  glUniform3f(glGetUniformLocation(gShader.program3dx, pAnsiChar(Name)), v1,v2,v3) ;
end;

procedure releaseFrame;
begin
  {$IFDEF COREGL}
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  {$ELSE}
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);  //only included in 3.0! https://www.opengl.org/sdk/docs/man/html/glBindFramebuffer.xhtml
  {$ENDIF}
end;

procedure freeFrame (var f : TFrameBuffer);
begin

  //Delete resources
   glDeleteTextures(1, @f.tex);
  {$IFDEF HEMISSAO} glDeleteTextures(1, @f.tex1); {$ENDIF}
  glDeleteTextures(1, @f.depthBuf);
  {$IFDEF COREGL}
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glDeleteFramebuffers(1, @f.frameBuf);
  {$ELSE}
  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
  glDeleteFramebuffersEXT(1, @f.frameBuf);
  {$ENDIF}
  //Bind 0, which means render to back buffer, as a result, frameBuf is unbound
end;

//{$DEFINE MULTISAMPLE}
{$IFDEF MULTISAMPLE}
function setFrame (wid, ht: integer; var f : TFrameBuffer; isMultiSample: boolean; var isOK: boolean) : boolean; //returns true if multi-sampling
//http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-14-render-to-texture/
//https://stackoverflow.com/questions/33587682/opengl-how-can-i-attach-a-depth-buffer-to-a-framebuffer-using-a-multisampled-2d

var
   w,h: integer;
   drawBuf: array[0..1] of GLenum;
begin
     {$IFNDEF COREGL}
       error requires core
     {$ENDIF}
     w := wid;
     h := ht;
     isOK := true;
     if isMultiSample then begin
        w := w * 2;
        h := h * 2;
     end;
     result := isMultiSample;
     if (w = f.w) and (h = f.h) then begin
         glBindFramebuffer(GL_FRAMEBUFFER, f.frameBuf);
         exit;
     end;
     freeframe(f);
     f.w := w;
     f.h := h;
     //https://www.opengl.org/wiki/Framebuffer_Object_Examples#Quick_example.2C_render_to_texture_.282D.29
     glGenTextures(1, @f.tex);
     glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, f.tex);
     glTexParameteri(GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
     glTexParameteri(GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
     glTexParameteri(GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
     glTexParameteri(GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
     //glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, f.w, f.h, 0,GL_RGBA, GL_UNSIGNED_BYTE, nil); //RGBA16 for AO
     glTexImage2DMultisample(GL_TEXTURE_2D_MULTISAMPLE, 4, GL_RGBA8, f.w, f.h, GL_TRUE);

     glGenFramebuffers(1, @f.frameBuf);
     glBindFramebuffer(GL_FRAMEBUFFER, f.frameBuf);
     //glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, f.tex, 0);
     glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D_MULTISAMPLE, f.tex, 0);

    (*glGenTextures(1, @f.depthBuf);
    glBindTexture(GL_RENDERBUFFER, f.depthBuf);
    glTexParameterf(GL_RENDERBUFFER, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterf(GL_RENDERBUFFER, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameterf(GL_RENDERBUFFER, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameterf(GL_RENDERBUFFER, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    //glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH24_STENCIL8, f.w, f.h, 0, GL_DEPTH_STENCIL, GL_UNSIGNED_INT_24_8, nil);
    //glTexImage2DMultisample(GL_TEXTURE_2D_MULTISAMPLE, 4, GL_DEPTH24_STENCIL8, f.w, f.h, GL_TRUE);
    glRenderbufferStorageMultisample(GL_RENDERBUFFER, 4, GL_DEPTH_COMPONENT24, f.w, f.h);
    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, f.depthBuf); *)
    glGenTextures(1, @f.depthBuf);
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, f.depthBuf);
    glTexParameterf(GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameterf(GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexImage2DMultisample(GL_TEXTURE_2D_MULTISAMPLE, 4, GL_DEPTH_COMPONENT, f.w, f.h, GL_TRUE);
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D_MULTISAMPLE, f.depthBuf, 0);

    //glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_RENDERBUFFER, f.depthBuf, 0);
     drawBuf[0] := GL_COLOR_ATTACHMENT0;
     drawBuf[1] := GL_COLOR_ATTACHMENT1;
     glDrawBuffers(1, @drawBuf[0]); // draw colors only
end;
{$ELSE}
function setFrame (wid, ht: integer; var f : TFrameBuffer; isMultiSample: boolean; var isOK: boolean) : boolean; //returns true if multi-sampling
//http://www.opengl-tutorial.org/intermediate-tutorials/tutorial-14-render-to-texture/
var
   w,h: integer;
   drawBuf: array[0..1] of GLenum;
   i: GLint;
begin
     w := wid;
     h := ht;
     isOK := true;
     if isMultiSample then begin
        w := w * 2;
        h := h * 2;
     end;
     result := isMultiSample;
     if (w = f.w) and (h = f.h) then begin
         {$IFDEF COREGL}
         glBindFramebuffer(GL_FRAMEBUFFER, f.frameBuf);
         {$ELSE}
         glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, f.frameBuf);    //only included in 3.0! https://www.opengl.org/sdk/docs/man/html/glBindFramebuffer.xhtml
         {$ENDIF}
         exit;
     end;
     freeframe(f);
     f.w := w;
     f.h := h;
     //https://www.opengl.org/wiki/Framebuffer_Object_Examples#Quick_example.2C_render_to_texture_.282D.29
     glGenTextures(1, @f.tex);
     (*if false then begin// not isMultiSample then begin
       glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, f.tex);
       glTexParameteri(GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
       glTexParameteri(GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
       glTexParameteri(GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
       glTexParameteri(GL_TEXTURE_2D_MULTISAMPLE, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
       glTexImage2DMultisample( GL_TEXTURE_2D_MULTISAMPLE, 4, GL_RGBA8, f.w, f.h, 0);
       //glTexImage2D(GL_TEXTURE_2D_MULTISAMPLE, 0,GL_RGBA8, f.w, f.h, 0,GL_RGBA, GL_UNSIGNED_BYTE, nil); //RGBA16 for AO
     end else begin *)
       glBindTexture(GL_TEXTURE_2D, f.tex);
       glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
       glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
       glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
       glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
       {$IFDEF COREGL}
       glTexImage2D(GL_PROXY_TEXTURE_2D, 0,GL_RGBA8, f.w, f.h, 0,GL_RGBA, GL_UNSIGNED_BYTE, nil); //RGBA16 for AO
       //glTexImage2DMultisample( GL_TEXTURE_2D_MULTISAMPLE, 4, GL_RGBA8, f.w, f.h, 0);
       glGetTexLevelParameteriv(GL_PROXY_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @i);
       if i < 1 then begin
         GLForm1.ShowmessageError(' setFrame error: reduce bitmap zoom');
         isOK := false;
          exit;
       end;
       {$ENDIF}
       glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, f.w, f.h, 0,GL_RGBA, GL_UNSIGNED_BYTE, nil); //RGBA16 for AO
     //end;
     {$IFDEF HEMISSAO}
     glGenTextures(1, @f.tex1);
     glBindTexture(GL_TEXTURE_2D, f.tex1);
     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
     glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
     glTexImage2D(GL_TEXTURE_2D, 0,GL_RGBA8, f.w, f.h, 0,GL_RGBA, GL_UNSIGNED_BYTE, nil); //RGBA16 for AO
     {$ENDIF}

     {$IFDEF COREGL}
     glGenFramebuffers(1, @f.frameBuf);
     glBindFramebuffer(GL_FRAMEBUFFER, f.frameBuf);
     //Attach 2D texture to this FBO
     //if not isMultiSample then
     //   glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D_MULTISAMPLE, f.tex, 0)
     //else
         glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, f.tex, 0);

     {$IFDEF HEMISSAO}glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT1, GL_TEXTURE_2D, f.tex1, 0);{$ENDIF}

     {$ELSE}

     glGenFramebuffersEXT(1, @f.frameBuf);
     glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, f.frameBuf);
     //Attach 2D texture to this FBO
     glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, f.tex, 0);

     {$IFDEF HEMISSAO}glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT1_EXT, GL_TEXTURE_2D, f.tex1, 0);{$ENDIF}

     {$ENDIF}
     //glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, f.tex, 0);
     // Create the depth buffer
    glGenTextures(1, @f.depthBuf);
    glBindTexture(GL_TEXTURE_2D, f.depthBuf);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    {$IFDEF COREGL}
    glTexImage2D(GL_PROXY_TEXTURE_2D, 0, GL_DEPTH24_STENCIL8, f.w, f.h, 0, GL_DEPTH_STENCIL, GL_UNSIGNED_INT_24_8, nil);
    glGetTexLevelParameteriv(GL_PROXY_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @i);
    if i < 1 then begin
      GLForm1.ShowmessageError(' setFrame error: reduce bitmap zoom');
      isOK := false;
       exit;
    end;
    {$ENDIF}
    glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH24_STENCIL8, f.w, f.h, 0, GL_DEPTH_STENCIL, GL_UNSIGNED_INT_24_8, nil);
    {$IFDEF COREGL}
     if  glGetError() <> GL_NO_ERROR  then begin
       //GLForm1.ShowmessageError('Frame buffer error 0x'+inttohex(glCheckFramebufferStatus(GL_FRAMEBUFFER),4) );
       GLForm1.ShowmessageError(' setFrame error: reduce bitmap zoom');
       isOK := false;
       exit;
    end;
    {$ENDIF}
    {$IFDEF COREGL}
    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, f.depthBuf, 0);
    {$ELSE}
    glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, f.depthBuf, 0);
    {$ENDIF}

    // Set "renderedTexture" as our colour attachement #0
     // Set the list of draw buffers.

     //drawBuf := GL_COLOR_ATTACHMENT0;
     //glDrawBuffers(1, @drawBuf); // "1" is the size of DrawBuffers
     drawBuf[0] := GL_COLOR_ATTACHMENT0;
     drawBuf[1] := GL_COLOR_ATTACHMENT1;
     {$IFDEF HEMISSAO}
     glDrawBuffers(2, @drawBuf[0]); // draw colors and normals
     {$ELSE}
     glDrawBuffers(1, @drawBuf[0]); // draw colors only
     {$ENDIF}
     {$IFDEF COREGL}
     if(glCheckFramebufferStatus(GL_FRAMEBUFFER) <> GL_FRAMEBUFFER_COMPLETE) then begin
       GLForm1.ShowmessageError('Frame buffer error 0x'+inttohex(glCheckFramebufferStatus(GL_FRAMEBUFFER),4) );
       isOK := false;
       exit;
     end;
     {$ENDIF}
     glEnable( GL_MULTISAMPLE );
end;
{$ENDIF}

procedure initFrame(var f : TFrameBuffer);
begin
     f.frameBuf := 0;
     f.tex := 0;
      {$IFDEF HEMISSAO} f.tex1 := 0; {$ENDIF}
     f.depthBuf := 0;
     f.w := 0;
     f.h := 0;
end;

procedure GetError(p: integer);  //report OpenGL Error
var
  Error: GLenum;
  s: string;
begin
     exit;
 Error := glGetError();
 if Error = GL_NO_ERROR then exit;
  {$IFDEF DGL}
  s := 'DGL' +inttostr(p)+'->' + inttostr(Error);
  {$ELSE}
  s := inttostr(p)+'->' + inttostr(Error);
  {$ENDIF}
 if Error = GL_INVALID_VALUE then
   s := s + '(GL_INVALID_VALUE)';
 GLForm1.ShowmessageError('OpenGL error : '+s );
end;

procedure ReportCompileShaderError(shaderType: GLEnum; glObjectID: GLuint);
var
  s : string;
  maxLength, isCompiled : GLint;
begin
  //status := 0;
    glGetShaderiv(glObjectID, GL_COMPILE_STATUS, @isCompiled);
    if (isCompiled <> 0) then exit; //report compiling errors.
    glGetError;
    glGetShaderiv(glObjectID, GL_INFO_LOG_LENGTH, @maxLength);
    if (maxLength < 1) then exit; //no info
     setlength(s, maxLength);
     {$IFDEF OLDDGL} //older DGL
     glGetShaderInfoLog(glObjectID, maxLength, maxLength, @s[1]);
     {$ELSE}
     glGetShaderInfoLog(glObjectID, maxLength, @maxLength, @s[1]);
     {$ENDIF}
     s:=trim(s);
     if shaderType = GL_VERTEX_SHADER then
       s := 'VERT '+ s
     else if shaderType = GL_GEOMETRY_SHADER then
       s := 'GEOM '+ s
     else if shaderType = GL_FRAGMENT_SHADER then
       s := 'FRAG '+ s;
     GLForm1.ShowmessageError('Shader compile error '+s);
end;

procedure ReportCompileProgramError(glObjectID: GLuint);
var
  s : string;
  maxLength : GLint;
begin
  glGetProgramiv(glObjectID, GL_LINK_STATUS, @maxLength);
  //if (maxLength = GL_TRUE) then exit;
  if (maxLength = 1) then exit; //DGL  GL_TRUE

  maxLength := 4096;
  setlength(s, maxLength);
  {$IFDEF OLDDGL} //older DGL
  glGetProgramInfoLog(glObjectID, maxLength, maxLength, @s[1]);
  {$ELSE}
  glGetProgramInfoLog(glObjectID, maxLength, @maxLength, @s[1]);
  {$ENDIF}
  if maxLength < 1 then begin
     GLForm1.ShowmessageError('Program compile error (unspecified)');
     exit
  end;
  s:=trim(s);
  if (length(s) < 2) then exit;
  GLForm1.ShowmessageError('Program compile error '+s);
end;

function compileShaderOfType (shaderType: GLEnum;  shaderText: string): GLuint;
begin
   result := glCreateShader(shaderType);
   {$IFDEF DGL}
   glShaderSource(result, 1, PPGLChar(@shaderText), nil);
   {$ELSE}
   glShaderSource(result, 1, PChar(@shaderText), nil);
   {$ENDIF}
   glCompileShader(result);
   //ReportCompileShaderHint(shaderType, result);
   ReportCompileShaderError(shaderType, result);
end;

function  initVertFrag(vert, geom, frag: string): GLuint;
var
   fs, gs, vs: GLuint;
begin
  result := 0;
  if (not gShader.isGeometryShaderSupported) and (length(geom) > 0) then begin
     {$IFDEF GEOM_GLEXT} //requires new version of glext.pp with glProgramParameteriEXT
     GLForm1.ShowmessageError('Error: graphics driver does not support geometry shaders');
     {$ELSE}
     GLForm1.ShowmessageError('Error: software not compiled to support geometry shaders');
     {$ENDIF}
    exit;
  end;
  glGetError(); //<- ignore proior errors
  //GetError(121); // <- report prior errors
  result := glCreateProgram();
  if (length(vert) > 0) then begin
     vs := compileShaderOfType(GL_VERTEX_SHADER, vert);
     if (vs = 0) then exit;
     glAttachShader(result, vs);
  end;
  fs := compileShaderOfType(GL_FRAGMENT_SHADER, frag);
  if (fs = 0) then exit;
  glAttachShader(result, fs);
  if (length(geom) > 0) then begin
    gs := compileShaderOfType(GL_GEOMETRY_SHADER, geom);
    if (gs = 0) then exit;
    glAttachShader(result, gs);
    //if length(gShader.GeometryProgram) > 0 then
    //glProgramParameteri ( gShader.program3d, GL_GEOMETRY_VERTICES_OUT, 3 );
    //glProgramParameteriEXT(result, GL_GEOMETRY_VERTICES_OUT_EXT, 3);  //GL_GEOMETRY_VERTICES_OUT_EXT = $8DDA;
    {$IFNDEF COREGL}
      //The next line requires the updated glext.pp http://mantis.freepascal.org/view.php?id=29051
      {$IFDEF GEOM_GLEXT} //requires new version of glext.pp with glProgramParameteriEXT
      glProgramParameteriEXT(result, GL_GEOMETRY_VERTICES_OUT_EXT, 3);  //GL_GEOMETRY_VERTICES_OUT_EXT = $8DDA;
      //glProgramParameteri(result, $8DDA, 3);  // <- using this on the old GLEXT does not work
      {$ENDIF}
    {$ENDIF}
  end;
  glLinkProgram(result);
  //ReportCompileShaderHint(666, result);
  ReportCompileProgramError(result);
  if (length(vert) > 0) then begin
     glDetachShader(result, vs);
     glDeleteShader(vs);
  end;
  glDetachShader(result, fs);
  glDeleteShader(fs);
  //glUseProgram(result);
  GetError(123);
  glGetError();
end;

procedure AdjustShaders (lShader: TShader);
//sends the uniform values to the GPU
var
  i: integer;
begin
  if (lShader.nUniform < 1) or (lShader.nUniform > kMaxUniform) then
    exit;

  for i := 1 to lShader.nUniform do begin
    if i = lShader.AOradiusU then continue;
    case lShader.Uniform[i].Widget of
      kFloat: uniform1f(lShader.Uniform[i].name,lShader.Uniform[i].defaultV);
      kInt: uniform1i(lShader.Uniform[i].name,round(lShader.Uniform[i].defaultV));
      kBool: begin
          if lShader.Uniform[i].bool then
            uniform1i(lShader.Uniform[i].name,1)
          else
            uniform1i(lShader.Uniform[i].name,0);
        end;
    end;//case
  end; //for each uniform
end; //AdjustShaders()

function HasGeometryShaderSupport: boolean; //in OpenGL 2.1 the geometry shader is optional, e.g. not in Mesa for Intel
var
  AllExtStr: string;
begin
  AllExtStr := glGetString(GL_EXTENSIONS);
  result := AnsiContainsText (AllExtStr, '_geometry_shader'); //GL_EXT_geometry_shader4, ARB_geometry_shader4
end;

function GLVersionError(major, minor: integer): boolean;
const
 kEps = 1.0E-6;
var
  s: string;
  current, req: double;
begin
  result := false; //assume OK
  //Use GL_VERSION to support OpenGL 2.1
  // for OpenGL 3.0+: glGetIntegerv(GL_MAJOR_VERSION, @lMajorV); glGetIntegerv(GL_MINOR_VERSION, @lMinorV);
  s := glGetString(GL_VERSION); //"4.1 Intel"
  if length(s) > 3 then
     s := copy(s, 1, 3);
  current := strtofloatdef(s, 2.0)+keps;
  req := major + (0.1*minor);
  if (current >= req) then exit; //all OK
  result := true; //ERROR
end;

function InitGLSL (isStartUp: boolean): boolean;
var
  lShader: TShader;
begin
   result := true;
  if isStartUp then begin
      {$IFDEF DGL}
              InitOpenGL;
              ReadExtensions;
      {$ELSE}
             //If your compiler does not find Load_GL_version_3_3_CORE you will need to update glext.pp
             {$IFDEF COREGL}
             gShader.isGeometryShaderSupported := true; //OpenGL core always supports the geometry shader
             gShader.TrackAmbient := 0.5;
             gShader.TrackDiffuse := 0.7;
             gShader.TrackSpecular := 0.2;
             if (not  Load_GL_version_3_3_CORE) then begin
             	showmessage('Unable to load OpenGL 3.3 Core (hint: try surficeOld)');
             {$ELSE}
             if not  (Load_GL_VERSION_2_1) then begin
                 //On Ubuntu 14.04 LTS on VirtualBox with Chromium 19 drivers Load_GL_VERSION_2_1 fails but still works...
                 //showmessage(format('Unable to load OpenGL %d.%d found %s. Vendor %s. GLSL %s',[GLForm1.GLBox.OpenGLMajorVersion, GLForm1.GLBox.OpenGLMinorVersion, glGetString(GL_VERSION), glGetString(GL_VENDOR),glGetString(GL_SHADING_LANGUAGE_VERSION)]));
                 showmessage('Unable to load OpenGL 2.1');
            {$ENDIF}
                 halt();
             end;
             (*if GLVersionError(GLForm1.GLBox.OpenGLMajorVersion, GLForm1.GLBox.OpenGLMinorVersion) then begin
                //showmessage(format('Requires OpenGL %d.%d found %s. Vendor %s. GLSL %s',[GLForm1.GLBox.OpenGLMajorVersion, GLForm1.GLBox.OpenGLMinorVersion, glGetString(GL_VERSION), glGetString(GL_VENDOR),glGetString(GL_SHADING_LANGUAGE_VERSION)]));
               showmessage('Requires more recent OpenGL');
               halt();
             end;*)
             {$IFNDEF COREGL}
             Load_GL_EXT_framebuffer_object;
             Load_GL_ARB_framebuffer_object;
             Load_GL_EXT_texture_object;
             {$IFDEF GEOM_GLEXT} //requires new version of glext.pp with glProgramParameteriEXT
             gShader.isGeometryShaderSupported := HasGeometryShaderSupport();
             if gShader.isGeometryShaderSupported then begin
               if not Assigned(glProgramParameteriEXT) then
                  SetglProgramParameteriEXT;
               if not Assigned(glProgramParameteriEXT) then
                  gShader.isGeometryShaderSupported := false;
             end;
             {$ELSE}
             gShader.isGeometryShaderSupported := false;
             {$ENDIF}
             {$ENDIF}
      {$ENDIF}
      gShader.Vendor := glGetString(GL_VENDOR)+' :: OpenGL  '+glGetString(GL_VERSION)+' :: GLSL ' +glGetString(GL_SHADING_LANGUAGE_VERSION);
      gShader.program2d := initVertFrag(kVert2D, '', kFrag2D);
      gShader.program3dx :=  initVertFrag(kVert3d, '', kFrag3d);
      gShader.programDefault :=  initVertFrag(kVert3d, '', kFrag3d);
      gShader.AOradiusU:= 0;
      {$IFDEF COREGL}
      if LoadShader(AppDir+'ao3.glsl', lShader)  then
          gShader.programAoID :=  initVertFrag(lShader.VertexProgram, lShader.GeometryProgram,  lShader.FragmentProgram)
      else
          gShader.programAoID := initVertFrag(kAoShaderVert, '', kAoShaderFrag);
       {$IFDEF TUBES}
       if LoadShader(AppDir+'tubes3.glsl', lShader)  then
          gShader.programTrackID :=  initVertFrag(lShader.VertexProgram, lShader.GeometryProgram,  lShader.FragmentProgram)
      else
       gShader.programTrackID :=  initVertFrag(kVert3d, '', kTrackShaderFrag);
       {$ELSE}
       if LoadShader(AppDir+'tracks3.glsl', lShader)  then
          gShader.programTrackID :=  initVertFrag(lShader.VertexProgram, lShader.GeometryProgram,  lShader.FragmentProgram)
      else
          gShader.programTrackID :=  initVertFrag(kTrackShaderVert, kTrackShaderGeom, kTrackShaderFrag);
       {$ENDIF}
      {$ELSE}
      if (not Assigned(glBindFramebufferEXT)) or
         (not Assigned(glBindTexture)) or
         (not Assigned(glCheckFramebufferStatus)) then
         result := false;//FrameBuffer and Texture not required in OpenGL2.1: avoid problems with VirtualBox/Centos6.6
      if LoadShader(AppDir+'ao.glsl', lShader)  then
          gShader.programAoID :=  initVertFrag(lShader.VertexProgram, lShader.GeometryProgram,  lShader.FragmentProgram)
      else
          gShader.programAoID := initVertFrag('','', kAoShaderFrag);
      //showmessage(AppDir+'tracks.glsl');
      gShader.programTrackID :=  initVertFrag(kTrackShaderVert,'',  kTrackShaderFrag);
      {$ENDIF}
      initFrame(gShader.f1);
      initFrame(gShader.f2);
      initFrame(gShader.fScreenShot);
  end;

  //glGetError(); //clear errors
  if (length(gShader.VertexProgram) > 0) then begin
     glUseProgram(0);
     glDeleteProgram(gShader.program3dx);
     //glGetError(); //clear error
     //glProgramParameteriEXT( gShader.program3d, GL_GEOMETRY_VERTICES_OUT_EXT, 3 );
     gShader.program3dx :=  initVertFrag(gShader.VertexProgram, gShader.GeometryProgram, gShader.FragmentProgram);
     if (gShader.program3dx = 0) then //failed to load custom shader - use default
        gShader.program3dx :=  initVertFrag(kVert3d, '', kFrag3d);
     gShader.VertexProgram := '';
     gShader.GeometryProgram := '';
     gShader.FragmentProgram := '';
     glUseProgram(0);
     {$IFDEF COREGL}
     (*glDeleteProgram(gShader.programAoID);
     if gShader.isToonAO then
        gShader.programAoID := initVertFrag(kAoShaderVert, '', kAoShaderFragToon)
     else
        gShader.programAoID := initVertFrag(kAoShaderVert, '', kAoShaderFrag);  *)
     {$ENDIF}

     {$IFDEF COREGL} {$IFNDEF TUBES}  //666Demo
     (* if not isStartUp then begin
        glUseProgram(0);
        if LoadShader(AppDir+'tracks3.glsl', lShader)  then begin
          glDeleteProgram(gShader.programTrackID);
          gShader.programTrackID :=  initVertFrag(lShader.VertexProgram, lShader.GeometryProgram,  lShader.FragmentProgram);
         end;
               //else
          //    gShader.programTrackID :=  initVertFrag(kTrackShaderVert, kTrackShaderGeom, kTrackShaderFrag);
         //glUseProgram(0);
         glUseProgram(gShader.programTrackID);
         glUseProgram(0);

     end; *)
     {$ENDIF}{$ENDIF}

  end;
end;

function strtofloat0 (lS:string): single;
var x : single;
	code : integer;
begin
  result := 0;
  if length(lS) < 1 then exit;
  if (upcase (lS[1]) = 'T')  or (upcase (lS[1]) = 'F') then exit; //old unsupported 'SET' used true/false booleans begin
  
     val(lS, x, code);
     result :=  x;
end;

function RStrToInt(lS: string; Default: integer): integer;
var
  lV: string;
  lC: char;
  lLen,lP,lN: integer;
begin
  result := Default;
  lV := '';
  for lP := 1 to length(lS) do
      if lS[lP] in ['0'..'9'] then
         lV := lV + lS[lP];
  result := StrToIntDef(lV, Default);
end;

function StrToUniform(lS: string): TUniform;
var
  lV: string;
  lC: char;
  lLen,lP,lN: integer;
begin
  result.Name := '';
  result.Widget := kError;
  lLen := length(lS);
  //read values
  lV := '';
  lP := 1;
  lN := 0;
  while (lP <= lLen) do begin
    if lS[lP] = '/' then
      exit;
    if lS[lP] <> '|' then
      lV := lV + lS[lP];
    if (lS[lP] = '|') or (lP = lLen) then begin
        inc(lN);
        case lN of
          1: result.Name := lV;
          2: begin
              lC := upcase (lV[1]);
              case lC of
                'S' : result.Widget := kSet;
                'B' : result.Widget := kBool;
                'I' : result.Widget := kInt;
                'F' : result.Widget := kFloat;
                'N' : begin
                    result.Widget := kNote;
                    exit;
                  end;
                else
                  showmessage('Unkown uniform type :'+lV);
                  exit;
              end;
            end;
          3: begin
            if (result.Widget = kBool) {or (result.Widget = kSet)} then begin
              result.bool := upcase (lV[1]) = 'T';
            end else
              result.min := strtofloat0(lV);
            end;
          4: result.defaultv := strtofloat0(lV);
          5: result.max := strtofloat0(lV);
        end;
        lV := '';
    end;
    inc(lP);
  end;
end;

procedure DefaultShader( var Shader: TShader);
begin
  Shader.Note := 'Please reinstall this software: Unable to find the shader folder';
  Shader.VertexProgram := kVert3d;
  Shader.GeometryProgram:= '';
  Shader.nUniform := 0;
  Shader.OverlayVolume := 0;//false;
  Shader.FragmentProgram :=  kFrag3d;
end;

function LoadShader(lFilename: string; var Shader: TShader): boolean;
const
  knone=0;
  kpref=1;
  kvert = 2;
  kfrag = 3;
  kgeom = 4;
  kBOM = chr($EF)+chr($BB)+chr($BF); //https://en.wikipedia.org/wiki/Byte_order_mark
  kPrefStr = '//pref';
  kPrefBOM = kBOM+kPrefStr;
  kVertStr = '//vert';
  kVertBOM = kBOM+kVertStr;
  kFragStr = '//frag';
  kFragBOM = kBOM + kFragStr;
  kGeomStr = '//geom';
  kGeomBOM = kBOM + kGeomStr;
var
  mode: integer;
  F : TextFile;
  S: string;
  U: TUniform;
begin
  result := false;
  Shader.Note := '';
  Shader.AOradiusU := 0;
  Shader.VertexProgram := '';
  Shader.GeometryProgram := '';
  Shader.nUniform := 0;
  Shader.OverlayVolume := 0;//false;
  Shader.FragmentProgram := '';
  if not fileexists(lFilename) then  lFilename := lFilename +'.txt';
  if not fileexists(lFilename) then begin
       DefaultShader(Shader);
    exit;
  end;
  mode := knone;
  FileMode := fmOpenRead;
  AssignFile(F,lFilename);
  Reset(F);
  while not Eof(F) do begin
    ReadLn(F, S);
    //if (PosEx(kAOStr,S) = 1) or (PosEx(kAOBOM,S) = 1)  then
    //  gShader.AOradius := RStrToInt(S, gShader.AOradius);
    if (S = kPrefStr) or (S = kPrefBOM)  then
      mode := kpref
    else if (S = kVertStr) or (S = kVertBOM) then
      mode := kvert
    else if (S = kGeomStr) or (S = kGeomBOM) then
      mode := kgeom
    else if (S = kFragStr) or (S = kFragBOM) then
      mode := kfrag
    else if mode = kpref then begin
      U := StrToUniform(S);
      if U.Widget = kSet then begin
        if U.Name = 'overlayVolume' then
          Shader.OverlayVolume:= round(U.min) ; //U.Bool;
      end else if U.Widget = kNote then
        Shader.Note := U.Name
      else if U.Widget <> kError then begin
        if (Shader.nUniform < kMaxUniform) then begin
          inc(Shader.nUniform);
          Shader.Uniform[Shader.nUniform] := U;
          if U.Name = 'AOradius' then
            Shader.AOradiusU := Shader.nUniform;
        end else
          showmessage('Too many preferences');
      end ;
    end else if mode = kfrag then
      Shader.FragmentProgram := Shader.FragmentProgram + S+#13#10 //kCR
    else if mode = kvert then
      Shader.VertexProgram := Shader.VertexProgram + S+#13#10
    else if mode = kgeom then
      Shader.GeometryProgram := Shader.GeometryProgram + S+#13#10;
  end;//EOF
  CloseFile(F);
  if Shader.VertexProgram = '' then
    Shader.VertexProgram := kVert3d;
  if Shader.FragmentProgram = '' then begin
    Shader.nUniform := 0;
    Shader.OverlayVolume := 0;//false;
    Shader.FragmentProgram :=  kFrag3d;
  end else
      result := true;
end;

procedure RunTrackGLSL (lineWidth, ScreenPixelX, ScreenPixelY: integer);
begin
     glUseProgram(gShader.programTrackID);
     //GLForm1.caption := inttostr(random(888));
     {$IFDEF COREGL}
     //AdjustShaders(gShader);
     SetTrackUniforms (lineWidth, ScreenPixelX, ScreenPixelY);
     {$ENDIF}
end;

procedure RunMeshGLSL (clipPlane: TPoint4f;  UseDefaultShader: boolean);
var
  lProg: gluint;
begin

  if UseDefaultShader then begin
    lProg := gShader.programDefault;
    glUseProgram(lProg) ;
  end else begin
    lProg := gShader.program3dx;
    glUseProgram(lProg);
    AdjustShaders(gShader);
  end;
  uniform4fx(lProg, 'ClipPlane', clipPlane);
  {$IFDEF COREGL}
  uniform3fx(lProg, 'LightPos',gShader.lightPos.X, gShader.lightPos.Y, gShader.lightPos.Z);
  SetCoreUniforms(lProg);
  {$ENDIF}
end;

procedure RunOverlayGLSL (clipPlane: TPoint4f);//(cp1,cp2,cp3,cp4: single);
begin
  //if not gPrefs.ShaderForBackgroundOnly then exit;
  //RunMeshGLSL(asPt4f(2.0, 0.0, 0.0, 0.0),  gPrefs.ShaderForBackgroundOnly);
  RunMeshGLSL(clipPlane,  gPrefs.ShaderForBackgroundOnly);
end;

{$IFDEF COREGL}
procedure CoreDrawQuad;
const
  left_ = -1.0;
  bottom_ = -1.0;
  right_ = 1.0;
  top_ = 1.0;
      kATTRIB_VERT = 0;  //vertex XYZ are positions 0,1,2
      kATTRIB_TEX = 3;   //color RGBA are positions 3,46
var
  points: array [0..19]  of single = (left_,  top_,0,    0,1 ,
                         left_, bottom_,    0,  0,0,
                         right_, top_, 0,   1,1,
                         right_,  bottom_, 0,  1, 0
 );
  i,nface: integer;
    faces: TInts;
     vbo_point, vao, vbo_face2d : GLuint;
  begin
    vbo_point := 0;
    glGenBuffers(1, @vbo_point);
    glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
    glBufferData(GL_ARRAY_BUFFER, 20*SizeOf(single), @points[0], GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    // Prepare vertrex array object (VAO)
    vao := 0;
    glGenVertexArrays(1, @vao);
    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
    //Vertices
    glVertexAttribPointer(kATTRIB_VERT, 3, GL_FLOAT, kGL_FALSE, 5*sizeof(single), PChar(0));
    glEnableVertexAttribArray(kATTRIB_VERT);
    //Color
    glVertexAttribPointer(kATTRIB_TEX, 2, GL_FLOAT, kGL_TRUE, 5*sizeof(single), PChar( sizeof(TPoint3f)));
    glEnableVertexAttribArray(kATTRIB_TEX);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
    glDeleteBuffers(1, @vbo_point);
    vbo_face2d := 0;
    glGenBuffers(1, @vbo_face2d);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbo_face2d);
    nface := 4; //each quad has 4 vertices
    setlength(faces,nface);
    for i := 0 to (nface-1) do
        faces[i] := i;
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, nface*sizeof(uint32), @faces[0], GL_STATIC_DRAW);
    glBindVertexArray(vao);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbo_face2d);
    glDrawElements(GL_TRIANGLE_STRIP, nface, GL_UNSIGNED_INT, nil);
    glDeleteVertexArrays(1,@vao);
    glDeleteBuffers(1, @vbo_face2d);
end;
{$ENDIF}

procedure RunAoGLSL (var f1, f2: TFrameBuffer; zoom : integer; alpha1, blend1, fracAO, distance: single);
{$IFNDEF COREGL}
var
  left, right, top, bottom: single;
{$ENDIF}
begin
  glUseProgram(gShader.programAoID);
  glActiveTexture( GL_TEXTURE1);
  glBindTexture(GL_TEXTURE_2D, f1.tex);//all items
  uniform1ix(gShader.programAoID, 'tex1', 1);
  glActiveTexture( GL_TEXTURE2);
  glBindTexture(GL_TEXTURE_2D, f2.tex);//overlays only
  uniform1ix(gShader.programAoID, 'tex2', 2);
  glActiveTexture( GL_TEXTURE3);
  glBindTexture(GL_TEXTURE_2D, f1.depthBuf);//depth map  all items
  uniform1ix(gShader.programAoID, 'depth_texture1', 3);
  glActiveTexture( GL_TEXTURE4);
  glBindTexture(GL_TEXTURE_2D, f2.depthBuf);//depth map  all items
  uniform1ix(gShader.programAoID, 'depth_texture2', 4);
   {$IFDEF HEMISSAO}
  glActiveTexture( GL_TEXTURE5);
  glBindTexture(GL_TEXTURE_2D, f1.tex1);//normal map
  uniform1ix(gShader.programAoID, 'norm1', 5);
 {$ENDIF}
 uniform1fx(gShader.programAoID, 'fracAO', fracAO);
 if gShader.AOradiusU > 0 then begin
  uniform1fx(gShader.programAoID, 'aoRadius', gShader.Uniform[gShader.AOradiusU].DefaultV);
  if gShader.Uniform[gShader.AOradiusU].DefaultV <= 0 then
    uniform1fx(gShader.programAoID, 'fracAO', 0.0);
 end else
  uniform1fx(gShader.programAoID, 'aoRadius', zoom * 16.0 / distance);
 uniform2fx(gShader.programAoID, 'texture_size', f1.w, f1.h);
  //   GLForm1.caption := floattostr(distance);
  //
  uniform1fx(gShader.programAoID, 'blend1', blend1);
  uniform1fx(gShader.programAoID, 'alpha1', alpha1);
  //if gShader.AOradius <= 0 then
  //   uniform1fx(gShader.programAoID, 'fracAO', 0.0)
  //else

  {$IFDEF COREGL}
  CoreDrawQuad;

  {$ELSE}
     left := 0; right := 1;
  bottom := 0; top := 1;
  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0); glVertex3f(left, bottom,  -5);    // Bottom Left Of The Texture and Quad
  glTexCoord2f(1.0, 0.0); glVertex3f( right, bottom,  -5);  // Bottom Right Of The Texture and Quad
  glTexCoord2f(1.0, 1.0); glVertex3f( right, top,  -5); // Top Right Of The Texture and Quad
  glTexCoord2f(0.0, 1.0); glVertex3f(left,  top,  -5);  // Top Left Of The Texture and Quad
  glEnd();
  {$ENDIF}
end;

initialization
  //gShader.Init := true;
  //gShader.program3d := 0;
end.

