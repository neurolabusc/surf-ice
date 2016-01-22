unit gl_2d;

{$mode objfpc}{$H+}

interface
{$Include opts.inc} //compile for either dglOpenGL or glext
uses
    {$IFDEF COREGL}gl_core_matrix,  {$ENDIF}
    {$IFDEF DGL}dglOpenGL, {$ELSE}  gl, glext, {$ENDIF}
   colorTable, matmath, define_types, prefs,
  Classes, SysUtils, mesh, math;


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
+#10'out vec2 texCoord;'
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

procedure Set2DDraw (w,h: integer; r,g,b: byte);

procedure DrawCube (lScrnWid, lScrnHt, lAzimuth, lElevation: integer);
procedure DrawCLUT ( lU: TUnitRect; lBorder: single; var lPrefs: TPrefs; lMesh: TMesh; window_width, window_height: integer );
procedure DrawText (var lPrefs: TPrefs; lScrnWid, lScrnHt: integer);

implementation

uses shaderu, mainunit;
const

  kVert : array [1..50] of tpoint = (
   (X:0;Y:0),(X:0;Y:4),(X:0;Y:8),(X:0;Y:12),(X:0;Y:13),
(X:0;Y:14),(X:0;Y:15),(X:0;Y:16),(X:0;Y:17),(X:0;Y:18),
(X:0;Y:22),(X:0;Y:24),(X:0;Y:28),(X:2;Y:14),(X:4;Y:0),
(X:4;Y:4),(X:4;Y:8),(X:4;Y:11),(X:4;Y:12),(X:4;Y:13),
(X:4;Y:15),(X:4;Y:16),(X:4;Y:17),(X:4;Y:22),(X:4;Y:24),
(X:4;Y:28),(X:8;Y:0),(X:8;Y:14),(X:8;Y:18),(X:8;Y:24),
(X:14;Y:0),(X:14;Y:4),(X:14;Y:12),(X:14;Y:13),(X:14;Y:16),
(X:14;Y:17),(X:14;Y:22),(X:14;Y:24),(X:14;Y:28),(X:16;Y:14),
(X:18;Y:0),(X:18;Y:4),(X:18;Y:11),(X:18;Y:12),(X:18;Y:13),
(X:18;Y:15),(X:18;Y:16),(X:18;Y:22),(X:18;Y:24),(X:18;Y:28)
);

 kStripRaw : array [0..11,1..28] of byte = (
    (16, 12, 2, 25, 15, 16, 31, 32, 42, 38, 49, 39, 25, 26, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (15, 25, 27, 30, 26, 25, 13, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (42, 41, 16, 1, 22, 4, 22, 47, 4, 33, 47, 39, 49, 26, 25, 12, 24, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (16, 3, 2, 17, 15, 16, 31, 32, 42, 33, 44, 40, 19, 22, 35, 33, 47, 39, 49, 26, 25, 12, 24, 11, 0, 0, 0, 0 ),
    (13, 26, 7, 21, 18, 46, 43, 50, 41, 39, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (49, 50, 25, 13, 20, 9, 20, 45, 9, 36, 45, 31, 42, 15, 16, 2, 17, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (38, 48, 49, 37, 39, 38, 26, 25, 12, 23, 9, 5, 36, 34, 20, 23, 5, 15, 2, 31, 32, 42, 33, 44, 35, 0, 0, 0 ),
    (11, 13, 24, 25, 13, 50, 25, 49, 38, 27, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (16, 4, 2, 19, 15, 16, 31, 32, 42, 33, 44, 40, 19, 22, 35, 33, 47, 39, 49, 26, 25, 12, 22, 8, 14, 22, 19, 4 ),
    (16, 3, 2, 17, 15, 16, 31, 32, 42, 33, 44, 47, 19, 22, 35, 33, 47, 39, 49, 26, 25, 12, 22, 8, 19, 0, 0, 0 ),
    (1, 15, 2, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (6, 10, 28, 29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    );
 kStripLocal : array [0..11,1..28] of byte = (
 (1, 2, 3, 4, 5, 1, 6, 7, 8, 9, 10, 11, 4, 12, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
   (1, 2, 3, 4, 5, 2, 6, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (1, 2, 3, 4, 5, 6, 5, 7, 6, 8, 7, 9, 10, 11, 12, 13, 14, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (1, 2, 3, 4, 5, 1, 6, 7, 8, 9, 10, 11, 12, 13, 14, 9, 15, 16, 17, 18, 19, 20, 21, 22, 0, 0, 0, 0 ),
    (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (1, 2, 3, 4, 5, 6, 5, 7, 6, 8, 7, 9, 10, 11, 12, 13, 14, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (1, 2, 3, 4, 5, 1, 6, 7, 8, 9, 10, 11, 12, 13, 14, 9, 11, 15, 16, 17, 18, 19, 20, 21, 22, 0, 0, 0 ),
    (1, 2, 3, 4, 2, 5, 4, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (1, 2, 3, 4, 5, 1, 6, 7, 8, 9, 10, 11, 4, 12, 13, 9, 14, 15, 16, 17, 18, 19, 12, 20, 21, 12, 4, 2 ),
    (1, 2, 3, 4, 5, 1, 6, 7, 8, 9, 10, 11, 12, 13, 14, 9, 11, 15, 16, 17, 18, 19, 13, 20, 12, 0, 0, 0 ),
    (1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
    );
 kStripCount : array [0..11] of byte = (15, 8, 18, 24, 11, 18, 25, 11, 28, 25, 4, 4 );
 kStripWid : array [0..11] of byte = (9, 5, 9, 9, 9, 9, 9, 9, 9, 9, 2, 4 );

  (*procedure Enter2D(lPrefs: TPrefs);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, lPrefs.window_width, 0, lPrefs.window_height,-1,1);//<- same effect as previous line
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glDisable(GL_DEPTH_TEST);
end;*)
{$IFDEF COREGL}
type
TVtxClr = Packed Record
  vtx   : TPoint3f; //vertex coordinates
  clr : TRGBA;
end;

var
    g2Dvnc: array of TVtxClr;
    g2Drgba : TRGBA;
    g2DNew: boolean;
procedure nglColor4f(r,g,b,a: single);
begin
  g2Drgba.r := round(r * 255);
  g2Drgba.g := round(g * 255);
  g2Drgba.b := round(b * 255);
  g2Drgba.a := round(a * 255);
end;

procedure nglColor4ub (r,g,b,a: byte);
begin
  g2Drgba.r := round(r );
  g2Drgba.g := round(g );
  g2Drgba.b := round(b );
  g2Drgba.a := round(a );
end;

procedure nglVertex3f(x,y,z: single);
begin
     setlength(g2Dvnc, length(g2Dvnc)+1);
     g2Dvnc[high(g2Dvnc)].vtx.X := x;
     g2Dvnc[high(g2Dvnc)].vtx.Y := y;
     g2Dvnc[high(g2Dvnc)].vtx.Z := z;
     g2Dvnc[high(g2Dvnc)].clr := g2Drgba;
     if not g2DNew then exit;
     g2DNew := false;
     setlength(g2Dvnc, length(g2Dvnc)+1);
     g2Dvnc[high(g2Dvnc)] := g2Dvnc[high(g2Dvnc)-1];
end;

procedure nglVertex2f(x,y: single);
begin
     nglVertex3f(x,y, 0);
end;

procedure nglBegin(mode: integer);
begin
     g2DNew := true;
end;

procedure nglEnd;
begin
     //add tail
     if length(g2Dvnc) < 1 then exit;
     setlength(g2Dvnc, length(g2Dvnc)+1);
     g2Dvnc[high(g2Dvnc)] := g2Dvnc[high(g2Dvnc)-1];
end;

procedure DrawTextCore (lScrnWid, lScrnHt: integer);
begin
  nglMatrixMode(nGL_MODELVIEW);
  nglLoadIdentity;
  nglMatrixMode (nGL_PROJECTION);
  nglLoadIdentity ();
  nglOrtho (0, lScrnWid,0, lScrnHt,-10,10);
end;

procedure DrawStrips (lScrnWid, lScrnHt: integer);
const
    kATTRIB_VERT = 0;  //vertex XYZ are positions 0,1,2
    kATTRIB_CLR = 3;   //color RGBA are positions 3,4,5,6
var
  i,nface: integer;
  faces: TInts;
   vbo_point : GLuint;
  mv : TnMat44;
  mvpMat: GLint;
begin
  //setup 2D
    if Length(g2Dvnc) < 1 then exit;
   //exit;
  //gShader.uModelViewProjectionMatrix2D := glGetUniformLocation(gShader.program2d, pAnsiChar('ModelViewProjectionMatrix'));
  glUseProgram(gShader.program2d);
  if gShader.vao_point2d <> 0 then
           glDeleteVertexArrays(1,@gShader.vao_point2d);
  glGenVertexArrays(1, @gShader.vao_point2d);
  if (gShader.vbo_face2d <> 0) then
        glDeleteBuffers(1, @gShader.vbo_face2d);
  glGenBuffers(1, @gShader.vbo_face2d);
  vbo_point := 0;
  glGenBuffers(1, @vbo_point);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  glBufferData(GL_ARRAY_BUFFER, Length(g2Dvnc)*SizeOf(TVtxClr), @g2Dvnc[0], GL_STATIC_DRAW);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  // Prepare vertrex array object (VAO)
  glBindVertexArray(gShader.vao_point2d);
  glBindBuffer(GL_ARRAY_BUFFER, vbo_point);
  //Vertices
  glVertexAttribPointer(kATTRIB_VERT, 3, GL_FLOAT, kGL_FALSE, sizeof(TVtxClr), PChar(0));
  glEnableVertexAttribArray(kATTRIB_VERT);
  //Color
  glVertexAttribPointer(kATTRIB_CLR, 4, GL_UNSIGNED_BYTE, kGL_TRUE, sizeof(TVtxClr), PChar( sizeof(TPoint3f)));
  glEnableVertexAttribArray(kATTRIB_CLR);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindVertexArray(0);
  glDeleteBuffers(1, @vbo_point);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, gShader.vbo_face2d);
  nface := Length(g2Dvnc); //each face has 3 vertices
  setlength(faces,nface);
  if nface > 0 then begin
     for i := 0 to (nface-1) do begin
         faces[i] := i;
         glBufferData(GL_ELEMENT_ARRAY_BUFFER, nface*sizeof(uint32), @faces[0], GL_STATIC_DRAW);
     end;
  end;
  mv := ngl_ModelViewProjectionMatrix;
  mvpMat := glGetUniformLocation(gShader.program2d, pAnsiChar('ModelViewProjectionMatrix'));
  glUniformMatrix4fv(mvpMat, 1, kGL_FALSE, @mv[0,0]); // note model not MVP!
  glBindVertexArray(gShader.vao_point2d);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, gShader.vbo_face2d);
  glDrawElements(GL_TRIANGLE_STRIP, nface, GL_UNSIGNED_INT, nil);
end;

procedure Enter2D(w, h: integer);
begin
  glUseProgram(gShader.program2d);
  glDisable(GL_DEPTH_TEST);
end;

procedure Set2DDraw (w,h: integer; r,g,b: byte);
begin

glDepthMask(kGL_TRUE); // enable writes to Z-buffer
glEnable(GL_DEPTH_TEST);
glDisable(GL_CULL_FACE); // glEnable(GL_CULL_FACE); //check on pyramid
glEnable(GL_BLEND);
glEnable(GL_NORMALIZE);
glClearColor(r/255, g/255, b/255, 0.0); //Set background
glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
glViewport( 0, 0, w, h); //required when bitmap zoom <> 1
end;


{$ELSE}

procedure Set2DDraw (w,h: integer; r,g,b: byte);
begin
glMatrixMode(GL_PROJECTION);
glLoadIdentity();
glOrtho (0, 1, 0, 1, -6, 12);
glMatrixMode (GL_MODELVIEW);
glLoadIdentity ();
{$IFDEF DGL}
glDepthMask(BYTEBOOL(1)); // enable writes to Z-buffer
{$ELSE}
glDepthMask(GL_TRUE); // enable writes to Z-buffer
{$ENDIF}

glEnable(GL_DEPTH_TEST);
glDisable(GL_CULL_FACE); // glEnable(GL_CULL_FACE); //check on pyramid
glEnable(GL_BLEND);
glEnable(GL_NORMALIZE);
glClearColor(r/255, g/255, b/255, 0.0); //Set background
glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
glViewport( 0, 0, w, h); //required when bitmap zoom <> 1
end;


procedure Enter2D(w, h: integer);
begin
  glUseProgram(gShader.program2d);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, w, 0, h,-1,1);//<- same effect as previous line
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glDisable(GL_DEPTH_TEST);
end;

procedure nglColor4f(r,g,b,a: single);
begin
  glColor4f(r,g,b,a);
end;

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

procedure nglEnd;
begin
     glEnd();
end;

{$ENDIF}


function PrintHt (Sz: single): single;
begin
  result := Sz * 14;//14-pixel tall font
end;

function Char2Int (c: char): integer;
begin
    result := ord(c)-48;//ascii '0'..'9' = 48..58
    if result = -3 then result := 11; // '-' minus;
    if (result < 0) or (result > 11) then result := 10; //'.'or''
end;

function PrintWid (Sz: single; NumStr: string): single;
var
  i: integer;
begin
  result := 0;
  if length(NumStr) < 1 then
    exit;
  for i := 1 to length(NUmStr) do begin
    result := result + kStripWid[Char2Int(NumStr[i])  ] + 1;  ;
  end;
  if result < 1 then
    exit;
  result := result -1;//fence post: no gap after last gap between character
  result := result * sz;
end;

procedure MakeCube(sz: single);
//draw a cube of size sz
var
  sz2: single;
begin
  sz2 := sz;
  nglColor4f(0.1,0.1,0.1,1);
  nglBegin(GL_TRIANGLE_STRIP); //* Bottom side
	nglVertex3f(-sz, -sz, -sz2);
	nglVertex3f(-sz, sz, -sz2);
	nglVertex3f(sz, -sz, -sz2);
        nglVertex3f(sz, sz, -sz2);
  nglEnd;
  nglColor4f(0.8,0.8,0.8,1);
  nglBegin(GL_TRIANGLE_STRIP); //* Top side
	nglVertex3f(-sz, -sz, sz2);
	nglVertex3f(sz, -sz, sz2);
        nglVertex3f(-sz, sz, sz2);
        nglVertex3f(sz, sz, sz2);
  nglEnd;
  nglColor4f(0,0,0.5,1);
  nglBegin(GL_TRIANGLE_STRIP); //* Front side
    nglVertex3f(-sz, sz2, -sz);
    nglVertex3f(-sz, sz2, sz);
    nglVertex3f(sz, sz2, -sz);
    nglVertex3f(sz, sz2, sz);
  nglEnd;
  nglColor4f(0.3,0,0.3,1);
  nglBegin(GL_TRIANGLE_STRIP);//* Back side
	nglVertex3f(-sz, -sz2, -sz);
	nglVertex3f(sz, -sz2, -sz);
	nglVertex3f(-sz, -sz2, sz);
	nglVertex3f(sz, -sz2, sz);
  nglEnd;
  nglColor4f(0.6,0,0,1);
  nglBegin(GL_TRIANGLE_STRIP); //* Left side
	nglVertex3f(-sz2, -sz, -sz);
	nglVertex3f(-sz2, -sz, sz);
	nglVertex3f(-sz2, sz, -sz);
	nglVertex3f(-sz2, sz, sz);
  nglEnd;
  nglColor4f(0,0.6,0,1);
  nglBegin(GL_TRIANGLE_STRIP); //* Right side
	//glNormal3f(1.0, -sz, -sz);
	nglVertex3f(sz2, -sz, -sz);
	nglVertex3f(sz2, sz, -sz);
	nglVertex3f(sz2, -sz, sz);
	nglVertex3f(sz2, sz, sz);
  nglEnd();
end; //MakeCube()

procedure DrawCubeCore (lScrnWid, lScrnHt, lAzimuth, lElevation: integer);
var
  sz: single;
begin

  sz := lScrnWid;
  if  sz > lScrnHt then sz := lScrnHt;
  if sz < 10 then exit;
  sz := sz * 0.03;
  {$IFDEF COREGL}
  nglMatrixMode(nGL_MODELVIEW);
  nglLoadIdentity;
  //glDisable(GL_DEPTH_TEST);
  nglMatrixMode (nGL_PROJECTION);
  nglLoadIdentity ();
  nglOrtho (0, lScrnWid,0, lScrnHt,-10*sz,10*sz);
  glEnable(GL_DEPTH_TEST);
  //glDisable (GL_LIGHTING);
  //glDisable (GL_BLEND);
  nglTranslatef(0,0,sz*8);
  nglTranslatef(1.8*sz,1.8*sz,0);
  //nglRotatef(90-lElevation,-1,0,0);
  //nglRotatef(-lAzimuth,0,0,1);
  nglRotatef(90-lElevation,-1,0,0);
  nglRotatef(-lAzimuth,0,0,1);
  //nglTranslatef(0,0,-30);
  {$ELSE}
  //Enter2D(lScrnWid, lScrnHt);


  glUseProgram(gShader.program2d);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, lScrnWid, 0, lScrnHt,-sz*4,0.01);//<- same effect as previous line
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  (*glUseProgram(gShader.program2d);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  //glDisable(GL_DEPTH_TEST);
  glMatrixMode (GL_PROJECTION);
  glLoadIdentity ();
  glOrtho (0, lScrnWid,0, lScrnHt,-10*sz,10*sz); *)
  glEnable(GL_DEPTH_TEST);
  //glDisable (GL_LIGHTING);
  //glDisable (GL_BLEND);
  glTranslatef(0,0,sz*2);
  //glTranslatef(0,0,0.5);

  glTranslatef(1.8*sz,1.8*sz,0);
  glRotatef(90-lElevation,-1,0,0);
  glRotatef(-lAzimuth,0,0,1);
  {$ENDIF}
  MakeCube(sz);
end;


procedure PrintXY (Xin,Y,Sz: single; NumStr: string;FontColor: TRGBA);
//draws numerical strong with 18-pixel tall characters. If Sz=2.0 then characters are 36-pixel tall
//Unless you use multisampling, fractional sizes will not look good...
var
  i, j, k: integer;
  X, Sc: single;
begin
  if length(NumStr) < 1 then
    exit;
  Sc := Sz * 0.5;
  X := Xin;
  nglColor4ub (FontColor.r, FontColor.g, FontColor.b,FontColor.a);
  for i := 1 to length(NUmStr) do begin
    j := Char2Int(NumStr[i]);
    nglBegin(GL_TRIANGLE_STRIP);
    for k := 1 to kStripCount[j] do
        nglVertex2f(Sc*kVert[kStripRaw[j,k]].X + X, Sc*kVert[kStripRaw[j,k]].Y +Y);
    nglEnd;
    X := X + ((kStripWid[j] + 1)* Sz);
  end;
end;

procedure TextArrow (X,Y,Sz: single; NumStr: string; orient: integer; FontColor,ArrowColor: TRGBA);
//orient code 1=left,2=top,3=right,4=bottom
const
 kZ = -0.1; //put border BEHIND text
var
  lW,lH,lW2,lH2,T: single;
begin
  if NumStr = '' then exit;
  glLoadIdentity();
  lH := PrintHt(Sz);
  lH2 := (lH/2);
  lW := PrintWid(Sz,NumStr);
  lW2 := (lW/2);
  nglColor4ub (ArrowColor.r, ArrowColor.g, ArrowColor.b,ArrowColor.a);
  case Orient of
    1: begin
      nglBegin(GL_TRIANGLE_STRIP);
        nglVertex3f(X-lH2-lW-2*Sz,Y+LH2+Sz, kZ);
        nglVertex3f(X-lH2-lW-2*Sz,Y-lH2-Sz, kZ);
        nglVertex3f(X-lH2,Y+lH2+Sz, kZ);
        nglVertex3f(X-lH2,Y-lH2-Sz, kZ);
        nglVertex3f(X,Y, kZ);
      nglEnd;
      PrintXY (X-lW-lH2-1.5*Sz,Y-lH2,Sz, NumStr,FontColor);
    end;
    3: begin
      nglBegin(GL_TRIANGLE_STRIP);
        nglVertex3f(X+lH2+lW+2*Sz,Y+LH2+Sz, kZ);
        nglVertex3f(X+lH2+lW+2*Sz,Y-lH2-Sz, kZ);
        nglVertex3f(X+lH2,Y+lH2+Sz, kZ);
        nglVertex3f(X+lH2,Y-lH2-Sz, kZ);
        nglVertex3f(X,Y, kZ);
      nglEnd;
      PrintXY (X+lH2,Y-lH2,Sz, NumStr,FontColor);
    end;
    4: begin //bottom
    nglBegin(GL_TRIANGLE_STRIP);
      nglVertex3f(X-lW2-Sz,Y-LH-lH2-2*Sz, kZ);//-
      nglVertex3f(X-lW2-Sz,Y-lH2, kZ);
      nglVertex3f(X+lW2+Sz,Y-LH-lH2-2*Sz, kZ);//-
      nglVertex3f(X+lW2+Sz,Y-lH2, kZ);
      nglVertex3f(X-lW2-Sz,Y-lH2, kZ);
      nglVertex3f(X,Y, kZ);
    nglEnd;
    PrintXY (X-lW2-Sz,Y-lH-LH2,Sz, NumStr,FontColor);
    end;
    else  begin
      if Orient = 5 then
        T := Y-LH-Sz-lH2
      else
        T := Y;
    nglBegin(GL_TRIANGLE_STRIP);
      nglVertex3f(X-lW2-Sz,T+LH+2*Sz+lH2, kZ);
      nglVertex3f(X-lW2-Sz,T+lH2, kZ);
      nglVertex3f(X+lW2+Sz,T+LH+2*Sz+lH2, kZ);
      nglVertex3f(X+lW2+Sz,T+lH2, kZ);
      nglVertex3f(X-lW2-Sz,T+lH2, kZ);
      nglVertex3f(X,T, kZ);
    nglEnd;
    PrintXY (X-lW2-Sz,T+lH2+Sz,Sz, NumStr,FontColor);
    end;
  end;//case
end;



procedure SetOrder (l1,l2: single; var lSmall,lLarge: single);
//set lSmall to be the lesser of l1/l2 and lLarge the greater value of L1/L2
begin
  if l1 < l2 then begin
    lSmall := l1;
    lLarge := l2;
  end else begin
    lSmall := l2;
    lLarge := l1;
  end;
end;

procedure DrawCLUTx (var lCLUT: TLUT; lU: TUnitRect; lPrefs: TPrefs);
var
  lL,lT,lR,lB,lN: single;
  lI: integer;
begin
  SetOrder(lU.L,lU.R,lL,lR);
  SetOrder(lU.T,lU.B,lT,lB);
  lL := lL*lPrefs.window_width;
  lR := lR*lPrefs.window_width;
  lT := lT*lPrefs.window_height;
  lB := lB*lPrefs.window_height;
  if (lR-lL) > (lB-lT) then begin
    lN := lL;
    nglBegin(GL_TRIANGLE_STRIP);
     nglColor4ub (lCLUT[0].R, lCLUT[0].G,lCLUT[0].B,255);
     nglVertex2f(lN,lT);
     nglVertex2f(lN,lB);
     for lI := 1 to (255) do begin
        lN := (lI/255 * (lR-lL))+lL;
        nglColor4ub (lCLUT[lI].R, lCLUT[lI].G,lCLUT[lI].B,255);
        nglVertex2f(lN,lT);
        nglVertex2f(lN,lB);
     end;
    nglEnd;//GL_TRIANGLE_STRIP
  end else begin //If WIDE, else TALL
    lN := lT;
    nglColor4ub (lCLUT[0].R, lCLUT[0].G,lCLUT[0].B,255);
    nglBegin(GL_TRIANGLE_STRIP);
     nglVertex2f(lR, lN);
     nglVertex2f(lL, lN);
     for lI := 1 to (255) do begin
        lN := (lI/255 * (lB-lT))+lT;
        nglColor4ub (lCLUT[lI].R, lCLUT[lI].G,lCLUT[lI].B,255);
        nglVertex2f(lR, lN);
        nglVertex2f(lL, lN);
     end;
    nglEnd;//GL_TRIANGLE_STRIP
  end;
end;

procedure DrawBorder (var lU: TUnitRect;lBorder: single; lPrefs: TPrefs);
const
 kZ = -0.1; //put border behind colorbar
var
    lL,lT,lR,lB: single;
begin
  if lBorder <= 0 then
    exit;
  SetOrder(lU.L,lU.R,lL,lR);
  SetOrder(lU.T,lU.B,lT,lB);
  nglColor4ub(lPrefs.GridAndBorder.r,lPrefs.GridAndBorder.g,lPrefs.GridAndBorder.b,lPrefs.GridAndBorder.a);
  nglBegin(GL_TRIANGLE_STRIP);
      nglVertex3f((lL-lBorder)*lPrefs.window_width,(lB+lBorder)*lPrefs.window_height, kZ);
      nglVertex3f((lL-lBorder)*lPrefs.window_width,(lT-lBorder)*lPrefs.window_height, kZ);
      nglVertex3f((lR+lBorder)*lPrefs.window_width,(lB+lBorder)*lPrefs.window_height, kZ);
      nglVertex3f((lR+lBorder)*lPrefs.window_width,(lT-lBorder)*lPrefs.window_height, kZ);
    nglEnd;//GL_TRIANGLE_STRIP
end;

procedure UOffset (var lU: TUnitRect; lX,lY: single);
begin
  lU.L := lU.L+lX;
  lU.T := lU.T+lY;
  lU.R := lU.R+lX;
  lU.B := lU.B+lY;
end;

procedure SetLutFromZero(var lMin,lMax: single);
//if both min and max are positive, returns 0..max
//if both min and max are negative, returns min..0
begin
    SortSingle(lMin,lMax);
    if (lMin > 0) and (lMax > 0) then
      lMin := 0
    else if (lMin < 0) and (lMax < 0) then
      lMax := 0;
end;



const
  kVertTextLeft = 1;
  kHorzTextBottom = 2;
  kVertTextRight = 3;
  kHorzTextTop = 4;

function ColorBarPos(var  lU: TUnitRect): integer;
begin
   SensibleUnitRect(lU);
   if abs(lU.R-lU.L) > abs(lU.B-lU.T) then begin //wide bars
    if (lU.B+lU.T) >1 then
      result := kHorzTextTop
    else
      result := kHorzTextBottom;
   end else begin //high bars
    if (lU.L+lU.R) >1 then
      result := kVertTextLeft
    else
      result := kVertTextRight;
   end;
end;

procedure DrawColorBarText(lMinIn,lMaxIn: single; var lUin: TUnitRect;lBorder: single; var lPrefs: TPrefs);
var
  lS: string;
  lOrient,lDesiredSteps,lPower,	lSteps,lStep,lDecimals,lStepPosScrn, lTextZoom: integer;
  lBarLength,lScrnL,lScrnT,lStepPos,l1stStep,lMin,lMax,lRange,lStepSize: single;
  lU: TUnitRect;
begin
  lU := lUin;
  lOrient := ColorBarPos(lU);
	 lMin := lMinIn;
	 lMax := lMaxIn;
   if (lMinIn < 0) and (lMaxIn <= 0) then begin
	  lMin := abs(lMinIn);
	  lMax := abs(lMaxIn);
   end;
   sortsingle(lMin,lMax);
   //next: compute increment
   lDesiredSteps := 4;
   lRange := abs(lMax - lMin);
   if lRange < 0.000001 then exit;
   lStepSize := lRange / lDesiredSteps;
   lPower := 0;
   while lStepSize >= 10 do begin
      lStepSize := lStepSize/10;
	    inc(lPower);
   end;
   while lStepSize < 1 do begin
	   lStepSize := lStepSize * 10;
	   dec(lPower);
   end;
   lStepSize := round(lStepSize) *Power(10,lPower);
   if lPower < 0 then
	    lDecimals := abs(lPower)
   else
	    lDecimals := 0;
   l1stStep := trunc((lMin)  / lStepSize)*lStepSize;
   lScrnL := lU.L * lPrefs.window_width;
   if lOrient =  kVertTextRight then
      lScrnL := lU.R * lPrefs.window_width;
   lScrnT := (lU.B) * lPrefs.window_height;
   if lOrient =  kHorzTextTop then
      lScrnT := ((lU.B) * lPrefs.window_height);
   if lOrient =  kHorzTextBottom then
      lScrnT := ((lU.T) * lPrefs.window_height);
   if l1stStep < (lMin) then l1stStep := l1stStep+lStepSize;
    lSteps := trunc( abs((lMax+0.0001)-l1stStep) / lStepSize)+1;
   if (lOrient = kVertTextLeft) or (lOrient = kVertTextRight) then //vertical bars
      lBarLength := lPrefs.window_height * abs(lU.B-lU.T)
   else
      lBarLength := lPrefs.window_width * abs(lU.L-lU.R);
   lTextZoom :=  trunc(lBarLength / 1000) + 1;
   for lStep := 1 to lSteps do begin
      lStepPos := l1stStep+((lStep-1)*lStepSize);
      lStepPosScrn := round( abs(lStepPos-lMin)/lRange*lBarLength);
      lS := realtostr(lStepPos,lDecimals);
      if (lMinIn < 0) and (lMaxIn <= 0) then
        lS := '-'+lS;
      if (lOrient = kVertTextLeft) or  (lOrient = kVertTextRight)  then
         TextArrow (lScrnL,lScrnT+ lStepPosScrn,lTextZoom,lS,lOrient,lPrefs.TextColor, lPrefs.TextBorder)
      else
         TextArrow (lScrnL+ lStepPosScrn,lScrnT,lTextZoom,lS,lOrient,lPrefs.TextColor, lPrefs.TextBorder);
		end;
    glLoadIdentity();
end; //DrawColorBarText

type
TColorBar = packed record
   mn, mx: single;
   LUT: TLUT;
 end;
TColorBars = array of TColorBar;

procedure DrawColorBars ( lU: TUnitRect; lBorder: single; var lPrefs: TPrefs; lColorBars: TColorBars; window_width, window_height: integer );
var
      lU2:TUnitRect;
   nLUT, lI: integer;
   lIsHorzTop: boolean;
     lX,lY,lMin,lMax: single;
begin
  nLUT := length(lColorBars);
  if (nLUT < 1) then exit;
  lPrefs.window_height:= window_height;
  lPrefs.window_width := window_width;
  lIsHorzTop := false;
  //Enter2D(lPrefs);
  Enter2D(window_width, window_height);

  glEnable (GL_BLEND);//allow border to be translucent
  glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  if abs(lU.R-lU.L) > abs(lU.B-lU.T) then begin //wide bars
    lX := 0;
    lY := abs(lU.B-lU.T)+lBorder;
    if (lU.B+lU.T) >1 then
      lY := -lY
    else
      lIsHorzTop := true;
  end else begin //high bars
    lX := abs(lU.R-lU.L)+lBorder;
    lY := 0;
    if (lU.L+lU.R) >1 then
      lX := -lX;
  end;
  //next - draw a border - do this once for all overlays, so
  //semi-transparent regions do not display regions of overlay
  SensibleUnitRect(lU);
  lU2 := lU;
  if nLUT > 1 then begin
    for lI := 2 to nLUT do begin
      if lX < 0 then
        lU2.L := lU2.L + lX
      else
        lU2.R := lU2.R + lX;
      if lY < 0 then
        lU2.B := lU2.B + lY
      else
        lU2.T := lU2.T + lY;
    end;
  end;
  DrawBorder(lU2,lBorder,lPrefs);
  lU2 := lU;

  for lI := 0 to (nLUT-1) do begin
    DrawCLUTx(lColorBars[lI].LUT,lU2,lPrefs);
    UOffset(lU2,lX,lY);
  end;
  lU2 := lU;
  for lI := 0 to (nLUT-1) do begin
    lMin := lColorBars[lI].mn;
    lMax := lColorBars[lI].mx;
    SortSingle(lMin,lMax);
    DrawColorBarText(lMin,lMax, lU2,lBorder,lPrefs);
    UOffset(lU2,lX,lY);
  end;
glDisable (GL_BLEND);
end;

procedure DrawCLUT ( lU: TUnitRect; lBorder: single; var lPrefs: TPrefs; lMesh: TMesh; window_width, window_height: integer );
var
     lU2:TUnitRect;
     lX,lY,lMin,lMax: single;
     lIsHorzTop: boolean;
     lIx,lI: integer;
     nLUT : integer;
     LUT: array [1..3] of TLUT;
     mn, mx: array [1..3] of single;
begin
  if (lMesh.OpenOverlays < 1) and (length(lMesh.nodes) < 1) then exit;
  if (lMesh.OpenOverlays > 0) then
    nLUT := lMesh.OpenOverlays
  else begin
    nLUT := 0;
  if (lMesh.nodePrefs.isNodeColorVaries) then begin
     inc(nLUT);
     LUT[nLUT] := UpdateTransferFunction (lMesh.nodePrefs.NodeLUTindex);
     if (lMesh.nodePrefs.isNodeThresholdBySize) then begin
        mn[nLUT] := lMesh.nodePrefs.minNodeColor;
        mx[nLUT] := lMesh.nodePrefs.maxNodeColor;

     end else begin
       mn[nLUT] := lMesh.nodePrefs.minNodeThresh;
       mx[nLUT] := lMesh.nodePrefs.maxNodeThresh;
     end;
     if mn[nLUT] = mx[nLUT] then
       nLUT := nLUT - 1; //no variability!
  end; //nodes
  if (lMesh.nodePrefs.isEdgeColorVaries) and (lMesh.nodePrefs.maxEdge <> lMesh.nodePrefs.minEdge) then begin
    if (lMesh.nodePrefs.maxEdge > 0)  and (not lMesh.nodePrefs.isNoPosEdge) and (lMesh.nodePrefs.minEdgeThresh <> lMesh.nodePrefs.maxEdgeThresh) then begin
       inc(nLUT);
       LUT[nLUT] := UpdateTransferFunction (lMesh.nodePrefs.edgeLUTindex);
       mn[nLUT] := lMesh.nodePrefs.minEdgeThresh;
       mx[nLUT] := lMesh.nodePrefs.maxEdgeThresh;
    end; //positive edges
    if (lMesh.nodePrefs.minEdge < 0)  and (not lMesh.nodePrefs.isNoNegEdge) and (lMesh.nodePrefs.minEdgeThresh <> lMesh.nodePrefs.maxEdgeThresh) then begin
       inc(nLUT);
       lI := lMesh.nodePrefs.edgeLUTindex + 1;
       LUT[nLUT] := UpdateTransferFunction (lI);
       mn[nLUT] := -lMesh.nodePrefs.minEdgeThresh;
       mx[nLUT] := -lMesh.nodePrefs.maxEdgeThresh;
    end; //negative edges
  end;  //edges
end; //if overlays else edges
  if (nLUT < 1) then exit;
  lPrefs.window_height := window_height;
  lPrefs.window_width := window_width;
  lIsHorzTop := false;
  Enter2D(window_width, window_height);
  glEnable (GL_BLEND);//allow border to be translucent
  glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  if abs(lU.R-lU.L) > abs(lU.B-lU.T) then begin //wide bars
    lX := 0;
    lY := abs(lU.B-lU.T)+lBorder;
    if (lU.B+lU.T) >1 then
      lY := -lY
    else
      lIsHorzTop := true;
  end else begin //high bars
    lX := abs(lU.R-lU.L)+lBorder;
    lY := 0;
    if (lU.L+lU.R) >1 then
      lX := -lX;
  end;
  //next - draw a border - do this once for all overlays, so
  //semi-transparent regions do not display regions of overlay
  SensibleUnitRect(lU);
  lU2 := lU;
  if nLUT > 1 then begin
    for lI := 2 to nLUT do begin
      if lX < 0 then
        lU2.L := lU2.L + lX
      else
        lU2.R := lU2.R + lX;
      if lY < 0 then
        lU2.B := lU2.B + lY
      else
        lU2.T := lU2.T + lY;
    end;
  end;
  {$IFDEF COREGL}
  setlength(g2Dvnc, 0);
  DrawTextCore(window_width, window_height);
  {$ENDIF}
  DrawBorder(lU2,lBorder,lPrefs);
  lU2 := lU;
  if (lMesh.OpenOverlays > 0) then begin
    for lI := 1 to lMesh.OpenOverlays do begin
      DrawCLUTx(lMesh.overlay[lI].LUT,lU2,lPrefs);
      UOffset(lU2,lX,lY);
    end;
    lU2 := lU;
    for lI := 1 to lMesh.OpenOverlays do begin
      lMin := lMesh.overlay[lI].WindowScaledMin;
      lMax := lMesh.overlay[lI].WindowScaledMax;
      SortSingle(lMin,lMax);
      DrawColorBarText(lMin,lMax, lU2,lBorder,lPrefs);
      UOffset(lU2,lX,lY);
    end;
  end else if nLUT > 0 then begin
          for lI := 1 to nLUT do begin
            DrawCLUTx(LUT[lI],lU2,lPrefs);
            UOffset(lU2,lX,lY);
          end;
          lU2 := lU;
          for lI := 1 to nLUT do begin
            lMin := mn[lI];
            lMax := mx[lI];
            SortSingle(lMin,lMax);
            DrawColorBarText(lMin,lMax, lU2,lBorder,lPrefs);
            UOffset(lU2,lX,lY);
          end;
  end; //if overlay else edges
glDisable (GL_BLEND);
{$IFDEF COREGL}
DrawStrips (window_width, window_height);
{$ENDIF}
end;

{$IFDEF COREGL}
procedure DrawCube (lScrnWid, lScrnHt, lAzimuth, lElevation: integer);
begin
  setlength(g2Dvnc, 0);
  DrawCubeCore (lScrnWid, lScrnHt, lAzimuth, lElevation);
  DrawStrips (lScrnWid, lScrnHt);
end;
{$ELSE}
procedure DrawCube (lScrnWid, lScrnHt, lAzimuth, lElevation: integer);
begin
  DrawCubeCore (lScrnWid, lScrnHt, lAzimuth, lElevation);
end;
{$ENDIF}

(*procedure TestColorBar (var lPrefs: TPrefs; window_width, window_height: integer);
var
  c: TColorBars;
  lU: TUnitRect;
  i : integer = 1;
begin
  lU := CreateUnitRect (0.1,0.1,0.9,0.2);
  setlength(c,2);
  i := 2;
  c[0].LUT := UpdateTransferFunction(i);
  c[0].mn := 2;
  c[0].mx := 10;
  i := 1;
  c[1].LUT := UpdateTransferFunction(i);
  c[1].mn := -2;
  c[1].mx := -10;
  DrawColorBars ( lU, 0.005,  lPrefs, c, window_width, window_height );
end;

procedure DrawCube (lScrnWid, lScrnHt, lAzimuth, lElevation: integer);
begin
  setlength(g2Dvnc, 0);
  DrawCubeCore (lScrnWid, lScrnHt, lAzimuth, lElevation);
  DrawStrips (lScrnWid, lScrnHt);
end;

procedure DrawTextCore (lScrnWid, lScrnHt: integer);
begin
  nglMatrixMode(nGL_MODELVIEW);
  nglLoadIdentity;
  nglMatrixMode (nGL_PROJECTION);
  nglLoadIdentity ();
  nglOrtho (0, lScrnWid,0, lScrnHt,-10,10);
  //clr.r := 22; clr.g := 22; clr.b := 222; clr.a := 255;
  //PrintXY(10,320, 2,'-123.9', clr);
  //clr2.r := 65; clr2.g := 10; clr2.b := 220; clr2.a := 128;
  //TextArrow (60,220, 2, '123.9', 2, clr,clr2);
end;

procedure DrawText (var lPrefs: TPrefs; lScrnWid, lScrnHt: integer);
begin
  setlength(g2Dvnc, 0);
  DrawTextCore(lScrnWid, lScrnHt);
  TestColorBar(lPrefs, lScrnWid, lScrnHt);
  DrawStrips (lScrnWid, lScrnHt);
end;   *)

procedure TestColorBar (var lPrefs: TPrefs; window_width, window_height: integer);
var
  c: TColorBars;
  lU: TUnitRect;
  i : integer = 1;
begin
  lU := CreateUnitRect (0.1,0.1,0.9,0.2);
  setlength(c,2);
  i := 2;
  c[0].LUT := UpdateTransferFunction(i);
  c[0].mn := 2;
  c[0].mx := 10;
  i := 1;
  c[1].LUT := UpdateTransferFunction(i);
  c[1].mn := -2;
  c[1].mx := -10;
  DrawColorBars ( lU, 0.005,  lPrefs, c, window_width, window_height );
end;

{$IFDEF COREGL}
procedure DrawText (var lPrefs: TPrefs; lScrnWid, lScrnHt: integer);
begin
 setlength(g2Dvnc, 0);
 DrawTextCore(lScrnWid, lScrnHt);
 TestColorBar(lPrefs, lScrnWid, lScrnHt);
 DrawStrips (lScrnWid, lScrnHt);
end;
{$ELSE}
procedure DrawText (var lPrefs: TPrefs; lScrnWid, lScrnHt: integer);
begin
 TestColorBar(lPrefs, lScrnWid, lScrnHt);
end;

{$ENDIF}

end.

