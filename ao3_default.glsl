//vert
#version 330
layout(location = 0) in vec3 Vert;
layout(location = 3) in vec2 Coord;
out vec2 texCoord;
void main () {
  gl_Position = vec4 (Vert, 1.0);
  texCoord = Coord;
}
//frag
#version 330
uniform sampler2D tex1, tex2, depth_texture1, depth_texture2;
uniform float blend1, alpha1, fracAO, aoRadius;
uniform vec2 texture_size;
#define PI    3.14159265
smooth in vec2 texCoord;
out vec4 color;
//general stuff
int samples = 32; //ao sample count
float aoclamp = 0.25; //depth clamp - reduces haloing at screen edges
bool noise = true; //use noise instead of pattern for sample dithering
float noiseamount = 0.0002; //dithering amount
float diffarea = 0.5; //self-shadowing reduction
float gdisplace = 0.4; //gauss bell center
vec2 rand(vec2 coord) {
	float noiseX = ((fract(1.0-coord.s*(texture_size.x/2.0))*0.25)+(fract(coord.t*(texture_size.y/2.0))*0.75))*2.0-1.0;
	float noiseY = ((fract(1.0-coord.s*(texture_size.x/2.0))*0.75)+(fract(coord.t*(texture_size.y/2.0))*0.25))*2.0-1.0;
	if (noise) {
	   noiseX = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233))) * 43758.5453),0.0,1.0)*2.0-1.0;
	   noiseY = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233)*2.0)) * 43758.5453),0.0,1.0)*2.0-1.0;
	}
	return vec2(noiseX,noiseY)*noiseamount;
}
float readDepth(in vec2 coord) {
   return 1.0 -  ((texture(depth_texture1, coord).x + 0.87) * 0.534);
}
float compareDepths(in float depth1, in float depth2,inout int far) {
	float garea = 2.0; //gauss bell width
	float diff = (depth1 - depth2)*100.0; //depth difference (0-100)
	if (diff<gdisplace) //reduce left bell width to avoid self-shadowing
	  garea = diffarea;
	else
	  far = 1;
	float gauss = pow(2.7182,-2.0*(diff-gdisplace)*(diff-gdisplace)/(garea*garea));
	return gauss;
}
float calAO(float depth, vec2 coordwh) {
	int far = 0;
	float temp = compareDepths(depth, readDepth(texCoord+coordwh),far);
	//DEPTH EXTRAPOLATION:
	if (far > 0) {
		float temp2 = compareDepths(readDepth(texCoord-coordwh),depth,far);
		temp += (1.0-temp)*temp2;
	}
	return temp;
}
float getAO(void) {
	vec2 noise = rand(texCoord);
	float depth = readDepth(texCoord);
	float dd = (1.0-depth)*aoRadius;
	float w = (1.0 / texture_size.x)/clamp(depth,aoclamp,1.0)+(noise.x*(1.0-noise.x));
	float h = (1.0 / texture_size.y)/clamp(depth,aoclamp,1.0)+(noise.y*(1.0-noise.y));
	float ao = 0.0;
	float dl = PI*(3.0-sqrt(5.0));
	float dz = 1.0/float(samples);
	float l = 0.0;
	float z = 1.0 - dz/2.0;
	for (int i = 0; i <= samples; i ++) {
		float r = sqrt(1.0-z);
		ao += calAO(depth, vec2(cos(l)*r*w*dd,sin(l)*r*h*dd));
		z = z - dz;
		l = l + dl;
	}
	ao /= float(samples);
	ao = clamp(ao, 0.0, 0.4) * 2.5; //threshold then LERP 0..1
	ao = smoothstep(0.0, 1.0, ao);
	ao = (1.0 - ao) * fracAO;
	return ao;
}
void main(void) {
  vec4 t1 = texture(tex1, texCoord);
  if (t1.a == 0.0) discard;
  vec4 t2 = texture(tex2, texCoord);
  //float ao = 1.0 - getAO(); color = vec4(ao, ao, ao, 1.0); return;
  if (fracAO > 0.0)
    t1.rgb = clamp(t1.rgb-getAO(), 0.0, 1.0);
  t1.rgb = mix(t2.rgb,t1.rgb, alpha1);
  float depth = 1.0 - (3.0 * (texture(depth_texture2, texCoord).x - texture(depth_texture1, texCoord).x));
  depth = clamp(depth, 0.0, 1.0);
  color = mix(t1, t2, blend1 * depth);
}