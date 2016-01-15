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
float width = texture_size.x; //texture width
float height = texture_size.y; //texture height
smooth in vec2 texCoord;
out vec4 color;
#define kPI    3.14159265
#define kEps    0.0001

int samples = 32; //ao sample count
float noiseamount = 0.0002; //dithering amount

vec2 rand(vec2 coord) {
	float noiseX = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233))) * 43758.5453),0.0,1.0)*2.0-1.0;
	float noiseY = clamp(fract(sin(dot(coord ,vec2(12.9898,78.233)*2.0)) * 43758.5453),0.0,1.0)*2.0-1.0;
	return vec2(noiseX,noiseY)*noiseamount;
}
float readDepth(in vec2 coord) {
   return texture(depth_texture1, coord).x;
}

float rad;
float calAOx(float depth, float dw, float dh, float r, inout float wt) {
    vec2 coord = vec2(dw*aoRadius, dh*aoRadius);
	vec3 p1 = vec3(texCoord + coord, 0.0);
	vec3 p2 = vec3(texCoord - coord, 0.0);
	p1.z = readDepth(p1.xy) - depth;
	p2.z = readDepth(p2.xy) - depth;
	// Attenuate the occlusion, similar to how you attenuate a light source.
    // The further the distance between points, the less effect AO has on the fragment.

	float rx = rad ;
	//if ((abs(p1.z+p2.z) > rx)) return 0.5;
	float isConcave = step(0.0, p1.z+p2.z);
	float cosAngle = dot(p1, p2);
	cosAngle = (cosAngle + 1.0) * 0.5; //normalize 0..1.0
	if (isConcave < 0.5) cosAngle *= -1.0; //now -1.0..1.0

	float dist = abs(p1.z+p2.z) / rx;
    float attenuation = 1.0 / (1.0 + (dist* 0.5));
	cosAngle *= attenuation;
	wt += attenuation;

	//wt += 1.0;
	//if (max(abs(p1.z), abs(p2.z)) > (rad * 0.01)) return 0.5;
	return cosAngle; //range -1.0..1.0
}
float getAO(void) {
	vec2 noise = rand(texCoord);
	float depth = readDepth(texCoord);
	rad = 4;//0.1 * aoRadius;
	float w = rad * (1.0 / width)+(noise.x*(1.0-noise.x));
	float h = rad * (1.0 / height)+(noise.y*(1.0-noise.y));
	rad *= (1.0 / width);
	float pw;
	float ph;
	float ao = 0.0;
	float dl = kPI*(3.0-sqrt(5.0));
	float dz = 1.0/float(samples);
	float l = 0.0;
	float z = 1.0 - dz/2.0;
	float wt = kEps;
	for (int i = 0; i <= samples; i ++) {
		float r = sqrt(1.0-z);
		pw = cos(l)*r;
		ph = sin(l)*r;
		ao += calAOx(depth,pw*w,ph*h, r, wt);
		z = z - dz;
		l = l + dl;
	}
	ao /= (wt * 2.0);
	ao += 0.5;
	ao = clamp(ao, 0.0, 1.0);
	//ao /= float(samples);
	//ao = clamp(ao, 0.0, 0.7) * 1.4; //threshold then LERP 0..1'
   //ao = clamp(ao, 0.0, 0.5) * 2.0; //threshold then LERP 0..1'
   ao = smoothstep(0.0, 1.0, ao);
   //ao = smoothstep(0.0, 1.0, ao);
   //ao = pow(ao, 2.0);
   ao = (1.0 - ao) * fracAO;
   return ao;
}
void main(void) {
  vec4 t1 = texture(tex1, texCoord);
  if (t1.a == 0.0) discard;
  vec4 t2 = texture(tex2, texCoord);
  //color = vec4(vec3(1.0 - getAO()), 1.0); return; //uncomment to test AO
  if (fracAO > 0.0)
    t1.rgb = clamp(t1.rgb-getAO(), 0.0, 1.0);
  t1.rgb = mix(t2.rgb,t1.rgb, alpha1);
  float depth = 1.0 - (3.0 * (texture(depth_texture2, texCoord).x - texture(depth_texture1, texCoord).x));
  depth = clamp(depth, 0.0, 1.0);
  color = mix(t1, t2, blend1 * depth);
}