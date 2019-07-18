//vert
#version 330
layout(location = 0) in vec3 Vert;
layout(location = 3) in vec2 Coord;
smooth out vec2 texCoord;
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
#define M_PI    3.14159265
smooth in vec2 texCoord;
out vec4 color;
//general stuff
int samples = 32; //ao sample count
float aoclamp = 0.25; //depth clamp - reduces haloing at screen edges
bool noise = true; //use noise instead of pattern for sample dithering
float noiseamount = 0.0002; //dithering amount
float diffarea = 0.1; //self-shadowing reduction
float gdisplace = 0.4; //gauss bell center
vec2 pixFrac = 1.0/texture_size; //texture spacing for one pixel

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
	if (far > 0) {
		float temp2 = compareDepths(readDepth(texCoord-coordwh),depth,far);
		temp += (1.0-temp)*temp2;
	}
	return temp;
}
float getAOorig(void) {
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
		ao += readDepth(vec2(cos(l)*r*w*dd,sin(l)*r*h*dd));
		//ao += calAO(depth, vec2(cos(l)*r*w*dd,sin(l)*r*h*dd));
		z = z - dz;
		l = l + dl;
	}
	ao /= float(samples);
	return depth;
	//if (ao < depth)
	//	ao = 0.5;
	//else
	//	ao = 0.0;
	//ao = clamp(ao, 0.0, 0.4) * 2.5; //threshold then LERP 0..1
	//ao = smoothstep(0.0, 1.0, ao);
	//ao = (1.0 - ao) * fracAO;
	return ao;
}

vec3 worldFromScreen(vec2 tx) { //texture positon 0..1, 0..1,
	return vec3(tx, readDepth(tx));
}

vec3 getNormal(vec2 tx) { //compute normal given texture coordinate "tx"
  vec3 p = worldFromScreen(tx); //center
  vec3 p1 = worldFromScreen(tx+ vec2(0.0, pixFrac.y));  //up
  vec3 p2 = worldFromScreen(tx+ vec2(pixFrac.x, 0.0));//right
  vec3 normal = cross(p2-p, p1-p);
  return normalize(normal);
}


float random(vec3 scale) {
	return fract(sin(dot(gl_FragCoord.xyz, scale)) * 43758.5453);
}

  vec3 getOffsetPositionVS(vec2 screenOrigin, vec2 unitOffset, float screenSpaceRadius) {
    // Offset by screenSpaceRadius pixels in the direction of unitOffset
    vec2 screenOffset = screenOrigin +
      screenSpaceRadius * unitOffset * pixFrac;

    // Get the world coordinate from the offset screen space coordinate
    return worldFromScreen(screenOffset);
  }

void main(void) {

  vec4 t1 = texture(tex1, texCoord);
  if (t1.a == 0.0) discard;
  //texture_size
  vec3 normal = getNormal(texCoord);
  vec3 pos = worldFromScreen(texCoord);
  color.rgb = normal;

  const int NUM_SAMPLES = 32;
  const int NUM_SPIRAL_TURNS = 7;
  const float EPSILON = 0.1;
  const float BIAS = 0.5;

  vec3 randomScale = vec3(12.9898, 78.233, 151.7182);
  float sampleNoise = random(randomScale);
  float initialAngle = 2.0 * M_PI * sampleNoise;
  float WORLD_SPACE_RADIUS = 0.2; // radius of influence in world space
  // radius of influence in screen space
  float screenSpaceSampleRadius  = aoRadius; //<-xxxx
  float occlusion = 0.0;
  float ao_value = 0.0;
  for (int sampleNumber = 0; sampleNumber < NUM_SAMPLES; sampleNumber++) {
      // Step 1:
      // Looking at the 2D image of the scene, sample the points surrounding the current one
      // in a spiral pattern

      float sampleProgress = (float(sampleNumber) + 0.5) * (1.0 / float(NUM_SAMPLES));
      float angle = sampleProgress * (float(NUM_SPIRAL_TURNS) * 2.0 * M_PI) + initialAngle;
      float sampleDistance = sampleProgress * screenSpaceSampleRadius;
      vec2 angleUnitVector = vec2(cos(angle), sin(angle));
	  float beta = 0.0005;
      // Step 2:
      // Get the 3d coordinate corresponding to the sample on the spiral
      vec3 q = getOffsetPositionVS(texCoord, angleUnitVector, sampleDistance);
	  vec3 v = q - pos;
	  // The original estimator in the paper, from Alchemy AO
	  // I tried getting their new recommended estimator running but couldn't get it to look nice,
	  // from taking a look at their AO shader it also looks like we compute this value quite differently
	  ao_value += max(0, dot(v, normal + pos.z * beta)) / (dot(v, v) + 0.01);


  }
	float kappa = 0.8;
	float sigma = 3.8;
	// The original method in paper, from Alchemy AO
	ao_value = max(0, 1.f - 2.f * sigma / NUM_SAMPLES * ao_value);
	ao_value = pow(ao_value, kappa);

  //if (occlusion > 0.0)
  //	occlusion = 1.0;
  /*
  	0 int use_rendered_normals;
	27 int n_samples;
	16 int turns;
	3.5 float ball_radius;
	3.8 float sigma;
	0.8 float kappa;
	0.0005 float beta;
	// Parameters for the blurring pass
	2 int filter_scale;
	0.8 float edge_sharpness;
*/
  color.rgb = vec3(ao_value, ao_value, ao_value);
  return;
  occlusion = 1.0 - occlusion / (4.0 * float(NUM_SAMPLES));
  //color.rgb = vec3(ao, ao, ao);
  float INTENSITY = 200.0;
  occlusion = clamp(pow(occlusion, 1.0 + INTENSITY), 0.0, 1.0);
  color.rgb = vec3(occlusion,occlusion,occlusion);

  //return;
  //vec4 t2 = texture(tex2, texCoord);
  //float ao = 1.0 - getAO(); color = vec4(ao, ao, ao, 1.0); return;
  //t1.r = getAO();
  //t1.g = t1.r;
  //t1.b = t1.r;
  //color = t1;
  //return;
  //if (fracAO > 0.0)
  //  t1.rgb = clamp(t1.rgb-getAO(), 0.0, 1.0);
  //t1.rgb = mix(t2.rgb,t1.rgb, alpha1);
  //float depth = 1.0 - (3.0 * (texture(depth_texture2, texCoord).x - texture(depth_texture1, texCoord).x));
  //depth = clamp(depth, 0.0, 1.0);
  //color = mix(t1, t2, blend1 * depth);
}