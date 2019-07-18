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
smooth in vec2 texCoord;
out vec4 color;
vec2 pixFrac = 1.0/texture_size; //texture spacing for one pixel

float readDepth(in vec2 coord) {
	return 1.0 -  ((texture(depth_texture1, coord).x + 0.87) * 0.534);
}

vec3 worldFromScreen(vec2 tx) { //texture positon 0..1, 0..1,
	return vec3(tx, readDepth(tx));
}

float random() {
	vec3 scale = vec3(12.9898, 78.233, 151.7182);
	return fract(sin(dot(gl_FragCoord.xyz, scale)) * 43758.5453);
}

vec3 getOffsetPositionVS(vec2 screenOrigin, vec2 unitOffset, float screenSpaceRadius) {
	// Offset by screenSpaceRadius pixels in the direction of unitOffset
	vec2 screenOffset = screenOrigin + screenSpaceRadius * unitOffset * pixFrac;
	//vec2 pixFrac2 = vec2(1.0, 1.0);
	//vec2 screenOffset = screenOrigin + screenSpaceRadius * unitOffset * pixFrac2;
	// Get the world coordinate from the offset screen space coordinate
	return worldFromScreen(screenOffset);
}

void main(void) {
	float kappa = 0.8;
	float beta = 0.0005;
	float sigma = 3.8;
	#define M_PI    3.14159265
	const int NUM_SAMPLES = 27;
	const int NUM_SPIRAL_TURNS = 16;
	vec4 t1 = texture(tex1, texCoord);
	if (t1.a == 0.0) discard;
	//texture_size
	vec3 pos = worldFromScreen(texCoord);
	vec3 normal = normalize(cross(dFdx(pos), dFdy(pos)));
	//color.rgb = normal; return;
	float sampleNoise = random();
	float initialAngle = 2.0 * M_PI * sampleNoise;
	float screenSpaceSampleRadius  = 8;//aoRadius;
	//float screenSpaceSampleRadius  = 16.0 * pixFrac.x * fracAO;
	float ao_value = 0.0;
	for (int sampleNumber = 0; sampleNumber < NUM_SAMPLES; sampleNumber++) {
		// Step 1:
		//  sample the points surrounding the central pixel one in a spiral pattern
		float sampleProgress = (float(sampleNumber) + 0.5) * (1.0 / float(NUM_SAMPLES));
		float angle = sampleProgress * (float(NUM_SPIRAL_TURNS) * 2.0 * M_PI) + initialAngle;
		float sampleDistance = sampleProgress * screenSpaceSampleRadius;
		vec2 angleUnitVector = vec2(cos(angle), sin(angle));
		// Step 2:
		//  Get the 3d coordinate corresponding to the sample on the spiral
		vec3 q = getOffsetPositionVS(texCoord, angleUnitVector, sampleDistance);
		vec3 v = q - pos;
		// The original estimator in the paper, from Alchemy AO
		// I tried getting their new recommended estimator running but couldn't get it to look nice,
		// from taking a look at their AO shader it also looks like we compute this value quite differently
		ao_value += max(0, dot(v, normal + pos.z * beta)) / (dot(v, v) + 0.01);
	}
	// The original method in paper, from Alchemy AO
	ao_value = max(0, 1.f - 2.f * sigma / NUM_SAMPLES * ao_value);
	ao_value = pow(ao_value, kappa);
	//float ao_valuer = 0.0;
	//if (abs(dFdx(pos.z)) > 0.005) {
	//	ao_valuer = 1.0;// -= dFdx(ao_value) ;
	//}
	//if (abs(dFdy(pos.z)) > 0.005) {
	//	ao_valuer = 1.0;// -= dFdx(ao_value) ;//-= dFdy(ao_value) ;
	//}

  color.rgb = vec3(ao_value, ao_value, ao_value);
  /*
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
  */
}