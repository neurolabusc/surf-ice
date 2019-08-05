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
// https://github.com/Twinklebear/ssao
// https://codepen.io/davepvm/details/zWdepm
uniform sampler2D tex1, tex2, depth_texture1, depth_texture2;
uniform float blend1, alpha1, fracAO, aoRadius;
uniform float aoRadiusPx, aoRadiusDx;
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
	float kappa = 0.8;  //contrast
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
	float screenSpaceSampleRadius  = aoRadiusPx;//aoRadius;
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
	ao_value *= fracAO;
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
  occlusion = pow(occlusion, 1.0 + INTENSITY);
  occlusion = clamp(occlusion, 0.0, 1.0);
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

/*
  const fragmentShaderSourceAO = `
  #extension GL_OES_standard_derivatives : enable
  precision highp float;

  uniform sampler2D diffuseBuf;
  uniform sampler2D normalBuf;
  uniform sampler2D positionBuf;
  uniform sampler2D depthBuf;

  uniform vec2 screenSize;

  const int NUM_SAMPLES = 11;
  const int NUM_SPIRAL_TURNS = 7;
  const float EPSILON = 0.1;
  const float BIAS = 0.5;
  const float WORLD_SPACE_RADIUS = 30.0; // radius of influence in world space
  const float INTENSITY = 200.0;

  const float M_PI = 3.1415926535897932384626433832795;

  int AND(int n1, int n2){
    float v1 = float(n1);
    float v2 = float(n2);

    int byteVal = 1;
    int result = 0;

    for(int i = 0; i < 32; i++){
      bool keepGoing = v1>0.0 || v2 > 0.0;
      if(keepGoing){

        bool addOn = mod(v1, 2.0) > 0.0 && mod(v2, 2.0) > 0.0;

        if(addOn){
          result += byteVal;
        }

        v1 = floor(v1 / 2.0);
        v2 = floor(v2 / 2.0);
        byteVal *= 2;
      } else {
        return result;
      }
    }
    return result;
  }

  float random(vec3 scale, float seed) {
    return fract(sin(dot(gl_FragCoord.xyz + seed, scale)) * 43758.5453 + seed);
  }

  vec3 worldFromScreen(const vec2 screen) {
    return texture2D(positionBuf, screen).xyz;
  }

  vec3 getOffsetPositionVS(vec2 screenOrigin, vec2 unitOffset, float screenSpaceRadius) {
    // Offset by screenSpaceRadius pixels in the direction of unitOffset
    vec2 screenOffset = screenOrigin +
      screenSpaceRadius * unitOffset * vec2(1.0 / screenSize.x, 1.0 / screenSize.y);

    // Get the world coordinate from the offset screen space coordinate
    return worldFromScreen(screenOffset);
  }

  void main() {
    vec2 screenSpaceOrigin = gl_FragCoord.xy * vec2(1.0/screenSize.x, 1.0/screenSize.y);
    ivec2 pixel = ivec2(gl_FragCoord.xy);

    vec3 worldSpaceOrigin = worldFromScreen(screenSpaceOrigin);
    vec3 normalAtOrigin = normalize(texture2D(normalBuf, screenSpaceOrigin).xyz);
    vec3 colorAtOrigin = texture2D(diffuseBuf, screenSpaceOrigin).xyz;

    vec3 randomScale = vec3(12.9898, 78.233, 151.7182);
    vec3 sampleNoise = vec3(
      random(randomScale, 0.0),
      random(randomScale, 1.0),
      random(randomScale, 2.0));

    float initialAngle = 2.0 * M_PI * sampleNoise.x;

    // radius of influence in screen space
    float screenSpaceSampleRadius  = 100.0 * WORLD_SPACE_RADIUS / worldSpaceOrigin.y;

    float occlusion = 0.0;
    for (int sampleNumber = 0; sampleNumber < NUM_SAMPLES; sampleNumber++) {
      // Step 1:
      // Looking at the 2D image of the scene, sample the points surrounding the current one
      // in a spiral pattern

      float sampleProgress = (float(sampleNumber) + 0.5) * (1.0 / float(NUM_SAMPLES));
      float angle = sampleProgress * (float(NUM_SPIRAL_TURNS) * 2.0 * M_PI) + initialAngle;

      float sampleDistance = sampleProgress * screenSpaceSampleRadius;
      vec2 angleUnitVector = vec2(cos(angle), sin(angle));

      // Step 2:
      // Get the 3d coordinate corresponding to the sample on the spiral
      vec3 worldSpaceSample =
        getOffsetPositionVS(screenSpaceOrigin, angleUnitVector, sampleDistance);

      // Step 3:
      // Approximate occlusion from this sample
      vec3 originToSample = worldSpaceSample - worldSpaceOrigin;
      float squaredDistanceToSample = dot(originToSample, originToSample);

      // vn is proportional to how close the sample point is to the origin point along
      // the normal at the origin
      float vn = dot(originToSample, normalAtOrigin) - BIAS;

      // f is proportional to how close the sample point is to the origin point in the
      // sphere of influence in world space
      float radiusSquared = WORLD_SPACE_RADIUS * WORLD_SPACE_RADIUS;
      float f = max(radiusSquared - squaredDistanceToSample, 0.0) / radiusSquared;
      float sampleOcclusion =  f * f * f * max(vn / (EPSILON + squaredDistanceToSample), 0.0);

      // Accumulate occlusion
      occlusion += sampleOcclusion;
    }

    occlusion = 1.0 - occlusion / (4.0 * float(NUM_SAMPLES));

    occlusion = clamp(pow(occlusion, 1.0 + INTENSITY), 0.0, 1.0);
    if (abs(dFdx(worldSpaceOrigin.z)) < 0.5) {
      occlusion -= dFdx(occlusion) * (float(AND(pixel.x, 1)) - 0.5);
    }
    if (abs(dFdy(worldSpaceOrigin.z)) < 0.5) {
      occlusion -= dFdy(occlusion) * (float(AND(pixel.y, 1)) - 0.5);
    }

    gl_FragData[0] = vec4(occlusion, occlusion, occlusion, 1.0);
  }
*/