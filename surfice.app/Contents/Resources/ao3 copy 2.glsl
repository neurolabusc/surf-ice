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
#define RADIUS 4
smooth in vec2 texCoord;
out vec4 color;
// True if we're blurring vertically, false if horizontally since we
// do the blurring in two passes, once for horizontal and once for vertical
//uniform ivec2 axis;


void main(void){
	ivec2 axis = ivec2(0, 1);
	//ivec2 px = ivec2(texCoord.xy);  //ivec2(gl_FragCoord.xy);
	//ivec2 px = ivec2(gl_FragCoord.xy);
	// Gaussian filter values from the author's blurring shader
	//const float gaussian[RADIUS + 1] = float[](0.153170, 0.144893, 0.122649, 0.092902, 0.062970);
	//vec4 t1 = texture(tex1, texCoord);
	//vec3 val = texture(tex1, px).xyz;
	vec2 px = texCoord;
	vec3 val = texture(tex1, px).rgb;
	const float gaussian[RADIUS + 1] = float[](0.153170, 0.144893, 0.122649, 0.092902, 0.062970);
	float weight = gaussian[0];
	vec3 sum = weight * val;
	for (int i = -RADIUS; i <= RADIUS; ++i){
		// We handle the center pixel above so skip that case
		if (i == 0) continue;
		// Filter scale effects how many pixels the kernel actually covers
		vec2 p = px + axis * i * aoRadius;
		//vec3 val = texelFetch(ao_in, p, 0).xyz;
		vec3 val = texture(tex1, p).xyz;
		//float z = val.y;
		float w = 0.3 + gaussian[abs(i)];
		// Decrease weight as depth difference increases. This prevents us from
		// blurring across depth discontinuities
		//w *= max(0.f, 1.f - (ao_params.edge_sharpness * 400.f) * abs(z_pos - z));
		sum += val * w;
		weight += w;
	}
	color = vec4(sum / (weight + 0.0001), 1.0);
}
