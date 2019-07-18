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
#define PI2    PI * 2
smooth in vec2 texCoord;
out vec4 color;
// True if we're blurring vertically, false if horizontally since we
// do the blurring in two passes, once for horizontal and once for vertical
//uniform ivec2 axis;

float getDepth(in vec2 coord) {
   return 1.0 -  ((texture(depth_texture1, coord).x + 0.87) * 0.534);
}

#define NUM_SAMPLES 7
#define NUM_RINGS 4
const float ANGLE_STEP = PI2 * float( NUM_RINGS ) / float( NUM_SAMPLES );
const float INV_NUM_SAMPLES = 1.0 / float( NUM_SAMPLES );


void main(void){
	vec4 clr = texture(tex1, texCoord);
	if (clr.a == 0.0) discard;
	clr.r = getDepth(texCoord);
	clr.g = clr.r;
	clr.b = clr.r;
	color = clr;

}
