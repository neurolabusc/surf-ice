//vert
#version 330
layout(location = 0) in vec3 Vert;
layout(location = 3) in vec3 Norm;
layout(location = 6) in vec4 Clr;
uniform mat4 ModelViewProjectionMatrix;
uniform mat3 NormalMatrix;
out vec4 vClr;
out vec4 vP;
out vec3 vN;
void main() {
    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);
    vP = gl_Position;
    vClr = Clr;
    vN = normalize((NormalMatrix * Norm));
}
//geom
#version 330
layout (triangle_strip, max_vertices = 5) out;
layout (lines_adjacency) in;
in vec4 vP[4];
in vec4 vClr[4];
in vec3 vN[4];
uniform float Radius = 1.0;
float THICKNESS = Radius;
uniform vec2 ScreenPixels = vec2(1600,1600);
out vec4 gClr;
out vec3 gN;
vec2 screen_space(vec4 vertex) {
	return vec2( vertex.xy / vertex.w ) * ScreenPixels;
}
void main(void) {
  vec2 p0 = screen_space( vP[0] );
  vec2 p1 = screen_space( vP[1] );
  vec2 p2 = screen_space( vP[2] );
  vec2 p3 = screen_space( vP[3] );
  vec2 v0 = normalize(p1-p0);
  vec2 v1 = normalize(p2-p1);
  vec2 v2 = normalize(p3-p2);
  vec2 n0 = vec2(-v0.y, v0.x);
  vec2 n1 = vec2(-v1.y, v1.x);
  vec2 n2 = vec2(-v2.y, v2.x);
  vec2 miter_a = normalize(n0 + n1);
  vec2 miter_b = normalize(n1 + n2);
  float kEps = 0.1;
  float length_a = 0.0;
  float length_b = 0.0;
  if ( abs(dot(miter_a, n1)) > kEps)
  	length_a = THICKNESS / dot(miter_a, n1);
  if ( abs(dot(miter_b, n1)) > kEps)
  	length_b = THICKNESS / dot(miter_b, n1);
  gN = normalize(vN[1] + vN[2]);
  gClr = vClr[1];
  if( dot(v0,n1) > 0 ) {
	gl_Position = vec4( (p1 - length_a * miter_a) / ScreenPixels, vP[1].z, 1.0 );
	EmitVertex();
    gl_Position = vec4( (p1 + THICKNESS * n1) / ScreenPixels, vP[1].z, 1.0 );
	EmitVertex();
 } else {
    gl_Position = vec4( (p1 - THICKNESS * n1) / ScreenPixels, vP[1].z, 1.0 );
	EmitVertex();
	gl_Position = vec4( (p1 + length_a * miter_a) / ScreenPixels, vP[1].z, 1.0 );
	EmitVertex();
  }
  gClr = vClr[2];
  if( dot(v2,n1) < 0 ) {
    gl_Position = vec4( (p2 - length_b * miter_b) / ScreenPixels, vP[2].z, 1.0 );
	EmitVertex();
    gl_Position = vec4( (p2 + THICKNESS * n1) / ScreenPixels, vP[2].z, 1.0 );
	EmitVertex();
    gl_Position = vec4( (p2 + THICKNESS * n2) / ScreenPixels, vP[2].z, 1.0 );
	EmitVertex();
  } else {
    gl_Position = vec4( (p2 - THICKNESS * n1) / ScreenPixels, vP[2].z, 1.0 );
	EmitVertex();
	gl_Position = vec4( (p2 + length_b * miter_b) / ScreenPixels, vP[2].z, 1.0 );
	EmitVertex();
	gl_Position = vec4( (p2 - THICKNESS * n2) / ScreenPixels, vP[2].z, 1.0 );
	EmitVertex();
  }
  EndPrimitive();
}
//frag
#version 330
in vec4 gClr;
in vec3 gN;
out vec4 color;
//uniform vec3 LightPos = vec3(0.0, 20.0, 30.0); //LR, -DU+, -FN+
void main() {
	vec3 specClr = vec3(0.7, 0.7, 0.7);
	vec3 difClr = gClr.rgb * 0.9;
	vec3 ambClr = gClr.rgb * 0.1;
	vec3 L = vec3(0.707, 0.707, 0.0);
	//vec3 L = normalize(LightPos);
    vec3 n = abs(normalize(gN));
   	float spec = pow(dot(n,L),100.0);
    float dif = dot(L,n);
	color = vec4(specClr*spec + difClr*dif + ambClr,1.0);
}