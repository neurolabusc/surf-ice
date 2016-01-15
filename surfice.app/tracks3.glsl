//vert
#version 330
layout(location = 0) in vec3 Vert;
layout(location = 3) in vec3 Norm;
layout(location = 6) in vec4 Clr;
out vec3 vClr, vNormal, vL, vV;
out vec4 vPosition;
uniform mat4 ModelViewProjectionMatrix;
uniform mat4 ModelViewMatrix;
uniform mat3 NormalMatrix;
uniform vec3 LightPos = vec3(0.0, 20.0, 30.0); //LR, -DU+, -FN+
void main() {
    vNormal = normalize((NormalMatrix * Norm));
    vPosition = vec4(Vert, 1.0);
    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);
    vL = normalize(LightPos);
    vV = -vec3(ModelViewMatrix*vec4(Vert,1.0));
    vClr = Clr.rgb;
}
//geom
#version 410 core
in vec3 vPosition[4];
in vec3 vNormal[4];
in vec3 vClr[4];
layout (lines_adjacency) in;
layout (line_strip, max_vertices = 14) out;
out vec3 gClr, gNormal;
uniform float Radius = 0.3;
void emit(vec4 startPos, vec4 endPos, vec4 jitter){
    gl_Position = startPos+jitter;
    gClr = vClr[1];
    gNormal = vNormal[1];
    EmitVertex();
    gl_Position = endPos+jitter;
    gClr = vClr[2];
    gNormal = vNormal[2];
    EmitVertex();
    EndPrimitive();
}
void main(void) {
 gClr = vClr[1];
 vec4 prev_vtx = gl_in[0].gl_Position;
 vec4 next_vtx = gl_in[3].gl_Position;
 float r = Radius * 0.01;
 vec4 jitter;
 for (int i = 0; i < 7; i++) {
 	jitter = vec4(0.0, 0.0, 0.0, 0.0);
 	if (i == 1) jitter.x = r;
 	if (i == 2) jitter.y = r;
 	if (i == 3) jitter.z = r;
 	if (i == 4) jitter.x = -r;
 	if (i == 5) jitter.y = -r;
 	if (i == 6) jitter.z = -r;
 	emit(gl_in[1].gl_Position, gl_in[2].gl_Position, jitter);
 }
}
//frag
#version 330
in vec3 gClr;
in vec3 gNormal;
out vec4 color;
void main()
{
	vec3 specClr = vec3(1.0, 1.0, 1.0);
	vec3 difClr = gClr.rgb * 0.9;
	vec3 ambClr = gClr.rgb * 0.1;
	vec3 L = vec3(0.707, 0.707, 0.0);
    vec3 n = abs(normalize(gNormal));
    float spec = pow(dot(n,L),100.0);
    float dif = dot(L,n);
    color = vec4(specClr*spec + difClr*dif + ambClr,1.0);
    //color = vec4(vClr.rgb,1.0);
}