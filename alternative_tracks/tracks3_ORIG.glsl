//vert
#version 330
layout(location = 0) in vec3 Vert;
layout(location = 3) in vec3 Norm;
layout(location = 6) in vec4 Clr;
out vec3 vN, vL, vV;
out vec4 vClr, vP;
uniform mat4 ModelViewProjectionMatrix;
uniform mat4 ModelViewMatrix;
uniform mat3 NormalMatrix;
uniform vec3 LightPos = vec3(0.0, 20.0, 30.0); //LR, -DU+, -FN+
void main() {
    vN = normalize((NormalMatrix * Norm));
    vP = vec4(Vert, 1.0);
    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);
    vL = normalize(LightPos);
    vV = -vec3(ModelViewMatrix*vec4(Vert,1.0));
    vClr = Clr;
}
//frag
#version 330
in vec4 vClr;
in vec3 vN;
out vec4 color;
void main()
{
	vec3 specClr = vec3(0.7, 0.7, 0.7);
	vec3 difClr = vClr.rgb * 0.9;
	vec3 ambClr = vClr.rgb * 0.1;
	vec3 L = vec3(0.707, 0.707, 0.0);
    vec3 n = abs(normalize(vN));
   //vec3 n = normalize(vN);
    float spec = pow(dot(n,L),100.0);
    float dif = dot(L,n);
    color = vec4(specClr*spec + difClr*dif + ambClr,1.0);
    //color = vec4(vClr.rgb,1.0);
    //color = vec4(1.0, 0.0, 0.0,1.0);
}