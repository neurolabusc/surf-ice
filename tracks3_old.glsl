//vert
#version 330
layout(location = 0) in vec3 Vert;
layout(location = 3) in vec4 Norm;
layout(location = 6) in vec4 Clr;
uniform mat4 ModelViewProjectionMatrix;
uniform mat4 ModelViewMatrix;
uniform mat4 ProjectionMatrix;
uniform float Radius;
uniform mat3 NormalMatrix;
out vec4 vClr;
out vec4 vP;
out vec3 vN;
out float TextCordCylinder;
out vec2 TextCordEnd;
out float fType;
void main() {
	float rScale = 0.01; //0.001
	float vType = Norm.a;
	vec3 VectorPoints = normalize(NormalMatrix*Norm.xyz);
	vec4 VertexPosition = ModelViewMatrix*vec4(Vert, 1.0);
	vec3 vecCameraPoint = normalize(-VertexPosition.xyz);
	vec4 VectorBillboard = normalize(vec4(cross(VectorPoints, vecCameraPoint),0.0));
	float kind = mod(gl_VertexID,2);
	if(vType==0.0){//Part of the Track Cylinders
		VectorBillboard = VectorBillboard*Radius*rScale;
		if(kind==0.0){
			VertexPosition = VertexPosition-VectorBillboard;
			TextCordCylinder = 0.0;
		}else{
			VertexPosition = VertexPosition+VectorBillboard;
			TextCordCylinder = 1.0;
		}
	}else if(vType==1.0){//It is end of track
		vec4 VectorEndBillboard = normalize(vec4(cross(VectorBillboard.xyz,VectorPoints.xyz),0.0))*Radius*rScale;
		VectorBillboard = VectorBillboard*Radius*rScale;
		if(kind==0.0){
			VertexPosition = VertexPosition-VectorBillboard+VectorEndBillboard;
			TextCordEnd = vec2(1.0,0.0);
		}else{
			VertexPosition = VertexPosition+VectorBillboard+VectorEndBillboard;
			TextCordEnd = vec2(1.0,1.0);
		}
	}else if(vType==-2.0){
		vec4 VectorEndBillboard = normalize(vec4(cross(VectorBillboard.xyz,VectorPoints.xyz),0.0))*Radius*rScale;
		VectorBillboard = VectorBillboard*Radius*rScale;
		if(kind==0.0){
			VertexPosition = VertexPosition-VectorBillboard-VectorEndBillboard;
			TextCordEnd = vec2(0.0,0.0);
		}else{
			VertexPosition = VertexPosition+VectorBillboard-VectorEndBillboard;
			TextCordEnd = vec2(0.0,1.0);
		}
	}else{
		vClr=vec4(0.0,0.0,0.0,1.0);
	}
	fType = vType;
	vClr = Clr;
	vP = ProjectionMatrix*VertexPosition;
	gl_Position = vP;
    vN = normalize((NormalMatrix * Norm.xyz));
}

//frag
#version 330
in vec4 vClr;
in vec3 vN;
in vec4 vP;
in float TextCordCylinder;
in float fType;
in vec2 TextCordEnd;
uniform sampler1D normalmaptexture;
uniform sampler2D normalmaptexturesphere;
uniform float Ambient = 0.2;
uniform float Diffuse = 0.8;
uniform float Specular = 0.5;
uniform float Shininess = 60.0;
uniform float Edge = 0.5;
uniform vec3 LightPos = vec3(0.0, 20.0, 30.0); //LR, -DU+, -FN+
out vec4 color;
void main() {
	vec4 N;
	if(fType==0.0){
		N = texture(normalmaptexture  , TextCordCylinder);
	}else{
		N = texture(normalmaptexturesphere  , TextCordEnd);
		if(N.a==0.0)discard;
	}
	vec3 NN =  normalize(2.0*N.xyz-1.0);
	vec3 difClr = vClr.rgb;
	//vec3 difClr = vec3(1.0, 1.0, 1.0);

	vec3 ambClr = vClr.rgb;
	//vec3 L = normalize(LightPos);
	vec3 L = normalize(LightPos);
	L.rgb = L.rbg;
	L.r = -L.r;
	L.gb = L.bg;

	//vec3 L = normalize(vec3(0.0, 0.2, 0.5));
	vec3 n = NN;
	float NormalSpec = pow(max(0.0,dot(vN,L)),1.0);
	//float BillboardSpec = pow(dot(N.xyz,L),Shininess)*Specular;
	//float BillboardSpec = pow(max(0.0,dot(N.xyz,L)), Shininess) * Specular;
	float dif = dot(n,L) * Diffuse;
	vec3 h = normalize(vec3(0.0, 0.35, 0.5)); //half vector between light and viewer
	float s = pow(max(0.0,dot(n,h)), Shininess) * Specular * 2.0;
	color = vec4(s + difClr*dif + Ambient*ambClr , 1.0);
	//color = vec4(difClr*dif + Ambient*ambClr, 1.0);
	//color = vec4(difClr*dx , 1.0);


	//color = vec4(NormalSpec*BillboardSpec*specClr + difClr*dif + Ambient*ambClr , 1.0);
}
