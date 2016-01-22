//vert
#version 330
layout(location = 0) in vec3 Vert;
layout(location = 3) in vec3 Norm;
layout(location = 6) in vec4 Clr;
out vec3 vN;//, vNx,
out vec3 vP;
out vec4 vClr;
uniform mat4 ModelViewProjectionMatrix;
uniform mat4 ModelViewMatrix;
uniform mat3 NormalMatrix;
uniform vec3 LightPos = vec3(0.0, 20.0, 30.0); //LR, -DU+, -FN+
void main() {
	vN = vec3(0.0, 0.0, 1.0);//normalize((NormalMatrix * Norm));
	//vN = normalize((NormalMatrix * Norm));
    vP = Vert;
    gl_Position = ModelViewProjectionMatrix * vec4(Vert, 1.0);
    vClr = Clr;
}
//geom
#version 330
out vec4 gClr;
out vec3 gN;

out vec3 gPosition;
out vec3 gEndpoints[4];
out vec3 gEndplanes[2];
layout (triangle_strip, max_vertices = 24) out;
//layout (line_strip, max_vertices = 24) out;
layout (lines_adjacency) in;
uniform mat4 ModelViewProjectionMatrix;
uniform mat4 ProjectionMatrix;
uniform mat4 ModelViewMatrix;
uniform float Radius = 1.0;
in vec3 vP[4]; // Four inputs since we're using GL_LINE_STRIP_ADJACENCY
in vec3 vN[4];   // Orientation vectors are necessary for consistent alignment
//in vec3 vNx[4];
in vec4 vClr[4];   // Color


vec4 obb[8];
vec4 obbPrime[8];

bool isFront(int a, int b, int c)
{
    vec3 i = vec3(obbPrime[b].xy - obbPrime[a].xy, 0);
    vec3 j = vec3(obbPrime[c].xy - obbPrime[a].xy, 0);
    return cross(i, j).z > 0.0;
}

void emit(int a, int b, int c, int d)
{
    gPosition = obb[a].xyz; gl_Position = obbPrime[a]; EmitVertex();
    gPosition = obb[b].xyz; gl_Position = obbPrime[b]; EmitVertex();
    gPosition = obb[c].xyz; gl_Position = obbPrime[c]; EmitVertex();
    gPosition = obb[d].xyz; gl_Position = obbPrime[d]; EmitVertex();
     EndPrimitive();
}

void main()
{
	gClr = vClr[1];
	gN = normalize(vN[1]+vN[2]);
    // Pass raytracing inputs to fragment shader:
    vec3 p0, p1, p2, p3, n0, n1, n2;

    p0 = (ModelViewMatrix * vec4(vP[0], 1.0)).xyz;
    p1 = (ModelViewMatrix * vec4(vP[1], 1.0)).xyz;
    p2 = (ModelViewMatrix * vec4(vP[2], 1.0)).xyz;
    p3 = (ModelViewMatrix * vec4(vP[3], 1.0)).xyz;


    //gl_Position =  ProjectionMatrix * ModelViewMatrix * vec4(vP[0], 1.0); EmitVertex();
    //gl_Position =  ProjectionMatrix * ModelViewMatrix * vec4(vP[1], 1.0); EmitVertex();
    //gl_Position =  ProjectionMatrix * ModelViewMatrix * vec4(vP[2], 1.0); EmitVertex();
    //gl_Position =  ProjectionMatrix * ModelViewMatrix * vec4(vP[3], 1.0); EmitVertex();

    //gl_Position = ModelViewProjectionMatrix * vec4(vP[0], 1.0); EmitVertex();
    //gl_Position = ModelViewProjectionMatrix * vec4(vP[1], 1.0); EmitVertex();
    //gl_Position = ModelViewProjectionMatrix * vec4(vP[2], 1.0); EmitVertex();
    //gl_Position = ModelViewProjectionMatrix * vec4(vP[3], 1.0); EmitVertex();
    //EndPrimitive(); return;

    n0 = normalize(p1-p0);
    n1 = normalize(p2-p1);
    n2 = normalize(p3-p2);
    gEndpoints[0] = p0; gEndpoints[1] = p1;
    gEndpoints[2] = p2; gEndpoints[3] = p3;
    gEndplanes[0] = normalize(n0+n1);
    gEndplanes[1] = normalize(n1+n2);

    // Compute object-space plane normals:
    p0 = vP[0]; p1 = vP[1];
    p2 = vP[2]; p3 = vP[3];
    n0 = normalize(p1-p0);
    n1 = normalize(p2-p1);
    n2 = normalize(p3-p2);
    vec3 u = normalize(n0+n1);
    vec3 v = normalize(n1+n2);

    // Generate a basis for the cuboid:
    vec3 j = n1;
    vec3 i = vN[1];
    vec3 k = cross(i, j);

    // Compute the eight corners:
    float r = Radius * 0.25;
    float d;
    d = 1.0/dot(u,j); p1 -= j*r*sqrt(d*d-1.0);
    d = 1.0/dot(v,j); p2 += j*r*sqrt(d*d-1.0);
    obb[0] = ModelViewMatrix*vec4(p1 + i*r + k*r,1);
    obb[1] = ModelViewMatrix*vec4(p1 + i*r - k*r,1);
    obb[2] = ModelViewMatrix*vec4(p1 - i*r - k*r,1);
    obb[3] = ModelViewMatrix*vec4(p1 - i*r + k*r,1);
    obb[4] = ModelViewMatrix*vec4(p2 + i*r + k*r,1);
    obb[5] = ModelViewMatrix*vec4(p2 + i*r - k*r,1);
    obb[6] = ModelViewMatrix*vec4(p2 - i*r - k*r,1);
    obb[7] = ModelViewMatrix*vec4(p2 - i*r + k*r,1);
    for (int i = 0; i < 8; i++)
        obbPrime[i] =  ProjectionMatrix * obb[i];

    // Emit the front faces of the cuboid:
    if (isFront(0,1,3)) emit(0,1,3,2);
    if (isFront(5,4,6)) emit(5,4,6,7);
    if (isFront(4,5,0)) emit(4,5,0,1);
    if (isFront(3,2,7)) emit(3,2,7,6);
    if (isFront(0,3,4)) emit(0,3,4,7);
    if (isFront(2,1,6)) emit(2,1,6,5);
}
//frag
#version 330
out vec4 color;
uniform vec4 Color = vec4(1.0, 0.0, 0.8, 1.0);

in vec3 gEndpoints[4];
in vec3 gEndplanes[2];
in vec3 gPosition;
in vec4 gClr;
in vec3 gN;
uniform float Radius = 1.0;
uniform mat4 ProjectionMatrix;
vec3 LightDirection = vec3(0.707, 0.707, 0.0);
//uniform vec3 LightDirection = vec3 (0.0, 1.0, 0.0);
uniform vec3 DiffuseMaterial = vec3(1.0, 0.5, 0.125);
uniform vec3 AmbientMaterial = vec3(0.125, 0.125, 0.0);
uniform vec3 SpecularMaterial = vec3(0.5, 0.5, 0.5);
uniform float Shininess = 50.0;

vec4 getClr() {
	vec3 specClr = vec3(0.7, 0.7, 0.7);
	vec3 difClr = gClr.rgb * 0.9;
	vec3 ambClr = gClr.rgb * 0.1;
	vec3 L = vec3(0.707, 0.707, 0.0);
    vec3 n = abs(normalize(gN));
   	float spec = pow(dot(n,L),100.0);
    float dif = dot(L,n);
    return vec4(specClr*spec + difClr*dif + ambClr,1.0);
}

vec3 perp(vec3 v)
{
    vec3 b = cross(v, vec3(0, 0, 1));
    if (dot(b, b) < 0.01)
        b = cross(v, vec3(0, 1, 0));
    return b;
}

bool IntersectCylinder(vec3 origin, vec3 dir, out float t)
{
    vec3 A = gEndpoints[1]; vec3 B = gEndpoints[2];
    float Epsilon = 0.0000001;
    float extent = distance(A, B);
    vec3 W = (B - A) / extent;
    vec3 U = perp(W);
    vec3 V = cross(U, W);
    U = normalize(cross(V, W));
    V = normalize(V);
    float r = Radius * 0.25;
    float rSqr = r*r;
    vec3 diff = origin - 0.5 * (A + B);
    mat3 basis = mat3(U, V, W);
    vec3 P = diff * basis;
    float dz = dot(W, dir);
    if (abs(dz) >= 1.0 - Epsilon) {
        float radialSqrDist = rSqr - P.x*P.x - P.y*P.y;
        if (radialSqrDist < 0.0)
            return false;
        t = (dz > 0.0 ? -P.z : P.z) + extent * 0.5;
        return true;
    }

    vec3 D = vec3(dot(U, dir), dot(V, dir), dz);
    float a0 = P.x*P.x + P.y*P.y - rSqr;
    float a1 = P.x*D.x + P.y*D.y;
    float a2 = D.x*D.x + D.y*D.y;
    float discr = a1*a1 - a0*a2;
    if (discr < 0.0)
        return false;

    if (discr > Epsilon) {
        float root = sqrt(discr);
        float inv = 1.0/a2;
        t = (-a1 + root)*inv;
        return true;
    }

    t = -a1/a2;
    return true;
}

vec3 ComputeLight(vec3 L, vec3 N, bool specular)
{
    float df = max(0.0,dot(N, L));
    vec3 colorf = df * DiffuseMaterial;
    if (df > 0.0 && specular) {
        vec3 E = vec3(0, 0, 1);
        vec3 R = reflect(L, N);
        float sf = max(0.0,dot(R, E));
        sf = pow(sf, Shininess);
        colorf += sf * SpecularMaterial;
    }
    return colorf;
}

void main()
{
	//color = vec4(gClr.rgb, 1.0); return;
    vec3 rayStart = gPosition;
    vec3 rayEnd = vec3(0);
    vec3 rayDir = normalize(rayEnd - rayStart);
color = vec4(gClr.rgb, 1.0); return;
    if (distance(rayStart, rayEnd) < 0.000001) {
        discard;
        return;
    }

    float d;
    if (!IntersectCylinder(rayStart, rayDir, d)) {
        //discard;
        //return;
    }

    vec3 hitPoint = rayStart + d * rayDir;
    if (dot(hitPoint - gEndpoints[1], gEndplanes[0]) < 0.0) {
        discard;
        return;
    }
    //color = getClr(); return;
    if (dot(hitPoint - gEndpoints[2], gEndplanes[1]) > 0.0) {
        discard;
        return;
    }
color = vec4(gClr.rgb, 1.0); return;

    // Compute a lighting normal:
    vec3 x0 = hitPoint;
    vec3 x1 = gEndpoints[1];
    vec3 x2 = gEndpoints[2];
    float length = distance(x1, x2);
    vec3 v = (x2 - x1) / length;
    float t = dot(x0 - x1, v);
    vec3 spinePoint = x1 + t * v;
    vec3 N = -normalize(hitPoint - spinePoint);
color = vec4(gClr.rgb+ ComputeLight(LightDirection, N, true), 1.0); return;



    // Perform lighting and write out a new depth value:
    //vec3 colorm = AmbientMaterial + ComputeLight(LightDirection, N, true);
    //vec3 colorm = gClr.rgb;
    vec3 colorm = gClr.rgb + ComputeLight(LightDirection, N, true);
    vec4 ndc = ProjectionMatrix * vec4(hitPoint, 1);
    //gl_FragDepth = ndc.z / ndc.w;
    color = vec4(colorm, 1.0);
}