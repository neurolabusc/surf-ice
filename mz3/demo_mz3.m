function mz3_demo
%creates different variations of the mz3 format
%Object is based on
% https://brainder.org/2011/09/25/braindering-with-ascii-files/

%vertex coordinates (x,y,z)
vert = [1 0 0; 0 1 0; 0 0 1; -1 0 0; 0 -1 0; 0 0 -1];
%definition of faces from vertex index (1st vertex is 1)
face = [1 2 3; 2 4 3; 4 5 3; 5 1 3; 2 1 6; 4 2 6; 5 4 6; 1 5 6];
%face = [1 2 3; 2 4 3; 4 5 3; 5 1 3; 2 6 1; 4 6 2; 5 6 4; 1 6 5];
%vertex colors RGB triplet for each vertex
vertRGB = [1 0 0; 0 1 0; 0 0 0; 0 0 1; 1 0 1; 1 1 1];
vertRGBA = [vertRGB, ones(size(vertRGB,1),1)];
%vertex intensity
inten = [1 2 3 4 5 6]';
%display results:
patch('Faces',face,'Vertices',vert,'FaceVertexCData',vertRGB, 'FaceColor','interp'); view(3); axis vis3d;
writeMz3('x3Mesh.mz3', face, vert);
writeMz3('x7ColoredMeshPerVertex.mz3', face, vert, vertRGB);
writeMz3('x8Scalar.mz3', [], [],inten);
writeMz3('x11ScalarMesh.mz3', face, vert,inten);
writeMz3('x15TemplateMesh.mz3', face, vert,[vertRGBA, inten]);
%per face colors
vert = [1 0 0; 0 1 0; 0 0 1; ...
        0 1 0; -1 0 0; 0 0 1; ...
        -1 0 0; 0 -1 0; 0 0 1; ...
        0 -1 0; 1 0 0; 0 0 1; ...
        0 1 0; 1 0 0; 0 0 -1; ...
        -1 0 0; 0 1 0; 0 0 -1; ...
        0 -1 0; -1 0 0; 0 0 -1; ...
        1 0 0; 0 -1 0; 0 0 -1];
face = reshape(1:24,3,8)';
vertRGB = [1 0 0; 0 1 0; 0 0 0; 0 0 1; 1 0 1; 1 1 1; 0 1 1; 1 1 0];
vertRGB = kron(vertRGB,ones(3,1));
writeMz3('x7ColoredMeshPerFace.mz3', face, vert, vertRGB);
%now create overlay based on 5124 face mesh
fnm = 'cortex_5124.mz3';
if ~exist(fnm,'file')
    error('Unable to find file: %s', fnm); 
end
[~, vertices] = readMz3(fnm);
%create overlay: each colordata for each vertex. 
cdata = vertices(:,1:2);
for i = 1: 2
    cdata(:,i) = abs(cdata(:,i));
    cdata(:,i) = cdata(:,i) - min(cdata(:,i)); %0..max
    cdata(:,i) = cdata(:,i) / max(cdata(:,i)); %0..1
end
%create "type 16" mz3: double precision, 2 layers 
fnm16 = 'x16DoubleOverlay_5124x2.mz3';
writeMz3(fnm16, [], [], double(cdata),[],false,true);
%create "type 8" mz3: single precision, 2 layers
fnm8 = 'x8ScalarOverlay_5124x2.mz3';
writeMz3(fnm8, [], [], single(cdata),[],false,true);


%end mz3_demo()
