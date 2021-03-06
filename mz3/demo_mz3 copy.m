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
writeMz3Sub('3Mesh.mz3', face, vert);
writeMz3Sub('7ColoredMeshPerVertex.mz3', face, vert, vertRGB);
writeMz3Sub('8Scalar.mz3', [], [],inten);
writeMz3Sub('11ScalarMesh.mz3', face, vert,inten);
writeMz3Sub('15TemplateMesh.mz3', face, vert,[vertRGBA, inten]);
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
writeMz3Sub('7ColoredMeshPerFace.mz3', face, vert, vertRGB);
%end mz3_demo()

function writeMz3Sub(filename, face, vertex,vertexColors,alpha, compress)
%function writeNv(vertex,faces,filename)
% --- save Face/Vertex data as Surfice MZ3 format file
%inputs:
% vertex: Vx3 array with X,Y,Z coordinates of each vertex
% vertexColors: Vx0 (empty), Vx1 (scalar), Vx3 (RGB), Vx4 (RGBA), (Vx5 RGBA+scalar), colors for each vertex
% face: Vx3 triangle list indexed from 1, e.g. 1,2,3 is triangle connecting first 3 vertices
% filename: name to save object
% alpha: (optional) if provided and vertexColors is Vx3 (RGB), sets transparency of vertex colors (RGB->RGBA), range 0..1
% compress: (optional) true(default)/false for lossless compression
%Example
% [f,v,c] = fileUtils.ply.readPly('stroke.ply');
% fileUtils.mz3.writeMz3('stroke.mz3',f,v,c)

if ~exist('vertexColors','var'), vertexColors = []; end;
if ~exist('alpha','var'), alpha = []; end;
if ~exist('compress','var'), compress = true; end;
if isempty(vertex) && isempty(vertexColors) && isempty(face), return; end;
if isempty(face)
    nFace = 0;
else
    nFace = size(face,1);
    if (size(face,2) ~= 3), error('triangular meshes require 3 not %d vertices per face: %s', size(face,2), mfilename); end;
end
isFace = (nFace > 0);
if isempty(vertex)
    nVert = 0;
else
    nVert = size(vertex,1);
    if (size(vertex,2) ~= 3), error('meshes require 3 (X,Y,Z) not %d dimensions per vertex: %s', size(vertex,2), mfilename); end;
end
isVert = (nVert > 0);
isRGBA = false;
if ~isempty(vertexColors) && (size(vertexColors,2) ~= 5) && (size(vertexColors,2) ~= 4) && (size(vertexColors,2) ~= 3) && (size(vertexColors,2) ~= 1)
    error('vertexColors must have 5 (RGBA+Scalar), 4 (RGBA), 3 (RGB) or 1 (Scalar) component per vertex');
end
if ~isempty(vertexColors) && (size(vertexColors,2) ~= 1)
    isRGBA = true;
    if (nVert > 0) && (size(vertexColors,1) ~= nVert), error('Number of vertices and colors must match');  end;
    nVert = size(vertexColors,1);
end
isScalar = false;
if (size(vertexColors,2) == 5) %RGBA+Scalar
    isScalar = true;
    scalarIntensity = vertexColors(:,5);
    vertexColors = vertexColors(:,1:4);
end
if (size(vertexColors,2) == 1)
    isScalar = true;
    if (nVert > 0) && (size(vertexColors,1) ~= nVert), error('Number of vertices and colors must match');  end;
    nVert = size(vertexColors,1);
    scalarIntensity = vertexColors;
end
[fid,Msg] = fopen(filename,'Wb', 'l');
if fid == -1, error(Msg); end;
%write header
attr = 0;
if isFace, attr = attr + 1; end;
if isVert, attr = attr + 2; end;
if isRGBA, attr = attr + 4; end;
if isScalar, attr = attr + 8; end;

if isFace, fprintf('has faces: attribute flags +1\n'); end;
if isVert, fprintf('has vertices: attribute flags +2\n'); end;
if isRGBA, fprintf('has colors: attribute flags +4\n'); end;
if isScalar, fprintf('has scalars: attribute flags +8\n'); end;
fprintf('%s has sum attribute flags = %d with %d vertices\n', filename, attr, nVert);

fwrite(fid, 23117, 'uint16'); %MAGIC SIG to catch ftp conversion errors http://en.wikipedia.org/wiki/Portable_Network_Graphics
fwrite(fid, attr, 'uint16'); %attr = ATTRIBUTES
fwrite(fid, nFace, 'uint32'); %nFace
fwrite(fid, nVert, 'uint32'); %nVert
fwrite(fid, 0, 'uint32'); %nSkip - bytes to skip
if isFace
    face = face - 1; %this format indexes from 0
    fwrite(fid,face','int32'); %triangle indices
end
if isVert
    fwrite(fid,vertex','float32'); %vertex coordinates
end;
if isRGBA
    if (size(vertexColors,2) ~= 4) %convert RGB -> RGBA
        if isempty(alpha), alpha = 1.0; end;
        a = ones(size(vertexColors,1),1) * alpha;
        vertexColors = [vertexColors, a];
    end
    vertexColors = vertexColors * 255; %save 0..255
    fwrite(fid,vertexColors','uint8'); %vertex coordinates
end;
if isScalar
    fwrite(fid,scalarIntensity,'float32'); %vertex coordinates
end
fclose(fid);
%compress data
% system(sprintf('/Users/rorden/Downloads/zopfli-master/zopfli -i100 %s', filename));
% system(sprintf('gzip -9 %s', filename));
if compress
    gzip(filename); %compress
    % delete(filename); %delete uncompressed
    movefile([filename '.gz'], filename); %rename
end;
%end writeMz3Sub()