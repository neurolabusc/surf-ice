function [faces, vertices, vertexColors, data] = readMz3(filename, isForceRGBA2RGB, isVerbose)
%function [faces, vertices, vertexColors, data] = readMz3(fileName)
%inputs:
%	filename: the nv file to open
%   isForceRGBA2RGB: (optional). If true (default) then 4-component RGBA and 5-component RGBA+scalar are read as RGB only 
%outputs:
%  faces: face matrix where cols are xyz and each row is face
%  vertices: vertices matrix where cols are xyz and each row a vertex
%  vertexColors: Vx0 (empty), Vx1 (scalar) or Vx3 (RGB) colors for each vertex
%  data: raw uncompressed bytes of mz3 file
%Mz3 is the native format of Surf Ice, it is small and fast to read
%if ~exist('filename','var'), filename = 'stroke.mz3'; end;
if ~exist ('isVerbose', 'var'), isVerbose = nargout < 1; end;
if ~exist ('filename', 'var')
   [A,Apth] = uigetfile({'*.mz3';'*.*'},'Select mz3 file');
   filename = [Apth, A]; 
   isVerbose = true;
end
faces = [];
vertices = [];
vertexColors = [];
if ~exist('isForceRGBA2RGB','var'), isForceRGBA2RGB = true; end;
if ~exist(filename,'file'), error('Unable to find MZ3 file named "%s"', filename); return; end;
try
    % Check if this is Octave:
    persistent isoct;
    if isempty(isoct),
        isoct = exist('OCTAVE_VERSION','builtin') ~= 0;
    end
    % If it's Octave, use the built-in gzip stream:
    if isoct,
        fid = fopen(filename,'rz','l');
        data = uint8(fread(fid,'uint8'));
        fclose(fid);
    else
        % Decode gzip data using Java if this is Matlab
        % http://undocumentedmatlab.com/blog/savezip-utility
        % http://www.mathworks.com/matlabcentral/fileexchange/39526-byte-encoding-utilities/content/encoder/gzipdecode.m
        streamCopier = com.mathworks.mlwidgets.io.InterruptibleStreamCopier.getInterruptibleStreamCopier;
        baos = java.io.ByteArrayOutputStream;
        fis  = java.io.FileInputStream(filename);
        zis  = java.util.zip.GZIPInputStream(fis);
        streamCopier.copyStream(zis,baos);
        fis.close;
        data = baos.toByteArray;
    end
catch
    fid = fopen(filename,'r','ieee-le');
    data = fread(fid);
    fclose(fid);
    data = uint8(data);
end
magic = typecast(data(1:2),'uint16');
if magic ~= 23117, error('Signature is not MZ3\n'); return; end;
%attr reports attributes and version
attr = typecast(data(3:4),'uint16');
if (attr == 0) || (attr > 31), error('This mz3 file uses (future) unsupported features\n'); end;
isFACE = bitand(attr,1);
isVERT = bitand(attr,2);
isRGBA = bitand(attr,4);
isSCALAR = bitand(attr,8);
isDOUBLE = bitand(attr,16);
%read attributes
NFACE = typecast(data(5:8),'uint32');
NVERT = typecast(data(9:12),'uint32');
NSKIP = typecast(data(13:16),'uint32');
hdrSz = 16+NSKIP; %header size in bytes
%read faces
if isFACE
    facebytes = NFACE * 3 * 4; %each face has 3 indices, each 4 byte int
    faces = typecast(data(hdrSz+1:hdrSz+facebytes),'int32');
    faces = double(faces')+1; %matlab indices arrays from 1 not 0
    faces = reshape(faces,3, NFACE)';
    hdrSz = hdrSz + facebytes;
end;
%read vertices
if isVERT
    vertbytes = NVERT * 3 * 4; %each vertex has 3 values (x,y,z), each 4 byte float
    vertices = typecast(data(hdrSz+1:hdrSz+vertbytes),'single');
    vertices = double(vertices); %matlab wants doubles
    vertices = reshape(vertices,3,NVERT)';
    hdrSz = hdrSz + vertbytes;
end
%read vertexColors
if isRGBA
    RGBAbytes = NVERT * 4; %each color has 4 values (r,g,b,a), each 1 byte
    vertexColors = typecast(data(hdrSz+1:hdrSz+RGBAbytes),'uint8');
    vertexColors = double(vertexColors)/255; %matlab wants values 0..1
    vertexColors = reshape(vertexColors,4,NVERT)';
    if isForceRGBA2RGB
        vertexColors = vertexColors(:,1:3);
    end
    hdrSz = hdrSz + RGBAbytes;
end
%
if isForceRGBA2RGB && isSCALAR && isRGBA
    %templates can include both RGB and SCALAR values - here we ignore this
    fprintf('Template mesh with both color (RGB) and intensity (region index) values. Ignoring region indices.\n');
    fprintf(' Use readMz3(''%s'', false) to load index values.\n', filename);
    return;
end
nSCALAR = 0;
%read scalar vertex properties, e.g. intensity
if isSCALAR
    scalars = typecast(data(hdrSz+1:end),'single');
    if isForceRGBA2RGB || ~isRGBA
        vertexColors = double(scalars); %matlab wants doubles 
        if (NVERT < 1), NVERT = numel(vertexColors); end 
        nSCALAR = numel(vertexColors) / NVERT; 
        vertexColors = reshape(vertexColors,NVERT,nSCALAR);
    else %both RGBA and Scalars present - set vertexColors to 5 components
        vertexColors = [vertexColors, scalars]; 
    end
end
%read double-precision scalar vertex properties, e.g. intensity
if isDOUBLE
    vertexColors = typecast(data(hdrSz+1:end),'double');
    if (NVERT < 1), NVERT = numel(vertexColors); end 
    nSCALAR = numel(vertexColors) / NVERT; 
    vertexColors = reshape(vertexColors,NVERT,nSCALAR);
end
if isVerbose
   fprintf('isFACE: %s\n', mat2str(isFACE ~= 0)); 
   fprintf('isVERT: %s\n', mat2str(isVERT ~= 0)); 
   fprintf('isRGBA: %s\n', mat2str(isRGBA ~= 0)); 
   fprintf('isSCALAR: %s\n', mat2str(isSCALAR ~= 0)); 
   fprintf('isDOUBLE: %s\n', mat2str(isDOUBLE  ~= 0)); 
   fprintf('NFACE: %d\n', NFACE); 
   fprintf('NVERT: %d\n', NVERT); 
   fprintf('NSKIP: %d\n', NSKIP); 
   fprintf('nSCALAR: %d\n', nSCALAR); 
end
%end readMz3()