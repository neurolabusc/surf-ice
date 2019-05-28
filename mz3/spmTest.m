function spmTest
%this loads sample overlays
% each overlay has two contrasts/timepoints
% examples saved in both GIfTI and MZ3 format

%load GIfTI
fnm = fullfile(spm('Dir'),'canonical','cortex_5124.surf.gii');
if ~exist(fnm,'file')
    error('Unable to find file (it comes with spm12): %s', fnm); 
end
tic;
g = gifti(fnm);
s=dir(fnm);
fprintf('GIfTI size: %d loadtime: %g\n', s.bytes, toc);
%load GIfTI
fnm = 'cortex_5124.mz3';
if ~exist(fnm,'file')
    error('Unable to find file: %s', fnm); 
end
m = gifti();
tic;
[m.faces, m.vertices] = readMz3(fnm);
s=dir(fnm);
fprintf('MZ3 size: %d loadtime: %g\n', s.bytes, toc);
%read mz3 file with two overlays
fnm = '16DoubleOverlay_5124x2.mz3';
if ~exist(fnm,'file')
    error('Unable to find file : %s', fnm); 
end
[~, ~, vertexColors] = readMz3(fnm);
%show first overlay on GIfTI file
gg1.cdata = vertexColors(:,1);
figure; plot(g, gg1);
%show second overlay on MZ3 file
gg2.cdata = vertexColors(:,2);
figure; plot(m, gg2);