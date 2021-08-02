unit pylib;
{$mode objfpc}
{$H+}
interface
{$IFDEF Darwin}
  {$modeswitch objectivec1}
  //{$DEFINE NEWPY} //prefer Python 3.* over Python2.7, however AppleSilicon will refuse to run Python3
{$ENDIF}
uses
  SysUtils, Classes
  {$IFDEF Windows}, Windows{$ENDIF};

function findPythonLib(def: string): string;

implementation

{$IFDEF Darwin}

uses CocoaAll, MacOSAll;

const
     kBasePath = '/Library/Frameworks/Python.framework/Versions/';
function ResourceDir (): string;
begin
	result := NSBundle.mainBundle.resourcePath.UTF8String;
end;
{$ELSE} //IF darwin ELSE not darwin
 {$IFDEF LINUX}
 var
   gResourceDir : string = '';

  function ResourceDir (): string;
  label
    111, 222, 333;
  var
     pths, nms, exts: TStringList;
     p,n,x: integer;
     verbose: boolean = false;
     str: string;
  begin
    if (length(gResourceDir) > 0) then
      exit(gResourceDir);
    result := extractfilepath(paramstr(0))+'Resources';
    if  DirectoryExists(result) then goto 333;
    str := FpGetEnv('MRICROGL_DIR');
    if (length(str) > 0) then begin
       result := str;
       if  DirectoryExists(result) then goto 333;
    end;
    pths := TStringList.Create;
    pths.Add('/opt/');
    pths.Add('/usr/local/');
    pths.Add('/usr/local/share/');
    pths.Add('/usr/share/');
    nms := TStringList.Create;
    if (CompareText('MRIcroGL', paramstr(0)) <> 0) then
       nms.Add(ExtractFileName(paramstr(0)));
    nms.Add('MRIcroGL');
    exts := TStringList.Create;
    exts.Add('/Resources');
    exts.Add('');
    111:
    for p := 0 to pths.Count -1 do
        for n := 0 to nms.Count -1 do
            for x := 0 to exts.Count -1 do begin
              result := pths[p]+nms[n]+exts[x];
              if  DirectoryExists(result) then
                  goto 222;
              if (verbose) then
                 writeln('  '+result)
            end;
    if not verbose then begin
      //report errors for second pass
      writeln('Unable to find Resources folder:');
      verbose := true;
      goto 111;
    end;
    222:
    exts.Free;
    nms.Free;
    pths.Free;
    333:
    gResourceDir := result;
  end;

 {$ELSE} //IF linux ELSE windows
 function ResourceDir (): string;
 begin
     result := extractfilepath(paramstr(0))+'Resources';
 end;
 {$ENDIF} //IF linux ELSE windows
{$ENDIF} //IF Darwin ELSE not-Darwin

{$IFDEF UNIX}
function InitPyLibraryPath: string;
function GetMacPath(NMinorVersion: integer): string;
begin
  Result:= Format('/Library/Frameworks/Python.framework/Versions/3.%d/lib/libpython3.%d.dylib',
    [NMinorVersion, NMinorVersion]);
end;
var
N: integer;
begin
Result:= '';
{$ifdef windows}
exit('python35.dll');
{$endif}

{$ifdef linux}
exit('libpython3.6m.so.1.0');
{$endif}

{$ifdef freebsd}
exit('libpython3.6m.so');
{$endif}

{$ifdef darwin}
for N:= 4 to 9 do
begin
  Result:= GetMacPath(N);
  if FileExists(Result) then exit;
end;
{$endif}
end;

function searchPy(pth: string): string;
var
 searchResult : TSearchRec;
begin
  result := '';
  {$IFDEF NEWPY}
    {$IFDEF Darwin}
    if FindFirst(IncludeTrailingPathDelimiter(pth)+'libpython3*.dylib', faDirectory, searchResult) = 0 then
    {$ELSE}
    if FindFirst(IncludeTrailingPathDelimiter(pth)+'libpython3*.so', faDirectory, searchResult) = 0 then
    {$ENDIF}
  {$ELSE}
    {$IFDEF Darwin}
    if FindFirst(IncludeTrailingPathDelimiter(pth)+'libpython*.dylib', faDirectory, searchResult) = 0 then
    {$ELSE}
    if FindFirst(IncludeTrailingPathDelimiter(pth)+'libpython*.so', faDirectory, searchResult) = 0 then
    {$ENDIF}
  {$ENDIF}
  result := IncludeTrailingPathDelimiter(pth)+(searchResult.Name);
  FindClose(searchResult);
end;
{$ENDIF}

{$IFDEF Darwin}
{$IFDEF NEWPY}
{$ifdef darwin}
function findMacOSLibPython3x: string;
var
N: integer;
S: String;
begin
for N:= 25 downto 5 do
begin
S:= Format('/Library/Frameworks/Python.framework/Versions/3.%d/lib/libpython3.%d.dylib',
  [N, N]);
if FileExists(S) then exit(S);
end;
exit('');
end;
{$endif}
{$ENDIF} //NEWPY

function findMacOSLibPython27: string;
const
   kPth = '/System/Library/Frameworks/Python.framework/Versions/Current/lib/libpython2.7.dylib';
begin
result := '';
if not FileExists(kPth) then exit;
result := kPth;
end;


{$ENDIF}

{$IFDEF LINUX}
function findFirstRecursive(const SearchPath: String; SearchMask: String): string;
//example findFirstRecursive('/usr/lib/', 'libpython3*so')
//uses LazFileUtils
var
ret: string;
procedure find1(Dir: string);
var
 SR: TSearchRec;
begin
   if (ret <> '') or (FileIsSymlink(Dir)) then exit;
   if FindFirst(IncludeTrailingBackslash(Dir) + SearchMask, faAnyFile or faDirectory, SR) = 0 then begin
       ret := IncludeTrailingBackslash(Dir) +SR.Name;
       FindClose(SR);
       exit;
    end;
    if FindFirst(IncludeTrailingBackslash(Dir) + '*.*', faAnyFile or faDirectory, SR) = 0 then
       try
         repeat
           if ((SR.Attr and faDirectory) <> 0) and (SR.Name <> '.') and (SR.Name <> '..') then
             find1(IncludeTrailingBackslash(Dir) + SR.Name);  // recursive call!
         until FindNext(Sr) <> 0;
       finally
         FindClose(SR);
       end;
end;
begin
ret := '';
find1(SearchPath);
result := ret;
end;

function findLinuxLibPython3(pthroot: string = '/usr/lib/'): string;
begin
   result := findFirstRecursive(pthroot, 'libpython3*so');
end;
{$ENDIF}

function findPythonLib(def: string): string;
{$IFDEF WINDOWS}
var
fnm: string;
begin
   result := def;
   if fileexists(def) then exit;
   result :=''; //assume failure
   fnm := ScriptDir + pathdelim + 'python35.dll';
   if not FileExists(fnm) then exit;
   if not FileExists(changefileext(fnm,'.zip')) then exit;
   result := fnm;
end;
{$ELSE}
{$IFDEF Linux}
const
     knPaths = 8;
     // /usr/lib/i386-linux-gnu/
     {$IFDEF CPU64}
     kBasePaths : array [1..knPaths] of string = ('/lib/','/lib64/','/usr/lib64/','/usr/lib/x86_64-linux-gnu/','/usr/lib/','/usr/local/lib/','/usr/lib/python2.7/config-x86_64-linux-gnu/','/opt/gitlab/embedded/lib/');
     {$ELSE}
     kBasePaths : array [1..knPaths] of string = ('/lib/','/lib32/','/usr/lib32/','/usr/lib/i386-linux-gnu/','/usr/lib/','/usr/local/lib/','/usr/lib/python2.7/config-i386-linux-gnu/','/opt/gitlab/embedded/lib/');
     {$ENDIF}
     kBaseName = 'libpython';
{$ENDIF}
{$IFDEF Darwin}
  const
     knPaths = 3;
     kBasePaths : array [1..knPaths] of string = (kBasePath, '/System'+kBasePath, '/System/Library/Frameworks/Python.framework/Versions/Current/lib/');
{$ENDIF}
  var
       searchResult : TSearchRec;
       pth, fnm: string;
       vers : TStringList;
       n: integer;
    begin
      result := def;
      if DirectoryExists(def) then begin //in case the user supplies libdir not the library name
           result := searchPy(def);
           if length(result) > 0 then exit;
      end;
      {$IFDEF Darwin}
      {$IFDEF NEWPY}
      result := findMacOSLibPython3x();
      if length(result) > 0 then exit;
      {$ENDIF}
      result := findMacOSLibPython27();
      if length(result) > 0 then exit;
      {$ENDIF}
      {$IFDEF LINUX}
      result := findLinuxLibPython3();
      if length(result) > 0 then exit;
      writeln('If scripts generate "PyUnicode_FromWideChar" errors, install Python3 and reset ("-R") this software.');
      {$ENDIF}
         {$IFDEF LCLCocoa}
         result := searchPy('/System/Library/Frameworks/Python.framework/Versions/Current/lib');
         if fileexists(result) then exit;
         {$ENDIF}
         //if fileexists(def) then exit;
         result := InitPyLibraryPath;
         if fileexists(result) then exit;
         {$IFDEF Darwin}  //check in application resource folder
         result := resourceDir + pathdelim+'libpython3.6.dylib';
         if fileexists(result) then exit;
         {$ENDIF}
         vers := TStringList.Create;
         n := 1;
         while (n <= knPaths) and (vers.Count < 1) do begin
           pth := kBasePaths[n];
           n := n + 1;
           if not DirectoryExists(pth) then continue;
           {$IFDEF Linux}
           if FindFirst(pth+'*.so', faDirectory, searchResult) = 0 then begin
           {$ELSE}
           if FindFirst(pth+'*', faDirectory, searchResult) = 0 then begin
           {$ENDIF}
             repeat
                    //showmessage('?'+searchResult.Name);
                    if (length(searchResult.Name) < 1) or (searchResult.Name[1] = '.') then continue;
                    {$IFDEF LINUX}
                    if (pos(kBaseName,searchResult.Name) < 1) then continue;
                    {$ELSE}
                    if (not (searchResult.Name[1] in ['0'..'9'])) then continue;
                    {$ENDIF}
                if (pos('libpython2.6',searchResult.Name) < 1) then
                   vers.Add(searchResult.Name);
              until findnext(searchResult) <> 0;
           end;
          FindClose(searchResult);
        end;
        if vers.Count < 1 then begin
           vers.Free;
           result :=''; //assume failure
           for n := 1 to knPaths do begin
             pth := kBasePaths[n];
             result := searchPy(pth);
             if fileexists(result) then exit;
           end;
           result := '';
           exit;
        end;
        vers.Sort;
        fnm := vers.Strings[vers.Count-1]; //newest version? what if 3.10 vs 3.9?
        vers.Free;
        {$IFDEF Darwin}
        fnm := kBasePath+fnm+'/lib/libpython'+fnm+'.dylib';
        {$ENDIF}
        {$IFDEF LINUX}
        fnm := pth+ fnm;
        {$ENDIF}
        if fileexists(fnm) then
           result := fnm;
    end;
{$ENDIF} //IF windows ELSE unix

end.
