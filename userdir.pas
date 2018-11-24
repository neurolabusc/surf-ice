unit userdir;
//returns directory where user has read/write permissions...
{$IFDEF FPC} {$mode delphi}{$H+} {$ENDIF}
interface
//returns number of cores: a computer with two dual cores will report 4
function IniName: string;
function DefaultsDir (lSubFolder: string): string;
function DesktopFolder: string;
{$IFDEF OLDOSX}
function AppDir: string;  //e.g. c:\folder\ for c:\folder\myapp.exe, but /folder/myapp.app/ for /folder/myapp.app/app
{$ELSE}
function AppDir: string;  //e.g. c:\folder\ for c:\folder\myapp.exe, but /myapp.app/Contents/Resources
{$ENDIF}
function AppDir2: string; //e.g. c:\folder\ for c:\folder\myapp.exe, but /folder/ for /folder/myapp.app/app
//function ResourceDir: string; //e.g. /MRIcroGL.app/Contents/Resources
 //function ExeDir: string;

implementation

{$IFDEF UNIX}
uses Process, SysUtils,classes,IniFiles,dialogs, define_types;
function DesktopFolder: string; //Returns path of destop folder with pathdelim, ~/Desktop/
begin
    result := GetEnvironmentVariable ('HOME')+pathdelim+ 'Desktop'+pathdelim;
    if not DirectoryExists(result) then
      result :=   GetEnvironmentVariable ('HOME')+pathdelim;
    //result := '~/Desktop/';
end;

function FileNameNoExt (lFilewExt:String): string;
//remove final extension
var
   lLen,lInc: integer;
   lName: String;
begin
	lName := '';
     lLen := length(lFilewExt);
	lInc := lLen+1;
	 if  lLen > 0 then begin
	   repeat
                 dec(lInc);
           until (lFileWExt[lInc] = '.') or (lInc = 1);
	 end;
     if lInc > 1 then
        for lLen := 1 to (lInc - 1) do
            lName := lName + lFileWExt[lLen]
     else
         lName := lFilewExt; //no extension
     Result := lName;
end;

function DefaultsDir (lSubFolder: string): string;
//for Linux: DefaultsDir is ~/appname/SubFolder/, e.g. /home/username/mricron/subfolder/
//Note: Final character is pathdelim
const
     pathdelim = '/';
var
   lBaseDir: string;
begin
     lBaseDir := GetEnvironmentVariable ('HOME')+pathdelim+'.'+ FileNameNoExt(ExtractFilename(paramstr(0) ) );
     if not DirectoryExists(lBaseDir) then begin
        {$I-}
        MkDir(lBaseDir);
        if IOResult <> 0 then begin
               //Msg('Unable to create new folder '+lBaseDir);
        end;
        {$I+}
     end;
     lBaseDir := lBaseDir+pathdelim;
     if lSubFolder <> '' then begin
         lBaseDir := lBaseDir + lSubFolder;
         if not DirectoryExists(lBaseDir) then begin
            {$I-}
            MkDir(lBaseDir);
            if IOResult <> 0 then begin
               //you may want to show an error, e.g. showmessage('Unable to create new folder '+lBaseDir);
               exit;
            end;
            {$I+}
         end;
         result := lBaseDir + pathdelim;
     end else
         result := lBaseDir;
end;

function IniName: string;
begin
  result := DefaultsDir('')+FileNameNoExt(extractfilename(paramstr(0)))+'.ini';
end;
{$ELSE} //If UNIX ELSE NOT Unix
uses
    SysUtils, Windows,shlobj;

//for administrators, we can write to folder with executable, otherwise we will save data to the user's AppDataFolder
function AppDataFolder: string;  //uses shlobj
{$IFDEF FPC} const CSIDL_APPDATA = 26; {$ENDIF}
var
   Path : pchar;
   idList : PItemIDList;
begin
     GetMem(Path, MAX_PATH);
     SHGetSpecialFolderLocation(0, CSIDL_APPDATA , idList);
     SHGetPathFromIDList(idList, Path);
     Result := string(Path);
     FreeMem(Path);
end;

function DesktopFolder: string; //Returns path of destop folder with pathdelim, ~/Desktop/
//uses shlobj
{$IFDEF FPC} const CSIDL_DESKTOPDIRECTORY = $0010; {$ENDIF}
var
   Path : pchar;
   idList : PItemIDList;
begin
     GetMem(Path, MAX_PATH);
     SHGetSpecialFolderLocation(0, CSIDL_DESKTOPDIRECTORY , idList);
     SHGetPathFromIDList(idList, Path);
     Result := string(Path);
     if (length(Result) > 1) and (Result[length(Result)] <> pathdelim) then
        result := result + pathdelim;
     FreeMem(Path);
end;

function IsAdmin: Boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS     = $00000220;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  x: Integer;
  bSuccess: BOOL;
 // LastError: integer;
begin

  if Win32Platform <> VER_PLATFORM_WIN32_NT then
  begin
    Result := True;
    exit;
  end;

  Result := False;
  bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True,
    hAccessToken);
  if not bSuccess then
  begin
    if GetLastError = ERROR_NO_TOKEN then
    bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY,
      hAccessToken);
  end;
  if bSuccess then
  begin
    GetMem(ptgGroups, 1024);
    {$IFDEF FPC}
    bSuccess := GetTokenInformation(hAccessToken, TokenGroups,
      ptgGroups, 1024, @dwInfoBufferSize);
    {$ELSE}
    bSuccess := GetTokenInformation(hAccessToken, TokenGroups,
      ptgGroups, 1024, dwInfoBufferSize);
    {$ENDIF}
    (*LastError := GetLastError;
    if not bSuccess then begin
      //you may want to show an error message..
      //showmessage(format('GetLastError %d',[LastError]));
    end;*)
    CloseHandle(hAccessToken);
    if bSuccess then
    begin
      AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS,
        0, 0, 0, 0, 0, 0, psidAdministrators);
      {$R-}
      for x := 0 to ptgGroups.GroupCount - 1 do
        if EqualSid(psidAdministrators, ptgGroups.Groups[x].Sid) then
        begin
          Result := True;
          break;
        end;
      {$R+}
      FreeSid(psidAdministrators);
    end;
    FreeMem(ptgGroups);
  end;
end;


function IniName: string;
//only administrators can write to c:\program files -use AppDataFolder for non-Administrators
begin
     if isAdmin then
        result := changefileext(paramstr(0),'.ini')
     else
         result := AppDataFolder+'\'+changefileext(extractfilename(paramstr(0)),'.ini');
end;

function DefaultsDir (lSubFolder: string): string;
const
     pathdelim = '\';
//for Administrators: DefaultsDir is in the location of the executable, e.g. c:\program files\mricron\subfolder\
//for non-Administrators, the AppDataFolder is returned
//Note: Final character is pathdelim
begin
    result := extractfilepath(IniName);
    if length(result) < 1 then exit;
    if result[length(result)] <> pathdelim then
       result := result + pathdelim;
    if lSubFolder = '' then
       exit;
    result := result + lSubFolder;
    if result[length(result)] <> pathdelim then
       result := result + pathdelim;

end;
{$ENDIF}


{$IFDEF Darwin}

function AppDirActual: string; //e.g. c:\folder\ for c:\folder\myapp.exe, but /folder/myapp.app/ for /folder/myapp.app/app
//OSX Sierra: randomlocation on your drive https://9to5mac.com/2016/06/15/macos-sierra-gatekeeper-changes/
var
   lInName,lPath,lName,lExt: string;
begin
 result := '';
 lExt := '';
 lInName := extractfilepath(paramstr(0));
 while (length(lInName) > 3) and (upcase(lExt) <> '.APP')  do begin
       FilenameParts (lInName, lPath,lName,lExt) ;
       lInName := ExpandFileName(lInName + '\..');
 end;
 if (upcase(lExt) <> '.APP') then begin
   lInName := GetCurrentDir;
   while (length(lInName) > 3) and (upcase(lExt) <> '.APP')  do begin
       FilenameParts (lInName, lPath,lName,lExt) ;
       lInName := ExpandFileName(lInName + '\..');
   end;
 end; //try GetCurrentDir if paramstr(0) fails
 if (upcase(lExt) = '.APP')  then
    result := lPath+lName+lExt+pathdelim;
end;

{$IFDEF OLDOSX}
function AppDir: string;  //e.g. c:\folder\ for c:\folder\myapp.exe, but /folder/myapp.app/ for /folder/myapp.app/app
begin
     result := AppDirActual;
end;
{$ELSE}
///MRIcroGL.app/Contents/Resources
function AppDir: string;  //e.g. c:\folder\ for c:\folder\myapp.exe, but /myapp.app/Contents/Resources
begin
     result := AppDirActual+'Contents'+pathdelim+'Resources'+pathdelim;
end;
{$ENDIF}

function AppDir2: string; //e.g. c:\folder\ for c:\folder\myapp.exe, but /folder/myapp.app/ for /folder/myapp.app/app
begin
 result := ExtractFilePath(ExtractFileDir(AppDirActual));
end;

{$ELSE}
function AppDir: string; //e.g. c:\folder\ for c:\folder\myapp.exe, but /folder/myapp.app/ for /folder/myapp.app/app
begin
 result := extractfilepath(paramstr(0))+'Resources'+pathdelim;
 if not DirectoryExists(result) then
	result := extractfilepath(paramstr(0));
end;

function AppDir2: string; //e.g. c:\folder\ for c:\folder\myapp.exe, but /folder/myapp.app/ for /folder/myapp.app/app
begin
 result := extractfilepath(paramstr(0));
end;
{$ENDIF}

(*function ExeDir: string; //e.g. c:\folder\ for c:\folder\myapp.exe, but /folder/myapp.app/ for /folder/myapp.app/app
begin
 result := extractfilepath(paramstr(0));
end;  *)

end.
