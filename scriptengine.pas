unit scriptengine;
{$include opts.inc}
{$H+}
{$D-,O+,Q-,R-,S-}
interface
{$IFDEF FPC} {$mode delphi}{$H+} {$ENDIF}
uses
{$ifndef isTerminalApp}
  {$IFDEF FPC}
         {$IFDEF Unix} LCLIntf,  {$ENDIF}
         //Resources,
  {$ELSE}
    Windows,
  {$ENDIF}
{$IFDEF LCLCocoa} nsappkitext, {$ENDIF}
  ClipBrd,
{$endif}
{$IFDEF Windows} uscaledpi,{$ENDIF}
{$IFDEF MYPY}PythonEngine, {$ENDIF}
 //{$IFNDEF USETRANSFERTEXTURE}  scaleimageintensity,{$ENDIF}
 SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, define_types, Menus, strutils,
  uPSComponent,commandsu;

(*OVERLAYLOADCLUSTER (lFilename: string; lThreshold, lClusterMM3: single; lSaveToDisk: boolean): integer; Will add the overlay named filename, only display voxels with intensity greater than threshold with a cluster volume greater than clusterMM and return the number of the overlay.

142211*)

type

  { TScriptForm }
  TScriptForm = class(TForm)
    colorbarvisible1: TMenuItem;
    backcolor1: TMenuItem;
    edgeload1: TMenuItem;
    edgecolor1: TMenuItem;
    edgesize1: TMenuItem;
    edgethresh1: TMenuItem;
    AppleMenu: TMenuItem;
    MenuItem1: TMenuItem;
    Close1: TMenuItem;
    camerapan1: TMenuItem;
    MenuItem2: TMenuItem;
    Atlas1: TMenuItem;
    atlasstatmap1: TMenuItem;
    atlassaturationalpha1: TMenuItem;
    atlashide1: TMenuItem;
    atlasgray1: TMenuItem;
    edgecreate1: TMenuItem;
    atlasmaxindex1: TMenuItem;
    ListCommands1: TMenuItem;
    bmpzoom1: TMenuItem;
    fontname1: TMenuItem;
    exists1: TMenuItem;
    scriptformvisible1: TMenuItem;
    version1: TMenuItem;
    meshoverlayorder1: TMenuItem;
    Advanced1: TMenuItem;
    NewPython1: TMenuItem;
    meshsave1: TMenuItem;
    meshcreate1: TMenuItem;
    meshreversefaces1: TMenuItem;
    nodethreshbysizenotcolor1: TMenuItem;
    nodecreate1: TMenuItem;
    overlaysmoothvoxelwisedata1: TMenuItem;
    overlaytranslucent1: TMenuItem;
    meshcurv1: TMenuItem;
    overlayinvert1: TMenuItem;
    Tracks1: TMenuItem;
    trackprefs1: TMenuItem;
    trackload1: TMenuItem;
    shaderxray1: TMenuItem;
    shaderambientocclusion1: TMenuItem;
    quit1: TMenuItem;
    overlayadditive1: TMenuItem;
    nodethresh1: TMenuItem;
    nodesize1: TMenuItem;
    ndepolarity1: TMenuItem;
    nodehemisphere1: TMenuItem;
    nodecolor1: TMenuItem;
    nodeload1: TMenuItem;
    Nodes1: TMenuItem;
    orientcubevisible1: TMenuItem;
    meshcolor1: TMenuItem;

    MRU10: TMenuItem;
    MRU9: TMenuItem;
    MRU8: TMenuItem;
    MRU7: TMenuItem;
    MRU6: TMenuItem;
    MRU5: TMenuItem;
    MRU4: TMenuItem;
    MRU1: TMenuItem;
    MRU3: TMenuItem;
    MRU2: TMenuItem;
    Splitter1: TSplitter;
    Memo1: TMemo;
    Memo2: TMemo;
    ScriptMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    Edit1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Insert1: TMenuItem;
    MainImage1: TMenuItem;
    meshload1: TMenuItem;
    Dialogs1: TMenuItem;
    modalmessage1: TMenuItem;
    modelessmessage1: TMenuItem;
    Overlays1: TMenuItem;
    overlayload1: TMenuItem;
    overlaycloseall1: TMenuItem;
    overlaycolorname1: TMenuItem;
    overlayminmax1: TMenuItem;
    overlaytransparencyonbackground1: TMenuItem;
    overlaycolorfromzero1: TMenuItem;
    overlayvisible1: TMenuItem;
    Shaders1: TMenuItem;
    shadername1: TMenuItem;
    shaderlightazimuthelevation1: TMenuItem;
    shaderadjust1: TMenuItem;
    Render1: TMenuItem;
    azimuth1: TMenuItem;
    cameradistance1: TMenuItem;
    clip1: TMenuItem;
    clipazimuthelevation1: TMenuItem;
    elevation1: TMenuItem;
    viewaxial1: TMenuItem;
    viewcoronal1: TMenuItem;
    viewsagittal1: TMenuItem;
    savebmp1: TMenuItem;
    savebmpxy1: TMenuItem;
    wait1: TMenuItem;
    resetdefaults1: TMenuItem;
    Toosl1: TMenuItem;
    Compile1: TMenuItem;
    N2: TMenuItem;
    Stop1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PSScript1: TPSScript;
    azimuthelevation1: TMenuItem;
    procedure AppleMenuClick(Sender: TObject);
    procedure Compile1Click(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListCommands1Click(Sender: TObject);
    procedure Memo1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ReportCaretPos;
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    function OpenScript(lFilename: string): boolean;
    //function OpenParamScript: boolean;
    //function OpenStartupScript: boolean;
    procedure Memo1Change(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Shaders1Click(Sender: TObject);
    procedure Stop1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure OpenSMRU(Sender: TObject);//open template or MRU
    procedure UpdateSMRU;
    procedure InsertCommand(Sender: TObject);
    //procedure AdjustSelText;
    procedure PSScript1Compile(Sender: TPSScript);
    procedure Memo1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Memo1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure DemoProgram (isPython: boolean = false);
    procedure ToPascal(s: string);
    procedure OpenStartupScript;
    {$IFDEF MYPY}
    function PyCreate: boolean;
    function PyIsPythonScript(): boolean;
    function PyExec(): boolean;
    procedure PyEngineAfterInit(Sender: TObject);
    procedure PyIOSendData(Sender: TObject; const Data: AnsiString);
    procedure PyIOSendUniData(Sender: TObject; const Data: UnicodeString);
    procedure PyModInitialization(Sender: TObject);
   {$ENDIF}
  private
    fn: string;
    gchanged: Boolean;
    function SaveTest: Boolean;
  public
    { Public declarations }
  end;
const
  kScriptExt = '.gls';
   {$IFDEF MYPY}
  kScriptFilter = 'Scripting ('+kScriptExt+')|*'+kScriptExt+'|Python|*.py';
  {$ELSE}
  kScriptFilter = 'Scripting ('+kScriptExt+')|*'+kScriptExt;
  {$ENDIF}
var
  ScriptForm: TScriptForm;

implementation

{$IFDEF FPC} {$R *.lfm}   {$ENDIF}
{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}
{$IFNDEF MYPY}
uses
   mainunit,userdir, prefs;

function ScriptDir: string;
begin
  result := AppDir+'script';
  {$IFDEF UNIX}
  if fileexists(result) then exit;
  result := '/usr/share/mricrogl/script';
  if fileexists(result) then exit;
  result := AppDir+'script'
  {$ENDIF}
end;

{$ELSE}
uses
   mainunit,userdir, prefs, proc_py;

function ScriptDir: string;
begin
  result := AppDir+'script';
  {$IFDEF UNIX}
  if fileexists(result) then exit;
  result := '/usr/share/mricrogl/script';
  if fileexists(result) then exit;
  result := AppDir+'script'
  {$ENDIF}
end;

var
  PythonIO : TPythonInputOutput;
  PyMod: TPythonModule;
  PyEngine: TPythonEngine = nil;
  {$IFDEF Darwin}
  const
       kBasePath = '/Library/Frameworks/Python.framework/Versions/';
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
         knPaths = 6;
         kBasePaths : array [1..knPaths] of string = ('/lib64/','/usr/lib64/','/usr/lib/x86_64-linux-gnu/','/usr/lib/','/usr/local/lib/','/usr/lib/python2.7/config-x86_64-linux-gnu/');
         kBaseName = 'libpython';

  {$ENDIF}
  {$IFDEF Darwin}
      const
         knPaths = 2;
         kBasePaths : array [1..knPaths] of string = (kBasePath, '/System'+kBasePath);

  {$ENDIF}
    var
       searchResult : TSearchRec;
       pth, fnm: string;
       vers : TStringList;
       n: integer;
    begin
         result := def;
        if DirectoryExists(def) then begin //in case the user supplies libdir not the library name
          result := '';
          {$IFDEF Darwin}
          if FindFirst(IncludeTrailingPathDelimiter(def)+'libpython*.dylib', faDirectory, searchResult) = 0 then
          {$ELSE}
          if FindFirst(IncludeTrailingPathDelimiter(def)+'libpython*.so', faDirectory, searchResult) = 0 then
          {$ENDIF}
             result := IncludeTrailingPathDelimiter(def)+(searchResult.Name);
          FindClose(searchResult);
          if length(result) > 0 then exit;
        end;
         if fileexists(def) then exit;
         result :=''; //assume failure
         vers := TStringList.Create;
         n := 1;
         while (n <= knPaths) and (vers.Count < 1) do begin
           pth := kBasePaths[n];
           n := n + 1;
           if not DirectoryExists(pth) then continue;
           if FindFirst(pth+'*', faDirectory, searchResult) = 0 then begin
             repeat
                    //showmessage('?'+searchResult.Name);
                    if (length(searchResult.Name) < 1) or (searchResult.Name[1] = '.') then continue;
                    {$IFDEF LINUX}
                    if (pos(kBaseName,searchResult.Name) < 1) then continue;
                    {$ELSE}
                    if (not (searchResult.Name[1] in ['0'..'9'])) then continue;
                    {$ENDIF}
                vers.Add(searchResult.Name);
              until findnext(searchResult) <> 0;
           end;
          FindClose(searchResult);
        end;
        if vers.Count < 1 then begin
           vers.Free;
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
  {$ENDIF}

function TScriptForm.PyCreate: boolean;
//const
// cPyLibraryMac = '/Library/Frameworks/Python.framework/Versions/2.7/lib/libpython2.7.dylib';
var
  S: string;
begin
  result := false;
  S:= findPythonLib(gPrefs.PyLib);
  if (S = '') then exit;
  gPrefs.PyLib := S;
  result := true;
  PythonIO := TPythonInputOutput.Create(ScriptForm);
  PyMod := TPythonModule.Create(ScriptForm);
  PyEngine := TPythonEngine.Create(ScriptForm);
  PyEngine.IO := PythonIO;
  PyEngine.PyFlags:=[pfIgnoreEnvironmentFlag];
  PyEngine.UseLastKnownVersion:=false;
  PyMod.Engine := PyEngine;
  PyMod.ModuleName := 'gl';
  PyMod.OnInitialization:=PyModInitialization;
  PythonIO.OnSendData := PyIOSendData;
  PythonIO.OnSendUniData:= PyIOSendUniData;
  PyEngine.DllPath:= ExtractFileDir(S);
  PyEngine.DllName:= ExtractFileName(S);
  PyEngine.LoadDll
end;
function TScriptForm.PyIsPythonScript(): boolean;
begin
  result := ( Pos('import gl', Memo1.Text) > 0); //any python project must import gl
end;

function TScriptForm.PyExec(): boolean;
begin
  result := false; //assume code is not Python
  if not (PyIsPythonScript) then exit;
  Memo2.lines.Clear;
  result := true;
  if PyEngine = nil then begin
    if not PyCreate then begin //do this the first time
      {$IFDEF Windows}
      Memo2.lines.Add('Unable to find Python library [place Python .dll and .zip in Script folder]');
      {$ENDIF}
      {$IFDEF Unix}
      Memo2.lines.Add('Unable to find Python library');
      {$IFDEF Darwin}
      Memo2.lines.Add('   For MacOS this is typically in: '+kBasePath+'');
      {$ELSE}
      Memo2.lines.Add('   run ''find -name "*libpython*"'' to find the library');
      Memo2.lines.Add('   if it does not exist, install it (e.g. ''apt-get install libpython2.7'')');
      {$ENDIF}
      Memo2.lines.Add('   if it does exist, set use the Preferences/Advanced to set ''PyLib''');
      {$IFDEF Darwin}
      Memo2.lines.Add('   PyLib should be the complete path and filename of libpython*.dylib');
      {$ELSE}
      Memo2.lines.Add('   PyLib should be the complete path and filename of libpython*.so');
      {$ENDIF}
      Memo2.lines.Add('   This file should be in your LIBDIR, which you can detect by running Python from the terminal:');
      Memo2.lines.Add('     ''import sysconfig; print(sysconfig.get_config_var("LIBDIR"))''');
      {$ENDIF}
      result := true;
      exit;

    end;
  end;
  Memo2.lines.Add('Running Python script');
  try
  PyEngine.ExecStrings(ScriptForm.Memo1.Lines);
  except
    caption := 'Python Engine Failed';
  end;
  Memo2.lines.Add('Python Succesfully Executed');
  result := true;
end;

procedure TScriptForm.PyIOSendData(Sender: TObject;
  const Data: AnsiString);
begin
  Memo2.Lines.Add(Data);
end;

procedure TScriptForm.PyIOSendUniData(Sender: TObject;
  const Data: UnicodeString);
begin
  Memo2.Lines.Add(Data);
end;

function PyVERSION(Self, Args : PPyObject): PPyObject; cdecl;
begin
  with GetPythonEngine do
    Result:= PyString_FromString(kVers);
end;

function PyRESETDEFAULTS(Self, Args : PPyObject): PPyObject; cdecl;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  RESETDEFAULTS;
end;

function PyMESHCURV(Self, Args : PPyObject): PPyObject; cdecl;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  MESHCURV;
  //GLForm1.Caption := inttostr(random(888));
end;

function PyMESHREVERSEFACES(Self, Args : PPyObject): PPyObject; cdecl;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  MESHREVERSEFACES;
end;


function BOOL(i: integer): boolean;
begin
     result := i <> 0;
end;

function PySAVEBMP(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:savebmp', @PtrName)) then
    begin
      StrName:= string(PtrName);
      SAVEBMP(StrName);
    end;
end;

function PySAVEBMPXY(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  x,y: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'sii:savebmpxy', @PtrName, @x, @y)) then
    begin
      StrName:= string(PtrName);
      SAVEBMPXY(StrName, x, y);
    end;
end;
function PyBACKCOLOR(Self, Args : PPyObject): PPyObject; cdecl;
var
  R,G,B: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'iii:backcolor', @R,@G,@B)) then
      BACKCOLOR(R,G,B);
end;

function PyMESHCOLOR(Self, Args : PPyObject): PPyObject; cdecl;
var
  R,G,B: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'iii:meshcolor', @R,@G,@B)) then
      MESHCOLOR(R,G,B);
end;

function PyATLASMAXINDEX(Self, Args : PPyObject): PPyObject; cdecl;
var
  i: integer;
begin
  Result:= GetPythonEngine.PyInt_FromLong(-1);
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:atlasmaxindex', @I)) then
      Result:= GetPythonEngine.PyInt_FromLong(ATLASMAXINDEX(I));
end;

function PyEXISTS(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:exists', @PtrName)) then
    begin
      StrName:= string(PtrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(EXISTS(StrName)));
    end;
end;

function PyAZIMUTH(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:azimuth', @A)) then
      AZIMUTH(A);
end;

function PyAZIMUTHELEVATION(Self, Args : PPyObject): PPyObject; cdecl;
var
  A,E: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ii:azimuthelevation', @A, @E)) then
      AZIMUTHELEVATION(A,E);
end;

function PyBMPZOOM(Self, Args : PPyObject): PPyObject; cdecl;
var
  Z: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:bmpzoom', @Z)) then
      bmpzoom(Z);
end;

function PyCAMERADISTANCE(Self, Args : PPyObject): PPyObject; cdecl;
var
  Z: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'f:cameradistance', @Z)) then
      CAMERADISTANCE(Z);
end;

function PySHADERAMBIENTOCCLUSION(Self, Args : PPyObject): PPyObject; cdecl;
var
  Z: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'f:shaderambientocclusion', @Z)) then
      SHADERAMBIENTOCCLUSION(Z);
end;

function PyCLIP(Self, Args : PPyObject): PPyObject; cdecl;
var
  D: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'f:clip', @D)) then
      CLIP(D);
end;

function PyCLIPAZIMUTHELEVATION(Self, Args : PPyObject): PPyObject; cdecl;
var
  D,A,E: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'fff:clipazimuthelevation', @D,@A,@E)) then
      CLIPAZIMUTHELEVATION(D,A,E);
end;

function PyTRACKPREFS(Self, Args : PPyObject): PPyObject; cdecl;
var
  D,A,E: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'fff:trackprefs', @D,@A,@E)) then
      TRACKPREFS(D,A,E);
end;

function PyATLASSATURATIONALPHA(Self, Args : PPyObject): PPyObject; cdecl;
var
  A,B: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ff:atlassaturationalpha', @A,@B)) then
      ATLASSATURATIONALPHA(A,B);
end;

function PyCAMERAPAN(Self, Args : PPyObject): PPyObject; cdecl;
var
  A,B: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ff:camerapan', @A,@B)) then
      CAMERAPAN(A,B);
end;

function PyNODESIZE(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: single;
  I: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'fi:nodesize', @A,@I)) then
      NODESIZE(A,Bool(I));
end;

function PyEDGESIZE(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: single;
  I: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'fi:edgesize', @A,@I)) then
      EDGESIZE(A,Bool(I));
end;

function PyOVERLAYINVERT(Self, Args : PPyObject): PPyObject; cdecl;
var
  B,I: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ii:overlayinvert', @I,@B)) then
      OVERLAYINVERT(I,Bool(B));
end;

function PyOVERLAYTRANSLUCENT(Self, Args : PPyObject): PPyObject; cdecl;
var
  B,I: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ii:overlaytranslucent', @I,@B)) then
      OVERLAYTRANSLUCENT(I,Bool(B));
end;

function PyEDGETHRESH(Self, Args : PPyObject): PPyObject; cdecl;
var
  A,B: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ff:edgethresh', @A,@B)) then
      EDGETHRESH(A,B);
end;

function PySHADERXRAY(Self, Args : PPyObject): PPyObject; cdecl;
var
  A,B: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ff:shaderxray', @A,@B)) then
      SHADERXRAY(A,B);
end;

function PyNODETHRESH(Self, Args : PPyObject): PPyObject; cdecl;
var
  A,B: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ff:nodethresh', @A,@B)) then
      NODETHRESH(A,B);
end;

function PyCOLORBARPOSITION(Self, Args : PPyObject): PPyObject; cdecl;
var
  P: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:colorbarposition', @P)) then
      COLORBARPOSITION (P);
end;

function PyMESHOVERLAYORDER(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:meshoverlayorder', @A)) then
      MESHOVERLAYORDER(BOOL(A));
end;

function PyORIENTCUBEVISIBLE(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:orientcubevisible', @A)) then
      ORIENTCUBEVISIBLE(BOOL(A));
end;

function PyOVERLAYADDITIVE(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:overlayadditive', @A)) then
      OVERLAYADDITIVE(BOOL(A));
end;

function PyOVERLAYSMOOTHVOXELWISEDATA(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:overlaysmoothvoxelwisedata', @A)) then
      OVERLAYSMOOTHVOXELWISEDATA(BOOL(A));
end;

function PySHADERFORBACKGROUNDONLY(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:shaderforbackgroundonly', @A)) then
      SHADERFORBACKGROUNDONLY(BOOL(A));
end;

function PyNODETHRESHBYSIZENOTCOLOR(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:nodethreshbysizenotcolor', @A)) then
      NODETHRESHBYSIZENOTCOLOR(BOOL(A));
end;

function PyCOLORBARVISIBLE(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:colorbarvisible', @A)) then
      COLORBARVISIBLE(BOOL(A));
end;

function PyFONTNAME(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:fontname', @PtrName)) then
    begin
      StrName:= string(PtrName);
      FONTNAME(StrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyELEVATION(Self, Args : PPyObject): PPyObject; cdecl;
var
  E: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:elevation', @E)) then
      ELEVATION(E);
end;

function PyNODEHEMISPHERE(Self, Args : PPyObject): PPyObject; cdecl;
var
  E: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:nodehemisphere', @E)) then
      NODEHEMISPHERE(E);
end;

function PyNODEPOLARITY(Self, Args : PPyObject): PPyObject; cdecl;
var
  E: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:nodepolarity', @E)) then
      NODEPOLARITY(E);
end;

function PyMESHLOAD(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:meshload', @PtrName)) then
    begin
      StrName:= string(PtrName);
      MESHLOAD(StrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyTRACKLOAD(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:trackload', @PtrName)) then
    begin
      StrName:= string(PtrName);
      TRACKLOAD(StrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyMESHSAVE(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:meshsave', @PtrName)) then
    begin
      StrName:= string(PtrName);
      MESHSAVE(StrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyNODELOAD(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:nodeload', @PtrName)) then
    begin
      StrName:= string(PtrName);
      NODELOAD(StrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyOVERLAYLOAD(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:overlayload', @PtrName)) then
    begin
      StrName:= string(PtrName);
      OVERLAYLOAD(StrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyEDGELOAD(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:edgeload', @PtrName)) then
    begin
      StrName:= string(PtrName);
      EDGELOAD(StrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyMODALMESSAGE(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:modalmessage', @PtrName)) then
    begin
      StrName:= string(PtrName);
      MODALMESSAGE(StrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyMODELESSMESSAGE(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:modelessmessage', @PtrName)) then
    begin
      StrName:= string(PtrName);
      MODELESSMESSAGE(StrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyOVERLAYCLOSEALL(Self, Args : PPyObject): PPyObject; cdecl;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(TRUE));
  OVERLAYCLOSEALL;
end;

function PyQUIT(Self, Args : PPyObject): PPyObject; cdecl;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(TRUE));
  QUIT;
end;

function PyOVERLAYCOLORNAME(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  V: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'is:overlaycolorname', @V, @PtrName)) then
    begin
      StrName:= string(PtrName);
      OVERLAYCOLORNAME(V, StrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PySHADERNAME(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  V: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:shadername', @PtrName)) then
    begin
      StrName:= string(PtrName);
      SHADERNAME(StrName);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PySHADERADJUST(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  f: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'sf:shaderadjust', @PtrName, @f)) then
    begin
      StrName:= string(PtrName);
      SHADERADJUST(StrName, f);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyEDGECOLOR(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  i: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'si:edgecolor', @PtrName, @i)) then
    begin
      StrName:= string(PtrName);
      EDGECOLOR(StrName, bool(i));
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyNODECOLOR(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  i: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'si:nodecolor', @PtrName, @i)) then
    begin
      StrName:= string(PtrName);
      NODECOLOR(StrName, bool(i));
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PyMESHCREATE(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName,PtrName2: PChar;
  StrName,StrName2: string;
  f,f2: single;
  i,i2: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ssffii:meshcreate', @PtrName, @PtrName2, @f, @f2, @i, @i2)) then
    begin
      StrName:= string(PtrName);
      StrName2:= string(PtrName2);
      MESHCREATE(StrName, StrName2, f, f2, i, i2);
      Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
    end;
end;

function PySCRIPTFORMVISIBLE(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:scriptformvisible', @A)) then
       SCRIPTFORMVISIBLE(BOOL(A));
end;

function PyVIEWAXIAL(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:viewaxial', @A)) then
       VIEWAXIAL(BOOL(A));
end;

function PyVIEWCORONAL(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:viewcoronal', @A)) then
       VIEWCORONAL(BOOL(A));
end;

function PyOVERLAYTRANSPARENCYONBACKGROUND(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:overlaytransparencyonbackground', @A)) then
       OVERLAYTRANSPARENCYONBACKGROUND(A);
end;

function PyWAIT(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:wait', @A)) then
       WAIT(A);
end;

function PySHADERLIGHTAZIMUTHELEVATION(Self, Args : PPyObject): PPyObject; cdecl;
var
  A,B: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ii:shaderlightazimuthelevation', @A, @B)) then
       SHADERLIGHTAZIMUTHELEVATION(A,B);
end;

function PyOVERLAYMINMAX(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
  B,C: single;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'iff:overlayminmax', @A, @B, @C)) then
       OVERLAYMINMAX(A,B,C);
end;

function PyOVERLAYVISIBLE(Self, Args : PPyObject): PPyObject; cdecl;
var
  A,B: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'ii:overlayvisible', @A, @B)) then
       OVERLAYVISIBLE(A,BOOL(B));
end;



function PyVIEWSAGITTAL(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'i:viewsagittal', @A)) then
       VIEWSAGITTAL(BOOL(A));
end;


(*function PyATLASGRAYBG(Self, Args : PPyObject): PPyObject; cdecl;
var
  A: array of integer;
begin
  exit;
  Result:= GetPythonEngine.PyBool_FromLong(Ord(True));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 'O:atlasgraybg', @A)) then
       ATLASGRAYBG(A);
end;*)

procedure TScriptForm.PyModInitialization(Sender: TObject);
begin
  with Sender as TPythonModule do begin
    //AddMethod('atlasgraybg', @PyATLASGRAYBG, '');
    AddMethod('atlasmaxindex', @PyATLASMAXINDEX, '');
    AddMethod('atlassaturationalpha', @PyATLASSATURATIONALPHA, '');
    AddMethod('azimuth', @PyAZIMUTH, '');
    AddMethod('azimuthelevation', @PyAZIMUTHELEVATION, '');
    AddMethod('backcolor', @PyBACKCOLOR, '');
    AddMethod('bmpzoom', @PyBMPZOOM, '');
    AddMethod('cameradistance', @PyCAMERADISTANCE, '');
    AddMethod('camerapan', @PyCAMERAPAN, '');
    AddMethod('clip', @PyCLIP, '');
    AddMethod('clipazimuthelevation', @PyCLIPAZIMUTHELEVATION, '');
    AddMethod('colorbarposition', @PyCOLORBARPOSITION, '');
    AddMethod('colorbarvisible', @PyCOLORBARVISIBLE, '');
    AddMethod('edgecolor', @PyEDGECOLOR, '');
    AddMethod('edgeload', @PyEDGELOAD, '');
    AddMethod('edgesize', @PyEDGESIZE, '');
    AddMethod('edgethresh', @PyEDGETHRESH, '');
    AddMethod('elevation', @PyELEVATION, '');
    AddMethod('exists', @PyEXISTS, '');
    AddMethod('fontname', @PyFONTNAME, '');
    AddMethod('meshcolor', @PyMESHCOLOR, '');
    AddMethod('meshcreate', @PyMESHCREATE, '');
    AddMethod('meshcurv', @PyMESHCURV, '');
    AddMethod('meshload', @PyMESHLOAD, '');
    AddMethod('meshoverlayorder', @PyMESHOVERLAYORDER, '');
    AddMethod('meshreversefaces', @PyMESHREVERSEFACES, '');
    AddMethod('meshsave', @PyMESHSAVE, '');
    AddMethod('modalmessage', @PyMODALMESSAGE, '');
    AddMethod('modelessmessage', @PyMODELESSMESSAGE, '');
    AddMethod('nodecolor', @PyNODECOLOR, '');
    AddMethod('nodehemisphere', @PyNODEHEMISPHERE, '');
    AddMethod('nodeload', @PyNODELOAD, '');
    AddMethod('nodepolarity', @PyNODEPOLARITY, '');
    AddMethod('nodesize', @PyNODESIZE, '');
    AddMethod('nodethresh', @PyNODETHRESH, '');
    AddMethod('nodethreshbysizenotcolor', @PyNODETHRESHBYSIZENOTCOLOR, '');
    AddMethod('orientcubevisible', @PyORIENTCUBEVISIBLE, '');
    AddMethod('overlayadditive', @PyOVERLAYADDITIVE, '');
    AddMethod('overlaycloseall', @PyOVERLAYCLOSEALL, '');
    AddMethod('overlaycolorname', @PyOVERLAYCOLORNAME, '');
    AddMethod('overlayinvert', @PyOVERLAYINVERT, '');
    AddMethod('overlayload', @PyOVERLAYLOAD, '');
    AddMethod('overlayminmax', @PyOVERLAYMINMAX, '');
    AddMethod('overlaysmoothvoxelwisedata', @PyOVERLAYSMOOTHVOXELWISEDATA, '');
    AddMethod('overlaytranslucent', @PyOVERLAYTRANSLUCENT, '');
    AddMethod('overlaytransparencyonbackground', @PyOVERLAYTRANSPARENCYONBACKGROUND, '');
    AddMethod('overlayvisible', @PyOVERLAYVISIBLE, '');
    AddMethod('quit', @PyQUIT, '');
    AddMethod('resetdefaults', @PyRESETDEFAULTS, '');
    AddMethod('savebmp', @PySAVEBMP, '');
    AddMethod('savebmpxy', @PySAVEBMPXY, '');
    AddMethod('scriptformvisible', @PySCRIPTFORMVISIBLE, '');
    AddMethod('shaderadjust', @PySHADERADJUST, '');
    AddMethod('shaderambientocclusion', @PySHADERAMBIENTOCCLUSION, '');
    AddMethod('shaderforbackgroundonly', @PySHADERFORBACKGROUNDONLY, '');
    AddMethod('shaderlightazimuthelevation', @PySHADERLIGHTAZIMUTHELEVATION, '');
    AddMethod('shadername', @PySHADERNAME, '');
    AddMethod('shaderxray', @PySHADERXRAY, '');
    AddMethod('trackload', @PyTRACKLOAD, '');
    AddMethod('trackprefs', @PyTRACKPREFS, '');
    AddMethod('version', @PyVERSION, '');
    AddMethod('viewaxial', @PyVIEWAXIAL, '');
    AddMethod('viewcoronal', @PyVIEWCORONAL, '');
    AddMethod('viewsagittal', @PyVIEWSAGITTAL, '');
    AddMethod('wait', @PyWAIT, '');
  end;
end;

procedure TScriptForm.PyEngineAfterInit(Sender: TObject);
var
  dir: string;
begin
  dir:= ExtractFilePath(Application.ExeName);
  {$ifdef windows}
  Py_SetSysPath([ScriptDir, changefileext(gPrefs.PyLib,'.zip')], false);
  {$endif}
  Py_SetSysPath([dir+'Py'], true);
end;
{$ENDIF} //IFDEF MYPY


procedure TScriptForm.OpenStartupScript;
//do this after clrbar and gltext are created
begin
 if gPrefs.initScript = '' then begin
   OpenSMRU(nil);
   if gPrefs.StartupScript then Compile1Click(nil);
 end
 else begin
     gPrefs.PrevScript := gPrefs.InitScript;
   Memo1.Lines.Clear;
   if FileExistsF(gPrefs.initScript) then
     Memo1.Lines.LoadFromFile(gPrefs.initScript)
   else
       ToPascal(gPrefs.initScript);//Memo1.Lines.Add(gPrefs.initScript);
   Compile1Click(nil);
 end;
end; //RunStartUpScripts()

procedure TScriptForm.DemoProgram( isPython: boolean = false);
begin
Memo1.lines.clear;
 if isPython then begin
   Memo1.Lines.Add('import gl');
   Memo1.Lines.Add('import sys');
   Memo1.Lines.Add('print(sys.version)');
   Memo1.Lines.Add('print(gl.version())');
   Memo1.Lines.Add('# Note that resetdefaults() closes open meshes, overlays, track nodes');
   Memo1.Lines.Add('gl.resetdefaults()');
   Memo1.lines.Add('');
   Memo1.SelStart := maxint;
   exit;
 end;
//Memo1.lines.Add('PROGRAM Demo;');
Memo1.lines.Add('BEGIN');
Memo1.lines.Add('//Insert commands here...');
Memo1.lines.Add('');
Memo1.lines.Add('END.');
{$IFDEF UNIX}
Memo1.SelStart := 32;
{$ELSE}
Memo1.SelStart := 34;//windows uses CR+LF line ends, UNIX uses LF
{$ENDIF}
//p.X := 2;
//p.Y := 2;
//Memo1.CaretPos := p;
//Memo1.CaretPos := Point(2,2);
end;

procedure MyWriteln(const s: string);
begin
  ScriptForm.Memo2.lines.add(S);
end;

procedure TScriptForm.OpenSMRU(Sender: TObject);//open template or MRU
//Templates have tag set to 0, Most-Recently-Used items have tag set to position in gMRUstr
begin
  if Sender = nil then begin
    if (gPrefs.PrevScriptName[1] <> '') and (FileexistsF(gPrefs.PrevScriptName[1])) then
      OpenScript (gPrefs.PrevScriptName[1]);
  end else begin
    OpenScript (gPrefs.PrevScriptName[(Sender as TMenuItem).tag]);
    Compile1Click(nil);
  end;
end;

procedure TScriptForm.UpdateSMRU;
const
     kMenuItems = 7;//with OSX users quit from application menu
var
  lPos,lN,lM : integer;
begin
 lN := File1.Count-kMenuItems;
 if lN > knMRU then
    lN := knMRU;
 lM := kMenuItems;
  for lPos :=  1 to lN do begin
      if gPrefs.PrevScriptName[lPos] <> '' then begin
          File1.Items[lM].Caption :=ExtractFileName(gPrefs.PrevScriptName[lPos]);//(ParseFileName(ExtractFileName(lFName)));
	  File1.Items[lM].Tag := lPos;
          File1.Items[lM].onclick :=  OpenSMRU; //Lazarus
          File1.Items[lM].Visible := true;
      end else
          File1.Items[lM].Visible := false;
      inc(lM);
  end;//for each MRU
end;  //UpdateMRU

procedure TScriptForm.PSScript1Compile(Sender: TPSScript);
var
   i: integer;
begin
  //Sender.AddFunction( @TScriptForm.MyWriteln,'procedure Writeln(const s: string);');
  Sender.AddFunction(@MyWriteln, 'procedure Writeln(s: string);');
  for i := 1 to knFunc do
      Sender.AddFunction(kFuncRA[i].Ptr,'function '+kFuncRA[i].Decl+kFuncRA[i].Vars+';');
  for i := 1 to knProc do
      Sender.AddFunction(kProcRA[i].Ptr,'procedure '+kProcRA[i].Decl+kProcRA[i].Vars+':');
end;

procedure TScriptForm.Compile1Click(Sender: TObject);
var
  i: integer;
  compiled: boolean;
begin
  {$IFDEF MYPY}
  if PyExec() then exit;
  if (not (AnsiContainsText(Memo1.Text, 'begin'))) then begin
      Memo2.Lines.Clear;
      Memo2.Lines.Add('Error: script must contain "import gl" (for Python) or "begin" (for Pascal).');
      exit;
  end;
  {$ENDIF}
  Memo2.Lines.Clear;
  PSScript1.Script.Text := Memo1.Lines.Text;
  //PSScript1.Script.Text := Memo1.Lines.GetText; //<- this will leak! requires StrDispose
  Compiled := PSScript1.Compile;
  for i := 0 to PSScript1.CompilerMessageCount -1 do
    MyWriteln( PSScript1.CompilerMessages[i].MessageToString);
  if Compiled then
    MyWriteln('Successfully Compiled Script');
  if Compiled then begin
    if PSScript1.Execute then
      MyWriteln('Succesfully Executed')
    else
      MyWriteln('Error while executing script: '+
                  PSScript1.ExecErrorToString);
  end;
end;

procedure TScriptForm.File1Click(Sender: TObject);
begin

end;

procedure TScriptForm.FormActivate(Sender: TObject);
begin
  GLForm1.DisplayMenu.Enabled:= false;
  // obsolete{ $IFDEF LCLCocoa} Memo2.ReadOnly:=false; {$ENDIF} //https://bugs.freepascal.org/view.php?id=33153
end;

procedure TScriptForm.FormDeactivate(Sender: TObject);
begin
  GLForm1.DisplayMenu.Enabled:= true;
end;

procedure TScriptForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  //caption := Filenames[1];
end;

procedure TScriptForm.FormHide(Sender: TObject);
begin
    {$IFDEF Darwin}Application.MainForm.SetFocus;{$ENDIF}
end;

procedure TScriptForm.FormShow(Sender: TObject);
begin
  {$IFDEF LCLCocoa}
  setThemeMode(Self.Handle, gPrefs.DarkMode);
  if gPrefs.DarkMode then begin
     Memo1.Color := clGray;
     Memo2.Color := clGray;
  end else begin
      Memo1.Color := Graphics.clDefault;
      Memo2.Color := Graphics.clDefault;
  end;
  {$ENDIF}
end;

function IsPythonCompatible(lType: integer): boolean;
//current Python can not handle passing array types
var
  lTstr: string;
  i, len, n, t: integer;
begin
  result := true;
  lTStr := inttostr(lType);
  len := length(lTStr);
  i := 1;
  while i <= len do begin
    if i = len then
      n := 1
    else begin
      n := strtoint(lTStr[i]);
      inc(i);
    end;
    t := strtoint(lTStr[i]);
    if (t = 8) or (t = 9) then
       result := false;
    inc(i);
  end;
end;

procedure TScriptForm.ListCommands1Click(Sender: TObject);
var
   i,j: integer;
   M, M2: TMenuItem;
   cmds: TStringList;
begin
  cmds := TStringList.Create;
  for i := 0 to (Insert1.Count -1) do begin
      M := Insert1.Items[i];
      if (M.Visible) and (length(M.Hint) > 1) then begin
         if (not IsPythonCompatible (M.Tag)) then
            cmds.Add(M.Hint+' [Not compatible with Python]')
         else
            cmds.Add(M.Hint);
      end;
      if (M.Count > 1) then begin
         for j := 0 to (M.Count -1) do begin
             M2 := M.Items[j];
             if (M2.Visible) and (length(M2.Hint) > 1) then begin
                if (not IsPythonCompatible (M2.Tag)) then
                   cmds.Add(M2.Hint+' [Not compatible with Python]')
                else
                   cmds.Add(M2.Hint);
             end;
         end;
      end;
  end;
  Memo2.Lines.Clear;
  cmds.Sort;
  Memo2.Lines.AddStrings(cmds);
  cmds.Free;
end;

procedure TScriptForm.Memo1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  //
end;

procedure TScriptForm.AppleMenuClick(Sender: TObject);
begin

end;

function EndsStr( const Needle, Haystack : string ) : Boolean;
//http://www.delphibasics.co.uk/RTL.asp?Name=AnsiEndsStr
var
  szN,szH: integer;
  s : string;
begin
  result := false;
  szH := length(Haystack);
  szN := length(Needle);
  if szN > szH then exit;
  s := copy ( Haystack,  szH-szN + 1, szN );
  if comparestr(Needle,s) = 0 then result := true;
end;

function isNewLine(s: string): boolean;
var
  sz: integer;
begin
  result := false;
  sz := length(s);
  if sz < 1 then exit;
  result := true;
  if s[sz] = ';' then exit;
  if EndsStr('var', s) then exit;
  if EndsStr('begin', s) then exit;
  result := false;
end;

procedure TScriptForm.ToPascal(s: string);
var
  i: integer;
  l: string;
begin
  if length(s) < 1 then exit;
  l := '';
  for i := 1 to length(s) do begin
      l := l + s[i];
      if isNewLine(l) then begin
        Memo1.lines.Add(l);
        l := '';
      end;
  end;
  Memo1.lines.Add(l);
end;

procedure TScriptForm.FormCreate(Sender: TObject);
var
  lPos: integer;
begin
  {$IFDEF Windows} ScaleDPIX(ScriptForm, 96); {$ENDIF}
  {$IFNDEF Darwin} AppleMenu.Visible := false; {$ENDIF}
  fn := '';
  gchanged := False;
  DemoProgram;
  {$IFNDEF MYPY} NewPython1.Visible := false;{$ENDIF}
  FillMRU (gPrefs.PrevScriptName, ScriptDir+pathdelim,kScriptExt,True);
  //FillMRU (gPrefs.PrevScriptName, ScriptDir+pathdelim,kScriptExt,False);
  UpdateSMRU;
  OpenDialog1.InitialDir := ScriptDir;
  SaveDialog1.InitialDir := ScriptDir;
 {$IFDEF Darwin}
        Copy1.enabled := false; //https://bugs.freepascal.org/view.php?id=33632
        Cut1.ShortCut := ShortCut(Word('X'), [ssMeta]);
        Copy1.ShortCut := ShortCut(Word('C'), [ssMeta]);
        Paste1.ShortCut := ShortCut(Word('V'), [ssMeta]);
        Stop1.ShortCut := ShortCut(Word('H'), [ssMeta]);
         Compile1.ShortCut := ShortCut(Word('R'), [ssMeta]);
 {$ENDIF}
 for lPos := 0 to 8 do begin
       {$IFDEF Darwin}
      File1.Items[lPos+6].ShortCut := ShortCut(Word('1')+ord(lPos), [ssMeta]);
      {$ELSE}
      File1.Items[lPos+6].ShortCut := ShortCut(Word('1')+ord(lPos), [ssCtrl]);
      {$ENDIF}
 end;
end;

function TScriptForm.SaveTest: Boolean;
begin
  result := True;
(*  if changed then
  begin
    case MessageDlg('File is not saved, save now?', mtWarning, mbYesNoCancel, 0) of
      mrYes:
        begin
          Save1Click(nil);
          Result := not changed;
        end;
      mrNo: Result := True;
    else
      Result := False;
    end;
  end
  else
    Result := True;
*)
end;


function TScriptForm.OpenScript(lFilename: string): boolean;
begin
  result := false;
  //GLForm1.StopTimers;
  ScriptForm.Stop1Click(nil);
  if not fileexistsf (lFilename) then begin
    Showmessage('Can not find '+lFilename);
    exit;
  end;
  gPrefs.PrevScript:=lFilename;
  ScriptForm.Hint := extractfilename(lFilename);
  ScriptForm.Caption := 'Script loaded: '+ScriptForm.Hint;
  Memo1.Lines.LoadFromFile(lFileName);
    gchanged := False;
    Memo2.Lines.Clear;
    fn := lFileName;
    //Add2MRU(gPrefs.PrevScriptName,fn);
    //UpdateSMRU;
    result := true;
end;

(*function TScriptForm.OpenParamScript: boolean;
var
  lF: string;
begin
     result := false;
     if (ParamCount = 0) then exit;
     lF := Paramstr(1);
     if fileexists(lF) then
       result := OpenScript(lF);
end; *)

(*function TScriptForm.OpenStartupScript: boolean;
var
  lF: string;
begin
  result := false;
  lF := ScriptDir +pathdelim+'startup'+kScriptExt;
  if fileexists(lF) then
    result := OpenScript(lF);
  //if result then
  //  Compile1Click(nil);
end;  *)


procedure TScriptForm.Open1Click(Sender: TObject);
var
   lS: string;
begin
  if not SaveTest then
    exit;
  lS :=  GetCurrentDir;
  if not OpenDialog1.Execute then
    exit;
   SetCurrentDir(lS);
  OpenScript(OpenDialog1.FileName);
  Compile1Click(nil);
end;


procedure TScriptForm.Save1Click(Sender: TObject);
begin
  if fn = '' then
    Saveas1Click(nil)
  else begin
    Memo1.Lines.SaveToFile(fn);
    gchanged := False;
  end;
end;

procedure TScriptForm.SaveAs1Click(Sender: TObject);
begin
  SaveDialog1.FileName := '';
  if not SaveDialog1.Execute then
    exit;
  fn := SaveDialog1.FileName;
  Memo1.Lines.SaveToFile(fn);
  gchanged := False;
  //Add2MRU(gPrefs.PrevScriptName,fn);
  //UpdateSMRU;
end;

procedure TScriptForm.Memo1Change(Sender: TObject);
begin
     inherited;
  gchanged := True;
end;

procedure TScriptForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := SaveTest;
end;

procedure TScriptForm.Shaders1Click(Sender: TObject);
begin

end;

procedure TScriptForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TScriptForm.Stop1Click(Sender: TObject);
begin

  if PSScript1.Running then
    PSScript1.Stop;
end;

procedure TScriptForm.New1Click(Sender: TObject);
begin
  ScriptForm.Stop1Click(nil);
  if not SaveTest then
    exit;
  Memo2.Lines.Clear;
  fn := '';
  DemoProgram((Sender as TMenuItem).tag = 1 );
end;

procedure CleanStr (var lStr: string);
//remove symbols, set lower case...
var
  lLen,lPos: integer;
  lS: string;
begin
  lLen := length(lStr);
  if lLen < 1 then
    exit;
  lS := '';
  for lPos := 1 to lLen do
    if lStr[lPos] in ['0'..'9','a'..'z','A'..'Z'] then
      lS := lS + AnsiLowerCase(lStr[lPos]);
    lStr := lS;
end;

function TypeStr (lType: integer; isPy: boolean = false): string;
var
  lTStr,lStr : string;
  i,n,len,lLoop,lT: integer;//1=boolean,2=integer,3=float,4=string[filename]
begin
  result := '';
  if (lType = 0) and (isPy) then
     result := '()';
  if lType = 0 then
    exit;
  lTStr := inttostr(lType);
  lStr := '(';
  len := length(lTStr);
  i := 1;
  while i <= len do begin
    if i = len then
      n := 1
    else begin
      n := strtoint(lTStr[i]);
      inc(i);
    end;
    lT := strtoint(lTStr[i]);
    inc(i);
    for lLoop := 1 to n do begin
      case lT of
        1:  begin
          if isPy then
             lStr := lStr +'1'
          else
              lStr := lStr +'true';

          end;
        2:  lStr := lStr +'1';
        3:  begin
            if lLoop <= 3 then //for Cutout view, we need six values - make them different so this is a sensible cutout
              lStr := lStr +'0.5'
            else
              lStr := lStr +'1.0';
            end;
        4:  lStr := lStr +'''filename''';
        5: lStr := lStr + '''0.2 0.4 0.6; 0.8 S 0.5''';
        6: begin //byte
            if lLoop <= 3 then //for Cutout view, we need six values - make them different so this is a sensible cutout
              lStr := lStr +'1'
            else
              lStr := lStr +'255';
            end;
        7: lStr := lStr +'5';//kludge - make integer where 1 is not a good default, e.g. shaderquality
        8: lStr := lStr +'[1, 2, 4]';
        9: lStr := lStr +'[1.1, 2.5, 4.2]';
        else lStr := lStr + '''?''';
      end;//case
      if lLoop < n then
        lStr := lStr+', ';
    end;//for each loop
    if i < len then
        lStr := lStr+', ';
  end;
  lStr := lStr + ')';
  result := lStr;
end;

procedure TScriptForm.InsertCommand(Sender: TObject);
var
  lStr: string;
  isPy: boolean;
begin
  {$IFDEF MYPY}
  isPy := PyIsPythonScript();
  {$ELSE}
  isPy := false;
  {$ENDIF}
  lStr := (Sender as TMenuItem).Hint;
  if lStr <> '' then begin
          Memo2.Lines.Clear;
          Memo2.Lines.Add(lStr);
  end;
  lStr := (Sender as TMenuItem).Caption;
  CleanStr(lStr);
  if isPy then begin
     if IsPythonCompatible((Sender as TMenuItem).Tag) then
        lStr := 'gl.'+lStr+TypeStr((Sender as TMenuItem).Tag, isPy)
     else
       lStr := '#not yet Python Compatible: gl.'+lStr+TypeStr((Sender as TMenuItem).Tag, isPy)
  end else
      lStr := lStr+TypeStr((Sender as TMenuItem).Tag)+ ';';
  Clipboard.AsText := lStr;
  {$IFDEF UNIX}
  Memo1.SelText := (lStr)+ kUNIXeoln;
  {$ELSE}
  Memo1.SelText := (lStr)+ #13#10;
  {$ENDIF}
end;

procedure TScriptForm.ReportCaretPos;
var lPos : TPoint;
begin
  lPos := Memo1.CaretPos; //+1 as indexed from zero
  caption := ScriptForm.Hint +'  '+inttostr(lPos.Y+1)+':'+inttostr(lPos.X+1);
end;

procedure TScriptForm.Memo1Click(Sender: TObject);
begin
  inherited;
  ReportCaretPos;
end;

procedure TScriptForm.Memo1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    inherited;
    //{$IFNDEF LCLCocoa}
    ReportCaretPos;
    //{$ENDIF}
end;

procedure TScriptForm.Copy1Click(Sender: TObject);
begin
  if length(Memo1.SelText) < 1 then
    Memo1.SelectAll;
  //Clipboard.AsText := Memo1.SelText;
  Memo1.CopyToClipboard;
end;

procedure TScriptForm.Cut1Click(Sender: TObject);
begin
  if length(Memo1.SelText) < 1 then
    Memo1.SelectAll;
  Memo1.CutToClipboard;
end;

procedure TScriptForm.Paste1Click(Sender: TObject);
begin
  Memo1.PasteFromClipboard;
end;

initialization
{$IFDEF FPC}
 //   {$I scriptengine.lrs}
{$ENDIF}
end.
