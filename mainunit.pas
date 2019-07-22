unit mainunit;
 {$Include opts.inc} //optiosn: DGL, CoreGL or legacy GL
{$mode delphi}{$H+}
{$DEFINE MYPY}
interface
uses
  {$IFDEF DGL} dglOpenGL, {$ELSE DGL} {$IFDEF COREGL}glcorearb, {$ELSE} gl, {$ENDIF}  {$ENDIF DGL}
  fphttpclient, strutils,
  {$IFDEF MYPY}PythonEngine, {$ENDIF}
  //{$IFDEF SCRIPTING}
  //{$ENDIF}
  {$IFNDEF UNIX} shellapi, {$ELSE}  Process,  {$ENDIF}
  {$IFDEF COREGL} gl_core_3d, {$ELSE}     gl_legacy_3d, {$ENDIF}
  uPSComponent,Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  math, ExtCtrls, OpenGLContext, mesh, LCLintf, ComCtrls, Menus, graphtype,
  curv, ClipBrd, shaderui, shaderu, prefs, userdir, LCLtype, Grids, Spin,
  Buttons, matmath, colorTable, Track, types, glcube, glclrbar, define_types,
  meshify, zstream, gl_core_matrix, meshify_simplify, CheckLst;

type
  { TGLForm1 }
  TGLForm1 = class(TForm)
LayerList: TCheckListBox;
LayerDarkLabel: TLabel;
LayerDarkEdit: TEdit;
LayerBrightEdit: TEdit;
LayerBrightLabel: TLabel;
LayerColorDrop: TComboBox;
LayerAlphaLabel: TLabel;
LayerAlphaTrack: TTrackBar;
LayerOptionsBtn: TButton;
 LeftSplitter: TSplitter;
 CenterPanel: TPanel;
 LayerAOMapMenu: TMenuItem;
 LayerPainHideMenu: TMenuItem;
 PaintModeAutomatic: TMenuItem;
 PaintModeHideBrightHideDarkMenu: TMenuItem;
 PaintModeHideDarkShowBrightMenu: TMenuItem;
 PaintModeShowDarkHideBrightMenu: TMenuItem;
 PaintModeShowBirghtShowDark: TMenuItem;
 shadermatcap1: TMenuItem;
 NextOverlayMenu: TMenuItem;
 PrevOverlayMenu: TMenuItem;
 OverlaySep: TMenuItem;
 overlayload1: TMenuItem;
 overlayvisible1: TMenuItem;
  PSScript1: TPSScript;
  S3Label: TLabel;
  BGShader: TLabel;
  SaveScriptDialog: TSaveDialog;
 ScriptingInsertMenu: TMenuItem;
mesh1: TMenuItem;
meshload1: TMenuItem;
meshcolor1: TMenuItem;
meshcurv1: TMenuItem;
meshcreate1: TMenuItem;
meshreversefaces1: TMenuItem;
meshsave1: TMenuItem;
overlays1: TMenuItem;
overlayadditive1: TMenuItem;
overlaycloseall1: TMenuItem;
overlaycolorname1: TMenuItem;
overlayminmax1: TMenuItem;
overlaytransparencyonbackground1: TMenuItem;
overlaycolorfromzero1: TMenuItem;
overlaytranslucent1: TMenuItem;
overlayinvert1: TMenuItem;
overlaysmoothvoxelwisedata1: TMenuItem;
meshoverlayorder1: TMenuItem;
Nodes1: TMenuItem;
edgeload1: TMenuItem;
edgecolor1: TMenuItem;
edgecreate1: TMenuItem;
edgesize1: TMenuItem;
edgethresh1: TMenuItem;
nodeload1: TMenuItem;
nodecolor1: TMenuItem;
nodecreate1: TMenuItem;
nodehemisphere1: TMenuItem;
ndepolarity1: TMenuItem;
nodesize1: TMenuItem;
nodethresh1: TMenuItem;
nodethreshbysizenotcolor1: TMenuItem;
MatCapDrop: TComboBox;
Tracks1: TMenuItem;
trackload1: TMenuItem;
trackprefs1: TMenuItem;
Atlas1: TMenuItem;
atlasgray1: TMenuItem;
atlashide1: TMenuItem;
atlasmaxindex1: TMenuItem;
atlassaturationalpha1: TMenuItem;
atlasstatmap1: TMenuItem;
Dialogs1: TMenuItem;
modalmessage1: TMenuItem;
modelessmessage1: TMenuItem;
Shaders1: TMenuItem;
shaderadjust1: TMenuItem;
shaderambientocclusion1: TMenuItem;
shadername1: TMenuItem;
shaderlightazimuthelevation1: TMenuItem;
shaderxray1: TMenuItem;
MenuItem1: TMenuItem;
Render1: TMenuItem;
azimuth1: TMenuItem;
azimuthelevation1: TMenuItem;
backcolor1: TMenuItem;
cameradistance1: TMenuItem;
camerapan1: TMenuItem;
MenuItem2: TMenuItem;
colorbarvisible1: TMenuItem;
clip1: TMenuItem;
clipazimuthelevation1: TMenuItem;
elevation1: TMenuItem;
orientcubevisible1: TMenuItem;
viewaxial1: TMenuItem;
viewcoronal1: TMenuItem;
viewsagittal1: TMenuItem;
Advanced1: TMenuItem;
bmpzoom1: TMenuItem;
exists1: TMenuItem;
fontname1: TMenuItem;
savebmp1: TMenuItem;
savebmpxy1: TMenuItem;
scriptformvisible1: TMenuItem;
version1: TMenuItem;
quit1: TMenuItem;
Close1: TMenuItem;
resetdefaults1: TMenuItem;
wait1: TMenuItem;


 ScriptingPascalMenu: TMenuItem;
	ScriptPanel: TPanel;
	ScriptBox: TGroupBox;
	ScriptMemo: TMemo;
	ScriptOutputMemo: TMemo;
	ScriptSplitter: TSplitter;
	RightSplitter: TSplitter;
    AOLabel: TLabel;
    CurvMenu: TMenuItem;
    CurvMenuTemp: TMenuItem;
    ShaderForBackgroundOnlyCheck: TCheckBox;
    GoldColorMenu: TMenuItem;
    ConvertAtlas: TMenuItem;
    ColorBarMenu: TMenuItem;
    BlackClrbarMenu: TMenuItem;
    ColorbarSep: TMenuItem;
    meshAlphaTrack: TTrackBar;
    MeshBlendTrack: TTrackBar;
    ROImeshMenu: TMenuItem;
    XRayLabel: TLabel;
    TransBlackClrbarMenu: TMenuItem;
    ColorBarVisibleMenu: TMenuItem;
    WhiteClrbarMenu: TMenuItem;
    TransWhiteClrBarMenu: TMenuItem;
    NewWindow1: TMenuItem;
    S1Check: TCheckBox;
    S6Label: TLabel;
    S6Track: TTrackBar;
    S1Label: TLabel;
    S7Label: TLabel;
    S7Track: TTrackBar;
    S2Label: TLabel;
    S1Track: TTrackBar;
    RestrictSep2Menu: TMenuItem;
    RestrictHideNodesWithoutEdges: TMenuItem;
    S8Label: TLabel;
    S8Track: TTrackBar;
    S2Track: TTrackBar;
    S9Label: TLabel;
    S9Track: TTrackBar;
    S4Label: TLabel;
    S3Track: TTrackBar;
    S5Label: TLabel;
    S4Track: TTrackBar;
    S10Label: TLabel;
    S5Track: TTrackBar;
    S10Track: TTrackBar;
    TrackScalarRangeBtn: TButton;
    HelpMenu: TMenuItem;
    DisplaySepMenu: TMenuItem;
    AdvancedMenu: TMenuItem;
    AdditiveOverlayMenu: TMenuItem;
    CenterMeshMenu: TMenuItem;
    TrackScalarLUTdrop: TComboBox;
    TrackScalarNameDrop: TComboBox;
    SimplifyMeshMenu: TMenuItem;
    SimplifyTracksMenu: TMenuItem;
    TransparencySepMenu: TMenuItem;
    ReverseFacesMenu: TMenuItem;
    SwapYZMenu: TMenuItem;
    SaveMeshMenu: TMenuItem;
    VolumeToMeshMenu: TMenuItem;
    ResetMenu: TMenuItem;
    OrientCubeMenu: TMenuItem;
    Pref2Menu: TMenuItem;
    About2Menu: TMenuItem;
    EdgeSizeVariesCheck: TCheckBox;
    FileSepMenu: TMenuItem;
    occlusionTrack: TTrackBar;
    SaveTracksMenu: TMenuItem;
    NodeSizeVariesCheck: TCheckBox;
    PrefMenu: TMenuItem;
    NodeMaxEdit: TFloatSpinEdit;
    NodeMinEdit: TFloatSpinEdit;
    NodeThreshLabel: TLabel;
    NodeThreshDrop: TComboBox;
    EdgeSizeLabel: TLabel;
    NodeScaleTrack: TTrackBar;
    EdgeMinEdit: TFloatSpinEdit;
    EdgeMaxEdit: TFloatSpinEdit;
    LUTdropEdge: TComboBox;
    EdgeBox: TGroupBox;
    EdgeColorVariesCheck: TCheckBox;
    NodeScaleLabel: TLabel;
    EdgeThreshLabel: TLabel;
    edgeScaleTrack: TTrackBar;
    RestrictSepMenu: TMenuItem;
    RestrictAnyEdgeMenu: TMenuItem;
    RestrictPosEdgeMenu: TMenuItem;
    RestrictNegEdgeMenu: TMenuItem;
    RestrictRightMenu: TMenuItem;
    RestrictLeftMenu: TMenuItem;
    RestrictNoMenu: TMenuItem;
    RestrictMenu: TMenuItem;
    NodeColorVariesCheck: TCheckBox;
    GrayColorMenu: TMenuItem;
    BlueColorMenu: TMenuItem;
    GreenColorMenu: TMenuItem;
    ExitMenu: TMenuItem;
    DisplayMenu: TMenuItem;
    AnteriorMenu: TMenuItem;
    LeftMenu: TMenuItem;
    CloseMenu: TMenuItem;
    AddNodesMenu: TMenuItem;
    CloseNodesMenu: TMenuItem;
    LUTdropNode: TComboBox;
    SaveMeshDialog: TSaveDialog;
    NodeMenu: TMenuItem;
    RightMenu: TMenuItem;
    InferiorMenu: TMenuItem;
    SuperiorMenu: TMenuItem;
    PosteriorMenu: TMenuItem;
    RedColorItem: TMenuItem;
    QuickColorMenu: TMenuItem;
    NodeBox: TGroupBox;
    MeshColorBox: TGroupBox;
    SatLabel: TLabel;
    MeshSaturationTrack: TTrackBar;
    TrackWidthLabel: TLabel;
    TrackLengthTrack: TTrackBar;
    TrackLengthLabel: TLabel;
    LightAziTrack: TTrackBar;
    ClipAziTrack: TTrackBar;
    ClipBox: TGroupBox;
    ClipTrack: TTrackBar;
    ColorDialog1: TColorDialog;
    LightElevTrack: TTrackBar;
    ClipElevTrack: TTrackBar;
    TrackBox: TGroupBox;
    LightLabel: TLabel;
    DepthLabel: TLabel;
    AzimuthLabel: TLabel;
    ElevationLabel: TLabel;
    MainMenu1: TMainMenu;
    AppleMenu: TMenuItem;
    FileMenu: TMenuItem;
    ColorMenu: TMenuItem;
    BackColorMenu: TMenuItem;
    AboutMenu: TMenuItem;
    EditMenu: TMenuItem;
    CopyMenu: TMenuItem;
    Memo1: TMemo;
    AddOverlayMenu: TMenuItem;
    CloseOverlaysMenu: TMenuItem;
    AddTracksMenu: TMenuItem;
    CloseTracksMenu: TMenuItem;
    NodeColorLabel: TLabel;
    EdgeColorLabel: TLabel;
    TransLabel: TLabel;
    TrackDitherLabel: TLabel;
    TrackWidthTrack: TTrackBar;
    TracksMenu: TMenuItem;
    MeshTransparencyTrack: TTrackBar;
    TrackDitherTrack: TTrackBar;
    Transparency75: TMenuItem;
    Transparency25: TMenuItem;
    Transparency50: TMenuItem;
    Transparency0: TMenuItem;
    TransparencyMenu: TMenuItem;
    OverlaysMenu: TMenuItem;
    OpenDialog: TOpenDialog;
    OverlayBox: TGroupBox;
    ShaderBox: TGroupBox;
    ShaderDrop: TComboBox;
    ErrorTimer: TTimer;
    OverlayTimer: TTimer;
    UpdateTimer: TTimer;
    ToolPanel: TScrollBox;
    SaveBitmapDialog: TSaveDialog;
    SaveMenu: TMenuItem;
    ObjectColorMenu: TMenuItem;
    OpenMenu: TMenuItem;
    BackgroundBox: TGroupBox;
    ScriptingMenu: TMenuItem;
	ScriptingNewMenu: TMenuItem;
	ScriptingOpenMenu: TMenuItem;
	ScriptingTemplatesMenu: TMenuItem;
	ScriptingRunMenu: TMenuItem;
	ScriptingSaveMenu: TMenuItem;
ScriptOpenDialog: TOpenDialog;
LayerPopup: TPopupMenu;
LayerInvertColorsMenu: TMenuItem;
LayerShowHeaderMenu: TMenuItem;
procedure CenterPanelClick(Sender: TObject);
procedure LayerListClickCheck(Sender: TObject);
procedure LayerPopupPopup(Sender: TObject);
procedure LayerInvertColorsMenuClick(Sender: TObject);
procedure LayerShowHeaderMenuClick(Sender: TObject);
procedure LayerWidgetChange(Sender: TObject);
procedure LayerOptionsBtnClick(Sender: TObject);
procedure LayerContrastKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
procedure LayerListSelectionChange(Sender: TObject; User: boolean);
procedure LayerAlphaTrackMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LayerListShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure LeftSplitterCanOffset(Sender: TObject; var NewOffset: Integer;
      var Accept: Boolean);
    procedure LeftSplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure LeftSplitterChangeBounds(Sender: TObject);
    procedure LeftSplitterMoved(Sender: TObject);
    procedure LayerAOMapMenuClick(Sender: TObject);
    procedure MatCapDropChange(Sender: TObject);
    procedure PaintModeAutomaticMenu(Sender: TObject);
    procedure PaintModeMenuClick(Sender: TObject);
    procedure PrevOverlayMenuClick(Sender: TObject);
    procedure Shaders1Click(Sender: TObject);
    procedure UpdateLayerBox(NewLayers: boolean);

	procedure ScriptingNewMenuClick(Sender: TObject);
	procedure ScriptingOpenMenuClick(Sender: TObject);
	procedure ScriptingTemplatesMenuClick(Sender: TObject);
        procedure ScriptingPascalMenuClick(Sender: TObject);

	procedure ScriptingRunMenuClick(Sender: TObject);
	procedure ScriptingSaveMenuClick(Sender: TObject);
	procedure ScriptingGenerateTemplateMenu(isPython: boolean);
        procedure ScriptFormVisible(vis: boolean);
    procedure OpenScript(scriptname: string; isShowScriptPanel: boolean = true);

    procedure FormDestroy(Sender: TObject);
    procedure NodeThreshDropChange(Sender: TObject);
    procedure ROImeshMenuClick(Sender: TObject);
    function UpdateClrbar: integer;
    procedure ClrbarClr(i: integer);
    procedure UpdateFont(initialSetup: boolean);
    procedure ClrbarMenuClick(Sender: TObject);
    procedure ColorBarVisibleMenuClick(Sender: TObject);
    procedure SetColorBarPosition;
    procedure FormChangeBounds(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function GLBoxBackingWidth: integer;
    function GLBoxBackingHeight: integer;
    procedure GLboxDblClick(Sender: TObject);
    procedure CurvMenuClick(Sender: TObject);
    procedure DepthLabelDblClick(Sender: TObject);
    procedure NewWindow1Click(Sender: TObject);
    procedure Quit2TextEditor;
    procedure CenterMeshMenuClick(Sender: TObject);
    procedure AdditiveOverlayMenuClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure GLBoxClick(Sender: TObject);
    procedure MeshColorBoxChange(Sender: TObject);
    function OpenNode(FilenameIn: string): boolean;
    function OpenTrack(FilenameIn: string): boolean;
    function OpenOverlay(FilenameIn: string): boolean;
    function OpenEdge(FilenameIn: string): boolean;
    function OpenMesh(FilenameIn: string): boolean;
    procedure CheckForUpdates(Sender: TObject);
    procedure AboutMenuClick(Sender: TObject);
    procedure AddNodesMenuClick(Sender: TObject);
    procedure AddOverlayMenuClick(Sender: TObject);
    procedure AddTracksMenuClick(Sender: TObject);
    procedure AzimuthLabelClick(Sender: TObject);
    procedure BackColorMenuClick(Sender: TObject);
    procedure ClipTrackChange(Sender: TObject);
    procedure CloseMenuClick(Sender: TObject);
    procedure CloseNodesMenuClick(Sender: TObject);
    procedure CloseOverlaysMenuClick(Sender: TObject);
    procedure CloseTracksMenuClick(Sender: TObject);
    procedure CopyMenuClick(Sender: TObject);
    procedure DepthLabelClick(Sender: TObject);
    procedure DisplayMenuClick(Sender: TObject);
    procedure ElevationLabelClick(Sender: TObject);
    procedure ErrorTimerTimer(Sender: TObject);
    procedure GLBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure UpdateLUT(lOverlay,lLUTIndex: integer);
    procedure NodePrefChange(Sender: TObject);
    procedure OrientCubeMenuClick(Sender: TObject);
    procedure OverlayTimerStart;
    procedure AdjustFormPos (var lForm: TForm);
    procedure OverlayBoxCreate;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure AppDropFiles(Sender: TObject; const FileNames: array of String);
    procedure CreateRender(w,h: integer; isToScreen: boolean);
    procedure GLboxPaint(Sender: TObject);
    procedure GLboxMouseMove(Sender: TObject; Shift: TShiftState; lX, lY: Integer);
    procedure GLboxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; lX, lY: Integer);
    procedure GLboxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; lX, lY: Integer);
    procedure ObjectColorMenuClick(Sender: TObject);
    procedure OpenMenuClick(Sender: TObject);
    procedure OverlayTimerTimer(Sender: TObject);
    procedure OverlayVisible(lOverlay: integer; lVisible: integer);
    procedure OverlayInvert(lOverlay: integer; lInvert: boolean);
    procedure PrefMenuClick(Sender: TObject);
    {$IFDEF LCLCocoa}
    procedure SetRetina;
    procedure SetDarkMode;
    {$ENDIF}
    procedure QuickColorClick(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure ResetMenuClick(Sender: TObject);
    procedure RestrictEdgeMenuClick(Sender: TObject);
    procedure RestrictHideNodesWithoutEdgesClick(Sender: TObject);
    procedure RestrictMenuClick(Sender: TObject);
    procedure ReverseFacesMenuClick(Sender: TObject);
    procedure SaveBitmap(FilenameIn: string); overload;
    procedure SaveBitmap(FilenameIn: string; lX, lY: integer); overload;
    procedure SaveMenuClick(Sender: TObject);
    //procedure SaveMz3(var mesh: TMesh; isSaveOverlays: boolean);
    procedure SaveTrack (var lTrack: TTrack);
    function SaveMeshCore(lFilename: string): boolean;
    procedure SaveMesh(var mesh: TMesh; isSaveOverlays: boolean);
    procedure SaveMeshMenuClick(Sender: TObject);
    procedure SaveTracksMenuClick(Sender: TObject);
    procedure ScalarDropChange(Sender: TObject);
    function ScreenShot(lForceRedraw: boolean = false): TBitmap;
    function ScreenShotX1: TBitmap;
    procedure ScriptPanelDblClick(Sender: TObject);
    procedure SetOverlayTransparency(Sender: TObject);
    procedure ShaderBoxResize(Sender: TObject);
    procedure ShaderDropChange(Sender: TObject);
    procedure ShowmessageError(s: string);
    procedure GLboxRequestUpdate(Sender: TObject);
    procedure SimplifyMeshMenuClick(Sender: TObject);
    procedure SimplifyTracksMenuClick(Sender: TObject);
    procedure SurfaceAppearanceChange(Sender: TObject);
    procedure SwapYZMenuClick(Sender: TObject);
    procedure TrackBoxChange(Sender: TObject);
    procedure TrackScalarRangeBtnClick(Sender: TObject);
   procedure UniformChange(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure UpdateImageIntensity;
    function ComboBoxName2Index(var lCombo: TComboBox; lName: string): integer;
    procedure SetDistance(Distance: single);
    procedure OVERLAYMINMAX (lOverlay: integer; lMin,lMax: single);
    procedure OVERLAYCOLORNAME(lOverlay: integer; lFilename: string);
    //procedure SetOrtho (w,h: integer; isMultiSample: boolean);
    procedure AddMRU(lFilename: string);
    procedure UpdateMRU;
    procedure CreateMRU;
    procedure OpenMRU(Sender: TObject);//open template or MRU
    procedure UpdateToolbar;
    procedure MultiPassRenderingToolsUpdate;
    procedure VolumeToMeshMenuClick(Sender: TObject);
    procedure ShaderForBackgroundOnlyClick(Sender: TObject);
    procedure GLInvalidate;
    procedure InsertCommand(Sender: TObject);
    procedure CompileMainClick(Sender: TObject);
    procedure PyIOSendData(Sender: TObject; const Data: AnsiString);
    procedure PyIOSendUniData(Sender: TObject; const Data: UnicodeString);

    function PyIsPythonScriptMain(): boolean;
    function PyExecMain(): boolean;
    function PyCreate: boolean;
    procedure PyModInitialization(Sender: TObject);
    procedure PSScript1Compile(Sender: TPSScript);

    private
    { private declarations }
  public
    { public declarations }
  end;

var
  GLForm1: TGLForm1;
  gCube : TGLCube;
  gClrbar: TGLClrbar;
  gPrefs : TPrefs;
  gElevation : integer =20;
  gAzimuth : integer = 250;
  gMesh: TMesh;
implementation
//{$IFDEF COREGL}
{$IFDEF LCLcarbon}
 This program does not support Carbon
  Please choose Project/ProjectOptions, go to the CompilerOptions/Additions&Overrides and set the BuildMode pull-down to "MacOS"
{$ENDIF}
//{$ENDIF}

{$R *.lfm}
{$IFDEF LCLCocoa}
uses
  commandsu,UserNotification, nsappkitext, glcocoanscontext;
{$ELSE}
uses
  commandsu;

{$ENDIF}
var
  PythonIO : TPythonInputOutput;
  PyMod: TPythonModule;
  PyEngine: TPythonEngine = nil;

  gNode: TMesh;
  gTrack: TTrack;
  gnLUT: integer = 0;
  isBusy: boolean = true;
  {$IFDEF Darwin}gRetinaScale : single = 1;{$ENDIF}
  gDistance : single = 1;
  gMouseX : integer = -1;
  gMouseY : integer = -1;
  GLerror : string = '';
  clipPlane : TPoint4f; //clipping bottom
  GLbox: TOpenGLControl;
const
  kFname=0;
  kLUT=1;
  kMin=2;
  kMax=3;
  kTrackFilter = 'Camino, VTK, MRTrix, Quench, TrakVis, DTIstudio|*.Bfloat;*.Bfloat.gz;*.trk.gz;*.trk;*.tck;*.pdb;*.fib;*.vtk;*.dat|Any file|*.*';

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

procedure MyWriteln(const s: string);
begin
  GLForm1.ScriptOutputMemo.lines.add(S);
  {$IFDEF Unix}writeln(s);{$ENDIF}
end;

procedure TGLForm1.PSScript1Compile(Sender: TPSScript);
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

procedure TGLForm1.InsertCommand(Sender: TObject);
var
  lStr: string;
  isPy: boolean;
begin
  {$IFDEF MYPY}
  isPy := PyIsPythonScriptMain();
  {$ELSE}
  isPy := false;
  {$ENDIF}
  lStr := (Sender as TMenuItem).Hint;
  if lStr <> '' then begin
          ScriptOutputMemo.Lines.Clear;
          ScriptOutputMemo.Lines.Add(lStr);
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
  ScriptMemo.SelText := (lStr)+ kUNIXeoln;
  {$ELSE}
  ScriptMemo.SelText := (lStr)+ #13#10;
  {$ENDIF}
end;

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


function searchPy(pth: string): string;
var
   searchResult : TSearchRec;
begin
    result := '';
    {$IFDEF Darwin}
    if FindFirst(IncludeTrailingPathDelimiter(pth)+'libpython*.dylib', faDirectory, searchResult) = 0 then
    {$ELSE}
    if FindFirst(IncludeTrailingPathDelimiter(pth)+'libpython*.so', faDirectory, searchResult) = 0 then
    {$ENDIF}
    result := IncludeTrailingPathDelimiter(pth)+(searchResult.Name);
    FindClose(searchResult);
end;
{$IFDEF Darwin}
  const
       kBasePath = '/Library/Frameworks/Python.framework/Versions/';
{$ENDIF}

  {$IFDEF UNIX}
  function InitPyLibraryPath: string;
    //
    function GetMacPath(NMinorVersion: integer): string;
    begin
      Result:= Format('/Library/Frameworks/Python.framework/Versions/3.%d/lib/libpython3.%d.dylib',
        [NMinorVersion, NMinorVersion]);
    end;
    //
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
               (*{$IFDEF Darwin}
               if FindFirst(IncludeTrailingPathDelimiter(def)+'libpython*.dylib', faDirectory, searchResult) = 0 then
               {$ELSE}
               if FindFirst(IncludeTrailingPathDelimiter(def)+'libpython*.so', faDirectory, searchResult) = 0 then
               {$ENDIF}
                  result := IncludeTrailingPathDelimiter(def)+(searchResult.Name);
               FindClose(searchResult);  *)
               if length(result) > 0 then exit;
             end;
             {$IFDEF LCLCocoa}
             result := searchPy('/System/Library/Frameworks/Python.framework/Versions/Current/lib');
             if fileexists(result) then exit;
             {$ENDIF}
             //if fileexists(def) then exit;
             result := InitPyLibraryPath;
             if fileexists(result) then exit;
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
  {$ENDIF}

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

function PySHADERMATCAP(Self, Args : PPyObject): PPyObject; cdecl;
var
  PtrName: PChar;
  StrName: string;
  V: integer;
begin
  Result:= GetPythonEngine.PyBool_FromLong(Ord(FALSE));
  with GetPythonEngine do
    if Bool(PyArg_ParseTuple(Args, 's:shadermatcap', @PtrName)) then
    begin
      StrName:= string(PtrName);
      SHADERMATCAP(StrName);
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

(*function pyAbort(Self, Args : PPyObject): PPyObject; cdecl;
begin
  Result := nil;
  Abort;
end;*)


procedure TGLForm1.PyModInitialization(Sender: TObject);
begin
  with Sender as TPythonModule do begin
    //AddMethod('atlasgraybg', @PyATLASGRAYBG, '');
    AddMethod('atlasmaxindex', @PyATLASMAXINDEX, ' atlasmaxindex(overlayNum) -> Returns maximum region humber in specified atlas. For example, if you load the CIT168 atlas (which has 15 regions) as your background image, then atlasmaxindex(0) will return 15.');
    AddMethod('atlassaturationalpha', @PyATLASSATURATIONALPHA, ' atlassaturationalpha(saturation, transparency) -> Set saturation and transparency of atlas. A desaturated atlas will appear gray, a transparent atlas will reveal the background color.');
    AddMethod('azimuth', @PyAZIMUTH, ' azimuthe(azi) -> Rotate image by specified degrees.');
    AddMethod('azimuthelevation', @PyAZIMUTHELEVATION, ' azimuthelevation(azi, elev) -> Sets the camera location.');
    AddMethod('backcolor', @PyBACKCOLOR, ' backcolor(r, g, b) -> changes the background color, for example backcolor(255, 0, 0) will set a bright red background');
    AddMethod('bmpzoom', @PyBMPZOOM, ' bmpzoom(z) -> changes resolution of savebmp(), for example bmpzoom(2) will save bitmaps at twice screen resolution');
    AddMethod('cameradistance', @PyCAMERADISTANCE, ' cameradistance(z) -> Sets the viewing distance from the object.');
    AddMethod('camerapan', @PyCAMERAPAN, ' camerapan(x, y) -> Translate image horizontally (x) and vertically (y). range -1..+1, where 0 is centered.');
    AddMethod('clip', @PyCLIP, ' clip(depth) -> Creates a clip plane that hides information close to the viewer.');
    AddMethod('clipazimuthelevation', @PyCLIPAZIMUTHELEVATION, ' clipazimuthelevation(depth, azi, elev) -> Set a view-point independent clip plane.');
    AddMethod('colorbarposition', @PyCOLORBARPOSITION, ' colorbarposition(p) -> Set colorbar position (1=bottom, 2=left, 3=top, 4=right).');
    AddMethod('colorbarvisible', @PyCOLORBARVISIBLE, ' colorbarvisible(v) -> Show (1) or hide (0) the color bar.');
    AddMethod('edgecolor', @PyEDGECOLOR, ' edgecolor(name, varies) -> Select color scheme for connectome edge map. If varies=1 then edge color depends on strength of connection.');
    AddMethod('edgeload', @PyEDGELOAD, ' edgeload(filename) -> Loads a BrainNet Viewer format Edge file, e.g. connectome map.');
    AddMethod('edgesize', @PyEDGESIZE, ' edgesize (size, varies) -> Set the diameters of the cylinders of the connectome. If varies=1 then edge diameter depends on strength of connection.');
    AddMethod('edgethresh', @PyEDGETHRESH, ' edgethresh (lo, hi) -> Set minimum and maximum values for connectome edge diameters.');
    AddMethod('elevation', @PyELEVATION, ' elevation(degrees) -> Rotates volume rendering relative to camera.');
    AddMethod('exists', @PyEXISTS, ' exists(filename) -> Returns true if filename is found.');
    AddMethod('fontname', @PyFONTNAME, ' fontname(name) -> Set typeface for display.');
    AddMethod('meshcolor', @PyMESHCOLOR, ' meshcolor(r, g, b) -> Set red/green/blue components of main image. Each component is an integer 0..255.');
    AddMethod('meshcreate', @PyMESHCREATE, ' meshcreate(niiname, meshname, threshold, decimateFrac, minimumClusterVox, smoothStyle) -> Convert a NIfTI voxel-based image into a mesh.');
    AddMethod('meshcurv', @PyMESHCURV, ' meshcurv() -> Displays mesh curvature, so crevices appear dark.');
    AddMethod('meshload', @PyMESHLOAD, ' meshload(imageName) -> Close all open images and load new background image.');
    AddMethod('meshoverlayorder', @PyMESHOVERLAYORDER, ' meshoverlayorder (flip) -> If flip=1, the mesh will be drawn after the overlay, and xray sliders will influence overlay not mesh.');
    AddMethod('meshreversefaces', @PyMESHREVERSEFACES, ' meshreversefaces() -> reverse triangle winding to reverse front/back faces.');
    AddMethod('meshsave', @PyMESHSAVE, ' meshsave(filename) -> Saves currently open mesh to disk.');
    AddMethod('modalmessage', @PyMODALMESSAGE, ' modalmessage(msg) -> Shows a modal dialog, script stops until user presses ''OK'' button to dismiss dialog.');
    AddMethod('modelessmessage', @PyMODELESSMESSAGE, ' modelessmessage(msg) -> Prints text in the bottom status region of the scripting window.');
    AddMethod('nodecolor', @PyNODECOLOR, ' nodecolor(name, varies) -> set colorscheme used for nodes. If varies=1, the color of nodes will differ depending on size or intensity.');
    AddMethod('nodehemisphere', @PyNODEHEMISPHERE, ' nodehemisphere (val) -> Set -1 for left hemipshere, 0 for both, 1 for right');
    AddMethod('nodeload', @PyNODELOAD, ' nodeload(filename) -> Loads BrainNet viewer format node file.');
    AddMethod('nodepolarity', @PyNODEPOLARITY, ' nodepolarity(val) -> Set -1 for negative only, 0 for either, 1 for positive only.');
    AddMethod('nodesize', @PyNODESIZE, ' nodesize(size, varies) -> Determine size scaling factor for nodes.');
    AddMethod('nodethresh', @PyNODETHRESH, ' nodethresh(lo, hi) -> Set the minimum and maximum range for nodes.');
    AddMethod('nodethreshbysizenotcolor', @PyNODETHRESHBYSIZENOTCOLOR, ' nodethreshbysizenotcolor(NodeThresholdBySize) -> If true (1) then nodes will be hidden if they are smaller than the provided threshold. If false (0), they will be hidden if their color intensity is below the provided threshold.');
    AddMethod('orientcubevisible', @PyORIENTCUBEVISIBLE, ' orientcubevisible (visible) -> Show (1) or hide (0) cube that indicates object rotation');
    AddMethod('overlayadditive', @PyOVERLAYADDITIVE, ' overlayadditive (add) -> Determines whether overlay colors are combined by adding or mixing the colors. For example, overlap of red and green overlays will appear yellow if additive is true (1)');
    AddMethod('overlaycloseall', @PyOVERLAYCLOSEALL, ' overlaycloseall() -> Close all open overlays.');
    AddMethod('overlaycolorname', @PyOVERLAYCOLORNAME, ' overlaycolorname(overlayLayer, filename) -> Set the colorscheme for the target overlay to a specified name.');
    AddMethod('overlayinvert', @PyOVERLAYINVERT, ' overlayinvert(overlaLayer, invert) -> Toggle whether overlay color scheme is inverted.');
    AddMethod('overlayload', @PyOVERLAYLOAD, ' overlayload(filename) -> Load an image on top of prior images.');
    AddMethod('overlayminmax', @PyOVERLAYMINMAX, ' overlayminmax(layer, min, max) -> Sets the color range for the overlay (layer 0 = background).');
    AddMethod('overlaysmoothvoxelwisedata', @PyOVERLAYSMOOTHVOXELWISEDATA, ' overlaysmoothvoxelwisedata(smooth) -> Determines if overlays are loaded using interpolation (smooth, 1) or nearest neighbor (un-smoothed, 0) interpolation.');
    AddMethod('overlaytranslucent', @PyOVERLAYTRANSLUCENT, ' overlaytranslucent(overlayLayer, translucent) -> This feature allows you to make individual overlays translucent or opaque.');
    AddMethod('overlaytransparencyonbackground', @PyOVERLAYTRANSPARENCYONBACKGROUND, ' overlaytransparencyonbackground(percent) -> Controls the opacity of the overlays on the background.');
    AddMethod('overlayvisible', @PyOVERLAYVISIBLE, ' overlayvisible(overlayLayer, visible) -> This feature allows you to make individual overlays visible or invisible.');
    AddMethod('quit', @PyQUIT, ' quit() -> Terminate the application.');
    AddMethod('resetdefaults', @PyRESETDEFAULTS, ' resetdefaults() -> Revert settings to sensible values.');
    AddMethod('savebmp', @PySAVEBMP, ' savebmp(pngName) -> Save screen display as bitmap. For example "savebmp(''test.png'')"');
    AddMethod('savebmpxy', @PySAVEBMPXY, ' savebmpxy(pngName, x, y) -> Saves the currently viewed image as a PNG bitmap image. Specify the image width (x) and height (y).');
    AddMethod('scriptformvisible', @PySCRIPTFORMVISIBLE, ' scriptformvisible (visible) -> Show (1) or hide (0) the scripting window.');
    AddMethod('shaderadjust', @PySHADERADJUST, ' shaderadjust(sliderName, sliderValue) -> Set level of shader property. Example "gl.shaderadjust(''Diffuse'', 0.6)"');
    AddMethod('shaderambientocclusion', @PySHADERAMBIENTOCCLUSION, ' shaderambientocclusion(amount) -> Specify a value in the range 0..1 to set the strength of the crevice shadows');
    AddMethod('shaderforbackgroundonly', @PySHADERFORBACKGROUNDONLY, ' shaderforbackgroundonly(onlybg) -> If true (1) selected shader only influeces background image, otherwise shader influences background, overlays, tracks and nodes.');
    AddMethod('shaderlightazimuthelevation', @PySHADERLIGHTAZIMUTHELEVATION, ' shaderlightazimuthelevation (azimuth, elevation) -> Changes location of light source.');
    AddMethod('shadermatcap', @PySHADERMATCAP, ' shadermatcap(name) -> Set material capture file (assumes "matcap" shader. For example, "shadermatcap(''mc01'')" selects mc01 matcap.');
    AddMethod('shadername', @PySHADERNAME, ' shadername(name) -> Choose rendering shader function. For example, "shadername(''phong'')" renders using Phong shading.');
    AddMethod('shaderxray', @PySHADERXRAY, ' shaderxray (object, overlay) -> See occluded overlays/tracks/nodes by making either object transparent (0..1) or overlay/tracks/nodes emphasized (0..1)');
    AddMethod('trackload', @PyTRACKLOAD, ' trackload (filename) -> Load fiber steam lines from a file.');
    AddMethod('trackprefs', @PyTRACKPREFS, ' trackprefs(length, width, dither) -> Set the size and properties for streamlines.');
    AddMethod('version', @PyVERSION, ' version() -> Return the version of Surfice.');
    AddMethod('viewaxial', @PyVIEWAXIAL, ' viewaxial(SI) -> Show rendering with camera superior (1) or inferior (0) of volume.');
    AddMethod('viewcoronal', @PyVIEWCORONAL, ' viewcoronal(AP) -> Show rendering with camera posterior (1) or anterior (0) of volume.');
    AddMethod('viewsagittal', @PyVIEWSAGITTAL, ' viewsagittal(LR) -> Show rendering with camera left (1) or right (0) of volume.');
    AddMethod('wait', @PyWAIT, ' wait(ms) -> Pause script for (at least) the desired milliseconds.');
  end;
end;


procedure TGLForm1.PyIOSendData(Sender: TObject;
  const Data: AnsiString);
begin
  ScriptOutputMemo.Lines.Add(Data);
end;

procedure TGLForm1.PyIOSendUniData(Sender: TObject;
  const Data: UnicodeString);
begin
  ScriptOutputMemo.Lines.Add(Data);
end;
function TGLForm1.PyCreate: boolean;
//const
// cPyLibraryMac = '/Library/Frameworks/Python.framework/Versions/2.7/lib/libpython2.7.dylib';
var
  S: string;
begin
  result := false;
  if FileExists(gPrefs.PyLib) then begin
     {$IFDEF UNIX}writeln('Using PyLib from preferences "'+gPrefs.PyLib+'"');{$ENDIF}
     S := gPrefs.PyLib;
  end else
      S:= findPythonLib(gPrefs.PyLib);
  if (S = '') then exit;
  gPrefs.PyLib := S;
  result := true;
  PythonIO := TPythonInputOutput.Create(GLForm1);
  PyMod := TPythonModule.Create(GLForm1);
  PyEngine := TPythonEngine.Create(GLForm1);
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


function TGLForm1.PyIsPythonScriptMain(): boolean;
begin
  result := ( Pos('import gl', GLForm1.ScriptMemo.Lines.Text) > 0); //any python project must import gl
end;

function TGLForm1.PyExecMain(): boolean;
begin
  result := false; //assume code is not Python
  if not (PyIsPythonScriptMain) then exit;
  GLForm1.ScriptOutputMemo.lines.Clear;
  result := true;
  if PyEngine = nil then begin
    if not PyCreate then begin //do this the first time
      {$IFDEF Windows}
      GLForm1.ScriptOutputMemo.lines.Add('Unable to find Python library [place Python .dll and .zip in Script folder]');
      {$ENDIF}
      {$IFDEF Unix}
      GLForm1.ScriptOutputMemo.lines.Add('Unable to find Python library');
      {$IFDEF Darwin}
      GLForm1.ScriptOutputMemo.lines.Add('   For MacOS this is typically in: '+kBasePath+'');
      {$ELSE}
      GLForm1.ScriptOutputMemo.lines.Add('   run ''find -name "*libpython*"'' to find the library');
      GLForm1.ScriptOutputMemo.lines.Add('   if it does not exist, install it (e.g. ''apt-get install libpython2.7'')');
      {$ENDIF}
      GLForm1.ScriptOutputMemo.lines.Add('   if it does exist, set use the Preferences/Advanced to set ''PyLib''');
      {$IFDEF Darwin}
      GLForm1.ScriptOutputMemo.lines.Add('   PyLib should be the complete path and filename of libpython*.dylib');
      {$ELSE}
      GLForm1.ScriptOutputMemo.lines.Add('   PyLib should be the complete path and filename of libpython*.so');
      {$ENDIF}
      GLForm1.ScriptOutputMemo.lines.Add('   This file should be in your LIBDIR, which you can detect by running Python from the terminal:');
      GLForm1.ScriptOutputMemo.lines.Add('     ''import sysconfig; print(sysconfig.get_config_var("LIBDIR"))''');
      {$ENDIF}
      result := true;
      exit;

    end;
  end;
  GLForm1.ScriptOutputMemo.lines.Add('Running Python script');
  try
  PyEngine.ExecStrings(GLForm1.ScriptMemo.Lines);
  except
    caption := 'Python Engine Failed';
  end;
  GLForm1.ScriptOutputMemo.lines.Add('Python Succesfully Executed');
  result := true;
  ToolPanel.refresh;

  ToolPanel.refresh;
end;


procedure TGLForm1.CompileMainClick(Sender: TObject);
var
  i: integer;
  compiled: boolean;
begin
  {$IFDEF MYPY}
  if PyExecMain() then exit;
  if (not (AnsiContainsText(GLForm1.ScriptMemo.Lines.Text, 'begin'))) then begin
      GLForm1.ScriptOutputMemo.Lines.Clear;
      GLForm1.ScriptOutputMemo.Lines.Add('Error: script must contain "import gl" (for Python) or "begin" (for Pascal).');
      exit;
  end;
  {$ENDIF}
  GLForm1.ScriptOutputMemo.Lines.Clear;
  PSScript1.Script.Text := GLForm1.ScriptMemo.Lines.Text;
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
  GLForm1.Refresh;
  ToolPanel.refresh;
end;

procedure TGLForm1.ScriptingGenerateTemplateMenu(isPython: boolean);
var
 i: integer;
 scriptPath, scriptName: string;
 scriptNames : TStringList;
 newMenu: TMenuItem;
 begin
  //auto generate template script
  scriptPath := ScriptDir;
  if not DirectoryExists(scriptPath) then showmessage('Unable to find scripts "'+scriptPath+'"');
  if isPython then
     scriptNames := FindAllFiles(scriptPath, '*.py', false)
  else
      scriptNames := FindAllFiles(scriptPath, '*.gls', false);
  //showmessage(inttostr(scriptNames.Count));
  if scriptNames.Count > 0 then begin
     scriptNames.Sort;
     for i := 0 to (scriptNames.Count-1) do begin
         scriptName := ChangeFileExt(ExtractFileName(scriptNames[i]),'');
         if (length(scriptName) < 1) or (scriptName[1] = '_') or (scriptName[1] = '.') then
            continue;
         newMenu := TMenuItem.Create(MainMenu1);
         newMenu.Caption := scriptName;
         //newMenu.AutoCheck := true;
         //newMenu.RadioItem := true;
         if isPython then begin
            newMenu.OnClick := ScriptingTemplatesMenuClick;
            //newMenu.GroupIndex := 132;
            ScriptingTemplatesMenu.Add(newMenu)
         end else begin
            newMenu.OnClick := ScriptingPascalMenuClick;
            //newMenu.GroupIndex := 133;
            ScriptingPascalMenu.Add(newMenu);
         end;
     end;
  end;
  scriptNames.Free;
end;

procedure TGLForm1.ScriptFormVisible(vis: boolean);
begin
  if (vis) and (GLForm1.ScriptPanel.Width < GLForm1.ToolPanel.Constraints.MaxWidth) then
     GLForm1.ScriptPanel.Width := GLForm1.ToolPanel.Constraints.MaxWidth
  else if (not vis) then
      GLForm1.ScriptPanel.width := 0;
  //{$IFDEF METALAPI}
  //ViewGPU1.Invalidate;
  //{$ENDIF}
end;

procedure TGLForm1.ScriptPanelDblClick(Sender: TObject);
begin
  ScriptPanel.Width := 4;
end;

procedure TGLForm1.ScriptingNewMenuClick(Sender: TObject);
begin
  ScriptFormVisible(true);
  ScriptMemo.Lines.Clear;
  ScriptMemo.Lines.Add('import gl');
  ScriptMemo.Lines.Add('gl.resetdefaults()');
end;

procedure TGLForm1.OpenScript(scriptname: string; isShowScriptPanel: boolean = true);
begin
     if not fileexists(scriptname) then exit;
     if (ScriptPanel.Width < 24) and (isShowScriptPanel) then
        ScriptPanel.Width := 240;
     gPrefs.PrevScript :=  scriptname;
     ScriptMemo.Lines.LoadFromFile(scriptname);
     gPrefs.InitScript:='';
     ScriptingRunMenuClick(nil);
end;

procedure TGLForm1.ScriptingOpenMenuClick(Sender: TObject);
begin
  if not ScriptOpenDialog.execute then exit;
  OpenScript(ScriptOpenDialog.Filename);
end;

procedure TGLForm1.ScriptingTemplatesMenuClick(Sender: TObject);
var
 scriptName: string;
begin
 //shaderName := ResourceDir+pathdelim+'script' + pathdelim + (Sender as TMenuItem).caption+'.py';
 scriptName := ScriptDir + pathdelim + (Sender as TMenuItem).caption+'.py';
 if not fileexists(scriptName) then
    showmessage('Unable to find '+scriptName);
 OpenScript(scriptName);
end;

procedure TGLForm1.ScriptingPascalMenuClick(Sender: TObject);
var
 scriptName: string;
begin
 //shaderName := ResourceDir+pathdelim+'script' + pathdelim + (Sender as TMenuItem).caption+'.py';
 scriptName := ScriptDir + pathdelim + (Sender as TMenuItem).caption+'.gls';
 if not fileexists(scriptName) then
    showmessage('Unable to find '+scriptName);
 OpenScript(scriptName);
end;
procedure TGLForm1.ScriptingRunMenuClick(Sender: TObject);
begin
  CompileMainClick(Sender);// PyExecMain();
end;

procedure TGLForm1.ScriptingSaveMenuClick(Sender: TObject);
begin
 SaveScriptDialog.InitialDir:= ScriptDir;
 if PyIsPythonScriptMain() then
     SaveScriptDialog.DefaultExt := '.py'
  else
    SaveScriptDialog.DefaultExt := '.gls';
 if PyIsPythonScriptMain() then
     SaveScriptDialog.Filter := 'Python script|*.py'
  else
    SaveScriptDialog.Filter := 'Pascal script|*.gls';
  SaveScriptDialog.FileName := '';
  if not SaveScriptDialog.Execute then
    exit;
  ScriptMemo.Lines.SaveToFile(SaveScriptDialog.Filename);
end;

procedure TGLForm1.UpdateLUT(lOverlay,lLUTIndex: integer);
begin
  if (gMesh.OpenOverlays > kMaxOverlays)  then
    exit;
  if lLUTIndex >= LayerColorDrop.Items.Count then
    gMesh.Overlay[lOverlay].LUTindex:= 0
  else
    gMesh.Overlay[lOverlay].LUTindex:= lLUTIndex;
  gMesh.overlay[lOverlay].LUT := UpdateTransferFunction (gMesh.Overlay[lOverlay].LUTindex, gMesh.Overlay[lOverlay].LUTinvert);
  //LUTdropLoad(gMesh.Overlay[lOverlay].LUTindex, gMesh.Overlay[lOverlay].LUT, LUTdrop.Items[lLUTindex], gOverlayCLUTrec[lOverlay]);
end;

procedure TGLForm1.LayerInvertColorsMenuClick(Sender: TObject);
var
   i: integer;
   //s: string;
   //mn, mx: single;
begin
  i := LayerList.ItemIndex+ 1;
  if (i < 1) or (i > gMesh.OpenOverlays) then exit;
  OverlayInvert(i, not gMesh.Overlay[i].LUTinvert);
  gnLUT := -1;
end;

procedure TGLForm1.LayerPopupPopup(Sender: TObject);
var
   i: integer;
begin
  i := LayerList.ItemIndex+ 1;
  if (i < 1) or (i > gMesh.OpenOverlays) then exit;
   LayerInvertColorsMenu.Checked := gMesh.Overlay[i].LUTinvert;
   LayerAOMapMenu.Checked := gMesh.Overlay[i].aoMap;
   LayerPainHideMenu.Enabled :=  (not gMesh.Overlay[i].aoMap) and (length(gMesh.Overlay[i].intensity) > 0);
   if not LayerPainHideMenu.Enabled  then exit;
   case gMesh.Overlay[i].PaintMode of
        //kPaintHideDefaultBehavior = -1;
        kPaintHideDarkHideBright: PaintModeHideBrightHideDarkMenu.Checked := true;
        kPaintHideDarkShowBright: PaintModeHideDarkShowBrightMenu.Checked := true;
        kPaintShowDarkHideBright: PaintModeShowDarkHideBrightMenu.Checked := true;
        kPaintShowDarkShowBright: PaintModeShowBirghtShowDark.Checked := true;
        else PaintModeAutomatic.Checked := true;

   end;
end;

procedure TGLForm1.LayerListClickCheck(Sender: TObject);
begin
  UpdateLayerBox(false);
  if LayerList.Checked[LayerList.ItemIndex] then
     LayerAlphaTrack.Position := 100
  else
      LayerAlphaTrack.Position := 0;
  LayerWidgetChange(sender);
end;

procedure TGLForm1.CenterPanelClick(Sender: TObject);
begin

end;

procedure TGLForm1.LayerShowHeaderMenuClick(Sender: TObject);
var
   i: integer;
   s: string;
begin
  i := LayerList.ItemIndex+ 1;
  if (i < 1) or (i > gMesh.OpenOverlays) then exit;
  s :='';
  if (gMesh.Overlay[i].atlasMaxIndex > 0) then
     s := 'Atlas';
  showmessage(format('%s Layer %d, range %.6g..%.6g, Name: %s', [s, i, gMesh.Overlay[i].minIntensity, gMesh.Overlay[i].maxIntensity, gMesh.Overlay[i].Filename]));
end;

procedure TGLForm1.LayerWidgetChange(Sender: TObject);
var
  i: integer;
  //mn,mx: single;
  //lutName: string;
  //isChange: boolean = false;
begin
 i := LayerList.ItemIndex+ 1;
 if (i < 1) or (i > gMesh.OpenOverlays) then exit;
 gMesh.Overlay[i].WindowScaledMin := strtofloatdef(LayerDarkEdit.Caption, gMesh.Overlay[i].WindowScaledMin);
 gMesh.Overlay[i].WindowScaledMax := strtofloatdef(LayerBrightEdit.Caption, gMesh.Overlay[i].WindowScaledMax);
 gMesh.Overlay[i].OpacityPercent := LayerAlphaTrack.position;
 PrevOverlayMenu.Enabled := (gMesh.Overlay[i].volumes > 1);
 NextOverlayMenu.Enabled := (gMesh.Overlay[i].volumes > 1);
 if (gMesh.Overlay[i].LUTindex <> LayerColorDrop.ItemIndex) then begin
   //gMesh.Overlay[i].LUTindex := LayerColorDrop.ItemIndex;
   //UpdateLUT(intRow,GLForm1.LUTdrop.ItemIndex,true);
   UpdateLUT(i, LayerColorDrop.ItemIndex);
 end;
 UpdateImageIntensity;
 OverlayTimerStart;
end;

procedure TGLForm1.LayerOptionsBtnClick(Sender: TObject);
begin
  LayerPopup.PopUp;

end;

(**)
procedure TGLForm1.LayerContrastKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
     LayerWidgetChange(Sender);
end;

procedure TGLForm1.LayerListSelectionChange(Sender: TObject; User: boolean);
begin
     UpdateLayerBox(false);
end;

procedure TGLForm1.LayerListShowHint(Sender: TObject; HintInfo: PHintInfo);
begin

end;

procedure TGLForm1.LeftSplitterCanOffset(Sender: TObject;
  var NewOffset: Integer; var Accept: Boolean);
begin
    //caption := inttostr(random(888));
end;

procedure TGLForm1.LeftSplitterCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
begin
  (*caption := inttostr(random(888));
  ToolPanel.AutoSize := not ToolPanel.AutoSize;
  if not ToolPanel.AutoSize then
     ToolPanel.Width := 2;*)
  //ToolPanel.Visible := not ToolPanel.Visible;
end;

procedure TGLForm1.LeftSplitterChangeBounds(Sender: TObject);
begin
end;

procedure TGLForm1.LeftSplitterMoved(Sender: TObject);
begin

  //caption := inttostr(random(888));
end;

procedure TGLForm1.LayerAOMapMenuClick(Sender: TObject);
 var
    i: integer;
 begin
   i := LayerList.ItemIndex+ 1;
   if (i < 1) or (i > gMesh.OpenOverlays) then exit;
   gMesh.Overlay[i].aoMap := not gMesh.Overlay[i].aoMap;
   LayerWidgetChange(sender);
end;

procedure TGLForm1.PrevOverlayMenuClick(Sender: TObject);
var
  i: integer;
begin
  i := LayerList.ItemIndex+ 1;
  if (i < 1) or (i > gMesh.OpenOverlays) then exit;
  if (Sender as TMenuItem).tag = 1 then
     gMesh.Overlay[i].CurrentVolume := gMesh.Overlay[i].CurrentVolume + 1
  else
     gMesh.Overlay[i].CurrentVolume := gMesh.Overlay[i].CurrentVolume - 1;
  if gMesh.Overlay[i].CurrentVolume > gMesh.Overlay[i].Volumes then
     gMesh.Overlay[i].CurrentVolume := 1;
  if gMesh.Overlay[i].CurrentVolume < 1 then
     gMesh.Overlay[i].CurrentVolume := gMesh.Overlay[i].Volumes;
  UpdateLUT(i, LayerColorDrop.ItemIndex);
  LayerWidgetChange(nil);
  UpdateLayerBox(true);
end;

procedure TGLForm1.Shaders1Click(Sender: TObject);
begin

end;

function  DefuzzX(const x:  single):  single;
//instead of "5.9e-6" write "0.0"
const
 fuzz = 1.0E-5;
begin
  if  ABS(x) < fuzz then exit(0.0);
  exit(x);
end {Defuzz};

procedure TGLForm1.LayerAlphaTrackMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //caption := format('%d %d', [ViewGPU1.width, ViewGPU1.clientWidth]);
  LayerWidgetChange(sender);
end;

procedure TGLForm1.UpdateLayerBox(NewLayers: boolean);
var
   i: integer;
   s: string;
   isAtlas: boolean;
begin
     if (NewLayers) then begin
        LayerList.Items.Clear;
        if  gMesh.OpenOverlays < 1 then exit;
        for i := 1 to  gMesh.OpenOverlays do begin
            s := gMesh.Overlay[i].FileName;
            if gMesh.Overlay[i].volumes > 1 then
                 LayerList.Items.add(format('%d/%d: %s',[gMesh.Overlay[i].CurrentVolume, gMesh.Overlay[i].volumes, s]))
            else
                LayerList.Items.add(s);
            LayerList.Checked[i-1] := gMesh.Overlay[i].OpacityPercent <>  kLUTinvisible;//true;
        end;
        LayerList.ItemIndex := gMesh.OpenOverlays - 1;
     end;
     if (LayerList.ItemIndex < 0)  then
        LayerList.ItemIndex := LayerList.Items.Count -1;
     if (gMesh.OpenOverlays < 1) then exit;
     i := LayerList.ItemIndex + 1;
     if (i < 1) or (i > gMesh.OpenOverlays) then exit;
      PrevOverlayMenu.Enabled := (gMesh.Overlay[i].volumes > 1);
      NextOverlayMenu.Enabled := (gMesh.Overlay[i].volumes > 1);
     isAtlas := (gMesh.Overlay[i].atlasMaxIndex > 0);
     LayerDarkEdit.Enabled := not isAtlas;
     LayerBrightEdit.Enabled := not isAtlas;
     LayerColorDrop.Enabled := not isAtlas;
     LayerDarkEdit.Text := format('%.6g', [gMesh.Overlay[i].WindowScaledMin]);
     LayerBrightEdit.Text := format('%.6g', [gMesh.Overlay[i].WindowScaledMax]);
     OverlayBox.Hint := format('image intensity range %g..%g',[DefuzzX(gMesh.Overlay[i].minIntensity), DefuzzX(gMesh.Overlay[i].maxIntensity)]);
     LayerColorDrop.ItemIndex := gMesh.Overlay[i].LUTindex;
     LayerAlphaTrack.Position := gMesh.Overlay[i].OpacityPercent;
end;

{$IFDEF LCLCocoa}
procedure TGLForm1.SetDarkMode;
begin
  //setThemeMode(Self.Handle, gPrefs.DarkMode);
  setThemeMode(Self, gPrefs.DarkMode);
  if gPrefs.DarkMode then
     Memo1.Color := clGray
  else
      Memo1.Color := Graphics.clDefault;
  ScriptMemo.Color := Memo1.Color;
  ScriptOutputMemo.Color := Memo1.Color;
end;

procedure TGLForm1.SetRetina;
begin
  (*if gPrefs.RetinaDisplay then
     GLBox.Options := [ocoMacRetinaMode]
  else
    GLBox.Options := [];
  GLBox.MultiSampling:=GLBox.MultiSampling;
  *)
  LSetWantsBestResolutionOpenGLSurface(gPrefs.RetinaDisplay, GLBox.Handle);
  //GLBox.WantsBestResolutionOpenGLSurface:=gPrefs.RetinaDisplay;
  if (GLbox.Height < 1) or (GLBoxBackingHeight <= GLbox.Height) then
     gRetinaScale := 1
  else
      gRetinaScale := GLBoxBackingHeight/GLbox.Height;
end;

procedure SetFormDarkMode(var f: TForm);
begin
  f.PopupMode:= pmAuto;
  f.HandleNeeded;
  //setThemeMode(f.Handle, true);
  setThemeMode(f, true);
end;

procedure Mouse2Retina(var X,Y: integer);
begin
     if not gPrefs.RetinaDisplay then exit;
     X := round(X * gRetinaScale);
     Y := round(Y * gRetinaScale);
end;
{$ELSE}
procedure Mouse2Retina(var X,Y: integer);
begin
     //Retina display is MacOS feature
end;
{$ENDIF}

function FindFileExt(Filename: string): string;
var
  p,n,x: string;
  i : integer;
  searchResult : TSearchRec;
begin
   result := Filename;
   if FileExistsF(result) then exit;
   FilenameParts (Filename, p,n,x);
   //try location of last meshes
   for i := 1 to knMRU do begin
       p := ExtractFilePath(gPrefs.PrevFilename[i]);
       if p = '' then continue;
       result := p+n+x;
       if FileExistsF(result) then exit;
   end;
   //try location of last overlay
   p := ExtractFilePath(gPrefs.PrevOverlayname);
   result := p+n+x;
   if FileExistsF(result) then exit;
   //try location of last track
   p := ExtractFilePath(gPrefs.PrevTrackname);
   result := p+n+x;
   if FileExistsF(result) then exit;
   //try location of last node
   p := ExtractFilePath(gPrefs.PrevNodename);
   result := p+n+x;
   if FileExistsF(result) then exit;
   //try location of last script
   p := ExtractFilePath(gPrefs.PrevScript);
   result := p+n+x;
   if FileExistsF(result) then exit;
   //try application directory
   p := AppDir2;
   result := p+n+x;
   if FileExistsF(result) then exit;
   result := DefaultToHomeDir(n+x); //set path to home if not provided
   if FileExistsF(result) then exit;
   SetCurrentDir(p);
   if findfirst('*', faDirectory, searchResult) = 0 then begin
    repeat
      // Only show directories
      if (searchResult.attr and faDirectory) = faDirectory then begin
       //ShowMessage('Directory = '+searchResult.Name);
       result := p+ searchResult.Name + pathdelim+n+x;
       if FileExistsF(result) then begin
          FindClose(searchResult);
          exit;
       end;
      end;
    until FindNext(searchResult) <> 0;
    // Must free up resources used by these successful finds
    FindClose(searchResult);
  end;
   result := ''; //failed!
end;

function FindFile(fnm: string): string;
var
  Filename, p,n,x: string;
begin
  Filename := fnm;
  {$IFDEF UNIX}
  if Filename[1] = '~' then
     Filename := ExpandFileName(Filename);
  {$ENDIF}
  result := FindFileExt(Filename);
  if result <> '' then exit;
  FilenameParts (Filename, p,n,x);  // if user selects 'jhu' then open 'jhu.mz3'
  if x <> '' then exit;
  result :=  FindFileExt(ChangeFileExt(Filename,'.mz3'));
  if result <> '' then exit;
  result :=  FindFileExt(ChangeFileExt(Filename,'.gii'));
  if result <> '' then exit;
  result :=  FindFileExt(ChangeFileExt(Filename,'.ply'));
  if result <> '' then exit;
  result :=  FindFileExt(ChangeFileExt(Filename,'.obj'));
  //if result <> '' then exit;*)
end;

procedure TGLForm1.GLInvalidate;
begin
   GLBox.Invalidate;
end;

function TGLForm1.GLBoxBackingWidth: integer;
begin
   {$IFDEF LCLCocoa}
     result := Round(GLBox.Width * LBackingScaleFactor(GLBox.Handle));
   {$ELSE}
    result := GLBox.Width;
   {$ENDIF}
end;

procedure TGLForm1.FormChangeBounds(Sender: TObject);
{$IFDEF LCLCocoa} var lprev: single; {$ENDIF}
begin
   {$IFDEF LCLCocoa}
   if (gPrefs.RetinaDisplay)  then begin //detect if window moved between retina and non-retina display
      lprev := gRetinaScale;
      SetRetina;
      if (lprev <> gRetinaScale) then
        GLBox.Invalidate;
        //GLboxResize(Sender);
   end;
   {$ENDIF}
end;

procedure TGLForm1.ClrbarClr(i: integer);
begin
 if (i < 1) or (i > 4) then i := 4;
 gPrefs.ColorbarColor:= i;
 Case i of
      1: begin
        gClrbar.BackColor := (RGBA(255,255,255,255));
        gClrbar.FontColor := (RGBA(0,0,0,255));
      end;
      2: begin
        gClrbar.BackColor := (RGBA(255,255,255,168));
        gClrbar.FontColor := (RGBA(0,0,0,255));
      end;
      3: begin
        gClrbar.BackColor := (RGBA(0,0,0,255));
        gClrbar.FontColor := (RGBA(255,255,255,255));
      end;
      else begin
        gClrbar.BackColor := (RGBA(0,0,0,168));
        gClrbar.FontColor := (RGBA(255,255,255,255));
      end;
 end;

end;

procedure TGLForm1.ClrbarMenuClick(Sender: TObject);
begin
     ClrbarClr((sender as TMenuItem).Tag);
     GLBox.Invalidate;
end;

procedure TGLForm1.GLboxDblClick(Sender: TObject);
begin
     gPrefs.ColorBarPosition := gPrefs.ColorBarPosition + 1;
     SetColorbarPosition;
     GLbox.invalidate;
     //caption := 'doubleclick'+inttostr(random(888)); //1/2018: Cocoa generates dblClicks for single clicks
end;

procedure TGLForm1.ColorBarVisibleMenuClick(Sender: TObject);
begin
 gPrefs.Colorbar := not gPrefs.Colorbar;
 ColorBarVisibleMenu.Checked := gPrefs.Colorbar;
 GLBox.Invalidate;
end;

function TGLForm1.GLBoxBackingHeight: integer;
begin
   {$IFDEF LCLCocoa}
   result := Round(GLBox.Height * LBackingScaleFactor(GLBox.Handle));
   {$ELSE}
    result := GLBox.Height;
   {$ENDIF}
end;

procedure TGLForm1.MultiPassRenderingToolsUpdate;
var
  lBetter: boolean;
begin
  lBetter := (gPrefs.RenderQuality <> kRenderPoor) and (gPrefs.SupportBetterRenderQuality);
  AOLabel.Visible:= lBetter;
  occlusionTrack.Visible:= lBetter;
  ShaderForBackgroundOnlyCheck.Visible:= lBetter;
  if lBetter then begin
   XRayLabel.Caption := 'XRay';
   BGShader.Caption := 'Shader For Background Only';
  end else begin
      XRayLabel.Caption := 'Disabled: ';

      if not gPrefs.SupportBetterRenderQuality then
         BGShader.Caption := 'Poor Hardware'
      else
          BGShader.Caption := 'Poor Quality (see Preferences)';
  end;
  MeshBlendTrack.Visible:= lBetter;
  meshAlphaTrack.visible :=  lBetter;
end;

procedure TGLForm1.VolumeToMeshMenuClick(Sender: TObject);
const
{$IFDEF FOREIGNVOL}
 //kVolFilter = 'NIfTI volume|*.hdr;*.nii;*nii.gz';
 kVolFilter = 'Neuroimaging (*.nii)|*.hdr;*.nii;*.nii.gz;*.voi;*.HEAD;*.mgh;*.mgz;*.mha;*.mhd;*.nhdr;*.nrrd';
{$ELSE}
 kVolFilter = 'NIfTI volume|*.hdr;*.nii;*nii.gz';
{$ENDIF}
begin
  OpenDialog.Filter := kVolFilter;
  OpenDialog.Title := 'Select volume to convert';
  if not OpenDialog.Execute then exit;
  if (sender as TMenuItem).tag = 1 then
     Atlas2Mesh(OpenDialog.FileName)
  else
      Nii2Mesh(OpenDialog.FileName);
end;

procedure TGLForm1.ShaderForBackgroundOnlyClick(Sender: TObject);
begin
  gPrefs.ShaderForBackgroundOnly := ShaderForBackgroundOnlyCheck.Checked;
  //ShaderForBackgroundOnlyCheck.Checked := gPrefs.ShaderForBackgroundOnly ;
  GLBoxRequestUpdate(nil);
end;

function meshBackgroundOpen: boolean;
//returns true if a mesh is open as an overlay
// returns false if there are no overlays or if they are vertex colors
var lI: integer;
begin
 result := false;
  if (gMesh.OpenOverlays < 1) then exit;
  for lI := 1 to gMesh.OpenOverlays do
      if  length(gMesh.overlay[lI].faces) > 1 then
         result := true;
end;

procedure TGLForm1.UpdateToolbar;
//var
//  isOverlayNodeTrack : boolean;
begin
 OverlayBox.Visible := (gMesh.OpenOverlays > 0);
 //isOverlays  := (length(gNode.nodes) > 0) or (gTrack.n_count > 0) or ((gMesh.OpenOverlays > 0) and (meshBackgroundOpen));
 //BGShader.Enabled := isOverlays;
 //ShaderForBackgroundOnlyCheck.Enabled := isOverlays;
 //MeshBlendTrack.Enabled := isOverlays;
 //isOverlayNodeTrack := (length(gNode.nodes) > 0) or (gTrack.n_count > 0) or ((gMesh.OpenOverlays > 0) and (meshBackgroundOpen));
 //MeshBlendTrack.Visible := isOverlayNodeTrack;
 //BGShader.Visible := isOverlayNodeTrack;
 //ShaderForBackgroundOnlyCheck.Visible := isOverlayNodeTrack;
 //BackgroundBox.Visible := (length(gNode.nodes) > 0) or (gTrack.n_count > 0) or ((gMesh.OpenOverlays > 0) and (meshBackgroundOpen));
 NodeBox.Visible:= (length(gNode.nodes) > 0) ;
 if (length(gNode.edges) > 0) and (EdgeBox.Visible = false) and (BackgroundBox.Visible) then begin
    //this keeps node and edge boxes next to each other
    // executed when node is opened (which displays the background) and then edge is opened
    BackgroundBox.Visible := false;
    EdgeBox.Visible := true;
    BackgroundBox.Visible := true;
 end;
 EdgeBox.Visible:= (length(gNode.edges) > 0) ;
 TrackBox.Visible:= (gTrack.n_count > 0);
 MeshColorBox.Visible := (length(gMesh.vertexRGBA) > 0);
 gnLUT := -1; //refresh colorbar
 Memo1.Lines.clear;
 ToolPanel.Refresh;
end; //UpdateToolbar()

function TGLForm1.OpenNode(FilenameIn: string): boolean;
var
     FileName, edgename: string;
begin
 result := false;
 Filename := FindFile(FileNameIn);
 if Filename = '' then exit;
  if not gNode.LoadFromFile(FileName) then exit;
 result := true;
 gPrefs.PrevNodename := FileName;
 NodeBox.Visible:= true;
 if gNode.NodePrefs.isNodeThresholdBySize then begin
  NodeThreshDrop.ItemIndex := 0;  //threshold by size
  NodeMinEdit.Value:=gNode.NodePrefs.minNodeSize;
  NodeMaxEdit.Value:=gNode.NodePrefs.maxNodeSize;
 end else begin
   NodeThreshDrop.ItemIndex := 1; //threshold by color
   NodeMinEdit.Value:=gNode.NodePrefs.minNodeColor;
   NodeMaxEdit.Value:=gNode.NodePrefs.maxNodeColor;
 end;
 gNode.nodePrefs.minNodeThresh := NodeMinEdit.value;
 gNode.nodePrefs.maxNodeThresh := NodeMaxEdit.value;
 edgename := ChangeFileExt(FileName, '.edge');
 if fileexists(edgename) then
    OpenEdge(edgename);
 OpenDialog.InitialDir:= ExtractFileDir(FileName);
 UpdateToolbar;
 GLBoxRequestUpdate(nil);
end;

procedure TGLForm1.MeshColorBoxChange(Sender: TObject);
begin
  gMesh.vertexRgbaAlpha := MeshTransparencyTrack.Position / MeshTransparencyTrack.Max;
  gMesh.vertexRgbaSaturation := MeshSaturationTrack.Position / MeshSaturationTrack.Max;
  gMesh.isRebuildList:= true;
  GLBoxRequestUpdate(nil);
end;

procedure TGLForm1.AdditiveOverlayMenuClick(Sender: TObject);
var
    i: integer;
    isIntensityOverlay: boolean;
begin
   gPrefs.AdditiveOverlay :=  AdditiveOverlayMenu.Checked;
   if gMesh.OpenOverlays < 1 then exit;
   isIntensityOverlay := false;
   for i :=  gMesh.OpenOverlays downto 1 do
          if length(gMesh.overlay[i].intensity) > 1 then
            isIntensityOverlay := true;
   if (not isIntensityOverlay) and (gPrefs.AdditiveOverlay) then begin
      Memo1.Lines.Clear;
      Memo1.lines.add('Hint: Additive effect only influences painted surfaces, not meshes');
   end;
   OverlayTimerStart;
end;

function TGLForm1.OpenEdge(FilenameIn: string): boolean;
var
  Filename, ext, nodename: string;
begin
   result := false;
   Filename := FindFile(FilenameIn);
   if Filename = '' then exit;
   result := true;
 ext := UpperCase(ExtractFileExt(Filename));
 setlength(gNode.edges,0); //clear edges array
 if (ext = '.NODEZ') or (ext = '.NODE') or (length(gNode.nodes) < 1) then begin
     nodename := ChangeFileExt(FileName, '.node');
     if fileexists(nodename) then begin
        OpenNode(nodename);
        UpdateToolbar;
        exit;
     end;
 end;
 if length(gNode.edges) < 1 then //only if edges not loaded by openNode
    if not gNode.LoadEdge(Filename, false) then exit;
 UpdateToolbar;
 edgeMinEdit.Value := 0;
 edgeMaxEdit.Value := gNode.nodePrefs.maxEdgeAbs;
 OpenDialog.InitialDir:= ExtractFileDir(FileName);
 GLBoxRequestUpdate(nil);
end;

function TGLForm1.OpenOverlay(FilenameIn: string): boolean;
var
  Filename: string;
begin
   result := false;
   Filename := FindFile(FilenameIn);
   if Filename = '' then exit;
   if not gMesh.LoadOverlay(FileName, gPrefs.SmoothVoxelwiseData) then begin //gPrefs.SmoothVoxelwiseData
     GLBoxRequestUpdate(nil);
     UpdateToolbar;
     exit;
   end;
   result := true;
   gPrefs.PrevOverlayname := FileName;
   OpenDialog.InitialDir:= ExtractFileDir(FileName);
   UpdateToolbar;
   UpdateLayerBox(true);
   GLBoxRequestUpdate(nil);
end;

function TGLForm1.OpenTrack(FilenameIN: string): boolean;
var
  Filename: string;
  i: integer;
  isMultiProperties: boolean = false;
begin
   result := false;
 Filename := FindFile(FilenameIN);
 if Filename = '' then exit;
 if (gTrack.LoadFromFile(FileName)) and (gTrack.n_count > 0) then begin
    result := true;
    OpenDialog.InitialDir:= ExtractFileDir(FileName);
    gPrefs.PrevTrackname := FileName;
    if (gTrack.maxObservedFiberLength * 0.5) < TrackLengthTrack.Position then
       TrackLengthTrack.Position := round(gTrack.maxObservedFiberLength * 0.5);
 end;
 if (length(gTrack.scalars) > 0) then begin
    {$IFDEF LCLcocoa}
    TrackBox.Height := 135;
    {$ELSE}
    TrackBox.ClientHeight := TrackScalarNameDrop.Top + TrackScalarNameDrop.Height + 2;
    {$ENDIF}

    TrackScalarNameDrop.Items.Clear;
    TrackScalarNameDrop.Items.Add('Direction');
    for i := 0 to (length(gTrack.scalars) -1) do
        TrackScalarNameDrop.Items.Add(gTrack.scalars[i].name);
    TrackScalarNameDrop.ItemIndex := 0;
    TrackScalarLUTdrop.ItemIndex := 1;
    TrackScalarLUTdrop.Enabled := false;
    TrackScalarRangeBtn.Enabled := false;
    isMultiProperties := true;
 end else begin
   TrackScalarNameDrop.Items.Clear;
   TrackScalarNameDrop.Items.Add('Direction');
   TrackScalarNameDrop.ItemIndex := 0;
   TrackScalarLUTdrop.Enabled := false;
   TrackScalarRangeBtn.Enabled := false;
   isMultiProperties := false;

 end;
 TrackScalarNameDrop.Visible := isMultiProperties;
 TrackScalarLUTdrop.Visible := isMultiProperties;
 TrackScalarRangeBtn.Visible := isMultiProperties;
 //{$IFDEF LCLcocoa}
 //TrackBox.Height := 105;
 //{$ELSE}
 //TrackBox.ClientHeight := TrackDitherTrack.Top + TrackDitherTrack.Height;
 //{$ENDIF}
 UpdateToolbar;
 GLBoxRequestUpdate(nil);
end;

function isVtkMesh (filename: string): boolean; //vtk files can be tracks (" LINES" ->Tracks/Open) or meshes ("POLYGONS " -> File/Open, Overlay/Open)
var
      f: file;
      Str: string;
      szRead: integer;
begin
     result := false;
     if not fileexistsF(filename) then exit;
     FileMode := fmOpenRead;
     AssignFile(f, FileName);
     Reset(f,1);
     FileMode := fmOpenRead;
     szRead := FileSize(f);
     SetLength(Str, szRead);
     BlockRead(f, Str[1],szRead);
     CloseFile(f);
     if (pos('POLYGONS ', Str) > 0) then result := true; //faces
     if (pos('TRIANGLE_STRIPS ', Str) > 0) then result := true; //faces
end;

function isGiiMesh (filename: string): boolean;
//returns true if file is a valid mesh (faces+vertices), returns false if overlay map
var
      f: file;
      Str: string;
      szRead: integer;
begin
     result := false;
     if not fileexistsF(filename) then exit;
     result := true;
     AssignFile(f, FileName);
     Reset(f,1);
     FileMode := fmOpenRead;
     szRead := FileSize(f);
     SetLength(Str, szRead);
     BlockRead(f, Str[1],szRead);
     CloseFile(f);
     if (pos('Intent="NIFTI_INTENT_TRIANGLE"', Str) > 0) then exit; //faces
     if (pos('Intent="NIFTI_INTENT_POINTSET"', Str) > 0) then exit; //vertices
     result := false;
end;

function isMz3Mesh (filename: string): boolean;
//returns true if file is a valid mesh (faces+vertices), returns false if overlay map
const
 kMagic =  23117; //"MZ"
 kChunkSize = 16;
label 666;
var
  i: integer;
  Magic, Attr: uint16;
  nFace, nVert: uint32;
  isFace, isVert: boolean;
  mStream : TMemoryStream;
  zStream: TGZFileStream;
  bytes : array of byte;
begin
     result := false;
     if not fileexistsF(Filename) then exit;
     mStream := TMemoryStream.Create;
     zStream := TGZFileStream.create(FileName, gzopenread);
     setlength(bytes, kChunkSize);
     i := zStream.read(bytes[0],kChunkSize);
     mStream.Write(bytes[0],i) ;
     if i < kChunkSize then goto 666;
     mStream.Position := 0;
     mStream.Read(Magic,2);
     mStream.Read(Attr,2);
     mStream.Read(nFace,4);
     mStream.Read(nVert,4);
     if (magic <> kMagic) then goto 666;
     isFace := (Attr and 1) > 0;
     isVert := (Attr and 2) > 0;
     result := (nFace > 0) and (nVert > 0) and (isFace) and (isVert);
   666 :
     zStream.Free;
     mStream.Free;
end; //isMz3Mesh

function TGLForm1.OpenMesh(FilenameIN: string): boolean;
var
    Filename, curvname, ext, ext2: string;
begin
  result := false;
  if FilenameIN <> '-' then
     Filename := FindFile(FilenameIN)
  else
      Filename := FilenameIN;
  if Filename = '' then exit;
  result := true;
  ext := ExtractFileExtGzUpper(Filename);
  ext2 := UpCaseExt2(FileName); // "file.gii.dset" -> ".GII.DSET"
  if (ext = '.GLS') then begin
     OpenScript(Filename);
     exit;
  end;
  if (ext2 = '.NIML.DSET') then begin
    Showmessage('.NIML.DSET format not supported: use ConvertDset to convert to GIfTI.');
    exit;
  end;
  //ext := UpperCase(ExtractFileExt(Filename));
  if (ext2 = '.1D.DSET') or (ext = '.COL') or (ext = '.ANNOT') or (ext = '.MGH') or (ext = '.MGZ')  or (ext = '.NII') or (ext = '.HDR')  or (ext = '.NII.GZ') or (ext = '.DPV') or (ext = '.ANNOT') or (ext = '.W') or (ext = '.CURV')  then begin
    OpenOverlay(Filename);
    exit;
  end else if (ext = '.VTK') and (not isVtkMesh (Filename)) then begin
    OpenTrack(Filename);  //.vtk files can be either meshes or tracks - autodetect
    exit;
  end else if (length(gMesh.Faces) > 0) and (ext = '.MZ3') and (not isMz3Mesh (Filename)) then begin
    OpenOverlay(Filename);  //GIfTI files can be meshes or overlays - autodetect
    exit;
  end else if (length(gMesh.Faces) > 0) and ((ext2 = '.GII.DSET') or (ext = '.GII')) and (not isGiiMesh (Filename)) then begin
    OpenOverlay(Filename);  //GIfTI files can be meshes or overlays - autodetect
    exit;
  end else if (ext = '.DAT') or  (ext = '.TRK') or  (ext = '.TRK.GZ') or (ext = '.FIB') or (ext = '.PDB') or (ext = '.TCK') or (ext = '.BFLOAT') or (ext = '.BFLOAT.GZ')  then begin
    OpenTrack(Filename);
    exit;
  end else if (ext = '.EDGE') then begin
    OpenEdge(Filename);
    exit;
  end else if (ext = '.NODE') or (ext = '.NODZ') then begin
    OpenNode(Filename);
    exit;
  end;
  if (ssShift in KeyDataToShiftState(vk_Shift)) then begin
     OpenOverlay(Filename);
     exit;
  end;
  CloseOverlaysMenuClick(nil);
  CloseTracksMenuClick(nil);
  CloseNodesMenuClick(nil);
  if not gMesh.LoadFromFile(Filename) then begin  //only add successful loads to MRU
     UpdateToolbar;
     GLBoxRequestUpdate(nil);
     exit;
  end;
  OpenDialog.InitialDir:= ExtractFileDir(Filename);
  UpdateToolbar;
  if gMesh.OpenOverlays > 0 then begin //e.g. MZ3 with both MESH and SCALAR intensity
    UpdateLayerBox(true);
  end;
  AddMRU(Filename);
  //if gMesh.isFreeSurferMesh then begin
  curvname := changefileext(Filename, '.curv');
  if fileexistsF(curvname) then
     OpenOverlay(curvname);
  curvname := changefileext(Filename, '.thickness');
  if fileexistsF(curvname) then
     OpenOverlay(curvname);
  curvname := changefileext(Filename, '.area');
  if fileexistsF(curvname) then
     OpenOverlay(curvname);
  //sulc usually similar to curv
  //curvname := changefileext(Filename, '.sulc');
  //if fileexistsF(curvname) then
  //   OpenOverlay(curvname);
  //end;
  GLBoxRequestUpdate(nil);
end;

procedure TGLForm1.OpenMRU(Sender: TObject);//open template or MRU
begin
     OpenMesh(gPrefs.PrevFilename[(sender as TMenuItem).tag]);
end;

procedure TGLForm1.CreateMRU;
var
  lPos : integer;
  NewItem: TMenuItem;
begin
 for lPos :=  1 to knMRU do begin
        NewItem := TMenuItem.Create(FileMenu);
        NewItem.Caption :='';//(ParseFileName(ExtractFileName(lFName)));
        NewItem.Tag := lPos;
        NewItem.onclick :=  OpenMRU; //Lazarus
        NewItem.Visible := false;
        if lPos < 10 then begin
          {$IFDEF Darwin}
          NewItem.ShortCut := ShortCut(Word('1')+ord(lPos-1), [ssMeta]);
          {$ELSE}
          NewItem.ShortCut := ShortCut(Word('1')+ord(lPos-1), [ssCtrl]);
          {$ENDIF}
        end;
        FileMenu.Add(NewItem);
  end;//for each MRU
end;

procedure TGLForm1.UpdateMRU;//most-recently-used RestrictMenu
var
   lCount, lPos : integer;
begin
 lCount := FileMenu.IndexOf(FileSepMenu);
 for lPos :=  1 to knMRU do begin
      if gPrefs.PrevFilename[lPos] <> '' then begin
        FileMenu.Items[lCount + lPos].Visible:= true;
        FileMenu.Items[lCount + lPos].Caption:= ExtractFileName(gPrefs.PrevFilename[lPos]);
      end else
          FileMenu.Items[lCount + lPos].Visible:= false;
  end;//for each MRU
end;  //UpdateMRU

procedure TGLForm1.AddMRU(lFilename: string);
var
  i, rep: integer;
  prev: TMRU;
begin
 rep := 1024;
 for i := 1 to knMRU do begin
     prev[i] := gPrefs.PrevFilename[i];
     if prev[i] = lFilename then
        rep := i;
 end;
 gPrefs.PrevFilename[1] := lFilename;
 for i := 1 to (knMRU-1) do begin
     if i >= rep then
        gPrefs.PrevFilename[i+1] := prev[i+1]
     else
         gPrefs.PrevFilename[i+1] := prev[i];
 end;
 UpdateMRU;
end;

function GetOrigin(out scale: single): TPoint3f;
begin
     result := ptf(0,0,0);
     scale := 0.0;
     if (length(gMesh.faces) > 0) then begin
        scale := gMesh.Scale;
        result :=  ptf(gMesh.Origin.X,gMesh.Origin.Y,gMesh.Origin.Z) ;
     end;
     if (length(gNode.faces) > 0) and (gNode.Scale > scale) then begin
        scale := gNode.Scale;
        result :=  ptf(gNode.Origin.X, gNode.Origin.Y, gNode.Origin.Z) ;
     end;
     if (gTrack.n_count > 0) and (gTrack.Scale > scale) then begin
       scale := gTrack.Scale;
       result :=  ptf(gTrack.Origin.X, gTrack.Origin.Y, gTrack.Origin.Z) ;
     end;
end;

procedure IncTrackBar (T: TTrackBar; isDepthTrack: boolean);
var
   i: integer;
begin
  i := (T.Max div 4);
  i := ((i+T.Position) div i) * i;
  if i >= T.Max then i := T.Min;
  T.position := i;
  if not(isDepthTrack) and (T.position <> 0) and (GLForm1.ClipTrack.position = 0) then
     GLForm1.ClipTrack.Position := GLForm1.ClipTrack.Max div 2;
end;

procedure TGLForm1.OverlayTimerStart;
begin
     OverlayTimer.enabled := true;
end;

procedure TGLForm1.ShowmessageError(s: string);
begin
 if  GLerror <> '' then exit;
     GLerror := s;
     ErrorTimer.Enabled := true;
end;

procedure TGLForm1.SetOverlayTransparency(Sender: TObject);
begin
  gMesh.OverlayTransparency := (sender as TMenuItem).tag;
  OverlayTimerStart;
end;

(*procedure TGLForm1.SetOverlayTransparency(Sender: TObject);
var
   i, n, v: integer;
begin
   n :=  gMesh.OpenOverlays;
   if n < 1 then exit;
   v := (sender as TMenuItem).tag;
   if (v > 95) then
      v := kLUTopaque
   else if (v > 0) then
     v := kLUTtranslucent
   else
       v := kLUTinvisible;
   for i := 1 to n do
       gMesh.Overlay[i].LUTvisible := v;
   //gMesh.OverlayTransparency := (sender as TMenuItem).tag;
  OverlayTimerStart;
end;*)

procedure TGLForm1.ShaderBoxResize(Sender: TObject);
const
kMinMemoSz= 32;
var
   lDesiredControlSz: integer;
begin
  if not ShaderBox.Visible then exit;
  lDesiredControlSz := ShaderPanelHeight;
  if ShaderBox.ClientHeight > (lDesiredControlSz+kMinMemoSz) then begin
    //if ShaderBox.Height > (lDesiredControlSz+kMinMemoSz) then begin
    //Memo1.Height := ShaderBox.Height - lDesiredControlSz;
    Memo1.Height := ShaderBox.ClientHeight - lDesiredControlSz;
    {$IFDEF LCLCocoa}
    //Memo1.Height := ShaderBox.ClientHeight - lDesiredControlSz + 22;

    {$ENDIF}
    Memo1.visible := true;
  end
  else
     Memo1.visible := false;
  ShaderBox.Refresh;
end;

function ResetIniDefaults : boolean;
begin
     if ParamCount > 0 then begin //e.g. Matlab users often launch system commands using keyboard shortcuts. These uses should use -R to force reset
        result := false;
        exit;
     end;
     //result := ( GetKeyState(VK_MENU)<> 0) or (GetKeyState(VK_LWIN) <> 0) or (GetKeyState(VK_CONTROL) <> 0)  or (ssShift in KeyDataToShiftState(VK_SHIFT)) ;
     {$IFDEF Windows}
     result := (ssShift in KeyDataToShiftState(VK_SHIFT)) ;
     {$ELSE}
     result := ( GetKeyState(VK_MENU)<> 0) or (GetKeyState(VK_LWIN) <> 0)  or (ssShift in KeyDataToShiftState(VK_SHIFT)) ;
     {$ENDIF}
end;

procedure TGLForm1.ShaderDropChange(Sender: TObject);
begin
  SetShader(ShaderDir+pathdelim+ShaderDrop.Items[ShaderDrop.ItemIndex]+'.txt');
  ShaderBoxResize(Sender);
  GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.MatCapDropChange(Sender: TObject);
begin
 {$IFDEF MATCAP}
 if MatCapDrop.Items.Count < 1 then exit;
 if MatCapDrop.ItemIndex < 0 then
    MatCapDrop.ItemIndex := 0;
 GLBox.MakeCurrent;
 SetMatCap(MatCapDir+pathdelim+MatCapDrop.Items[MatCapDrop.ItemIndex]+'.jpg');
 GLBox.ReleaseContext;
 GLBoxRequestUpdate(Sender);
 {$ENDIF}
end;

procedure TGLForm1.PaintModeAutomaticMenu(Sender: TObject);
var
   i: integer;
begin
  i := LayerList.ItemIndex+ 1;
  if (i < 1) or (i > gMesh.OpenOverlays) then exit;
  gMesh.Overlay[i].PaintMode := (Sender as TMenuItem).Tag;
  caption := format('%d -> %d', [i, gMesh.Overlay[i].PaintMode]);
  LayerWidgetChange(sender);
end;

procedure TGLForm1.PaintModeMenuClick(Sender: TObject);
begin

end;

procedure TGLForm1.GLboxMouseMove(Sender: TObject; Shift: TShiftState; lX, lY: Integer);
var
   X,Y:integer;
begin
 if gMouseX < 0 then exit; //mouse is not down
 X := lX; Y := lY; Mouse2Retina(X,Y);
 if (ssShift in Shift) then begin
    //Pan image
    gPrefs.ScreenPan.X := gPrefs.ScreenPan.X + (1/GLBoxBackingWidth * (X - gMouseX));
    if (gPrefs.ScreenPan.X > 1) then gPrefs.ScreenPan.X := 1;
    if (gPrefs.ScreenPan.X < -1) then gPrefs.ScreenPan.X := -1;
    gPrefs.ScreenPan.Y := gPrefs.ScreenPan.Y - (1/GLboxBackingHeight * (Y - gMouseY));
    if (gPrefs.ScreenPan.Y > 1) then gPrefs.ScreenPan.Y := 1;
    if (gPrefs.ScreenPan.Y < -1) then gPrefs.ScreenPan.Y := -1;

 end else begin
   gElevation := gElevation + (Y - gMouseY);
   gAzimuth := gAzimuth - (X - gMouseX);
   while gAzimuth > 360 do
         gAzimuth := gAzimuth -360;
   while gAzimuth < -360 do
         gAzimuth := gAzimuth + 360;
 end;
 gMouseX := X;
 gMouseY := Y;
 GLBox.invalidate;//GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.GLboxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; lX, lY: Integer);
begin
     gMouseX := -1; //released
end;

procedure TGLForm1.GLboxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; lX, lY: Integer);
var
   X,Y: integer;
begin
  X := lX; Y := lY; Mouse2Retina(X,Y);
     gMouseX := X;
     gMouseY := Y;
end;

procedure sph2cartDeg90x(Azimuth,Elevation,R: single; var lX,lY,lZ: single);
//convert spherical AZIMUTH,ELEVATION,RANGE to Cartesion
var
  n: integer;
  E,Phi,Theta: single;
begin
  Theta := DegToRad(Azimuth-90);
  E := Elevation;
  if (E > 360) or (E < -360)  then begin
    n := trunc(E / 360) ;
    E := E - (n * 360);
  end;
  if ((E > 89) and (E < 91)) or (E < -269) and (E > -271) then
    E := 90;
  if ((E > 269) and (E < 271)) or (E < -89) and (E > -91) then
    E := -90;
  Phi := DegToRad(E);
  lX := r * cos(Phi)*cos(Theta);
  lY := r * cos(Phi)*sin(Theta);
  lZ := r * sin(Phi);
end;

procedure TGLForm1.ClipTrackChange(Sender: TObject);
var
  a,e,scale: single;
begin
 GetOrigin(scale);
 a := ClipAziTrack.Position;
 e := ClipElevTrack.Position;
 (*if not gPrefs.ObjectBasedClipPlane then begin
    a := gAzimuth - a - 180;
    e := gElevation - e;
 end;*)
 sph2cartDeg90x(a,e,1,clipPlane.X,clipPlane.Y,clipPlane.Z);
 if ClipTrack.Position < 1 then
    clipPlane.X := 2 //tell GLSL that plane is disabled: normalized value must be <= 1.0
 else
   clipPlane.W := ((ClipTrack.Position/ClipTrack.Max) - 0.5) * scale * 2.0;
 Memo1.Lines.clear;
 Memo1.Lines.Add(format('Clipping Amount %d',[ClipTrack.Position]));
 Memo1.Lines.Add(format('Clipping Azimuth %d',[ClipAziTrack.Position]));
 Memo1.Lines.Add(format('Clipping Elevation %d',[ClipElevTrack.Position]));
 GLBox.invalidate;  //show change immediately!, for delay: GLBoxRequestUpdate(Sender);
end;

(*procedure TGLForm1.ClipTrackChange(Sender: TObject);
var
  scale: single;
begin
 GetOrigin(scale);
 sph2cartDeg90x(ClipAziTrack.Position,ClipElevTrack.Position,1,clipPlane.X,clipPlane.Y,clipPlane.Z);
 if ClipTrack.Position < 1 then
    clipPlane.X := 2 //tell GLSL that plane is disabled: normalized value must be <= 1.0
 else
   clipPlane.W := ((ClipTrack.Position/ClipTrack.Max) - 0.5) * scale * 2.0;
 Memo1.Lines.clear;
 Memo1.Lines.Add(format('Clipping Amount %d',[ClipTrack.Position]));
 Memo1.Lines.Add(format('Clipping Azimuth %d',[ClipAziTrack.Position]));
 Memo1.Lines.Add(format('Clipping Elevation %d',[ClipElevTrack.Position]));
 GLBox.invalidate;  //show change immediately!, for delay: GLBoxRequestUpdate(Sender);
end; *)

procedure TGLForm1.CloseMenuClick(Sender: TObject);
begin
  gMesh.Close;
  gNode.Close;
  gTrack.Close;
  UpdateToolbar;
  GLboxRequestUpdate(Sender);
end;

procedure TGLForm1.CloseNodesMenuClick(Sender: TObject);
begin
  gNode.Close;
  UpdateToolbar;
  GLboxRequestUpdate(sender);
end;

procedure TGLForm1.CloseOverlaysMenuClick(Sender: TObject);
begin
  (*if (Sender <> nil) and (gMesh.OpenOverlays > 0) then begin
    if isFreeSurferLUT(gMesh.Overlay[1].LUTindex) then
         if MessageDlg('Curvature overlay open', 'Close the FreeSurfer CURV file?', mtConfirmation, [mbYes, mbNo],0) = mrNo then
           exit;
  end;*)
  gMesh.CloseOverlays;
  GLForm1.SetFocusedControl(nil); //GLForm1.ActiveControl := nil;
  UpdateToolbar;
  GLboxRequestUpdate(sender);
end;

procedure TGLForm1.CloseTracksMenuClick(Sender: TObject);
begin
  gTrack.Close;
  UpdateToolbar;
  GLboxRequestUpdate(sender);
end;

procedure TGLForm1.GLboxRequestUpdate(Sender: TObject);
var
  scale: single;
begin
 GetOrigin(scale);
  sph2cartDeg90x(LightAziTrack.position,LightElevTrack.position, scale * 2, gShader.lightPos.X, gShader.lightPos.Z,gShader.lightPos.Y);
  gShader.lightPos.Z := -gShader.lightPos.Z;
  gShader.lightPos.X := gShader.lightPos.X * scale;
  gShader.lightPos.Y := gShader.lightPos.Y * scale;
  gShader.lightPos.Z := gShader.lightPos.Z * scale;
  UpdateTimer.Enabled := true;
end;

procedure TGLForm1.SaveTrack (var lTrack: TTrack);
const
    kTrackFilter  = 'VTK (.vtk)|*.vtk|Camino (.Bfloat)|*.Bfloat|CaminoGZ (.Bfloat.gz)|*.Bfloat.gz|TrackVis (.trk)|*.trk|TrackVisGZ (.trk.gz)|*.trk.gz';
var
  nam: string;
begin
  if (lTrack.n_count < 1) then begin
   showmessage('Unable to save tracks: no tracks open (use Tracks/AddTracks)');
   exit;
 end;
 SaveMeshDialog.Filter := kTrackFilter;
 SaveMeshDialog.Title := 'Save track file';
 nam := gPrefs.PrevTrackname;
 SaveMeshDialog.InitialDir:= ExtractFileDir(nam);
 if not fileexists(nam) then
   nam := 'Track.vtk';
 nam := extractfilename (nam);
 if gPrefs.SaveAsFormatTrack = kSaveAsTrackTrk then begin
   SaveMeshDialog.DefaultExt:= '.trk';
   SaveMeshDialog.FileName := changeFileExt(nam, '.trk');
   SaveMeshDialog.FilterIndex := 4;
 end else if gPrefs.SaveAsFormatTrack = kSaveAsTrackBfloat then begin
   SaveMeshDialog.DefaultExt:= '.BFloat';
   SaveMeshDialog.FileName := changeFileExt(nam, '.BFloat');
   SaveMeshDialog.FilterIndex := 2;
 end else begin
     SaveMeshDialog.DefaultExt:= '.vtk';
     SaveMeshDialog.FileName := changeFileExt(nam, '.vtk');
     SaveMeshDialog.FilterIndex := 1;
 end;
 if not SaveMeshDialog.Execute then exit;
 nam := UpperCase(ExtractFileExt(SaveMeshDialog.Filename));
 if (SaveMeshDialog.FilterIndex = 4) or (SaveMeshDialog.FilterIndex = 5) or (nam = '.TRK') or (nam = '.TRK.GZ') then
    lTrack.SaveTrk(SaveMeshDialog.Filename)
 else if (SaveMeshDialog.FilterIndex = 2) or (SaveMeshDialog.FilterIndex = 3) or (nam = '.BFLOAT') or (nam = '.BFLOAT.GZ') then
    lTrack.SaveBfloat(SaveMeshDialog.Filename)
 else
     lTrack.SaveVtk(SaveMeshDialog.Filename);
end;

(*86function  SimplifyPref(out Tol, minLength: single): boolean;
var
    PrefForm: TForm;
    OkBtn: TButton;
    TolLabel, minLengthLabel: TLabel;
    TolEdit, minLengthEdit: TEdit;
begin
	Tol := 0.5;
	minLength := 10;
	PrefForm:=TForm.Create(nil);
	PrefForm.SetBounds(100, 100, 520, 112);
	PrefForm.Caption:='Track simplification preferences';
	PrefForm.Position := poScreenCenter;
	PrefForm.BorderStyle := bsDialog;
	//Tolerance
	TolLabel:=TLabel.create(PrefForm);
	TolLabel.Caption:= 'Tolerance ("1" will allow track to deviate 1mm from original)';
	TolLabel.Left := 8;
	TolLabel.Top := 12;
	TolLabel.Parent:=PrefForm;
	TolEdit:=TEdit.create(PrefForm);
	TolEdit.Caption := FloatToStrF(Tol, ffGeneral, 8, 4);
	TolEdit.Top := 12;
	TolEdit.Width := 92;
	TolEdit.Left := PrefForm.Width - TolEdit.Width - 8;
	TolEdit.Parent:=PrefForm;
	//minLength
	minLengthLabel:=TLabel.create(PrefForm);
	minLengthLabel.Caption:= 'Enter minimum fiber length';
	minLengthLabel.Left := 8;
	minLengthLabel.Top := 42;
	minLengthLabel.Parent:=PrefForm;
	minLengthEdit:=TEdit.create(PrefForm);
	minLengthEdit.Caption := FloatToStr(minLength);
	minLengthEdit.Top := 42;
	minLengthEdit.Width := 92;
	minLengthEdit.Left := PrefForm.Width - minLengthEdit.Width - 8;
	minLengthEdit.Parent:=PrefForm;
	//OK button
	OkBtn:=TButton.create(PrefForm);
	OkBtn.Caption:='OK';
	OkBtn.Top := 72;
	OkBtn.Width := 128;
	OkBtn.Left := PrefForm.Width - OkBtn.Width - 8;
	OkBtn.Parent:=PrefForm;
	OkBtn.ModalResult:= mrOK;
	{$IFNDEF Darwin}
        ScaleDPI(PrefForm, 96);
        {$ENDIF}
        {$IFDEF LCLCocoa}
        if gPrefs.DarkMode then SetFormDarkMode(PrefForm);
        {$ENDIF}
        PrefForm.ShowModal;
	Tol := StrToFloatDef(TolEdit.Caption, Tol);
	minLength := StrToFloatDef(minLengthEdit.Caption, minLength);
	result :=  PrefForm.ModalResult = mrOK;
	FreeAndNil(PrefForm);
end;*)
function  SimplifyPref(out Tol, minLength: single): boolean;
var
    PrefForm: TForm;
    OkBtn: TButton;
    TolLabel, minLengthLabel: TLabel;
    TolEdit, minLengthEdit: TEdit;
begin
	Tol := 0.5;
	minLength := 10;
	PrefForm:=TForm.Create(nil);
        PrefForm.AutoSize := True;
        PrefForm.BorderWidth := 8;
	//PrefForm.SetBounds(100, 100, 520, 112);
	PrefForm.Caption:='Track simplification preferences';
	PrefForm.Position := poScreenCenter;
	PrefForm.BorderStyle := bsDialog;
	//Tolerance
	TolLabel:=TLabel.create(PrefForm);
	TolLabel.Caption:= 'Tolerance ("1" will allow track to deviate 1mm from original)';
	//TolLabel.Left := 8;
	//TolLabel.Top := 12;
        TolLabel.AutoSize := true;
        TolLabel.AnchorSide[akTop].Side := asrTop;
        TolLabel.AnchorSide[akTop].Control := PrefForm;
        TolLabel.BorderSpacing.Top := 6;
        TolLabel.AnchorSide[akLeft].Side := asrLeft;
        TolLabel.AnchorSide[akLeft].Control := PrefForm;
        TolLabel.BorderSpacing.Left := 6;
	TolLabel.Parent:=PrefForm;
	TolEdit:=TEdit.create(PrefForm);
	TolEdit.Caption := FloatToStrF(Tol, ffGeneral, 8, 4);
	//TolEdit.Top := 12;
	//TolEdit.Width := 92;
	//TolEdit.Left := PrefForm.Width - TolEdit.Width - 8;
        TolEdit.Constraints.MinWidth:= 128;
        TolEdit.AutoSize := true;
        TolEdit.AnchorSide[akTop].Side := asrTop;
        TolEdit.AnchorSide[akTop].Control := PrefForm;
        TolEdit.BorderSpacing.Top := 4;
        TolEdit.AnchorSide[akLeft].Side := asrRight;
        TolEdit.AnchorSide[akLeft].Control := TolLabel;
        TolEdit.BorderSpacing.Left := 6;
	TolEdit.Parent:=PrefForm;
	//minLength
	minLengthLabel:=TLabel.create(PrefForm);
	minLengthLabel.Caption:= 'Enter minimum fiber length';
	//minLengthLabel.Left := 8;
	//minLengthLabel.Top := 42;
        minLengthLabel.AutoSize := true;
        minLengthLabel.AnchorSide[akTop].Side := asrBottom;
        minLengthLabel.AnchorSide[akTop].Control := TolEdit;
        minLengthLabel.BorderSpacing.Top := 6;
        minLengthLabel.AnchorSide[akLeft].Side := asrLeft;
        minLengthLabel.AnchorSide[akLeft].Control := PrefForm;
        minLengthLabel.BorderSpacing.Left := 6;
	minLengthLabel.Parent:=PrefForm;
	minLengthEdit:=TEdit.create(PrefForm);
	minLengthEdit.Caption := FloatToStr(minLength);
	//minLengthEdit.Top := 42;
	//minLengthEdit.Width := 92;
	//minLengthEdit.Left := PrefForm.Width - minLengthEdit.Width - 8;
        minLengthEdit.Constraints.MinWidth:= 128;
        minLengthEdit.AutoSize := true;
        minLengthEdit.AnchorSide[akTop].Side := asrBottom;
        minLengthEdit.AnchorSide[akTop].Control := TolEdit;
        minLengthEdit.BorderSpacing.Top := 4;
        minLengthEdit.AnchorSide[akLeft].Side := asrRight;
        minLengthEdit.AnchorSide[akLeft].Control := minLengthLabel;
        minLengthEdit.BorderSpacing.Left := 6;
	minLengthEdit.Parent:=PrefForm;
	//OK button
	OkBtn:=TButton.create(PrefForm);
	OkBtn.Caption:='OK';
	//OkBtn.Top := 72;
	//OkBtn.Width := 128;
	//OkBtn.Left := PrefForm.Width - OkBtn.Width - 8;
        OkBtn.AutoSize := true;
        OkBtn.AnchorSide[akTop].Side := asrBottom;
        OkBtn.AnchorSide[akTop].Control := minLengthEdit;
        OkBtn.BorderSpacing.Top := 6;
        OkBtn.AnchorSide[akLeft].Side := asrCenter;
        OkBtn.AnchorSide[akLeft].Control := PrefForm;
        OkBtn.Constraints.MinWidth:= 64;
	OkBtn.Parent:=PrefForm;
	OkBtn.ModalResult:= mrOK;
	{$IFDEF LCLCocoa}
        if gPrefs.DarkMode then SetFormDarkMode(PrefForm);
        {$ENDIF}
        PrefForm.ShowModal;
	Tol := StrToFloatDef(TolEdit.Caption, Tol);
	minLength := StrToFloatDef(minLengthEdit.Caption, minLength);
	result :=  PrefForm.ModalResult = mrOK;
	FreeAndNil(PrefForm);
end;


procedure TGLForm1.SimplifyTracksMenuClick(Sender: TObject);
var
  tol, minLength: single;
  lTrack: TTrack;
begin
     //showmessage(gPrefs.PrevTrackname);
 (*if DefaultFormatSettings.DecimalSeparator = '.' then
    s := '0.1'
  else
      s := '0,1';
  if not inputquery('Track simplify', 'Enter tolerance (e.g. "1" will allow track to deviate 1mm from original)', s) then exit;
  if not TryStrToFloat(s, tol) then begin
    showmessage('Unable convert value to a number');
    exit;
  end; *)
  if not SimplifyPref(Tol, minLength) then exit;
  OpenDialog.Filter := kTrackFilter;
  OpenDialog.Title := 'Select track file';
  if Fileexists(gPrefs.PrevTrackname) then begin
     OpenDialog.InitialDir := ExtractFileDir(gPrefs.PrevTrackname);
     OpenDialog.FileName:= gPrefs.PrevTrackname;
  end;
  if not OpenDialog.Execute then exit;
  lTrack := TTrack.Create;
  if lTrack.LoadFromFile(OpenDialog.FileName) then begin
     gPrefs.PrevTrackname := OpenDialog.FileName;
     if lTrack.SimplifyMM(Tol, minLength) then begin
      SaveTrack(lTrack);
     end;
  end;
  lTrack.Close;
  lTrack.Free;
end;

procedure TGLForm1.SaveTracksMenuClick(Sender: TObject);
begin
     SaveTrack(gTrack);
end;

procedure TGLForm1.ScalarDropChange(Sender: TObject);
var
  i: integer;
begin
  gTrack.scalarSelected := TrackScalarNameDrop.ItemIndex -1;//-1 for direction color, 0 for first scalar, etc.
  TrackScalarLUTdrop.Enabled := (gTrack.scalarSelected >= 0); //disable if color based on direction not scalar
  TrackScalarRangeBtn.Enabled := TrackScalarLUTdrop.Enabled;
  i := TrackScalarLUTdrop.ItemIndex;
  gTrack.scalarLUT := UpdateTransferFunction(i, false);
  gTrack.isRebuildList:= true;
  gnLUT := -1; //refresh colorbar
  GLBoxRequestUpdate(Sender);
end;

// 'Defuzz' is used for comparisons and to avoid propagation of 'fuzzy',
//  nearly-zero values.  DOUBLE calculations often result in 'fuzzy' values.
//  The term 'fuzz' was adapted from the APL language.
(*FUNCTION  Defuzz(CONST x:  DOUBLE):  DOUBLE;
const
 fuzz = 1.0E-6;
BEGIN
  IF  ABS(x) < fuzz
  THEN RESULT := 0.0
  ELSE RESULT := x
END {Defuzz};
  *)
(*86 function  ScalarPref(var min, max: single; var ColorBarPrecedenceTracksNotOverlays: boolean): boolean;
var
    PrefForm: TForm;
    OkBtn: TButton;
    minLabel, maxLabel: TLabel;
    minEdit, maxEdit: TEdit;
    ColorBarCheck: TCheckBox;
begin
    PrefForm:=TForm.Create(nil);
    PrefForm.SetBounds(100, 100, 520, 142);
    PrefForm.Caption:='Track simplification preferences';
    PrefForm.Position := poScreenCenter;
    PrefForm.BorderStyle := bsDialog;
    //Tolerance
    minLabel:=TLabel.create(PrefForm);
    minLabel.Caption:= 'Minimum intensity';
    minLabel.Left := 8;
    minLabel.Top := 12;
    minLabel.Parent:=PrefForm;
    minEdit:=TEdit.create(PrefForm);
    minEdit.Caption := FloatToStrF(min, ffGeneral, 8, 4);
    minEdit.Top := 12;
    minEdit.Width := 92;
    minEdit.Left := PrefForm.Width - minEdit.Width - 8;
    minEdit.Parent:=PrefForm;
    //minLength
    maxLabel:=TLabel.create(PrefForm);
    maxLabel.Caption:= 'Maximum intensity';
    maxLabel.Left := 8;
    maxLabel.Top := 42;
    maxLabel.Parent:=PrefForm;
    maxEdit:=TEdit.create(PrefForm);
    maxEdit.Caption := FloatToStrF(max, ffGeneral, 8, 4);
    maxEdit.Top := 42;
    maxEdit.Width := 92;
    maxEdit.Left := PrefForm.Width - maxEdit.Width - 8;
    maxEdit.Parent:=PrefForm;
    //Precedence   ColorBarPrecedenceTracksNotOverlays
    ColorBarCheck:=TCheckBox.create(PrefForm);
    ColorBarCheck.Checked := ColorBarPrecedenceTracksNotOverlays;
    ColorBarCheck.Caption:='Colorbar for tracks, even if overlay loaded';
    ColorBarCheck.Left := 8;
    ColorBarCheck.Top := 72;
    ColorBarCheck.Parent:=PrefForm;
    //OK button
    OkBtn:=TButton.create(PrefForm);
    OkBtn.Caption:='OK';
    OkBtn.Top := 102;
    OkBtn.Width := 128;
    OkBtn.Left := PrefForm.Width - OkBtn.Width - 8;
    OkBtn.Parent:=PrefForm;
    OkBtn.ModalResult:= mrOK;
    {$IFNDEF Darwin} ScaleDPI(PrefForm, 96);{$ENDIF}
    {$IFDEF LCLCocoa}
    if gPrefs.DarkMode then SetFormDarkMode(PrefForm);
    {$ENDIF}
    PrefForm.ShowModal;
    min := StrToFloatDef(minEdit.Caption, min);
    max := StrToFloatDef(maxEdit.Caption, max);
    ColorBarPrecedenceTracksNotOverlays := ColorBarCheck.Checked;
    result :=  PrefForm.ModalResult = mrOK;
    FreeAndNil(PrefForm);
  end;*)
function  ScalarPref(var min, max, viewMin, viewmax: single; var ColorBarPrecedenceTracksNotOverlays, HideDark, HideBright: boolean; isScalarPerFiberColor: boolean): boolean;
var
    PrefForm: TForm;
    OkBtn: TButton;
    minLabel, maxLabel: TLabel;
    minEdit, maxEdit: TEdit;
    DarkCheck, BrightCheck, ColorBarCheck: TCheckBox;
begin
    PrefForm:=TForm.Create(nil);
    //PrefForm.SetBounds(100, 100, 520, 142);
    PrefForm.AutoSize := True;
    PrefForm.BorderWidth := 8;
    PrefForm.Caption:=format('Track  preferences (%g..%g)', [min, max]);
    PrefForm.Position := poScreenCenter;
    PrefForm.BorderStyle := bsDialog;
    //minEdit
    minLabel:=TLabel.create(PrefForm);
    minLabel.Caption:= 'Minimum intensity';
    minLabel.AutoSize := true;
    minLabel.AnchorSide[akTop].Side := asrTop;
    minLabel.AnchorSide[akTop].Control := PrefForm;
    minLabel.BorderSpacing.Top := 6;
    minLabel.AnchorSide[akLeft].Side := asrLeft;
    minLabel.AnchorSide[akLeft].Control := PrefForm;
    minLabel.BorderSpacing.Left := 6;
    minLabel.Parent:=PrefForm;
    minEdit:=TEdit.create(PrefForm);
    minEdit.Caption := FloatToStrF(viewMin, ffGeneral, 8, 4);
    minEdit.Constraints.MinWidth:= 128;
    minEdit.AutoSize := true;
    minEdit.AnchorSide[akTop].Side := asrTop;
    minEdit.AnchorSide[akTop].Control := PrefForm;
    minEdit.BorderSpacing.Top := 4;
    minEdit.AnchorSide[akLeft].Side := asrRight;
    minEdit.AnchorSide[akLeft].Control := minLabel;
    minEdit.BorderSpacing.Left := 6;
    minEdit.Left := PrefForm.Width - minEdit.Width - 8;
    minEdit.Parent:=PrefForm;
    //DarkCheck
    DarkCheck:=TCheckBox.create(PrefForm);
    DarkCheck.Checked := HideDark;
    DarkCheck.Caption:='Hide tracks darker than minimum';
    DarkCheck.AutoSize := true;
    DarkCheck.AnchorSide[akTop].Side := asrBottom;
    DarkCheck.AnchorSide[akTop].Control := minEdit;
    DarkCheck.BorderSpacing.Top := 6;
    DarkCheck.AnchorSide[akLeft].Side := asrLeft;
    DarkCheck.AnchorSide[akLeft].Control := PrefForm;
    DarkCheck.BorderSpacing.Left := 6;
    DarkCheck.enabled := isScalarPerFiberColor;
    if not isScalarPerFiberColor then
       DarkCheck.Checked := false;
    DarkCheck.Parent:=PrefForm;
    //maxEdit
    maxLabel:=TLabel.create(PrefForm);
    maxLabel.Caption:= 'Maximum intensity';
    maxLabel.AutoSize := true;
    maxLabel.AnchorSide[akTop].Side := asrBottom;
    maxLabel.AnchorSide[akTop].Control := DarkCheck;
    maxLabel.BorderSpacing.Top := 6;
    maxLabel.AnchorSide[akLeft].Side := asrLeft;
    maxLabel.AnchorSide[akLeft].Control := PrefForm;
    maxLabel.BorderSpacing.Left := 6;
    maxLabel.Parent:=PrefForm;
    maxEdit:=TEdit.create(PrefForm);
    maxEdit.Caption := FloatToStrF(viewMax, ffGeneral, 8, 4);
    maxEdit.Constraints.MinWidth:= 128;
    maxEdit.AutoSize := true;
    maxEdit.AnchorSide[akTop].Side := asrBottom;
    maxEdit.AnchorSide[akTop].Control := DarkCheck;
    maxEdit.BorderSpacing.Top := 4;
    maxEdit.AnchorSide[akLeft].Side := asrRight;
    maxEdit.AnchorSide[akLeft].Control := maxLabel;
    maxEdit.BorderSpacing.Left := 6;
    maxEdit.Parent:=PrefForm;
    //BrightCheck
    BrightCheck:=TCheckBox.create(PrefForm);
    BrightCheck.Checked := HideBright;
    BrightCheck.Caption:='Hide tracks brighter than maximum';
    BrightCheck.AutoSize := true;
    BrightCheck.AnchorSide[akTop].Side := asrBottom;
    BrightCheck.AnchorSide[akTop].Control := maxEdit;
    BrightCheck.BorderSpacing.Top := 6;
    BrightCheck.AnchorSide[akLeft].Side := asrLeft;
    BrightCheck.AnchorSide[akLeft].Control := PrefForm;
    BrightCheck.BorderSpacing.Left := 6;
    BrightCheck.Enabled := isScalarPerFiberColor;
    if not isScalarPerFiberColor then
       BrightCheck.Checked := false;
    BrightCheck.Parent:=PrefForm;
    //ColorBarCheck
    ColorBarCheck:=TCheckBox.create(PrefForm);
    ColorBarCheck.Checked := ColorBarPrecedenceTracksNotOverlays;
    ColorBarCheck.Caption:='Colorbar for tracks, even if overlay loaded';
    ColorBarCheck.AutoSize := true;
    ColorBarCheck.AnchorSide[akTop].Side := asrBottom;
    ColorBarCheck.AnchorSide[akTop].Control := BrightCheck;
    ColorBarCheck.BorderSpacing.Top := 6;
    ColorBarCheck.AnchorSide[akLeft].Side := asrLeft;
    ColorBarCheck.AnchorSide[akLeft].Control := PrefForm;
    ColorBarCheck.BorderSpacing.Left := 6;
    ColorBarCheck.Parent:=PrefForm;
    //OK button
    OkBtn:=TButton.create(PrefForm);
    OkBtn.Caption:='OK';
    //OkBtn.Top := 102;
    //OkBtn.Width := 128;
    //OkBtn.Left := PrefForm.Width - OkBtn.Width - 8;
    OkBtn.AutoSize := true;
    OkBtn.AnchorSide[akTop].Side := asrBottom;
    OkBtn.AnchorSide[akTop].Control := ColorBarCheck;
    OkBtn.BorderSpacing.Top := 6;
    OkBtn.AnchorSide[akLeft].Side := asrCenter;
    OkBtn.AnchorSide[akLeft].Control := PrefForm;
    OkBtn.Constraints.MinWidth:= 64;
    OkBtn.Parent:=PrefForm;
    OkBtn.ModalResult:= mrOK;
    {$IFDEF LCLCocoa}
    if gPrefs.DarkMode then SetFormDarkMode(PrefForm);
    {$ENDIF}
    PrefForm.ShowModal;
    viewMin := StrToFloatDef(minEdit.Caption, viewMin);
    HideBright := BrightCheck.Checked;
    HideDark := DarkCheck.Checked;
    viewMax := StrToFloatDef(maxEdit.Caption, viewMax);
    ColorBarPrecedenceTracksNotOverlays := ColorBarCheck.Checked;
    result :=  PrefForm.ModalResult = mrOK;
    FreeAndNil(PrefForm);
  end;


procedure TGLForm1.TrackScalarRangeBtnClick(Sender: TObject);
begin
 if (gTrack.scalarSelected < 0) or (gTrack.scalarSelected >= length(gTrack.scalars)) then exit;
 ScalarPref(gTrack.scalars[gTrack.scalarSelected].mn, gTrack.scalars[gTrack.scalarSelected].mx, gTrack.scalars[gTrack.scalarSelected].mnView, gTrack.scalars[gTrack.scalarSelected].mxView, gPrefs.ColorBarPrecedenceTracksNotOverlays, gPrefs.HideDarkTracks, gPrefs.HideBrightTracks,gTrack.HasScalarPerFiberColor);
 gTrack.isRebuildList:= true;
 gnLUT := -1; //refresh colorbar
 GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.SurfaceAppearanceChange(Sender: TObject);
begin
  Memo1.Lines.Clear;
  GLForm1.Memo1.Lines.Add(gShader.note);
  if OcclusionTrack.Visible then begin
     Memo1.Lines.Add(format('Ambient Occlusion %d',[OcclusionTrack.position]));
     Memo1.Lines.Add(format('XRay Background %d Overlay %d',[meshAlphaTrack.position, MeshBlendTrack.position]));
  end;
  Memo1.Lines.Add(format('Light Elevation %d Azimuth %d',[LightElevTrack.position, LightAziTrack.position]));
  ReportUniformChange(Sender);
  GLboxRequestUpdate(Sender); //++ 2018 : required for dynamic light position change
  //GLbox.Invalidate; //-- 2017

end;

function IsDigit (letter : char) : boolean;
begin
  result := ((letter <= '9') and (letter >= '0'));
end;

function HasDigit (var lS: string): boolean;
//do not attempt to convert '-', '.', or '-.' as a number...
var
   lI,lLen: integer;
begin
     result := false;
     lLen := length (lS);
     if lLen < 1 then
        exit;
     for lI := 1 to lLen do begin
         if lS[lI] in ['0'..'9'] then begin
            result := true;
            exit;
         end;
     end;
end;

procedure TGLForm1.OverlayVisible(lOverlay: integer; lVisible: integer);
begin
  if (lOverlay > gMesh.OpenOverlays) or (lOverlay < 1) then
    exit;
  if (lVisible < kLUTinvisible) or (lVisible > kLUTopaque) then
     gMesh.Overlay[lOverlay].OpacityPercent := kLUTopaque
  else
     gMesh.Overlay[lOverlay].OpacityPercent := lVisible;
  UpdateLayerBox(false);
  OverlayTimerStart;
end;

procedure TGLForm1.OverlayInvert(lOverlay: integer; lInvert: boolean);
begin
  if (lOverlay > gMesh.OpenOverlays) or (lOverlay < 1) then
    exit;
  gMesh.Overlay[lOverlay].LUTinvert := lInvert;
  UpdateLayerBox(false);
  gMesh.overlay[lOverlay].LUT := UpdateTransferFunction (gMesh.Overlay[lOverlay].LUTindex, gMesh.Overlay[lOverlay].LUTinvert);
  OverlayTimerStart;
end;

procedure TGLForm1.UpdateFont(initialSetup: boolean);
var
  p,f: string;
begin
     p := (ClutDir+pathdelim+gPrefs.FontName+'.png');
     f := (ClutDir+pathdelim+gPrefs.FontName+'.json');
     if (gPrefs.FontName = '') or (not fileexistsf(p)) or (not fileexistsf(f)) then begin
       gPrefs.FontName := '';
       p := '';
     end;
     if initialSetup then begin
       gClrbar:= TGLClrbar.Create(p, GLBox);
       if (gPrefs.ColorbarSize < 0.01) or (gPrefs.ColorbarSize > 0.3) then
          gPrefs.ColorbarSize := gClrbar.SizeFraction;
       gClrbar.SizeFraction := gPrefs.ColorbarSize;
     end else begin
         gClrBar.ChangeFontName(p, GLBox);
     end;
end;

{$IFDEF FPC}
function latestGitRelease(url: string): string;
//Returns string for latest release (error will return empty string)
//example
// latestGitRelease('https://api.github.com/repos/rordenlab/dcm2niix/releases/latest');
//will return
// "v1.0.20171204"
const
     key = '"tag_name":"';
var
  s, e: integer;
  cli: TFPHTTPClient;  //uses fphttpclient
begin
  result := '';
  cli := TFPHTTPClient.Create(nil);
  cli.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
  try
    try
      result := Cli.Get(url);
    except
      result := '';
    end;
  finally
    cli.free
  end;
  if length(result) < 1 then exit;
  s := posex(key, result);
  if s < 1 then begin
     result := '';
     exit;
  end;
  s := s+length(key);
  e:= posex('"', result, s);
  if e < 1 then begin
     result := '';
     exit;
  end;
  result := copy(result, s, e-s);
end;

procedure TGLForm1.CheckForUpdates(Sender: TObject);
const
     kBase = '/neurolabusc/surf-ice/releases/latest';
     kUrl = 'https://github.com' + kBase;
     kApi = 'https://api.github.com/repos' + kBase;
var
  s: string;
  latest, current: integer;
begin
     s := latestGitRelease(kApi);
     if length(s) < 8 then begin  //last 8 digits are date: v.1.0.20170101
        showmessage('Unable to detect latest version: are you connected to the web and do you have libssl installed? '+kApi);
        exit;
     end;
     if CompareText(s, kVers) = 0 then begin
        showmessage('You are running the latest release '+kVers);
        exit;
     end;
     latest := strtointdef(RightStr(s,8),0);
     current := strtointdef(RightStr(kVers,8),0);
     if current > latest then
        showmessage('You are running a beta release '+kVers+', the latest stable release is '+s)
     else
         showmessage('You are running an old release '+kVers+', the latest stable release is '+s+'. Visit '+kUrl );
end;
{$ELSE}
procedure TGLForm1.CheckForUpdates(Sender: TObject);
begin
    //not used in Windows
end;
{$ENDIF}

procedure TGLForm1.PrefMenuClick(Sender: TObject);
var
  PrefForm: TForm;
  OkBtn, AdvancedBtn: TButton;
  {$IFDEF LCLCocoa} DarkModeCheck, RetinaCheck,{$ENDIF}
  BlackDefaultBackgroundCheck, BitmapAlphaCheck, SmoothVoxelwiseDataCheck, TracksAreTubesCheck: TCheckBox;
  bmpEdit: TEdit;
  s: string;
  Quality: integer;
  searchRec: TSearchRec;
  FontCombo, ZDimIsUpCombo, QualityCombo, SaveAsFormatCombo: TComboBox;
  bmpLabel, QualityLabel: TLabel;
  isFontChanged, isAdvancedPrefs {$IFDEF LCLCocoa}, isDarkModeChanged, isRetinaChanged {$ENDIF} : boolean;
begin
  PrefForm:=TForm.Create(nil);
  //PrefForm.SetBounds(100, 100, 520, 422);
  PrefForm.AutoSize := True;
  PrefForm.BorderWidth := 8;
  PrefForm.Caption:='Preferences';
  PrefForm.Position := poScreenCenter;
  PrefForm.BorderStyle := bsDialog;
    //Bitmap Alpha
  BitmapAlphaCheck:=TCheckBox.create(PrefForm);
  BitmapAlphaCheck.Checked := gPrefs.ScreenCaptureTransparentBackground;
  BitmapAlphaCheck.Caption:='Background transparent in bitmaps';
  //BitmapAlphaCheck.Left := 8;
  //BitmapAlphaCheck.Top := 8;
  BitmapAlphaCheck.AutoSize := true;
  BitmapAlphaCheck.AnchorSide[akTop].Side := asrTop;
  BitmapAlphaCheck.AnchorSide[akTop].Control := PrefForm;
  BitmapAlphaCheck.BorderSpacing.Top := 6;
  BitmapAlphaCheck.AnchorSide[akLeft].Side := asrLeft;
  BitmapAlphaCheck.AnchorSide[akLeft].Control := PrefForm;
  BitmapAlphaCheck.BorderSpacing.Left := 6;
  BitmapAlphaCheck.Parent:=PrefForm;
  //SmoothVoxelwiseData
  SmoothVoxelwiseDataCheck:=TCheckBox.create(PrefForm);
  SmoothVoxelwiseDataCheck.Checked := gPrefs.SmoothVoxelwiseData;
  SmoothVoxelwiseDataCheck.Caption:='Smooth voxel-based images';
  //SmoothVoxelwiseDataCheck.Left := 8;
  //SmoothVoxelwiseDataCheck.Top := 38;
  SmoothVoxelwiseDataCheck.AutoSize := true;
  SmoothVoxelwiseDataCheck.AnchorSide[akTop].Side := asrBottom;
  SmoothVoxelwiseDataCheck.AnchorSide[akTop].Control := BitmapAlphaCheck;
  SmoothVoxelwiseDataCheck.BorderSpacing.Top := 6;
  SmoothVoxelwiseDataCheck.AnchorSide[akLeft].Side := asrLeft;
  SmoothVoxelwiseDataCheck.AnchorSide[akLeft].Control := PrefForm;
  SmoothVoxelwiseDataCheck.BorderSpacing.Left := 6;
  SmoothVoxelwiseDataCheck.Parent:=PrefForm;
  //TracksAreTubes
  TracksAreTubesCheck:=TCheckBox.create(PrefForm);
  TracksAreTubesCheck.Checked := gPrefs.TracksAreTubes;
  TracksAreTubesCheck.Caption:='Better (but slower) tracks';
  //TracksAreTubesCheck.Left := 8;
  //TracksAreTubesCheck.Top := 68;
  TracksAreTubesCheck.AutoSize := true;
  TracksAreTubesCheck.AnchorSide[akTop].Side := asrBottom;
  TracksAreTubesCheck.AnchorSide[akTop].Control := SmoothVoxelwiseDataCheck;
  TracksAreTubesCheck.BorderSpacing.Top := 6;
  TracksAreTubesCheck.AnchorSide[akLeft].Side := asrLeft;
  TracksAreTubesCheck.AnchorSide[akLeft].Control := PrefForm;
  TracksAreTubesCheck.BorderSpacing.Left := 6;
  TracksAreTubesCheck.Parent:=PrefForm;
  //ShaderForBackgroundOnly
  (*ShaderForBackgroundOnlyCombo := TComboBox.create(PrefForm);
  ShaderForBackgroundOnlyCombo.Items.Add('Tracks, nodes and overlays use fixed shader');
  ShaderForBackgroundOnlyCombo.Items.Add('Tracks, nodes and overlays use background shader');
  if (gPrefs.ShaderForBackgroundOnly) then
     ShaderForBackgroundOnlyCombo.ItemIndex := 0
  else
      ShaderForBackgroundOnlyCombo.ItemIndex := 1;
  ShaderForBackgroundOnlyCombo.Left := 8;
  ShaderForBackgroundOnlyCombo.Top := 98;
  ShaderForBackgroundOnlyCombo.Width := PrefForm.Width -16;
  ShaderForBackgroundOnlyCombo.Style := csDropDownList;
  ShaderForBackgroundOnlyCombo.Parent:=PrefForm; *)
  //ZDimIsUp
  ZDimIsUpCombo := TComboBox.create(PrefForm);
  ZDimIsUpCombo.Items.Add('Z-dimension is up (Neuroimaging/Talairach)');
  ZDimIsUpCombo.Items.Add('Y-dimension is up (Blender/OpenGL)');
  if (gPrefs.ZDimIsUp) then
     ZDimIsUpCombo.ItemIndex := 0
  else
      ZDimIsUpCombo.ItemIndex := 1;
  //ZDimIsUpCombo.Left := 8;
  //ZDimIsUpCombo.Top := 128;
  //ZDimIsUpCombo.Width := PrefForm.Width -16;
  ZDimIsUpCombo.Style := csDropDownList;
  ZDimIsUpCombo.Constraints.MinWidth:= 320;
  ZDimIsUpCombo.AutoSize := true;
  ZDimIsUpCombo.AnchorSide[akTop].Side := asrBottom;
  ZDimIsUpCombo.AnchorSide[akTop].Control := TracksAreTubesCheck;
  ZDimIsUpCombo.BorderSpacing.Top := 6;
  ZDimIsUpCombo.AnchorSide[akLeft].Side := asrLeft;
  ZDimIsUpCombo.AnchorSide[akLeft].Control := PrefForm;
  ZDimIsUpCombo.BorderSpacing.Left := 6;
  ZDimIsUpCombo.Parent:=PrefForm;
  //SinglePass
  (*MultiPassRenderingCheck:=TCheckBox.create(PrefForm);
  MultiPassRenderingCheck.Checked := gPrefs.MultiPassRendering;
  MultiPassRenderingCheck.Caption:='Better rendering (slower)';
  MultiPassRenderingCheck.Left := 8;
  MultiPassRenderingCheck.Top := 128;
  MultiPassRenderingCheck.Parent:=PrefForm; *)
  //Smooth
  Quality := gPrefs.RenderQuality;
  if (Quality = kRenderBetter) and (gPrefs.OcclusionAmount > 0) then
     Quality := Quality + 1; //0=Poor, 1=Better, 2=Better+Occlusion
  QualityCombo:=TComboBox.create(PrefForm);
  //QualityCombo.Left := 8;
  //QualityCombo.Top := 158;
  //QualityCombo.Width := PrefForm.Width -16;
  QualityCombo.Items.Add('Quality: Poor (old hardware)');
  QualityCombo.Items.Add('Quality: Fair (no ambient occlusion by default)');
  QualityCombo.Items.Add('Quality: Better');
  //QualityCombo.Items.Add('Quality: Best');
  QualityCombo.ItemIndex:= Quality;
  QualityCombo.Style := csDropDownList;
  QualityCombo.Constraints.MinWidth:= 320;
  QualityCombo.AutoSize := true;
  QualityCombo.AnchorSide[akTop].Side := asrBottom;
  QualityCombo.AnchorSide[akTop].Control := ZDimIsUpCombo;
  QualityCombo.BorderSpacing.Top := 6;
  QualityCombo.AnchorSide[akLeft].Side := asrLeft;
  QualityCombo.AnchorSide[akLeft].Control := PrefForm;
  QualityCombo.BorderSpacing.Left := 6;
  QualityCombo.Parent:=PrefForm;
  //gPrefs.SupportBetterRenderQuality := true;
  if not gPrefs.SupportBetterRenderQuality then begin
      QualityCombo.Visible := false;
      QualityLabel:=TLabel.create(PrefForm);
      //QualityLabel.Left := 8;
      //QualityLabel.Top := 158;
      //QualityLabel.Width := PrefForm.Width -16;
      QualityLabel.AutoSize := true;
      QualityLabel.AnchorSide[akTop].Side := asrBottom;
      QualityLabel.AnchorSide[akTop].Control := ZDimIsUpCombo;
      QualityLabel.BorderSpacing.Top := 6;
      QualityLabel.AnchorSide[akLeft].Side := asrLeft;
      QualityLabel.AnchorSide[akLeft].Control := PrefForm;
      QualityLabel.BorderSpacing.Left := 6;
      QualityLabel.Caption := 'NOTE: Hardware only supports poor rendering.';
      QualityLabel.Parent:=PrefForm;
  end;
  //SingleShader
  bmpLabel:=TLabel.create(PrefForm);
  //bmpLabel.Left := 8;
  //bmpLabel.Top := 188;
  //bmpLabel.Width := PrefForm.Width - 86;
  bmpLabel.Caption := 'Bitmap zoom (large values create huge images)';
  bmpLabel.AutoSize := true;
  bmpLabel.AnchorSide[akTop].Side := asrBottom;
  if not gPrefs.SupportBetterRenderQuality then
     bmpLabel.AnchorSide[akTop].Control := QualityLabel
  else
      bmpLabel.AnchorSide[akTop].Control := QualityCombo;
  bmpLabel.BorderSpacing.Top := 6;
  bmpLabel.AnchorSide[akLeft].Side := asrLeft;
  bmpLabel.AnchorSide[akLeft].Control := PrefForm;
  bmpLabel.BorderSpacing.Left := 6;
  bmpLabel.Parent:=PrefForm;
  //bmp edit
  bmpEdit := TEdit.Create(PrefForm);
  //bmpEdit.Left := PrefForm.Width - 76;
  //bmpEdit.Top := 188;
  //bmpEdit.Width := 60;
  bmpEdit.Text := inttostr(gPrefs.ScreenCaptureZoom);
  bmpEdit.Constraints.MinWidth:= 128;
  bmpEdit.AutoSize := true;
  bmpEdit.AnchorSide[akTop].Side := asrCenter;
  bmpEdit.AnchorSide[akTop].Control := bmpLabel;
  bmpEdit.BorderSpacing.Top := 4;
  bmpEdit.AnchorSide[akLeft].Side := asrRight;
  bmpEdit.AnchorSide[akLeft].Control := bmpLabel;
  bmpEdit.BorderSpacing.Left := 6;
  bmpEdit.Parent:=PrefForm;
  //Select Font
  FontCombo:=TComboBox.create(PrefForm);
  //FontCombo.Left := 8;
  //FontCombo.Top := 218;
  //FontCombo.Width := PrefForm.Width -16;
  FontCombo.Items.Add('Default Font');
  FontCombo.ItemIndex:= 0;
  if FindFirst(ClutDir+pathdelim+'*.json', faAnyFile, searchRec) = 0 then begin
    repeat
      s :=ParseFileName(ExtractFileName(searchRec.Name));
      if (length(s) > 1) and (s[1] <> '.') and (fileexists(ClutDir+pathdelim+s+'.png')) then begin
         FontCombo.Items.Add(s);
         if (s = gPrefs.FontName) then
            FontCombo.ItemIndex := FontCombo.Items.Count-1;
      end;
    until (FindNext(searchRec) <> 0);
  end; //find fonts
  FindClose(searchRec);
  FontCombo.Style := csDropDownList;
  FontCombo.AutoSize := true;
  FontCombo.Constraints.MinWidth:= 320;
  FontCombo.AnchorSide[akTop].Side := asrBottom;
  FontCombo.AnchorSide[akTop].Control := bmpEdit;
  FontCombo.BorderSpacing.Top := 6;
  FontCombo.AnchorSide[akLeft].Side := asrLeft;
  FontCombo.AnchorSide[akLeft].Control := PrefForm;
  FontCombo.BorderSpacing.Left := 6;
  FontCombo.Parent:=PrefForm;
  //SaveAsFormatCombo
  SaveAsFormatCombo:=TComboBox.create(PrefForm);
  //SaveAsFormatCombo.Left := 8;
  //SaveAsFormatCombo.Top := 248;
  //SaveAsFormatCombo.Width := PrefForm.Width -16;
  SaveAsFormatCombo.Items.Add('Save mesh as: OBJ (Widely supported)');
  SaveAsFormatCombo.Items.Add('Save mesh as: GIfTI (Neuroimaging)');
  SaveAsFormatCombo.Items.Add('MZ3 (Small and fast)');
  SaveAsFormatCombo.Items.Add('PLY (Widely supported)');
  //QualityCombo.Items.Add('Quality: Best');
  SaveAsFormatCombo.ItemIndex:= gPrefs.SaveAsFormat;
  SaveAsFormatCombo.Constraints.MinWidth:= 320;
  SaveAsFormatCombo.Style := csDropDownList;
  SaveAsFormatCombo.AutoSize := true;
  SaveAsFormatCombo.AnchorSide[akTop].Side := asrBottom;
  SaveAsFormatCombo.AnchorSide[akTop].Control := FontCombo;
  SaveAsFormatCombo.BorderSpacing.Top := 6;
  SaveAsFormatCombo.AnchorSide[akLeft].Side := asrLeft;
  SaveAsFormatCombo.AnchorSide[akLeft].Control := PrefForm;
  SaveAsFormatCombo.BorderSpacing.Left := 6;
  SaveAsFormatCombo.Parent:=PrefForm;
  //
  BlackDefaultBackgroundCheck:=TCheckBox.create(PrefForm);
  BlackDefaultBackgroundCheck.Checked := gPrefs.BlackDefaultBackground;
  BlackDefaultBackgroundCheck.Caption:='Black Default Background';
  //BlackDefaultBackgroundCheck.Left := 8;
  //BlackDefaultBackgroundCheck.Top := 278;
  BlackDefaultBackgroundCheck.AutoSize := true;
  BlackDefaultBackgroundCheck.AnchorSide[akTop].Side := asrBottom;
  BlackDefaultBackgroundCheck.AnchorSide[akTop].Control := SaveAsFormatCombo;
  BlackDefaultBackgroundCheck.BorderSpacing.Top := 6;
  BlackDefaultBackgroundCheck.AnchorSide[akLeft].Side := asrLeft;
  BlackDefaultBackgroundCheck.AnchorSide[akLeft].Control := PrefForm;
  BlackDefaultBackgroundCheck.BorderSpacing.Left := 6;

  BlackDefaultBackgroundCheck.Parent:=PrefForm;
  {$IFDEF LCLCocoa}
  RetinaCheck:=TCheckBox.create(PrefForm);
  RetinaCheck.Checked := gPrefs.RetinaDisplay;
  RetinaCheck.Caption:='Retina display (better but slower)';
  //RetinaCheck.Left := 8;
  //RetinaCheck.Top := 308;
  RetinaCheck.AutoSize := true;
  RetinaCheck.AnchorSide[akTop].Side := asrBottom;
  RetinaCheck.AnchorSide[akTop].Control := BlackDefaultBackgroundCheck;
  RetinaCheck.BorderSpacing.Top := 6;
  RetinaCheck.AnchorSide[akLeft].Side := asrLeft;
  RetinaCheck.AnchorSide[akLeft].Control := PrefForm;
  RetinaCheck.BorderSpacing.Left := 6;
  RetinaCheck.Parent:=PrefForm;
  //DarkMode
  DarkModeCheck:=TCheckBox.create(PrefForm);
  DarkModeCheck.visible := isDarkModeSupported;
  DarkModeCheck.Checked := gPrefs.DarkMode;
  DarkModeCheck.Caption:='Dark Mode';
  //DarkModeCheck.Left := 8;
  //DarkModeCheck.Top := 338;
  DarkModeCheck.AutoSize := true;
  DarkModeCheck.AnchorSide[akTop].Side := asrBottom;
  DarkModeCheck.AnchorSide[akTop].Control := RetinaCheck;
  DarkModeCheck.BorderSpacing.Top := 6;
  DarkModeCheck.AnchorSide[akLeft].Side := asrLeft;
  DarkModeCheck.AnchorSide[akLeft].Control := PrefForm;
  DarkModeCheck.BorderSpacing.Left := 6;
  DarkModeCheck.Parent:=PrefForm;
  if gPrefs.DarkMode then SetFormDarkMode(PrefForm);
  {$ENDIF}
    AdvancedBtn:=TButton.create(PrefForm);
  AdvancedBtn.Caption:='Advanced';
  //AdvancedBtn.Left := PrefForm.Width - 256;
  //AdvancedBtn.Width:= 100;
  //AdvancedBtn.Top := 378;
  AdvancedBtn.Constraints.MinWidth:= 128;
  AdvancedBtn.AutoSize := true;
  AdvancedBtn.AnchorSide[akTop].Side := asrBottom;
  {$IFDEF LCLCocoa}
  AdvancedBtn.AnchorSide[akTop].Control := DarkModeCheck;
  {$ELSE}
  AdvancedBtn.AnchorSide[akTop].Control := BlackDefaultBackgroundCheck;
  {$ENDIF}
  AdvancedBtn.BorderSpacing.Top := 4;
  AdvancedBtn.AnchorSide[akLeft].Side := asrLeft;
  AdvancedBtn.AnchorSide[akLeft].Control := PrefForm;
  AdvancedBtn.BorderSpacing.Left := 120;
  AdvancedBtn.Parent:=PrefForm;
  AdvancedBtn.ModalResult:= mrYesToAll;

  //OK button
  OkBtn:=TButton.create(PrefForm);
  OkBtn.Caption:='OK';
  //OkBtn.Left := PrefForm.Width - 128;
  //OkBtn.Width:= 100;
  //OkBtn.Top := 378;
  OkBtn.AutoSize := true;
  OkBtn.Constraints.MinWidth:= 128;
  OkBtn.AnchorSide[akTop].Side := asrTop;
  OkBtn.AnchorSide[akTop].Control := AdvancedBtn;
  OkBtn.BorderSpacing.Top := 0;
  OkBtn.AnchorSide[akLeft].Side := asrRight;
  OkBtn.AnchorSide[akLeft].Control := AdvancedBtn;
  OkBtn.BorderSpacing.Left := 60;
  OkBtn.Parent:=PrefForm;
  OkBtn.ModalResult:= mrOK;


  PrefForm.ShowModal;
  if (PrefForm.ModalResult <> mrOK) and (PrefForm.ModalResult <> mrYesToAll) then begin
    FreeAndNil(PrefForm);
  	exit; //if user closes window with out pressing "OK"
  end;
  {$IFDEF LCLCocoa}
  isRetinaChanged := gPrefs.RetinaDisplay <> RetinaCheck.Checked;
  gPrefs.RetinaDisplay := RetinaCheck.Checked;
  isDarkModeChanged := gPrefs.DarkMode <> DarkModeCheck.Checked;
  gPrefs.DarkMode := DarkModeCheck.Checked;
  {$ENDIF}
  s := '';
  if FontCombo.ItemIndex > 0 then
     s := FontCombo.Items[FontCombo.ItemIndex];
  isFontChanged := (s <> gPrefs.FontName);
  gPrefs.FontName := s;
  gPrefs.ScreenCaptureTransparentBackground :=  BitmapAlphaCheck.Checked;
  gPrefs.SmoothVoxelwiseData := SmoothVoxelwiseDataCheck.Checked;
  gPrefs.BlackDefaultBackground := BlackDefaultBackgroundCheck.Checked;
  gPrefs.ScreenCaptureZoom:= strtointdef(bmpEdit.Text,1);
  (*if ShaderForBackgroundOnlyCombo.ItemIndex = 1 then
     gPrefs.ShaderForBackgroundOnly := false
  else
      gPrefs.ShaderForBackgroundOnly := true; *)
  if ZDimIsUpCombo.ItemIndex = 1 then
     gPrefs.ZDimIsUp := false
  else
      gPrefs.ZDimIsUp := true;
  gMesh.isZDimIsUp := gPrefs.ZDimIsUp;
  gNode.isZDimIsUp := gPrefs.ZDimIsUp;
  if isFontChanged then
       GLForm1.UpdateFont(false);
  //gPrefs.SaveAsFormat := SaveAsCombo.ItemIndex;
  gPrefs.SaveAsFormat := SaveAsFormatCombo.ItemIndex;
  if (QualityCombo.ItemIndex) <> Quality then begin
     if QualityCombo.ItemIndex = 0 then
        gPrefs.RenderQuality := kRenderPoor
     else
        gPrefs.RenderQuality := kRenderBetter;
     if QualityCombo.ItemIndex = 2 then //only for best quality
        gPrefs.OcclusionAmount := 25
     else
         gPrefs.OcclusionAmount := 0;
     MultiPassRenderingToolsUpdate;
     GLForm1.ResetMenuClick(nil);
     GLBoxRequestUpdate(Sender);
  end;
  if (gPrefs.TracksAreTubes <> TracksAreTubesCheck.Checked) then begin
    gPrefs.TracksAreTubes := TracksAreTubesCheck.Checked;
    gTrack.isTubes:= gPrefs.TracksAreTubes;
    gTrack.isRebuildList:= true;

  end;
  isAdvancedPrefs := (PrefForm.ModalResult = mrYesToAll);
  FreeAndNil(PrefForm);
  {$IFDEF LCLCocoa}
  if isRetinaChanged then
    SetRetina;//GLBox.WantsBestResolutionOpenGLSurface:=gPrefs.RetinaDisplay;
  if isDarkModeChanged then
    SetDarkMode;
  {$ENDIF}
      GLBoxRequestUpdate(Sender);
  if  isAdvancedPrefs then
     Quit2TextEditor;
end; // PrefMenuClick()
procedure TGLForm1.QuickColorClick(Sender: TObject);
begin
  case (sender as TMenuItem).tag of
       1: gPrefs.ObjColor:= RGBToColor(210,148,148); //red
       2: gPrefs.ObjColor:= RGBToColor(128,162,128); //green
       3: gPrefs.ObjColor:= RGBToColor(167,171,253); //blue
       4: gPrefs.ObjColor:= RGBToColor(226,171,0); //gold
       else gPrefs.ObjColor:= RGBToColor(192,192,192); //gray
  end;
  //{$IFDEF COREGL}
  gMesh.isRebuildList := true;
  //{$ENDIF}
  GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.ResetMenuClick(Sender: TObject);
begin
     if gPrefs.BlackDefaultBackground then
         gPrefs.BackColor := RGBToColor(0,0,0)
     else
         gPrefs.BackColor := RGBToColor(255,255,255);
     //gPrefs.Colorbar := true;
     TransBlackClrbarMenu.Checked:=true;
     gPrefs.ScreenPan.X := 0; gPrefs.ScreenPan.Y := 0; gPrefs.ScreenPan.Z := 0;
     gDistance := 1;
     gElevation := 20;
     gAzimuth := 250;
     Transparency0.Click;
     gPrefs.ShaderForBackgroundOnly:= true;
     ShaderForBackgroundOnlyCheck.Checked := gPrefs.ShaderForBackgroundOnly;
     gPrefs.isFlipMeshOverlay:= false;
     gPrefs.AdditiveOverlay:= false;
     gMesh.isAdditiveOverlay:= gPrefs.AdditiveOverlay;
     setlength(gMesh.atlasHideFilter,0);
     setlength(gMesh.atlasTransparentFilter,0);
     AdditiveOverlayMenu.Checked:= gPrefs.AdditiveOverlay;
     gPrefs.ObjColor:= RGBToColor(192,192,192);
     //set nodes/edges
     NodeScaleTrack.Position := 20;
     EdgeScaleTrack.Position:= 37;
     //set tracks
     TrackLengthTrack.Position:= 20;
     TrackWidthTrack.Position := 2; ;  //12 for 666Demo
     TrackDitherTrack.Position := 3;
     //mesh colors
     MeshSaturationTrack.Position := 100;
     MeshTransparencyTrack.Position:= 100;
     //clipping
     ClipTrack.Position := 0;
     ClipAziTrack.Position := 180;
     ClipElevTrack.Position := 0;
     //set shaders
     OcclusionTrack.Position := gPrefs.OcclusionAmount;
     MeshAlphaTrack.Position := 100;
     MeshBlendTrack.Position:= 0;
     LightElevTrack.Position:= 25;
     LightAziTrack.Position := 0;
     if (ShaderDrop.ItemIndex <> 0) then begin
        ShaderDrop.ItemIndex:= 0;
        ShaderDropChange(Sender);
     end;
     {$IFDEF MATCAP}
     if (MatCapDrop.Items.Count > 1) and (MatCapDrop.ItemIndex <> 0) then begin
        MatCapDrop.ItemIndex := 0;
        MatCapDropChange(Sender);

     end;
     {$ENDIF}
end;

procedure TGLForm1.RestrictEdgeMenuClick(Sender: TObject);
begin
 gNode.nodePrefs.isNoPosEdge:=false;
 gNode.nodePrefs.isNoNegEdge:=false;
 if (sender as TMenuItem).tag = 1 then
    gNode.nodePrefs.isNoNegEdge:=true;
 if (sender as TMenuItem).tag = 2 then
    gNode.nodePrefs.isNoPosEdge:=true;
 gNode.isRebuildList := true;
 GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.RestrictHideNodesWithoutEdgesClick(Sender: TObject);
begin
 gNode.nodePrefs.isNoNodeWithoutEdge := RestrictHideNodesWithoutEdges.checked;
 if length(gNode.nodes) < 1 then exit;
 gNode.isRebuildList := true;
 GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.RestrictMenuClick(Sender: TObject);
begin
 gNode.nodePrefs.isNoLeftNodes:=false;
 gNode.nodePrefs.isNoRightNodes:=false;
 if (sender as TMenuItem).tag = 1 then
    gNode.nodePrefs.isNoRightNodes:=true;
 if (sender as TMenuItem).tag = 2 then
    gNode.nodePrefs.isNoLeftNodes:=true;
 if length(gNode.nodes) < 1 then exit;
 gNode.isRebuildList := true;
 GLBoxRequestUpdate(Sender);
end;
(*86
function GetFloat(prompt: string; min,def,max: extended): extended;
var
    PrefForm: TForm;
    OkBtn: TButton;
    promptLabel: TLabel;
    valEdit: TEdit;
begin
  PrefForm:=TForm.Create(nil);
  PrefForm.SetBounds(100, 100, 640, 112);
  PrefForm.Caption:='Value required';
  PrefForm.Position := poScreenCenter;
  PrefForm.BorderStyle := bsDialog;
  //label
  promptLabel:=TLabel.create(PrefForm);
  promptLabel.Caption:= prompt;
  if (min < max) then
     promptLabel.Caption:= format('%s (range %g..%g)', [prompt, min, max]);
  promptLabel.Left := 8;
  promptLabel.Top := 12;
  promptLabel.Parent:=PrefForm;
  //edit
  valEdit:=TEdit.create(PrefForm);
  valEdit.Caption := FloatToStrF(def, ffGeneral, 8, 4);
  valEdit.Top := 42;
  valEdit.Width := PrefForm.Width - 16;
  valEdit.Left :=  8;
  valEdit.Parent:=PrefForm;
  //OK button
  OkBtn:=TButton.create(PrefForm);
  OkBtn.Caption:='OK';
  OkBtn.Top := 78;
  OkBtn.Width := 128;
  OkBtn.Left := PrefForm.Width - OkBtn.Width - 8;
  OkBtn.Parent:=PrefForm;
  OkBtn.ModalResult:= mrOK;
  {$IFNDEF Darwin} ScaleDPI(PrefForm, 96);{$ENDIF}
  {$IFDEF LCLCocoa}
  if gPrefs.DarkMode then SetFormDarkMode(PrefForm);
  {$ENDIF}
  PrefForm.ShowModal;
  result := def;
  if (PrefForm.ModalResult = mrOK) then begin
    result := StrToFloatDef(valEdit.Caption, def);
    if (min < max) and (result < min) then
      result := min;
    if (min < max) and (result > max) then
      result := max;
  end;
  FreeAndNil(PrefForm);
end; //GetFloat()*)

function GetFloat(prompt: string; min,def,max: double): double;
var
    PrefForm: TForm;
    CancelBtn,OkBtn: TButton;
    promptLabel: TLabel;
    valEdit: TEdit;
begin
  PrefForm:=TForm.Create(nil);
  //PrefForm.SetBounds(100, 100, 512, 212);
  PrefForm.AutoSize := True;
  PrefForm.BorderWidth := 8;
  PrefForm.Caption:='Value required';
  PrefForm.Position := poScreenCenter;
  PrefForm.BorderStyle := bsDialog;
  //label
  promptLabel:=TLabel.create(PrefForm);
  promptLabel.Caption:= prompt;
  if (min < max) then
     promptLabel.Caption:= format('%s (range %0.3g..%0.3g)', [prompt, min, max]);
  //promptLabel.Left := 8;
  //promptLabel.Top := 12;
  promptLabel.AutoSize := true;
  promptLabel.AnchorSide[akTop].Side := asrTop;
  promptLabel.AnchorSide[akTop].Control := PrefForm;
  promptLabel.BorderSpacing.Top := 6;
  promptLabel.AnchorSide[akLeft].Side := asrLeft;
  promptLabel.AnchorSide[akLeft].Control := PrefForm;
  promptLabel.BorderSpacing.Left := 6;
  promptLabel.Parent:=PrefForm;
  //edit
  valEdit:=TEdit.create(PrefForm);
  valEdit.Caption := FloatToStrF(def, ffGeneral, 8, 4);
  //valEdit.Top := 42;
  //valEdit.Width := PrefForm.Width - 16;
  valEdit.Constraints.MinWidth:= 300;
  valEdit.AutoSize := true;
  valEdit.AnchorSide[akTop].Side := asrBottom;
  valEdit.AnchorSide[akTop].Control := promptLabel;
  valEdit.BorderSpacing.Top := 6;
  valEdit.AnchorSide[akLeft].Side := asrLeft;
  valEdit.AnchorSide[akLeft].Control := PrefForm;
  valEdit.BorderSpacing.Left := 6;
  valEdit.Parent:=PrefForm;
  //Cancel Btn
  CancelBtn:=TButton.create(PrefForm);
  CancelBtn.Caption:='Cancel';
  CancelBtn.AutoSize := true;
  CancelBtn.AnchorSide[akTop].Side := asrBottom;
  CancelBtn.AnchorSide[akTop].Control := valEdit;
  CancelBtn.BorderSpacing.Top := 6;
  CancelBtn.AnchorSide[akLeft].Side := asrLeft;
  CancelBtn.AnchorSide[akLeft].Control := PrefForm;
  CancelBtn.BorderSpacing.Left := 200;
  CancelBtn.Parent:=PrefForm;
  CancelBtn.ModalResult:= mrCancel;

  //OK button
  OkBtn:=TButton.create(PrefForm);
  OkBtn.Caption:='OK';
  OkBtn.AutoSize := true;
  OkBtn.AnchorSide[akTop].Side := asrBottom;
  OkBtn.AnchorSide[akTop].Control := valEdit;
  OkBtn.BorderSpacing.Top := 6;
  OkBtn.AnchorSide[akLeft].Side := asrRight;
  OkBtn.AnchorSide[akLeft].Control := CancelBtn;
  OkBtn.BorderSpacing.Left := 6;
  OkBtn.Parent:=PrefForm;
  OkBtn.ModalResult:= mrOK;
  //OK button
  //86
  //{$IFDEF Windows} ScaleDPI(PrefForm, 96);  {$ENDIF}
  //{$IFDEF Linux} ScaleDPIX(PrefForm, 96); {$ENDIF}
  {$IFDEF LCLCocoa}
  if gPrefs.DarkMode then SetFormDarkMode(PrefForm);
  {$ENDIF}
  PrefForm.ShowModal;
  result := def;
  if (PrefForm.ModalResult = mrOK) then begin
    result := StrToFloatDef(valEdit.Caption, def);
    if (min < max) and (result < min) then
      result := min;
    if (min < max) and (result > max) then
      result := max;
  end;
  FreeAndNil(PrefForm);
end; //GetFloat()

procedure TGLForm1.SimplifyMeshMenuClick(Sender: TObject);

var
  nTri: integer;
  msStart: Dword;
  s: string;
  r: single;
begin
 msStart := gettickcount();
 nTri := length(gMesh.Faces);
 r := GetFloat('Enter reduction factor (e.g. 0.2 will decimate 80% of all triangles)', 0.001,0.3,0.999);
 //s := '.3';
 //if not inputquery('Track simplify', 'Enter reduction factor (e.g. 0.2 will decimate 80% of all triangles)', s) then exit;
 //r := StrToFloatDef(s, 0.5);
 if (r <= 0.0) or (r > 1.0) then begin
    showmessage('Error: reduction factor should be BETWEEN 0 and 1');
    exit;
 end;
 if not ReducePatch(gMesh.faces, gMesh.vertices, r) then exit;
 caption := format('Faces %d -> %d (%.3f, %d ms)', [ nTri, length(gMesh.Faces), length(gMesh.Faces)/nTri , gettickcount() - msStart]) ;
 gMesh.isRebuildList:=true;
 GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.SwapYZMenuClick(Sender: TObject);
begin
 if gPrefs.ZDimIsUp then
     gMesh.SwapYZ
 else
     gMesh.SwapZY;
 GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.ReverseFacesMenuClick(Sender: TObject);
begin
  gMesh.ReverseFaces;
  GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.UpdateImageIntensity;
var
  i: integer;
begin
     //gTypeInCell := false;
     if gMesh.OpenOverlays > 0 then begin
       Memo1.Lines.clear;
        for i := 1 to gMesh.OpenOverlays do begin
            Memo1.Lines.Add(format('Overlay %d: %s',[i, extractfilename(gMesh.overlay[i].filename)]));
            Memo1.Lines.Add(format(' range min..max %.4g..%.4g',[gMesh.overlay[i].minIntensity, gMesh.overlay[i].maxIntensity]));
            Memo1.Lines.Add(format(' view min..max %.4g..%.4g',[gMesh.overlay[i].windowScaledMin, gMesh.overlay[i].windowScaledMax]));
        end;
     end;
     OverlayTimerStart;
end;

procedure TGLForm1.SetColorBarPosition;
begin
 if (gPrefs.ColorBarPosition < 1) or (gPrefs.ColorBarPosition > 4) then gPrefs.ColorBarPosition := 1;
 case gPrefs.ColorBarPosition of
     3: begin gClrbar.isTopOrRight := true; gClrbar.isVertical:=false; end; //top row
     4: begin gClrbar.isTopOrRight := true; gClrbar.isVertical:=true; end; //right column
     1: begin gClrbar.isTopOrRight := false; gClrbar.isVertical:=false; end;//bottom row
     2: begin gClrbar.isTopOrRight := false; gClrbar.isVertical:=true; end;//left column
 end;
 gCube.TopLeft := (gPrefs.ColorBarPosition = 1) or (gPrefs.ColorBarPosition = 2);
  //gClrbar.isTopOrRight := true; gClrbar.isVertical:=false;
end;

procedure TGLForm1.TrackBoxChange(Sender: TObject);
begin
  gTrack.minFiberLength := TrackLengthTrack.position;
  gTrack.LineWidth := TrackWidthTrack.Position;
  gTrack.ditherColorFrac := TrackDitherTrack.Position / TrackDitherTrack.Max;
  Memo1.Lines.clear;
  Memo1.Lines.Add(format('Track min length %d',[TrackLengthTrack.position]));
  Memo1.Lines.Add(format('Track line width %d',[TrackWidthTrack.Position]));
  Memo1.Lines.Add(format('Track dither %.2g',[gTrack.ditherColorFrac]));
  gTrack.isRebuildList:= true;
  GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.OVERLAYMINMAX (lOverlay: integer; lMin,lMax: single);
begin
  if (gMesh.OpenOverlays < 1) or (lOverlay > gMesh.OpenOverlays)  then exit;
  gMesh.Overlay[lOverlay].WindowScaledMin := lMin;
  gMesh.Overlay[lOverlay].WindowScaledMax := lMax;
  UpdateLayerBox(false);;
end;

function TGLForm1.UpdateClrBar: integer;
var
  nLUT, lI, lJ: integer;
  mn, mx:  single;
  isDuplicate : boolean;
begin
 nLUT := 0;
 result := 0;
 if (gTrack.n_count > 0) and (gTrack.scalarSelected >= 0) and (gTrack.scalarSelected < length(gTrack.scalars))  then begin
    inc(nLUT);
    lJ := TrackScalarLUTdrop.ItemIndex;
    gClrbar.SetLUT(nLUT, UpdateTransferFunction(lJ,false), gTrack.scalars[gTrack.scalarSelected].mnView,gTrack.scalars[gTrack.scalarSelected].mxView);
 end;
 result := nLUT;
 if ((gMesh.OpenOverlays < 1) and ((length(gNode.nodes) < 1))) then exit;

 if (gMesh.OpenOverlays > 0) then
    for lI := 1 to gMesh.OpenOverlays do
        //https://www.nitrc.org/forum/forum.php?thread_id=10001&forum_id=6713
        if (length(gMesh.overlay[lI].intensity) > 0) and (not gMesh.overlay[lI].aoMap) and (gMesh.overlay[lI].OpacityPercent <> kLUTinvisible) and (not isFreeSurferLUT(gMesh.overlay[lI].LUTindex)) then begin
         isDuplicate := false;
         lJ := 1;
         while (lJ < lI) do begin
			if (gMesh.overlay[lI].LUTindex = gMesh.overlay[lJ].LUTindex)  and(gMesh.overlay[lJ].OpacityPercent <> kLUTinvisible)
				and (gMesh.overlay[lI].windowScaledMin = gMesh.overlay[lJ].windowScaledMin)
				and (gMesh.overlay[lI].windowScaledMax = gMesh.overlay[lJ].windowScaledMax) then
					isDuplicate := true;
			lJ := lJ + 1;
         end;
         if isDuplicate then continue;
         inc(nLUT);
         gClrbar.SetLUT(nLUT, UpdateTransferFunction(gMesh.overlay[lI].LUTindex,gMesh.overlay[lI].LUTinvert), gMesh.overlay[lI].windowScaledMin,gMesh.overlay[lI].windowScaledMax);
        end;
 result := nLUT;
 if (length(gNode.nodes) < 1) then exit;
 if (gNode.nodePrefs.isNodeColorVaries) then begin
     if (gNode.nodePrefs.isNodeThresholdBySize) then begin
        mn := gNode.nodePrefs.minNodeColor;
        mx := gNode.nodePrefs.maxNodeColor;
     end else begin
       mn := gNode.nodePrefs.minNodeThresh;
       mx := gNode.nodePrefs.maxNodeThresh;
     end;
     if mn <> mx then begin
       nLUT := nLUT + 1;
       gClrbar.SetLUT(nLUT, UpdateTransferFunction(gNode.nodePrefs.NodeLUTindex,false), mn,mx);
     end;
 end; //nodes
 if (gNode.nodePrefs.isEdgeColorVaries) and (gNode.nodePrefs.maxEdge <> gNode.nodePrefs.minEdge) then begin
   if (gNode.nodePrefs.maxEdge > 0)  and (not gNode.nodePrefs.isNoPosEdge) and (gNode.nodePrefs.minEdgeThresh <> gNode.nodePrefs.maxEdgeThresh) then begin
      nLUT := nLUT + 1;
      gClrbar.SetLUT(nLUT, UpdateTransferFunction(gNode.nodePrefs.edgeLUTindex,false), gNode.nodePrefs.minEdgeThresh,gNode.nodePrefs.maxEdgeThresh);
   end; //positive edges
   if (gNode.nodePrefs.minEdge < 0)  and (not gNode.nodePrefs.isNoNegEdge) and (gNode.nodePrefs.minEdgeThresh <> gNode.nodePrefs.maxEdgeThresh) then begin
      nLUT := nLUT + 1;
      lJ := 1+gNode.nodePrefs.edgeLUTindex;
      gClrbar.SetLUT(nLUT, UpdateTransferFunction(lJ ,false), -gNode.nodePrefs.minEdgeThresh,-gNode.nodePrefs.maxEdgeThresh);
   end; //negative edges
 end;  //edges
 result := nLUT;
end;

procedure TGLForm1.FormDestroy(Sender: TObject);
begin
 //Showmessage(gPrefs.FontName);
 //IniFile(false,IniName,gPrefs);
 gMesh.Free;
 gNode.Free;
 gTrack.Free;
 gCube.Free;
 gClrBar.Free;
end;

procedure TGLForm1.NodeThreshDropChange(Sender: TObject);
begin
 if NodeThreshDrop.ItemIndex = 0 then begin
    NodeMinEdit.Value:=gNode.NodePrefs.minNodeSize;
    NodeMaxEdit.Value:=gNode.NodePrefs.maxNodeSize;
 end else begin
    NodeMinEdit.Value:=gNode.NodePrefs.minNodeColor;
    NodeMaxEdit.Value:=gNode.NodePrefs.maxNodeColor;
 end;
  NodePrefChange(Sender);
end;


procedure TGLForm1.ROImeshMenuClick(Sender: TObject);
const
 kRoiIntensityFilter = 'ROX Intensities|*.rox';
 kAtlasFilter = 'mz3 Atlas|*.mz3';
 kMeshFilter = 'mz3 Mesh|*.mz3';
label
  123;
var
  lMesh: TMesh;
  lRoiIntensityFilename: string;
begin
  showmessage('You will be asked to select a *.rox file. Next you will select a *.mz3 template. Finally, provide the name for your mesh. Each line of the rox file lists the region number followed by the region intensity. An example of a ROX file with two regions would be:'+kCR+'17 0.5'+kCR+'32 1.5');
  OpenDialog.Filter := kRoiIntensityFilter;
  OpenDialog.Title := 'Select ROX intensities';
  if not OpenDialog.Execute then exit;
  lRoiIntensityFilename := OpenDialog.FileName;
  OpenDialog.Filter := kAtlasFilter;
  OpenDialog.Title := 'Select Atlas Template';
  if not OpenDialog.Execute then exit;
  SaveMeshDialog.Filter := kMeshFilter;
  if not SaveMeshDialog.execute then exit;
  //convert
  lMesh := TMesh.Create;
  if not lMesh.LoadFromFile(OpenDialog.Filename) then
    goto 123;
  if (lMesh.AtlasMaxIndex < 1) then begin
     showmessage('This is not a template '+OpenDialog.Filename);
     goto 123;
  end;
  if not lMesh.LoadOverlay(lRoiIntensityFilename, false) then
    goto 123;
  if lMesh.OpenOverlays < 1 then
    goto 123;
  lMesh.SaveOverlay(SaveMeshDialog.Filename, 1);
  123:
  lMesh.Free;
  GLBoxRequestUpdate(nil);
end;

{$IFDEF COREGL}
procedure Set2DDraw (w,h: integer; r,g,b: byte);
begin
  glDepthMask(kGL_TRUE); // enable writes to Z-buffer
  glEnable(GL_DEPTH_TEST);
  glDisable(GL_CULL_FACE); // glEnable(GL_CULL_FACE); //check on pyramid
  glEnable(GL_BLEND);
  {$IFNDEF COREGL}glEnable(GL_NORMALIZE); {$ENDIF}
  glClearColor(r/255, g/255, b/255, 0.0); //Set background
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
  glViewport( 0, 0, w, h); //required when bitmap zoom <> 1
end;
{$ELSE}
procedure Set2DDraw (w,h: integer; r,g,b: byte);
begin
glMatrixMode(GL_PROJECTION);
glLoadIdentity();
glOrtho (0, 1, 0, 1, -6, 12);
glMatrixMode (GL_MODELVIEW);
glLoadIdentity ();
{$IFDEF DGL}
glDepthMask(BYTEBOOL(1)); // enable writes to Z-buffer
{$ELSE}
glDepthMask(GL_TRUE); // enable writes to Z-buffer
{$ENDIF}
glEnable(GL_DEPTH_TEST);
glDisable(GL_CULL_FACE); // glEnable(GL_CULL_FACE); //check on pyramid
glEnable(GL_BLEND);
glEnable(GL_NORMALIZE);
glClearColor(r/255, g/255, b/255, 0.0); //Set background
glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
glViewport( 0, 0, w, h); //required when bitmap zoom <> 1
end;
{$ENDIF}

procedure TGLForm1.CreateRender(w,h: integer; isToScreen: boolean);
var
  origin: TPoint3f;
  isMultiPass, isMultiSample, isOK, useMultiSample: boolean;
  meshAlpha, meshBlend, ambientOcclusionFrac, scale: single;
begin
  if (h < 1) or (w < 1) then exit;
  if gNode.isBusy or gMesh.isBusy or isBusy then begin //come back later
     UpdateTimer.enabled := true;
     exit;
  end;
  if (length(gShader.VertexProgram) > 0) then gTrack.isRebuildList:= true;
  InitGLSL(false);
  origin := GetOrigin(scale);
  //glUseProgram(gShader.program3d);
  {$IFNDEF COREGL}
  SetLighting(gPrefs);
  {$ENDIF}
  isMultiPass := true;
  if (gPrefs.RenderQuality = kRenderPoor) then isMultiPass := false;
  if not (gPrefs.SupportBetterRenderQuality) then isMultiPass := false;
  if (gPrefs.OcclusionAmount = 0) and (occlusionTrack.Position = 0) and (meshAlphaTrack.position = meshAlphaTrack.max ) and (MeshBlendTrack.position = 0) then isMultiPass := false;
  //nDrawScene(w, h, true, false, gPrefs, origin, lightPos, ClipPlane, scale, gShader.Distance, gelevation, gazimuth, gMesh,gNode, gTrack);
  if (isMultiPass)  then begin
    meshAlpha := meshAlphaTrack.position/meshAlphaTrack.max;
    meshBlend := MeshBlendTrack.position/MeshBlendTrack.max;
    ambientOcclusionFrac := occlusionTrack.Position/occlusionTrack.max;
    //first pass: 3D draw all items: framebuffer f1
    useMultiSample := isToScreen; //the screenshot is explicitly supersampling everything (inlcuding text), the screen just supersamples the objects
    isMultiSample := setFrame (w, h, gShader.f1, useMultiSample, isOK);
    if not isOK then exit;
    DrawScene(w,h, gPrefs.isFlipMeshOverlay, gPrefs.OverlayClip, true,isMultiSample, gPrefs, origin, ClipPlane, scale, gDistance, gElevation, gAzimuth, gMesh,gNode, gTrack);
    //second pass: 3D draw overlay items only: framebuffer f2
    isMultiSample := setFrame (w, h, gShader.f2, useMultiSample, isOK );
    if not isOK then exit;
    //isFlipOverlayBackground := not isFlipOverlayBackground;
    DrawScene(w,h, gPrefs.isFlipMeshOverlay, gPrefs.OverlayClip, false,isMultiSample, gPrefs, origin,  ClipPlane, scale, gDistance, gElevation, gAzimuth, gMesh,gNode, gTrack);
    if (isToScreen)  then begin
       releaseFrame; //GOOD: multipass, multisampling
       Set2DDraw (w,h, red(gPrefs.BackColor) ,green(gPrefs.BackColor), blue(gPrefs.BackColor));
       RunAoGLSL( gShader.f1,  gShader.f2, 1,  meshAlpha, meshBlend, ambientOcclusionFrac, gDistance);
    end else begin  //SCREENSHOT - multipass, multisampling, supersampled
        //isMultiSample := setFrameMS (w, h, gShader.fScreenShot, false, isOK );
        isMultiSample := setFrame (w, h, gShader.fScreenShot, false, isOK );
        if (not isOK)  then exit;
        Set2DDraw (w,h, red(gPrefs.BackColor) ,green(gPrefs.BackColor), blue(gPrefs.BackColor));
        RunAoGLSL( gShader.f1,  gShader.f2, gPrefs.ScreenCaptureZoom,  meshAlpha,meshBlend,ambientOcclusionFrac,gDistance);
    end;
  end else begin //else POOR quality : do not use framebuffers (single pass)
      //if isToScreen then
         releaseFrame;
      //else
      //    setFrame (w, h, gShader.fScreenShot, true ); //SCREENSHOT - supersampled
      DrawScene(w, h, gPrefs.isFlipMeshOverlay, gPrefs.OverlayClip, true, false, gPrefs, origin, ClipPlane, scale, gDistance, gelevation, gazimuth, gMesh,gNode, gTrack);
  end;

  if gPrefs.OrientCube then begin
     //DrawCube (w, h,  gAzimuth, gElevation);
     gCube.Azimuth:=gAzimuth;
     gCube.Elevation:=gElevation;
     if (gPrefs.ColorbarPosition = 1) or (gPrefs.ColorbarPosition = 2) then
       gCube.TopLeft:= true
     else
          gCube.TopLeft:= false;
     gCube.Draw(w,h);
     //DrawCube (w, h,  gAzimuth, gElevation);
  end;

  if gPrefs.Colorbar  then begin
    //RunOffGLSL; //turn off shading
   if gnLUT < 0 then //refresh
      gnLUT := UpdateClrBar();
   if gnLUT > 0 then
      gClrbar.Draw(gnLUT, w, h);
    (*vbm if (gMesh.OpenOverlays > 0) and ((gTrack.n_count < 1) or (not gPrefs.ColorBarPrecedenceTracksNotOverlays)) then
       DrawCLUT( gPrefs.ColorBarPos, 0.01, gPrefs, gMesh, w, h) //color bar based on overlays
    else if (gTrack.n_count > 0) and (gTrack.scalarSelected >= 0) and (gTrack.scalarSelected < length(gTrack.scalars))  then
          DrawCLUTtrk(gPrefs.ColorBarPos, 0.01, gTrack.scalars[gTrack.scalarSelected].mnView, gTrack.scalars[gTrack.scalarSelected].mxView, gPrefs, gTrack.scalarLUT, w, h) //color bar based on overlays
          //DrawCLUTtrk ( lU: TUnitRect; lBorder, lMin, lMax: single; var lPrefs: TPrefs; LUT: TLUT;window_width, window_height: integer );
    else
       DrawCLUT( gPrefs.ColorBarPos, 0.01, gPrefs, gNode, w, h); //color bar based on nodes*)
  end;

  //if (gTrack.scalarSelected < 0) or (gTrack.scalarSelected >= length(gTrack.scalars)) then exit;
 //ScalarPref(gTrack.scalars[gTrack.scalarSelected].mnView, gTrack.scalars[gTrack.scalarSelected].mxView);
  //TestColorBar(gPrefs, w, h);
  //DrawText (gPrefs, w, h);
  if (isToScreen) then
     GLbox.SwapBuffers;
  //nDraw;
    isBusy := false;
end;

procedure TGLForm1.GLboxPaint(Sender: TObject);
begin
 //if not gPrefs.ObjectBasedClipPlane then
 //  ClipTrackChange(Sender);
 CreateRender(GLBoxBackingWidth, GLboxBackingHeight, true);
 if UpdateTimer.enabled then
    UpdateTimerTimer(Sender);
end;

function TGLForm1.ScreenShotX1: TBitmap;
var
  RawImage: TRawImage;
  p: array of byte;
  w, h, x, y, BytePerPixel: integer;
  z: int64;
  DestPtr: PInteger;
  maxXY : array[0..1] of GLuint;
begin

 GLBox.MakeCurrent;
 glGetIntegerv(GL_MAX_VIEWPORT_DIMS, @maxXY);  //GL_MAX_TEXTURE_SIZE
 w := GLBoxBackingWidth * gPrefs.ScreenCaptureZoom;
 h := GLboxBackingHeight * gPrefs.ScreenCaptureZoom;
 Result:=TBitmap.Create;
 Result.Width:=w;
 Result.Height:=h;
 if gPrefs.ScreenCaptureTransparentBackground then
   Result.PixelFormat := pf32bit
 else
     Result.PixelFormat := pf24bit; //if pf32bit the background color is wrong, e.g. when alpha = 0
 RawImage := Result.RawImage;
 BytePerPixel := RawImage.Description.BitsPerPixel div 8;
 setlength(p, 4*w* h);
 glFlush;
 glFinish;//<-this would pause until all jobs finished: generally a bad idea! required here
 // GLbox.SwapBuffers;

 {$IFDEF Darwin} //http://lists.apple.com/archives/mac-opengl/2006/Nov/msg00196.html
 glReadPixels(0, 0, w, h, $80E1, $8035, @p[0]); //OSX-Darwin   GL_BGRA = $80E1;  GL_UNSIGNED_INT_8_8_8_8_EXT = $8035;
 {$ELSE}
  {$IFDEF Linux}
    glReadPixels(0, 0, w, h, GL_RGBA, GL_UNSIGNED_BYTE, @p[0]); //Linux-Windows   GL_RGBA = $1908; GL_UNSIGNED_BYTE
  {$ELSE}
   glReadPixels(0, 0, w, h, $80E1, GL_UNSIGNED_BYTE, @p[0]); //Linux-Windows   GL_RGBA = $1908; GL_UNSIGNED_BYTE
  {$ENDIF}
 {$ENDIF}
 GLbox.ReleaseContext;
 z := 0;
 if BytePerPixel <> 4 then begin
   for y:= h-1 downto 0 do begin
        DestPtr := PInteger(RawImage.Data);
        Inc(PByte(DestPtr), y * RawImage.Description.BytesPerLine );
        for x := 1 to w do begin
            DestPtr^ := p[z] + (p[z+1] shl 8) + (p[z+2] shl 16);
            Inc(PByte(DestPtr), BytePerPixel);
            z := z + 4;
        end;
    end; //for y : each line in image
 end else begin
     for y:= h-1 downto 0 do begin
         DestPtr := PInteger(RawImage.Data);
         Inc(PByte(DestPtr), y * RawImage.Description.BytesPerLine );
         System.Move(p[z], DestPtr^, w * BytePerPixel );
         z := z + ( w * 4 );
   end; //for y : each line in image
 end;
 setlength(p, 0);
 GLbox.ReleaseContext;
end;

{$IFDEF COREGL}
function textureSizeOK(w,h: integer): boolean;
begin
 glBindTexture(GL_TEXTURE_2D, 0);
 glTexImage2D(GL_PROXY_TEXTURE_2D, 0,GL_RGBA8, w, h, 0,GL_RGBA, GL_UNSIGNED_BYTE, nil);
 //glGetTexLevelParameteriv(GL_PROXY_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @i);
 //glTexImage2D(GL_PROXY_TEXTURE_2D, 0, 4, mTextureWidth, mTextureHeight, 0, mTexFormat, mTexType, NULL);
 result := GL_NO_ERROR = glGetError();

end;

function TGLForm1.ScreenShot(lForceRedraw: boolean = false): TBitmap;
var
  RawImage: TRawImage;
  p: array of byte;
  isOK: boolean;
  zoom, w2, h2, w, h, x, y, BytePerPixel,trackLineWidth: integer;
  z: int64;
  DestPtr: PInteger;
  maxXY : array[0..1] of GLuint;
  fbo : TFrameBuffer;
begin
 if (gPrefs.ScreenCaptureZoom < 2) and (not lForceRedraw) then begin //special case: no super sampling
    result := ScreenShotX1;
    exit;
 end;
 GLBox.MakeCurrent;
 glGetIntegerv(GL_MAX_VIEWPORT_DIMS, @maxXY);  //GL_MAX_TEXTURE_SIZE
 zoom := gPrefs.ScreenCaptureZoom;
 w := GLBox.Width * zoom;
 h := GLbox.Height * zoom;
   w2 := w*2;
   h2 := h*2;
 if (w2 > maxXY[0]) or (h2 > maxXY[1]) or (gPrefs.RenderQuality = kRenderPoor) or (not (gPrefs.SupportBetterRenderQuality)) then begin
  result := ScreenShotX1;
  exit;
 end;
 trackLineWidth := gTrack.LineWidth;
 if (gTrack.n_count > 0) and (not gTrack.isTubes) then begin  //tracks are drawn in pixels, so zoom appropriately!
     gTrack.LineWidth := 2 * gTrack.LineWidth * zoom;
     gTrack.isRebuildList:= true;
  end;
  Result:=TBitmap.Create;
  Result.Width:=w;
  Result.Height:=h;
  if gPrefs.ScreenCaptureTransparentBackground then
    Result.PixelFormat := pf32bit
  else
      Result.PixelFormat := pf24bit; //if pf32bit the background color is wrong, e.g. when alpha = 0
  RawImage := Result.RawImage;
  BytePerPixel := RawImage.Description.BitsPerPixel div 8;
  setlength(p, 4*w* h);
  CreateRender(w2, h2, false); //draw to framebuffer fScreenShot
  //setFrame (w, h, gShader.fScreenShot, false, isOK ); // <- release huge framebuffer
  //if not isOK then
  initFrame(fbo);
  if w2 <> w then begin
    setFrame (w, h, fbo, false,isOK );
    glBindFramebuffer(GL_READ_FRAMEBUFFER, gShader.fScreenShot.frameBuf);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, fbo.frameBuf);
    glBlitFramebuffer(0,0,w2 {x2},h2{x2},0,0,w,h,GL_COLOR_BUFFER_BIT,GL_LINEAR);
    glBindFramebuffer(GL_READ_FRAMEBUFFER, fbo.frameBuf);
  end;
  {$IFDEF Darwin} //http://lists.apple.com/archives/mac-opengl/2006/Nov/msg00196.html
  glReadPixels(0, 0, w, h, $80E1, $8035, @p[0]); //OSX-Darwin   GL_BGRA = $80E1;  GL_UNSIGNED_INT_8_8_8_8_EXT = $8035;
  {$ELSE}
   {$IFDEF Linux}
     glReadPixels(0, 0, w, h, GL_RGBA, GL_UNSIGNED_BYTE, @p[0]); //Linux-Windows   GL_RGBA = $1908; GL_UNSIGNED_BYTE
   {$ELSE}
    glReadPixels(0, 0, w, h, $80E1, GL_UNSIGNED_BYTE, @p[0]); //Linux-Windows   GL_RGBA = $1908; GL_UNSIGNED_BYTE
   {$ENDIF}
  {$ENDIF}
  freeFrame(fbo);
  setFrame (2, 2, gShader.fScreenShot, false,isOK ); // <- release huge framebuffer
  GLbox.ReleaseContext;
  z := 0;
  if BytePerPixel <> 4 then begin
    for y:= h-1 downto 0 do begin
         DestPtr := PInteger(RawImage.Data);
         Inc(PByte(DestPtr), y * RawImage.Description.BytesPerLine );
         for x := 1 to w do begin
             DestPtr^ := p[z] + (p[z+1] shl 8) + (p[z+2] shl 16);
             Inc(PByte(DestPtr), BytePerPixel);
             z := z + 4;
         end;
     end; //for y : each line in image
  end else begin
      for y:= h-1 downto 0 do begin
          DestPtr := PInteger(RawImage.Data);
          Inc(PByte(DestPtr), y * RawImage.Description.BytesPerLine );
          System.Move(p[z], DestPtr^, w * BytePerPixel );
          z := z + ( w * 4 );
    end; //for y : each line in image
  end;
  setlength(p, 0);
  if (gTrack.n_count > 0) and (not gTrack.isTubes)  then begin  //reset for un-zoomed tracks
     gTrack.LineWidth := trackLineWidth;
     gTrack.isRebuildList:= true;
  end;
  GLboxRequestUpdate(GLForm1);
end;
{$ELSE}
function TGLForm1.ScreenShot(lForceRedraw: boolean = false): TBitmap;
var
  RawImage: TRawImage;
  p: array of byte;
  isOK: boolean;
  zoom, w, h, x, y, BytePerPixel,trackLineWidth: integer;
  z: int64;
  DestPtr: PInteger;
  maxXY : array[0..1] of GLuint;
begin
 if (gPrefs.ScreenCaptureZoom < 2) and (not lForceRedraw) then begin //special case: no super sampling
    result := ScreenShotX1;
    exit;
 end;
 GLBox.MakeCurrent;
 glGetIntegerv(GL_MAX_VIEWPORT_DIMS, @maxXY);  //GL_MAX_TEXTURE_SIZE
 w := GLBoxBackingWidth * gPrefs.ScreenCaptureZoom;
 h := GLboxBackingHeight * gPrefs.ScreenCaptureZoom;
 if (w > maxXY[0]) or (h > maxXY[1]) or (gPrefs.RenderQuality = kRenderPoor) or (not (gPrefs.SupportBetterRenderQuality)) then begin
  result := ScreenShotX1;
  exit;
  (*w := GLBoxBackingWidth;
    h := GLboxBackingHeight;
    zoom := 1*)
 end else
     zoom := gPrefs.ScreenCaptureZoom;
 trackLineWidth := gTrack.LineWidth;
 if (gTrack.n_count > 0) and (not gTrack.isTubes) then begin  //tracks are drawn in pixels, so zoom appropriately!
     gTrack.LineWidth := 2 * gTrack.LineWidth * zoom;
     gTrack.isRebuildList:= true;
  end;
  Result:=TBitmap.Create;
  Result.Width:=w;
  Result.Height:=h;
  if gPrefs.ScreenCaptureTransparentBackground then
    Result.PixelFormat := pf32bit
  else
      Result.PixelFormat := pf24bit; //if pf32bit the background color is wrong, e.g. when alpha = 0
  RawImage := Result.RawImage;
  BytePerPixel := RawImage.Description.BitsPerPixel div 8;
  setlength(p, 4*w* h);
  if lForceRedraw then
    CreateRender(w, h, true) //use screen due to Intel multisampling weirdness
  else
      CreateRender(w, h, false); //draw to framebuffer fScreenShot
    glFlush;
  glFinish;//<-this would pause until all jobs finished: generally a bad idea! required here
  //GLbox.SwapBuffers;

  {$IFDEF Darwin} //http://lists.apple.com/archives/mac-opengl/2006/Nov/msg00196.html
  glReadPixels(0, 0, w, h, $80E1, $8035, @p[0]); //OSX-Darwin   GL_BGRA = $80E1;  GL_UNSIGNED_INT_8_8_8_8_EXT = $8035;
  {$ELSE}
   {$IFDEF Linux}
     glReadPixels(0, 0, w, h, GL_RGBA, GL_UNSIGNED_BYTE, @p[0]); //Linux-Windows   GL_RGBA = $1908; GL_UNSIGNED_BYTE
   {$ELSE}
    glReadPixels(0, 0, w, h, $80E1, GL_UNSIGNED_BYTE, @p[0]); //Linux-Windows   GL_RGBA = $1908; GL_UNSIGNED_BYTE
   {$ENDIF}
  {$ENDIF}
  setFrame (2, 2, gShader.fScreenShot, false,isOK ); // <- release huge framebuffer
  GLbox.ReleaseContext;
  z := 0;
  if BytePerPixel <> 4 then begin
    for y:= h-1 downto 0 do begin
         DestPtr := PInteger(RawImage.Data);
         Inc(PByte(DestPtr), y * RawImage.Description.BytesPerLine );
         for x := 1 to w do begin
             DestPtr^ := p[z] + (p[z+1] shl 8) + (p[z+2] shl 16);
             Inc(PByte(DestPtr), BytePerPixel);
             z := z + 4;
         end;
     end; //for y : each line in image
  end else begin
      for y:= h-1 downto 0 do begin
          DestPtr := PInteger(RawImage.Data);
          Inc(PByte(DestPtr), y * RawImage.Description.BytesPerLine );
          System.Move(p[z], DestPtr^, w * BytePerPixel );
          z := z + ( w * 4 );
    end; //for y : each line in image
  end;
  setlength(p, 0);
  if (gTrack.n_count > 0) and (not gTrack.isTubes)  then begin  //reset for un-zoomed tracks
     gTrack.LineWidth := trackLineWidth;
     gTrack.isRebuildList:= true;
  end;
  GLboxRequestUpdate(GLForm1);
end;
{$ENDIF}
procedure ScreenRes(var lVidX,lVidY: integer);
{$IFDEF FPC}
begin
    lVidX := Screen.Width;
    lVidY := Screen.Height;
end;
{$ELSE}
var
   DC: HDC;
begin
  DC := GetDC(0);
  try
   lVidX :=(GetDeviceCaps(DC, HORZRES));
   lVidY :=(GetDeviceCaps(DC, VERTRES));
  finally
       ReleaseDC(0, DC);
  end; // of try/finally
end;//screenres
{$ENDIF}

procedure TGLForm1.AdjustFormPos (var lForm: TForm);
{$IFDEF FPC}
const
     kBorderHt = 30;
     kBorderWid = 10;
{$ELSE}
const
     kBorderHt = 0;
     kBorderWid = 0;
{$ENDIF}
const
{$IFDEF FPC}
kExtra = 8;
{$ELSE}
kExtra = 0;
{$ENDIF}
var
  lPos: integer;
  lVidX,lVidY,lLeft,lTop: integer;
begin
  ScreenRes(lVidX,lVidY);
  lPos := lForm.Tag;
  if odd(lPos) then begin//form on left
    lLeft := GLForm1.Left-lForm.Width-kBorderWid;
    if lLeft < 0 then //try putting the form on the right
       lLeft := GLForm1.Left+GLForm1.Width+kExtra; //form on right
  end else begin
    lLeft := GLForm1.Left+GLForm1.Width+kExtra;//-default: right
    if ((lLeft+ lForm.Width) > lVidX) then
       lLeft := GLForm1.Left-lForm.Width-kBorderWid; //try on right
  end;
  if lPos < 3 then begin //align with top
    lTop := GLForm1.Top; //default - align with top
    if lTop < 0 then //backup - top of screen
       lTop := 0;
  end else if lPos > 4 then begin //align with vertical middle
    lTop := GLForm1.Top+(GLForm1.Height div 2)-(lForm.Height div 2)+kBorderHt; //default - align with bottom
    if ((lTop+lForm.Height) > lVidY) then
       lTop := GLForm1.Top; //backup - align with top
    if lTop < 0 then
       lTop := 0;
  end else begin //align with bottom
    lTop := GLForm1.Top+GLForm1.Height-lForm.Height+kBorderHt; //default - align with bottom
    if ((lTop+lForm.Height) > lVidY) then
       lTop := GLForm1.Top; //backup - align with top
    if lTop < 0 then
       lTop := 0;
  end;
  if (lPos = 0) or ((lLeft+ lForm.Width) > lVidX) or (lLeft < 0)
    or (lTop < 0) or ((lTop+lForm.Height) > lVidY) then
    lForm.Position := poScreenCenter
  else begin
    lForm.Position := poDesigned;
    lForm.Left := lLeft;
    lForm.Top := lTop;
  end;
end;


procedure TGLForm1.Quit2TextEditor;
{$IFDEF UNIX}
var
  AProcess: TProcess;
  {$IFDEF LINUX} I: integer; EditorFName : string; {$ENDIF}
begin
    {$IFDEF LINUX}
    EditorFName := FindDefaultExecutablePath('gedit');
   if EditorFName = '' then
     EditorFName := FindDefaultExecutablePath('tea');
    if EditorFName = '' then
      EditorFName := FindDefaultExecutablePath('nano');
    if EditorFName = '' then
      EditorFName := FindDefaultExecutablePath('pico');
    if EditorFName = '' then begin
       Showmessage(ExtractFilename(paramstr(0))+' will now quit. You can then use a text editor to modify the file '+IniName);
       Clipboard.AsText := EditorFName;
    end else begin
      EditorFName := '"'+EditorFName +'" "'+IniName+'"';
      Showmessage(ExtractFilename(paramstr(0))+' will now quit. Modify the settings with the command "'+EditorFName+'"');
         AProcess := TProcess.Create(nil);
         AProcess.InheritHandles := False;
         AProcess.Options := [poNewProcessGroup, poNewConsole];
         AProcess.ShowWindow := swoShow;
        for I := 1 to GetEnvironmentVariableCount do
            AProcess.Environment.Add(GetEnvironmentString(I));
         AProcess.Executable := EditorFName;
         AProcess.Execute;
         AProcess.Free;
    end;
    Clipboard.AsText := EditorFName;
    GLForm1.close;
    exit;
    {$ENDIF}
    Showmessage('Preferences will be opened in a text editor. The program '+ExtractFilename(paramstr(0))+' will now quit, so that the file will not be overwritten.');
    AProcess := TProcess.Create(nil);
    {$IFDEF UNIX}
      //AProcess.CommandLine := 'open -a TextEdit '+IniName;
      AProcess.Executable := 'open';
      AProcess.Parameters.Add('-e');
      AProcess.Parameters.Add(IniName);
    {$ELSE}
      AProcess.CommandLine := 'notepad '+IniName;
    {$ENDIF}
   Clipboard.AsText := AProcess.CommandLine;
  //AProcess.Options := AProcess.Options + [poWaitOnExit];
  AProcess.Execute;
  AProcess.Free;
  GLForm1.close;
end;

{$ELSE} //ShellExecute(Handle,'open', 'c:\windows\notepad.exe','c:\SomeText.txt', nil, SW_SHOWNORMAL) ;
begin
  gPrefs.SkipPrefWriting := true;
    Showmessage('Preferences will be opened in a text editor. The program '+ExtractFilename(paramstr(0))+' will now quit, so that the file will not be overwritten.');
   //GLForm1.SavePrefs;
    ShellExecute(Handle,'open', 'notepad.exe',PAnsiChar(AnsiString(IniName)), nil, SW_SHOWNORMAL) ;
  //WritePrefsOnQuit.checked := false;
  GLForm1.close;
end;
{$ENDIF}

procedure TGLForm1.DepthLabelDblClick(Sender: TObject);
begin
  gPrefs.OverlayClip := not gPrefs.OverlayClip;
  GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.CurvMenuClick(Sender: TObject);
var
  fnm: string;
  isTemp: boolean;
begin
    if length(gMesh.Faces) < 1 then begin
       showmessage('Unable to compute curvature: no mesh is loaded (use File/Open).');
       exit;
    end;
    isTemp := (Sender as TMenuItem).Tag = 1;
    fnm := changefileext(gPrefs.PrevFilename[1],'.curv');
    if isTemp then begin
       //fnm := DesktopFolder + ExtractFileName(fnm);
       fnm := DefaultToHomeDir(fnm, true);
    end;
    if fileexists(fnm) then begin
       showmessage('File already exists '+fnm);
       exit;
    end;
    //1/2019: next lines cause artifacts if .annot file open on mesh - not sure of intention
    //if (length(gMesh.vertexRGBA) > 0) then
    //   GenerateCurvRGB(fnm, gMesh.vertexRGBA, length(gMesh.faces))
    //else
    	GenerateCurv(fnm, gMesh.faces, gMesh.vertices, gPrefs.GenerateSmoothCurves);
    OpenOverlay(fnm);
    if isTemp then
      deletefile(fnm);
end;

procedure TGLForm1.NewWindow1Click(Sender: TObject);
{$IFNDEF UNIX}
begin
   ShellExecute(handle,'open',PChar(paramstr(0)), '','',SW_SHOWNORMAL); //uses ShellApi;
end;
{$ELSE}
var
    AProcess: TProcess;
    i : integer;
    //http://wiki.freepascal.org/Executing_External_Programs
begin
  IniFile(false,IniName,gPrefs);  //load new window with latest settings
  AProcess := TProcess.Create(nil);
  AProcess.InheritHandles := False;
  //AProcess.Options := [poNoConsole];  //poNoConsole is Windows only! http://lazarus-ccr.sourceforge.net/docs/fcl/process/tprocess.options.html
  //AProcess.ShowWindow := swoShow; //Windows only http://www.freepascal.org/docs-html/fcl/process/tprocess.showwindow.html
  for I := 1 to GetEnvironmentVariableCount do
      AProcess.Environment.Add(GetEnvironmentString(I));
  {$IFDEF Darwin}
  AProcess.Executable := 'open';
  AProcess.Parameters.Add('-n');
  AProcess.Parameters.Add('-a');
  AProcess.Parameters.Add(paramstr(0));
  {$ELSE}
  AProcess.Executable := paramstr(0);
  {$ENDIF}
  //AProcess.Parameters.Add('/Users/rorden/Documents/osx/MRIcroGL.app/Contents/MacOS/MRIcroGL');
  AProcess.Execute;
  AProcess.Free;
end;
{$ENDIF}

procedure TGLForm1.CenterMeshMenuClick(Sender: TObject);
begin
 gMesh.CenterOrigin;
 GLBoxRequestUpdate(Sender);
end;

{$IFDEF Darwin}
function GetHardwareVersion: string;
//returns number of CPUs for MacOSX computer
//example - will return 4 if the computer has two dual core CPUs
//requires Process in Uses Clause
//see http://wiki.lazarus.freepascal.org/Executing_External_Programs
var
   lProcess: TProcess;
   lStringList: TStringList;
begin
     Result := '';
     lProcess := TProcess.Create(nil);
     lStringList := TStringList.Create;
     lProcess.CommandLine := 'sysctl hw.model';
     lProcess.Options := lProcess.Options + [poWaitOnExit, poUsePipes];
     lProcess.Execute;
     lStringList.LoadFromStream(lProcess.Output);
     if lStringList.Count > 0 then
       result := lStringList.Strings[0];
     lStringList.Free;
     lProcess.Free;
end;

function GetOSVersion: string;
//returns number of CPUs for MacOSX computer
//example - will return 4 if the computer has two dual core CPUs
//requires Process in Uses Clause
//see http://wiki.lazarus.freepascal.org/Executing_External_Programs
var
   lProcess: TProcess;
   lStringList: TStringList;
begin
     Result := '';
     lProcess := TProcess.Create(nil);
     lStringList := TStringList.Create;
     lProcess.CommandLine := 'sw_vers';
     lProcess.Options := lProcess.Options + [poWaitOnExit, poUsePipes];
     lProcess.Execute;
     lStringList.LoadFromStream(lProcess.Output);
     if lStringList.Count > 1 then
       result := lStringList.Strings[1];
     lStringList.Free;
     lProcess.Free;
end;
{$ENDIF}

{$IFDEF Darwin}
function isExeReadOnly: string;
var
  attr : Longint;
begin
     result := ' missing';
     if not fileexists(paramstr(0)) then exit;
     attr:=FileGetAttr(paramstr(0));
     If (attr and faReadOnly)<>0 then
      result := ' read only'
     else
         result := ' writeable';
end;
{$ENDIF}

procedure TGLForm1.AboutMenuClick(Sender: TObject);
const
  kSamp = 36;
var
  //{$DEFINE TIMEABOUT}
  {$IFDEF TIMEABOUT}
  fpsStr: string;
  s: dword;
  {$ENDIF}
  titleStr, isAtlasStr, TrackStr, MeshStr, str: string;
  i: integer;
  scale: single;
  origin: TPoint3f;
begin
  MeshStr := '';
    if length(gMesh.vertices) > 0 then begin
       MeshStr := LineEnding + format('    %.4f..%.4f  %.4f..%.4f %.4f..%.4f',[gMesh.mnV.X, gMesh.mxV.X, gMesh.mnV.Y, gMesh.mxV.Y, gMesh.mnV.Z, gMesh.mxV.Z]);
    end;
    TrackStr := '';
    if (gTrack.n_count > 0) then begin
     if not gTrack.isWorldSpaceMM then
       TrackStr := 'Spatial Properties Underspecified';
     TrackStr := LineEnding + format('   %s %.4f..%.4f  %.4f..%.4f %.4f..%.4f',[TrackStr, gTrack.mnV.X, gTrack.mxV.X, gTrack.mnV.Y, gTrack.mxV.Y, gTrack.mnV.Z, gTrack.mxV.Z]);
 end;
 {$IFDEF TIMEABOUT}
 s := gettickcount();
 for i := 1 to kSamp do begin
     gAzimuth := (gAzimuth + 10) mod 360;
     GLbox.Repaint;
  end;
 fpsStr := '';
 if (gettickcount<> s) then
    fpsStr := LineEnding+' FPS ' +inttostr(round( (kSamp*1000)/(gettickcount-s)));
 {$ENDIF}
 origin := GetOrigin(scale);
  isAtlasStr := '';
  if (length(gMesh.vertexAtlas) > 0) then isAtlasStr := ' Indexed Atlas ';
  str :=  'Surf Ice '+kVers+' '
   {$IFDEF CPU64} + '64-bit'
   {$ELSE} + '32-bit'
   {$ENDIF}
   {$IFDEF LCLCarbon} + ' Carbon'{$ENDIF}
   {$IFDEF LCLCocoa} + ' Cocoa'{$ENDIF}
   {$IFDEF Linux} + ' Linux'{$ENDIF}
   {$IFDEF Windows} + ' Windows'{$ENDIF}
   {$IFDEF DGL} + ' DGL'{$ENDIF}
   {$IFNDEF COREGL}+' (Legacy OpenGL)'{$ENDIF}
   {$IFDEF Darwin}
           {$IFDEF LCLCocoa}
           +''; titleStr := Str; str := ' '+GetHardwareVersion
           +LineEnding+' '+ GetOSVersion
           {$ELSE}
           +LineEnding+' @: '+ AppDir2
           +LineEnding+' '+ isExeReadOnly
           +LineEnding+' '+GetHardwareVersion
           +LineEnding+' '+ GetOSVersion
           {$ENDIF}
   {$ENDIF}
   +LineEnding+' www.mricro.com :: BSD 2-Clause License (opensource.org/licenses/BSD-2-Clause)'
   {$IFDEF TIMEABOUT}+FPSstr {$ENDIF}
   +LineEnding+format(' Scale %.4f',[scale])
   +LineEnding+format(' Origin %.4fx%.4fx%.4f',[origin.X, origin.Y, origin.Z])
   +LineEnding+' Mesh Vertices '+inttostr(length(gMesh.vertices))+' Faces '+  inttostr(length(gMesh.faces)) +' Colors '+  inttostr(length(gMesh.vertexRGBA))
   +isAtlasStr
   +MeshStr
   +LineEnding+' Track Vertices '+inttostr(gTrack.n_vertices)+' Faces '+  inttostr(gTrack.n_faces) +' Count ' +inttostr(gTrack.n_count)
   +TrackStr
   +LineEnding+' Node Vertices '+inttostr(length(gNode.vertices))+' Faces '+  inttostr(length(gNode.faces))
   +LineEnding+' GPU '+gShader.Vendor;
  {$IFDEF LCLCocoa}
  ClipBoard.AsText:= titleStr + LineEnding + str;
  //ShowAlertSheet(GLForm1.Handle,titleStr, str);  //<- limited string length!
  MessageDlg(titleStr,str,mtInformation,[mbOK],0);
  {$ELSE}
  ClipBoard.AsText:= str;
  MessageDlg(str,mtInformation,[mbOK],0);
  {$ENDIF}
  //i := MessageDlg(str,mtInformation,[mbAbort, mbOK],0);
  //if i  = mrAbort then Quit2TextEditor;
end;

procedure TGLForm1.AddNodesMenuClick(Sender: TObject);
const
  kNodeFilter = 'BrainNet Node/Edge|*.node;*.nodz;*.edge|Any file|*.*';
var
  ext, f2: string;
begin
     if Fileexists(gPrefs.PrevNodename) then begin
        OpenDialog.InitialDir := ExtractFileDir(gPrefs.PrevNodename);
        OpenDialog.Filename := gPrefs.PrevNodename;
     end;
     OpenDialog.Filter := kNodeFilter;
     OpenDialog.Title := 'Select Node/Edge file';
     if not OpenDialog.Execute then exit;
     if FSize(OpenDialog.Filename) < 1 then
        showmessage('Unable to open file (check permissions) '+ OpenDialog.Filename);
     //OpenDialog.FileName := '/Users/rorden/Desktop/obj/myNodes.node';
     ext := UpperCase(ExtractFileExt(OpenDialog.Filename));
     if (ext = '.EDGE') and (length(gNode.nodes) < 1) then begin
        f2 := changefileext(OpenDialog.FileName, '.node');
        if fileexists(f2) then
           OpenNode(f2)
        else begin
             showmessage('Please load your NODE file before loading an edge file');
             exit;
        end;
     end;
     if (ext = '.EDGE') then
         OpenEdge(OpenDialog.FileName)
     else
         OpenNode(OpenDialog.FileName);
     //OpenEdge('/Users/rorden/Desktop/obj/Edge_Brodmann82.edge');
     UpdateToolbar;
end;

procedure TGLForm1.AddOverlayMenuClick(Sender: TObject);
const
{$IFDEF FOREIGNVOL}
 //kVolFilter = 'Neuroimaging (*.nii)|*.hdr;*.nii;*.nii.gz;*.voi;*.HEAD;*.mgh;*.mgz;*.mha;*.mhd;*.nhdr;*.nrrd';
 kOverlayFilter = 'Mesh (e.g. GIfTI) or Volume (e.g. NIfTI) |*.*';
{$ELSE}
kOverlayFilter = 'Mesh or NIfTI|*.*';
{$ENDIF}
begin
  OpenDialog.Filter := kOverlayFilter;
  OpenDialog.Title := 'Select overlay file';
  if Fileexists(gPrefs.PrevOverlayname) then begin
        OpenDialog.InitialDir := ExtractFileDir(gPrefs.PrevOverlayname);
        OpenDialog.FileName := gPrefs.PrevOverlayname;
  end;
  if not OpenDialog.Execute then exit;
  if FSize(OpenDialog.Filename) < 1 then
     showmessage('Unable to open file (check permissions) '+ OpenDialog.Filename);
  //OpenDialog.FileName := ('/Users/rorden/Desktop/Surf_Ice/other/motor_4t95vol.nii.gz');
  //OpenDialog.FileName := ('/Users/rorden/Desktop/Surf_Ice/other/motor_4t95mesh.gii');
  OpenOverlay(OpenDialog.FileName);
end;

procedure TGLForm1.AddTracksMenuClick(Sender: TObject);
begin
 OpenDialog.Filter := kTrackFilter;
 OpenDialog.Title := 'Select track file';
 if Fileexists(gPrefs.PrevTrackname) then begin
    OpenDialog.InitialDir := ExtractFileDir(gPrefs.PrevTrackname);
    OpenDialog.Filename := gPrefs.PrevTrackname;
 end;
 if not OpenDialog.Execute then exit;
 if FSize(OpenDialog.Filename) < 1 then
    showmessage('Unable to open file (check permissions) '+ OpenDialog.Filename);
 //OpenDialog.Filename := '/Users/rorden/Desktop/Surf_Ice/sample/stroke.trk';
 OpenTrack(OpenDialog.FileName);
end;

procedure TGLForm1.AzimuthLabelClick(Sender: TObject);
begin
 IncTrackBar(ClipAziTrack, false);
end;

procedure TGLForm1.BackColorMenuClick(Sender: TObject);
begin
 If (ssShift in KeyDataToShiftState(vk_Shift)) then begin
    if green(gPrefs.BackColor) > 128 then
       gPrefs.BackColor := RGBToColor(0,0,0)
    else
        gPrefs.BackColor := RGBToColor(255,255,255);
     GLBoxRequestUpdate(Sender);
     exit;
  end;
  ColorDialog1.color := gPrefs.BackColor;
  if not ColorDialog1.Execute then exit;
  gPrefs.BackColor := ColorDialog1.Color;
  GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.CopyMenuClick(Sender: TObject);
var
 bmp: TBitmap;
begin
 bmp := ScreenShot();
 Clipboard.Assign(bmp);
 bmp.Free;
end;

procedure TGLForm1.DepthLabelClick(Sender: TObject);
begin
     IncTrackBar(ClipTrack, true);
end;

procedure TGLForm1.DisplayMenuClick(Sender: TObject);
begin
     case (Sender as TMenuItem).tag of
          0: gAzimuth := 270; //left
          1: gAzimuth := 90; //right
          3: gAzimuth := 180;//anterior
          4: gAzimuth := 180;//inferior
          else gAzimuth := 0; //posterior, inferior, superior
     end;
     case (Sender as TMenuItem).tag of
          4: gElevation := -90; //inferior
          5: gElevation := 90;//superior
          else gElevation := 0; //other
     end;
     GLBox.Invalidate;
end;

procedure TGLForm1.ElevationLabelClick(Sender: TObject);
begin
  IncTrackBar(ClipElevTrack, false);
end;

procedure TGLForm1.ErrorTimerTimer(Sender: TObject);
begin
  ErrorTimer.Enabled := false;
  Showmessage(GLError);
  GLerror := '';
end;

procedure TGLForm1.SetDistance(Distance: single);
begin
     gDistance := Distance;
     if gDistance > kMaxDistance then gDistance := kMaxDistance;
     if gDistance < kMinDistance then gDistance := kMinDistance;
     GLBox.Invalidate;
end;

procedure TGLForm1.GLBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
 if abs(WheelDelta) < 5 then exit;
  if WheelDelta < 0 then
     SetDistance(gDistance * 0.9)
  else
    SetDistance(gDistance * 1.1);
end;

function TGLForm1.ComboBoxName2Index(var lCombo: TComboBox; lName: string): integer;
var
    lNameU, lItem : string;
    i: integer;
begin
     result := 0;
     if lCombo.Items.Count < 2 then exit;
     lNameU := uppercase(lName);
     i := 0;
     while i < lCombo.Items.Count do begin
         lItem := uppercase (lCombo.Items[i]);
         if (lItem = lNameU) then begin
            result := i;
            i := maxint-1;
         end;
         i := i + 1;
 end;//for each shader
end;

procedure TGLForm1.OVERLAYCOLORNAME(lOverlay: integer; lFilename: string);
var
   lLUTIndex: integer;
   //lName, lItem : string;
begin
 if (gMesh.OpenOverlays < 1) or (lOverlay > gMesh.OpenOverlays)  then exit;
 lLUTIndex := ComboBoxName2Index(LayerColorDrop, lFilename);
 UpdateLUT(lOverlay,lLUTIndex);
 //LayerWidgetChange(nil);
 UpdateLayerBox(false);
end;

procedure TGLForm1.NodePrefChange(Sender: TObject);
var
  lo, hi: single;
begin
 gNode.nodePrefs.scaleNodeSize := NodeScaleTrack.Position / 10;
  gNode.nodePrefs.nodeLUTindex := LUTdropNode.itemIndex;
  gNode.nodePrefs.isEdgeSizeVaries := EdgeSizeVariesCheck.checked;
  gNode.nodePrefs.isNodeColorVaries := NodeColorVariesCheck.checked;
  gNode.nodePrefs.isEdgeColorVaries := EdgeColorVariesCheck.checked;
  lo := nodeMinEdit.Value;
  hi := nodeMaxEdit.value;
  sortsingle(lo, hi);
  gNode.nodePrefs.minNodeThresh := lo;
  gNode.nodePrefs.maxNodeThresh := hi;
  gNode.nodePrefs.edgeLUTindex:= LUTdropEdge.itemIndex;
  gNode.nodePrefs.scaleEdgeSize:= edgeScaleTrack.Position / 10;
  lo := edgeMinEdit.Value;
  hi := edgeMaxEdit.value;
  sortsingle(lo, hi);
  gNode.nodePrefs.minEdgeThresh := lo;
  gNode.nodePrefs.maxEdgeThresh := hi;
  Memo1.Lines.clear;
  Memo1.Lines.Add(format('Node size range min..max %.4g..%.4g',[gNode.NodePrefs.minNodeSize, gNode.nodePrefs.maxNodeSize]));
  Memo1.Lines.Add(format('Node color range min..max %.4g..%.4g',[gNode.NodePrefs.minNodeColor, gNode.nodePrefs.maxNodeColor]));
  Memo1.Lines.Add(format('Node scale %.2g',[gNode.nodePrefs.scaleNodeSize]));
  Memo1.Lines.Add(format('Node color table %d',[gNode.nodePrefs.nodeLUTindex]) );
  if NodeThreshDrop.ItemIndex = 0 then begin
     gNode.nodePrefs.isNodeThresholdBySize := true;
     Memo1.Lines.Add(format('Node size threshold min..max %.4g..%.4g',[gNode.NodePrefs.minNodeThresh, gNode.nodePrefs.maxNodeThresh]));
  end else begin
    gNode.nodePrefs.isNodeThresholdBySize := false;
    Memo1.Lines.Add(format('Node color threshold min..max %.4g..%.4g',[gNode.NodePrefs.minNodeThresh, gNode.nodePrefs.maxNodeThresh]));
  end;
  Memo1.Lines.Add(format('Edge range min..max %.4g..%.4g',[gNode.NodePrefs.minEdge, gNode.nodePrefs.maxEdge]));
  Memo1.Lines.Add(format('Edge threshold min..max %.4g..%.4g',[gNode.NodePrefs.minEdgeThresh, gNode.nodePrefs.maxEdgeThresh]));
  Memo1.Lines.Add(format('Edge color table %d',[gNode.nodePrefs.edgeLUTindex]) );
  Memo1.Lines.Add(format('Edge scale %.2g',[gNode.nodePrefs.scaleEdgeSize]) );
  gNode.isRebuildList := true;
  gnLUT := -1; //refresh colorbar
  GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.OrientCubeMenuClick(Sender: TObject);
begin
 gPrefs.OrientCube := OrientCubeMenu.Checked;
 GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IniFile(false,IniName,gPrefs);
end;

procedure TGLForm1.ExitMenuClick(Sender: TObject);
begin
  //GLForm1.Close;
  Self.Close;
  //Application.Terminate;
end;

procedure TGLForm1.ObjectColorMenuClick(Sender: TObject);
begin
  ColorDialog1.color := gPrefs.ObjColor;
  if not ColorDialog1.Execute then exit;
  gPrefs.objColor := ColorDialog1.Color;
    {$IFDEF COREGL}
  gMesh.isRebuildList := true;
  {$ENDIF}
  GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.OpenMenuClick(Sender: TObject);
const
     kMeshFilter = 'Mesh (GIfTI, PLY, FreeSurfer, etc)|*.*';
begin
 if (ssCtrl in KeyDataToShiftState(vk_Shift))  then begin
   OpenMesh('-');
   exit;
 end;
 OpenDialog.Filter := kMeshFilter;
 OpenDialog.Title := 'Select mesh file';
 if Fileexists(gPrefs.PrevFilename[1]) then begin
    OpenDialog.InitialDir := ExtractFileDir(gPrefs.PrevFilename[1]);
    OpenDialog.Filename := gPrefs.PrevFilename[1];
  end;
  if not OpenDialog.Execute then exit;
  if FSize(OpenDialog.Filename) < 1 then
     showmessage('Unable to open file (check permissions) '+ OpenDialog.Filename);
  OpenMesh(OpenDialog.Filename);
end;

procedure TGLForm1.OverlayBoxCreate;
var
   lSearchRec: TSearchRec;
   lStr: string;
begin
  LUTdropNode.Items.Clear;
  LUTdropNode.Items.Add('Grayscale');
  LUTdropNode.Items.Add('Red-Yellow');
  LUTdropNode.Items.Add('Blue-Green');
  LUTdropNode.Items.Add('Red');
  LUTdropNode.Items.Add('Green');
  LUTdropNode.Items.Add('Blue');
  LUTdropNode.Items.Add('Violet [r+b]');
  LUTdropNode.Items.Add('Yellow [r+g]');
  LUTdropNode.Items.Add('Cyan [g+b]');
  LUTdropNode.Items.Add('Hot');
  LUTdropNode.Items.Add('Bone');
  LUTdropNode.Items.Add('Winter');
  LUTdropNode.Items.Add('GE');
  LUTdropNode.Items.Add('ACTC');
  LUTdropNode.Items.Add('X-Rain');
  LUTdropNode.Items.Add('FreeSurfer1');
  LUTdropNode.Items.Add('FreeSurfer2');
  LUTdropNode.Items.Add('FreeSurfer3');
  LUTdropNode.Items.Add('FreeSurfer4');
  if DirectoryExists(ClutDir) then  begin
     if FindFirst(CLUTdir+pathdelim+'*.clut', faAnyFile, lSearchRec) = 0 then
	 repeat
               lStr := ChangeFileExt (ExtractFileName (lSearchRec.Name), '');
               if (length(lStr) > 0) and (lStr[1] <> '.') then
                  LUTdropNode.Items.Add(lStr);
	 until (FindNext(lSearchRec) <> 0);
     FindClose(lSearchRec);
  end;
  LUTdropEdge.Items := LUTdropNode.Items;
  LayerColorDrop.Items.Clear;
  LayerColorDrop.Items := LUTdropNode.Items;
  LUTdropNode.ItemIndex := 3;
  LUTdropEdge.ItemIndex := 1;
  //Copy names for tracks
  TrackScalarLUTdrop.Items.Clear;
  TrackScalarLUTdrop.Items := LUTdropNode.Items;
  TrackScalarLUTdrop.ItemIndex := 1;
  //TrackScalarLUTdrop.Items.AddStrings := LUTdrop.Items;
end;

procedure TGLForm1.OverlayTimerTimer(Sender: TObject);
begin
     OverlayTimer.Enabled := false;
     gMesh.isRebuildList:= true;
     gMesh.isAdditiveOverlay := gPrefs.AdditiveOverlay;
     {$IFDEF FPC}{$IFDEF Windows}
     //StringGrid1.Refresh;
     {$ENDIF}{$ENDIF}
     gnLUT := -1; //refresh colorbar
     GLbox.Invalidate;
end;
{$DEFINE JPG}
{$IFDEF JPG}
{$IFDEF FPC}
procedure SaveImgAsJPGCore (lImage: TBitmap; lFilename: string);
var
  JpegImg : TJpegImage;
begin
   JpegImg := TJpegImage.Create;
   try
    JpegImg.Assign(lImage) ;
    JpegImg.SaveToFile(ChangeFileExt(lFilename,'.jpg')) ;
   finally
    JpegImg.Free
   end;
end;
{$ELSE}
procedure SaveImgAsJPGCore (lImage: TBitmap; lFilename: string);
begin
  lImage.SaveToFile(ChangeFileExt(lFilename,'.bmp'));
end;
{$ENDIF}
{$ENDIF}
procedure TGLForm1.SaveBitmap(FilenameIn: string; lX, lY: integer); overload;
 var
    bmp: TBitmap;
    png: TPortableNetworkGraphic;
    p,n,x,ext, filename: string;
    z: integer;
    {$IFDEF LCLCocoa}retina: boolean; {$ENDIF}
 begin
  FilenameParts (FilenameIn,p,n,x);
  if (p ='') or (not directoryexists(p)) then
     p := DesktopFolder;
  if (n = '') then n := 'SurfIce';
  if (x = '') then x := '.png';
  Filename := p+n+x;
  z := gPrefs.ScreenCaptureZoom;
  GLBox.Align := alNone;
  GLBox.Width:=lX;
  GLBox.Height:=lY;
  {$IFDEF LCLCocoa}
  if (gPrefs.RetinaDisplay) then begin
     retina := gPrefs.RetinaDisplay;
     gPrefs.RetinaDisplay := false;
     setRetina;
  end;
  {$ENDIF}
  GLBox.ClientWidth:=lX;
  GLBox.ClientHeight:=lY;
  gPrefs.ScreenCaptureZoom:=1;
  GLBox.Invalidate;
  bmp := ScreenShot(true);
  GLBox.Align := alClient;
  {$IFDEF LCLCocoa}
  if (retina) then begin
     gPrefs.RetinaDisplay := true;
     setRetina;
  end;
  {$ENDIF}
  GLBox.Invalidate;
  gPrefs.ScreenCaptureZoom := z;
  {$IFDEF JPG}
  //JPEG
  ext := upcase(x);
  if (ext = '.JPEG') or (ext = '.JPG') then begin
     SaveImgAsJPGCore (bmp, Filename);
     exit;
  end;
  {$ENDIF}
  //PNG
  png := TPortableNetworkGraphic.Create;
   try
     png.Assign(bmp);    //Convert data into png
     png.SaveToFile(changefileext(Filename,'.png'));
   finally
     png.Free;
   end;
   bmp.Free;
end;

procedure TGLForm1.SaveBitmap(FilenameIn: string); overload;
 var
    bmp: TBitmap;
    png: TPortableNetworkGraphic;
    p,n,x,ext, filename: string;
 begin
  FilenameParts (FilenameIn,p,n,x);
  if (p ='') or (not directoryexists(p)) then
     p := DesktopFolder;
  if (n = '') then n := 'SurfIce';
  if (x = '') then x := '.png';
  Filename := p+n+x;
  bmp := ScreenShot;
  {$IFDEF JPG}
  //JPEG
  ext := upcase(x);
  if (ext = '.JPEG') or (ext = '.JPG') then begin
     SaveImgAsJPGCore (bmp, Filename);
     exit;
  end;
  {$ENDIF}
  //PNG
  png := TPortableNetworkGraphic.Create;
   try
     png.Assign(bmp);    //Convert data into png
     png.SaveToFile(changefileext(Filename,'.png'));
   finally
     png.Free;
   end;
   bmp.Free;
end;

procedure TGLForm1.SaveMenuClick(Sender: TObject);
begin
  if not SaveBitmapDialog.execute then exit;
  SaveBitmap(SaveBitmapDialog.Filename);
end;

(*procedure TGLForm1.SaveMz3(var mesh: TMesh; isSaveOverlays: boolean);
var
   i : integer;
   nam: string;
begin
  Mesh.SaveMz3(SaveMeshDialog.Filename, gPrefs.ObjColor);
  if not isSaveOverlays then exit;
  for i :=  1 to gMesh.OpenOverlays do begin
        if length(gMesh.overlay[i].vertices) < 3 then continue; //only for meshes, not for colors
        nam := changefileext(SaveMeshDialog.Filename, '_'+inttostr(i)+extractfileext(SaveMeshDialog.Filename));
        gMesh.SaveOverlay(nam, i);
  end;
end;*)

//{$DEFINE XL}
{$IFDEF XL}
procedure TGLForm1.SaveMesh(var mesh: TMesh; isSaveOverlays: boolean);
var
  xSaveMeshDialog: TSaveDialog;
begin
  xSaveMeshDialog := TSaveDialog.Create(self);
  xSaveMeshDialog.Filter := '';//'gz|*.gz|nii.gz|*.nii.gz|nii|*.nii|roi|*.roi';
  xSaveMeshDialog.Filename := 'SaveName';
  //xSaveMeshDialog.FilterIndex:= 2;
  xSaveMeshDialog.DefaultExt := '';
  if not xSaveMeshDialog.Execute then exit;
  Caption := xSaveMeshDialog.Filename;
  xSaveMeshDialog.Free;
end;
(*procedure TGLForm1.SaveMesh(var mesh: TMesh; isSaveOverlays: boolean);
begin
  SaveMeshDialog.Filter := 'gz|*.gz|nii.gz|*.nii.gz|nii|*.nii|roi|*.roi';
  SaveMeshDialog.Filename := 'SaveName';
  SaveMeshDialog.FilterIndex:= 2;
  SaveMeshDialog.DefaultExt := '.roi';
  if not SaveMeshDialog.Execute then exit;
  Caption := SaveMeshDialog.Filename;
end; *)
{$ELSE}
procedure TGLForm1.SaveMesh(var mesh: TMesh; isSaveOverlays: boolean);
const
     //kMeshFilter = 'OBJ (Widely supported)|*.obj|GIfTI (Neuroimaging)|*.gii|MZ3 (Small and fast)|*.mz3|PLY (Widely supported)|*.ply|VRML (Shapeways color printing)|*.wrl|WebGL PRWM|*.prwm';
     //prwm expects normals
     kMeshFilter = 'OBJ (Widely supported)|*.obj|GIfTI (Neuroimaging)|*.gii|MZ3 (Small and fast)|*.mz3|PLY (Widely supported)|*.ply|VRML (Shapeways color printing)|*.wrl|STL (Large and slow)|*.stl';
var
   nam, ext, x: string;
begin
  //mesh.SaveMesh('/Users/rorden/nrrdify/tst.prwm'); exit;
  if length(mesh.Faces) < 1 then begin
     showmessage('Unable to save: no mesh is loaded (use File/Open).');
     exit;
  end;
  SaveMeshDialog.Filter  := kMeshFilter;
  SaveMeshDialog.FilterIndex := gPrefs.SaveAsFormat + 1;
  if gPrefs.SaveAsFormat = 4 then
     ext := '.ply'
  else if gPrefs.SaveAsFormat = 0 then
     ext := '.obj'
  else if gPrefs.SaveAsFormat = 1 then
    ext := '.gii'
  else
    ext := '.mz3'; //2
  SaveMeshDialog.DefaultExt := ext;
  if (fileexists(gPrefs.PrevFilename[1])) or (not isSaveOverlays) then begin
     if isSaveOverlays then
      nam := gPrefs.PrevFilename[1]
    else
      nam := SaveMeshDialog.Filename;
    SaveMeshDialog.InitialDir:= ExtractFileDir(nam);
    //nam := ChangeFileExtX(extractfilename (nam), ext);
    nam := ChangeFileExtX(extractfilename (nam), '');
    SaveMeshDialog.Filename := nam;
  end else
      SaveMeshDialog.Filename := '';
  if not SaveMeshDialog.Execute then exit;
  if length(SaveMeshDialog.Filename) < 1 then exit;
  //caption := inttostr(SaveMeshDialog.FilterIndex)+' '+SaveMeshDialog.Filename; exit; //666
  x := UpperCase(ExtractFileExt(SaveMeshDialog.Filename));
  if (x <> '.STL') and (x <> '.MZ3') and (x <> '.PLY') and (x <> '.OBJ')  and (x <> '.GII') and (x <> '.WRL') and (x <> '.PRWM')then begin
     x := ext;
     SaveMeshDialog.Filename := SaveMeshDialog.Filename + x;
  end;
  mesh.SaveMesh(SaveMeshDialog.Filename);
  (*if (x = '.WRL') then
     mesh.SaveVrml(SaveMeshDialog.Filename, gPrefs.ObjColor)
  else if (x = '.MZ3') then
     SaveMz3(mesh, isSaveOverlays)
  else if (x = '.GII') then
     mesh.SaveGii(SaveMeshDialog.Filename)
  else if (x = '.PLY') then
     mesh.SavePly(SaveMeshDialog.Filename)
  else
      mesh.SaveObj(SaveMeshDialog.Filename); *)
end;
{$ENDIF}

function TGLForm1.SaveMeshCore(lFilename: string): boolean;
begin
     gMesh.SaveMesh(lFilename);
     result := true;
end;

(*function TGLForm1.SaveMeshCore(lFilename: string): boolean;
var
   x: string;
begin
  result := false;
  if (lFilename = '') or (length(gMesh.Faces) < 1) then begin
     exit;
  end;
  result := true;
  x := UpperCase(ExtractFileExt(lFilename));
  if (x <> '.MZ3') and (x <> '.PLY') and (x <> '.OBJ')  and (x <> '.GII') then begin
     x := '.MZ3';
     lFilename := lFilename + x;
  end;
  xxx
  if (x = '.MZ3') then
     gMesh.SaveMz3(lFilename)
  else if (x = '.GII') then
     gMesh.SaveGii(lFilename)
  else if (x = '.PLY') then
     gMesh.SavePly(lFilename)
  else
      gMesh.SaveObj(lFilename);
end; *)


procedure TGLForm1.SaveMeshMenuClick(Sender: TObject);
begin
     SaveMesh(gMesh, true);
end;

(*procedure TGLForm1.SaveMeshMenuClick(Sender: TObject);
const
      kMeshFilter = 'OBJ (Widely supported)|*.obj|GIfTI (Neuroimaging)|*.gii|MZ3 (Small and fast)|*.mz3|PLY (Widely supported)|*.ply';
var
   nam, ext, x: string;
begin
  if length(gMesh.Faces) < 1 then begin
     showmessage('Unable to save: no mesh is loaded (use File/Open).');
     exit;
  end;
  SaveMeshDialog.Filter  := kMeshFilter;
  SaveMeshDialog.FilterIndex := gPrefs.SaveAsFormat + 1;
  if gPrefs.SaveAsFormat = 4 then
     ext := '.ply'
  else if gPrefs.SaveAsFormat = 0 then
     ext := '.obj'
  else if gPrefs.SaveAsFormat = 1 then
    ext := '.gii'
  else
    ext := '.mz3';
  SaveMeshDialog.DefaultExt := ext;
  if fileexists(gPrefs.PrevFilename[1]) then begin
    nam := gPrefs.PrevFilename[1];
    nam := changeFileExt(nam, ext);
    SaveMeshDialog.Filename := nam;
  end else
      SaveMeshDialog.Filename := '';
  if not SaveMeshDialog.Execute then exit;
  if length(SaveMeshDialog.Filename) < 1 then exit;
  x := UpperCase(ExtractFileExt(SaveMeshDialog.Filename));
  if (x <> '.MZ3') and (x <> '.PLY') and (x <> '.OBJ')  and (x <> '.GII') then begin
     x := ext;
     SaveMeshDialog.Filename := SaveMeshDialog.Filename + x;
  end;
  if (x = '.MZ3') then
     SaveMz3
  else if (x = '.GII') then
     gMesh.SaveGii(SaveMeshDialog.Filename)
  else if (x = '.PLY') then
     gMesh.SavePly(SaveMeshDialog.Filename)
  else
      gMesh.SaveObj(SaveMeshDialog.Filename);
end;  *)

procedure TGLForm1.UniformChange(Sender: TObject);
begin
  SurfaceAppearanceChange(Sender);
 //ZUniformChange(Sender);
  //Updatetimer.enabled := true;
end;

procedure TGLForm1.UpdateTimerTimer(Sender: TObject);
begin
  if isBusy or gMesh.isBusy then exit; //defer
  Updatetimer.enabled := false;
  if ( gPrefs.initScript <> '') then begin
  	  if (gPrefs.initScript = ('-')) and  FileExists(ParamStr(ParamCount)) then begin
    	gPrefs.initScript := ParamStr(ParamCount);
    	if (upcase(ExtractFileExt(gPrefs.initScript)) <> '.PY') and (upcase(ExtractFileExt(gPrefs.initScript )) <> '.TXT') then begin
        	{$IFDEF UNIX}writeln('Assuming file is image not script (not .py or .txt) '+gPrefs.initScript);{$ENDIF}
        	gPrefs.initScript := '';
        	OpenMesh(ParamStr(ParamCount));
        	exit;
       	end;
    end;
     OpenScript(gPrefs.initScript);
     gPrefs.initScript := '';
     Updatetimer.enabled := true; //On MacOS the panels may need to be re-drawn, force a refresh
  end;
  ToolPanel.Refresh;
  GLbox.Invalidate;
end;

//{$DEFINE RELOADTRACK}

procedure TGLForm1.FormShow(Sender: TObject);
begin
 GLBox.MakeCurrent(false);
 gPrefs.SupportBetterRenderQuality := InitGLSL(true);
 gCube := TGLCube.Create(GLBox);
 //gClrbar:= TGLClrbar.Create(GLBox);
 UpdateFont(true);
 ClrbarClr(gPrefs.ColorbarColor);
 if (gPrefs.ColorbarColor = WhiteClrbarMenu.tag) then WhiteClrbarMenu.checked := true;
 if (gPrefs.ColorbarColor = TransWhiteClrbarMenu.tag) then TransWhiteClrbarMenu.checked := true;
 if (gPrefs.ColorbarColor = BlackClrbarMenu.tag) then BlackClrbarMenu.checked := true;
 if (gPrefs.ColorbarColor = TransBlackClrbarMenu.tag) then TransBlackClrbarMenu.checked := true;
 SetColorbarPosition;
 GLFinish;
 GLBox.ReleaseContext;
 MultiPassRenderingToolsUpdate;
 ShaderDropChange(sender);
 {$IFDEF LCLCocoa} SetDarkMode; {$ENDIF}
 //{$IFDEF Windows}UpdateOverlaySpread;{$ENDIF}//July2017 - scripting on High-dpi, reset scaling
 if (gPrefs.initScript <> '' ) then
    UpdateTimer.enabled := true;
end;

procedure TGLForm1.FormCreate(Sender: TObject);
var
  i: integer;
  s : string;
  c: char;
  forceReset: boolean = false;
begin
  DefaultFormatSettings.DecimalSeparator := '.'; //OBJ/GII/Etc write real numbers as 1.23 not 1,23
  FileMode := fmOpenRead;  //in case files set with read-only permissions
  //check if user includes parameters
  gPrefs.initScript := ''; //e.g. 'c:\dir\script.gls'
  i := 1;

  while i <= ParamCount do begin
     s := ParamStr(i);
     if (length(s)> 1) and (s[1]='-') then begin
         c := upcase(s[2]);
         if c='R' then
            forceReset := true
         else if (i < paramcount) and (c='S') then begin
           inc(i);
           gPrefs.InitScript := ParamStr(i);
         end;
     end;// else if fileexists(ParamStr(i)) then //length > 1 char
        //gPrefs.InitScript := ParamStr(i);
     inc(i);
   end; //for each parameter
  //launch program
  CreateMRU;
  FormCreateShaders;
  gPrefs.RenderQuality:= kRenderBetter;// kRenderPoor; ;
  if (not ResetIniDefaults) and (not forceReset) then
    IniFile(true,IniName,gPrefs)
  else begin
    SetDefaultPrefs(gPrefs,true, true);//reset everything to defaults!
    if MessageDlg('Use advanced graphics? Press "Yes" for better quality. Press "Cancel" for old hardware.', mtConfirmation, [mbYes, mbCancel], 0) = mrCancel then
      gPrefs.RenderQuality:= kRenderPoor;
  end;
  //initscript:
  if (not forceReset) and (gPrefs.InitScript = '')  and (gPrefs.StartupScript) then begin
    s :=  ScriptDir + pathdelim + 'startup.py';
    if (fileexists(s)) then
       gPrefs.InitScript := s;
    s :=  ScriptDir + pathdelim + 'startup.gls';
    if (gPrefs.InitScript = '') and (fileexists(s)) then
       gPrefs.InitScript := s;
    if (gPrefs.InitScript = '') and (fileexists(gPrefs.PrevScriptName[1])) then
       gPrefs.InitScript := gPrefs.PrevScriptName[1];
  end;
  if (gPrefs.InitScript = '') and (ParamCount >= 1) and (not forceReset) and (fileexists(ParamStr(ParamCount))) then
  	gPrefs.initScript := '-'; //not sure if the user is passing script or file?
  OverlayBoxCreate;//after we read defaults
  {$IFDEF Darwin} Application.OnDropFiles:= AppDropFiles; {$ENDIF}
  //{$IFDEF Windows} //July 2017  - see overlay box create
          //StringGrid1.DefaultRowHeight := ScaleY(28,96);
  //{$ENDIF}
  {$IFDEF LCLCarbon}
  GLForm1.OnDropFiles:= nil; //avoid drop for form and application
  {$ENDIF}
  clipPlane.X := 2;
  gMesh := TMesh.Create;
  gMesh.isBusy := true;
  gNode := TMesh.Create;
  gMesh.isZDimIsUp := gPrefs.ZDimIsUp;
  gNode.isZDimIsUp := gPrefs.ZDimIsUp;
  gTrack := TTrack.Create;
  if (gPrefs.TrackTubeSlices > 2) and (gPrefs.TrackTubeSlices < 22) then
     gTrack.TrackTubeSlices := gPrefs.TrackTubeSlices;
  gTrack.isTubes := gPrefs.TracksAreTubes;
  Application.ShowButtonGlyphs:= sbgNever;
  GLbox:= TOpenGLControl.Create(CenterPanel);
  //GLBox.DepthBits:=16;
  GLBox.Parent := GLForm1;
 {$IFDEF COREGL}
   GLbox.OpenGLMajorVersion:= 3;
   GLbox.OpenGLMinorVersion:= 3;
   {$IFDEF Linux}
   writeln('OpenGL 3.3 with 8/8/8/24 bits of R/G/B/Dpth required. Use glxinfo to test capabilities.');
   {$ENDIF}
 {$ELSE}
   GLbox.OpenGLMajorVersion:= 2;
   GLbox.OpenGLMinorVersion:= 1;
    {$IFDEF Linux}
    writeln('OpenGL 2.1 with 8/8/8/24 bits of R/G/B/Dpth required. Use glxinfo to test capabilities.');
    {$ENDIF}
   {$ENDIF}
   GLbox.AutoResizeViewport:= true;   // http://www.delphigl.com/forum/viewtopic.php?f=10&t=11311
   if gPrefs.MultiSample then
      GLBox.MultiSampling:= 4;
   GLBox.OnDblClick :=  GLboxDblClick;
   GLBox.OnMouseDown := GLboxMouseDown;
   GLBox.OnMouseMove := GLboxMouseMove;
   GLBox.OnMouseUp := GLboxMouseUp;
   GLBox.OnClick:= GLBoxClick;
  GLBox.OnMouseWheel := GLboxMouseWheel;
  GLBox.OnPaint := GLboxPaint;
  GLBox.Align := alClient;
   {$IFDEF LCLCocoa}
   SetRetina;//GLBox.WantsBestResolutionOpenGLSurface:=gPrefs.RetinaDisplay;
   {$ENDIF}

  (*{$IFDEF COREGL}
  GLbox.OpenGLMajorVersion:= 3;
  GLbox.OpenGLMinorVersion:= 3;
  {$ELSE}
  GLbox.OpenGLMajorVersion:= 2;
  GLbox.OpenGLMinorVersion:= 1;
  {$ENDIF}
  GLbox.AutoResizeViewport:= true;   // http://www.delphigl.com/forum/viewtopic.php?f=10&t=11311
  if gPrefs.MultiSample then
     GLBox.MultiSampling:= 4;
  GLBox.OnMouseDown := GLboxMouseDown;
  GLBox.OnMouseMove := GLboxMouseMove;
  GLBox.OnMouseUp := GLboxMouseUp;
  {$IFDEF LCLCocoa}
  SetRetina;//GLBox.WantsBestResolutionOpenGLSurface:=gPrefs.RetinaDisplay;
  {$ENDIF}
  //GLBox.OnMouseWheel := GLboxMouseWheel;
  GLBox.OnPaint := GLboxPaint;
  FormCreateShaders;*)
  UpdateMRU;
  if (gPrefs.OcclusionAmount <> occlusionTrack.Position) and (gPrefs.OcclusionAmount >= 0) and (gPrefs.OcclusionAmount <= 100) then
     occlusionTrack.Position:= gPrefs.OcclusionAmount;
  ColorBarVisibleMenu.Checked := gPrefs.Colorbar;
  AdditiveOverlayMenu.Checked := gPrefs.AdditiveOverlay;
  gMesh.isAdditiveOverlay := gPrefs.AdditiveOverlay;
  if gPrefs.InitScript <> '' then
     gMesh.MakePyramid
  else begin
    if (gPrefs.LoadTrackOnLaunch) and fileexists(gPrefs.PrevTrackname) then
      OpenTrack(gPrefs.PrevTrackname)
    else if fileexists(gPrefs.PrevFilename[1]) then
      OpenMesh(gPrefs.PrevFilename[1])
    else
      gMesh.MakePyramid;
  end;
  ScriptingGenerateTemplateMenu(true);
  ScriptingGenerateTemplateMenu(false);

  gMesh.isBusy := false;
  isBusy := false;

  {$IFDEF Darwin}
  ExitMenu.Visible := false;
  //CopyMenu.enabled := false; //https://bugs.freepascal.org/view.php?id=33632
  ScriptingNewMenu.ShortCut := ShortCut(Word('N'), [ssMeta]);
  ScriptingRunMenu.ShortCut := ShortCut(Word('R'), [ssMeta]);
  CurvMenuTemp.ShortCut:= ShortCut(Word('K'), [ssMeta]);
  CloseMenu.ShortCut :=  ShortCut(Word('W'), [ssMeta]);
  SwapYZMenu.ShortCut :=  ShortCut(Word('X'), [ssMeta]);
  //ScriptMenu.ShortCut :=  ShortCut(Word('Z'), [ssMeta]);
  //ScriptMenu.ShortCut :=  ShortCut(Word('J'), [ssMeta]);
  OpenMenu.ShortCut :=  ShortCut(Word('O'), [ssMeta]);
  SaveMenu.ShortCut :=  ShortCut(Word('S'), [ssMeta]);
  CopyMenu.ShortCut :=  ShortCut(Word('C'), [ssMeta]);
  LeftMenu.ShortCut :=  ShortCut(Word('L'), [ssCtrl]);
  RightMenu.ShortCut :=  ShortCut(Word('R'), [ssCtrl]);
  AnteriorMenu.ShortCut :=  ShortCut(Word('A'), [ssCtrl]);
  PosteriorMenu.ShortCut :=  ShortCut(Word('P'), [ssCtrl]);
  SuperiorMenu.ShortCut :=  ShortCut(Word('S'), [ssCtrl]);
  InferiorMenu.ShortCut :=  ShortCut(Word('I'), [ssCtrl]);
  //HelpMenu.Visible := false;  //bizarre Cocoa behavior: crash with overlay box changes?!?
  {$ELSE}
  HelpMenu.Visible := true;
  LeftMenu.ShortCut :=  ShortCut(Word('L'), [ssAlt]);
  RightMenu.ShortCut :=  ShortCut(Word('R'), [ssAlt]);
  AnteriorMenu.ShortCut :=  ShortCut(Word('A'), [ssAlt]);
  PosteriorMenu.ShortCut :=  ShortCut(Word('P'), [ssAlt]);
  SuperiorMenu.ShortCut :=  ShortCut(Word('S'), [ssAlt]);
  InferiorMenu.ShortCut :=  ShortCut(Word('I'), [ssAlt]);
  AppleMenu.Visible := false;
  {$ENDIF}
  {$IFDEF COREGL} {$IFDEF LCLCarbon} ERROR - Carbon does not support OpenGL core profile: either switch to Cocoa or comment out "COREGL" in opts.inc{$ENDIF} {$ENDIF}
  OrientCubeMenu.Checked :=  gPrefs.OrientCube;

end;

procedure TGLForm1.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
   OpenMesh(Filenames[0]);
   //caption := 'abba'+inttostr(random(888));
end;

procedure TGLForm1.GLBoxClick(Sender: TObject);
begin

end;

procedure TGLForm1.AppDropFiles(Sender: TObject; const FileNames: array of String);
begin
  //With MacOS and Lazarus 1.9, thee following code caused OpenMesh to be called twice
 //OpenMesh(Filenames[0]);
end;

end.

