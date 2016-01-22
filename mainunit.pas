unit mainunit;
 {$Include opts.inc} //compile for either dglOpenGL or glext
{$mode delphi}{$H+}
interface
uses
  {$IFDEF DGL} dglOpenGL, {$ELSE} gl, glext, {$ENDIF}
  {$IFDEF COREGL} gl_core_3d, {$ELSE}     gl_legacy_3d, {$ENDIF}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,math,
  ExtCtrls, OpenGLContext, mesh, LCLintf, ComCtrls, Menus, graphtype,
  ClipBrd, shaderui, shaderu, prefs, userdir, LCLtype, Grids, Spin, matmath,
  colorTable, Track, types,  define_types, meshify,  gl_2d, zstream, gl_core_matrix;

type
  { TGLForm1 }
  TGLForm1 = class(TForm)
    AOLabel: TLabel;
    ColorbarMenu: TMenuItem;
    HelpMenu: TMenuItem;
    DisplaySepMenu: TMenuItem;
    AdvancedMenu: TMenuItem;
    AdditiveOverlayMenu: TMenuItem;
    GLBox: TOpenGLControl;
    TransparencySepMenu: TMenuItem;
    ReverseFacesMenu: TMenuItem;
    SwapYZMenu: TMenuItem;
    SaveMeshMenu: TMenuItem;
    VolumeToMeshMenu: TMenuItem;
    ResetMenu: TMenuItem;
    OrientCubeMenu: TMenuItem;
    Pref2Menu: TMenuItem;
    About2Menu: TMenuItem;
    XRayLabel: TLabel;
    EdgeSizeVariesCheck: TCheckBox;
    FileSepMenu: TMenuItem;
    occlusionTrack: TTrackBar;
    SaveTracksMenu: TMenuItem;
    meshAlphaTrack: TTrackBar;
    MeshBlendTrack: TTrackBar;
    NodeSizeVariesCheck: TCheckBox;
    PrefMenu: TMenuItem;
    NodeMaxEdit: TFloatSpinEdit;
    NodeMinEdit: TFloatSpinEdit;
    EdgeMinLabel1: TLabel;
    NodeThreshDrop: TComboBox;
    NodeScaleLabel1: TLabel;
    NodeScaleTrack: TTrackBar;
    EdgeMinEdit: TFloatSpinEdit;
    EdgeMaxEdit: TFloatSpinEdit;
    LUTdropEdge: TComboBox;
    EdgeBox: TGroupBox;
    EdgeColorVariesCheck: TCheckBox;
    NodeScaleLabel: TLabel;
    EdgeMinLabel: TLabel;
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
    TrackLengthLabel1: TLabel;
    MeshSaturationTrack: TTrackBar;
    TrackWidthLabel: TLabel;
    TrackLengthTrack: TTrackBar;
    TrackLengthLabel: TLabel;
    LightAziTrack: TTrackBar;
    ClipAziTrack: TTrackBar;
    ClipBox: TGroupBox;
    ClipTrack: TTrackBar;
    CollapseToolPanelBtn: TButton;
    ColorDialog1: TColorDialog;
    LightElevTrack: TTrackBar;
    ClipElevTrack: TTrackBar;
    TrackBox: TGroupBox;
    Label2: TLabel;
    DepthLabel: TLabel;
    AzimuthLabel: TLabel;
    ElevationLabel: TLabel;
    LUTdrop: TComboBox;
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
    TrackWidthLabel1: TLabel;
    TrackWidthLabel2: TLabel;
    TrackWidthLabel3: TLabel;
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
    CollapsedToolPanel: TPanel;
    OverlayBox: TGroupBox;
    ShaderBox: TGroupBox;
    ShaderDrop: TComboBox;
    ErrorTimer: TTimer;
    StringGrid1: TStringGrid;
    OverlayTimer: TTimer;
    UpdateTimer: TTimer;
    ToolPanel: TPanel;
    SaveDialog1: TSaveDialog;
    SaveMenu: TMenuItem;
    ObjectColorMenu: TMenuItem;
    OpenMenu: TMenuItem;
    procedure AdditiveOverlayMenuClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure MeshColorBoxChange(Sender: TObject);
    procedure OpenNode(Filename: string);
    procedure OpenTrack(Filename: string);
    procedure OpenOverlay(Filename: string);
    procedure OpenEdge(Filename: string);
    procedure OpenMesh(Filename: string);
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
    procedure CollapseToolPanelBtnClick(Sender: TObject);
    procedure ColorBarMenuClick(Sender: TObject);
    procedure CopyMenuClick(Sender: TObject);
    procedure DepthLabelClick(Sender: TObject);
    procedure DisplayMenuClick(Sender: TObject);
    procedure ElevationLabelClick(Sender: TObject);
    procedure ErrorTimerTimer(Sender: TObject);
    procedure GLBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure LUTdropChange(Sender: TObject);
    procedure NodePrefChange(Sender: TObject);
    procedure OrientCubeMenuClick(Sender: TObject);
    procedure OverlayTimerStart;
    procedure OverlayBoxCreate;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure AppDropFiles(Sender: TObject; const FileNames: array of String);
    procedure CreateRender(w,h: integer; isToScreen: boolean);
    procedure GLboxPaint(Sender: TObject);
    procedure GLboxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLboxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLboxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ObjectColorMenuClick(Sender: TObject);
    procedure OpenMenuClick(Sender: TObject);
    procedure OverlayTimerTimer(Sender: TObject);
    procedure OverlayVisible(lOverlay: integer; lVisible: boolean);
    procedure PrefMenuClick(Sender: TObject);
    procedure QuickColorClick(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure ResetMenuClick(Sender: TObject);
    procedure RestrictEdgeMenuClick(Sender: TObject);
    procedure RestrictMenuClick(Sender: TObject);
    procedure ReverseFacesMenuClick(Sender: TObject);
    procedure SaveMenuClick(Sender: TObject);
    procedure SaveMesh;
    procedure SaveMeshMenuClick(Sender: TObject);
    procedure SaveTracksMenuClick(Sender: TObject);
    function ScreenShot: TBitmap;
    procedure SetOverlayTransparency(Sender: TObject);
    procedure ShaderBoxResize(Sender: TObject);
    procedure ShaderDropChange(Sender: TObject);
    procedure ShowmessageError(s: string);
    procedure GLboxRequestUpdate(Sender: TObject);
    procedure SurfaceAppearanceChange(Sender: TObject);
    procedure ReadCell (ACol,ARow: integer; Update: boolean);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StringGrid1Exit(Sender: TObject);
    procedure StringGrid1KeyPress(Sender: TObject; var Key: char);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure SwapYZMenuClick(Sender: TObject);
    procedure TrackBoxChange(Sender: TObject);
   procedure UniformChange(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure UpdateImageIntensity;
    procedure UpdateLUT(lOverlay,lLUTIndex: integer; lChangeDrop: boolean);
    procedure UpdateOverlaySpread;// (lIndex: integer);
    //procedure SetOrtho (w,h: integer; isMultiSample: boolean);
    procedure AddMRU(lFilename: string);
    procedure UpdateMRU;
    procedure CreateMRU;
    procedure OpenMRU(Sender: TObject);//open template or MRU
    procedure UpdateToolbar;
    procedure MultiPassRenderingToolsUpdate;
    procedure VolumeToMeshMenuClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  GLForm1: TGLForm1;
  gPrefs : TPrefs;

implementation

{$R *.lfm}
var
  gMesh: TMesh;
  gNode: TMesh;
  gTrack: TTrack;
  isBusy: boolean = true;
  gTypeInCell: boolean = false;
  gEnterCell: boolean = false;

  gPrevCol, gPrevRow: integer; //last selected overlay
  gDistance : single = 1;
  gElevation : integer =20;
  gAzimuth : integer = 250;
  gMouseX : integer = -1;
  gMouseY : integer = -1;
  GLerror : string = '';
  clipPlane : TPoint4f; //clipping bottom
  //lightPos : TPoint3f;//array [1..3] of single = (0.0, 0.0, 0.0); //light RGB
const
  kScreenShotZoom  = 3;

  kFname=0;
  kLUT=1;
  kMin=2;
  kMax=3;

procedure TGLForm1.MultiPassRenderingToolsUpdate;
var
  lBetter: boolean;
begin
  lBetter := (gPrefs.RenderQuality <> kRenderPoor) and (gPrefs.SupportBetterRenderQuality);
  AOLabel.Visible:= lBetter;
  occlusionTrack.Visible:= lBetter;
  XRayLabel.Visible:= lBetter;
  MeshBlendTrack.Visible:= lBetter;
  meshAlphaTrack.visible :=  lBetter;
end;

procedure TGLForm1.VolumeToMeshMenuClick(Sender: TObject);
const
     kVolFilter = 'NIfTI volume|*.hdr;*.nii;*nii.gz';
begin
  OpenDialog.Filter := kVolFilter;
  OpenDialog.Title := 'Select volume to convert';
  if not OpenDialog.Execute then exit;
  Nii2Mesh(OpenDialog.FileName, gPrefs.SaveAsFormat);
end;

procedure TGLForm1.UpdateToolbar;
begin
 NodeBox.Visible:= (length(gNode.nodes) > 0) ;
 EdgeBox.Visible:= (length(gNode.edges) > 0) ;
 TrackBox.Visible:= (gTrack.n_count > 0);
 OverlayBox.Visible := (gMesh.OpenOverlays > 0);
 MeshColorBox.Visible := (length(gMesh.vertexRGBA) > 0);
 Memo1.Lines.clear;
end; //UpdateToolbar()

procedure TGLForm1.OpenNode(Filename: string);
 var
     edgename: string;
begin
  if not gNode.LoadFromFile(FileName) then exit;
  gPrefs.PrevNodename := FileName;
 NodeMinEdit.value := gNode.nodePrefs.minNodeThresh;
 NodeMaxEdit.value := gNode.nodePrefs.maxNodeThresh;
 if gNode.NodePrefs.isNodeThresholdBySize then
  NodeThreshDrop.ItemIndex := 0  //threshold by size
 else
   NodeThreshDrop.ItemIndex := 1; //threshold by color
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
   gPrefs.AdditiveOverlay :=  AdditiveOverlayMenu.Checked ;
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

procedure TGLForm1.OpenEdge(Filename: string);
var
  ext, nodename: string;
begin
 ext := UpperCase(ExtractFileExt(Filename));
 if (ext = '.NODE') or (length(gNode.nodes) < 1) then begin
     nodename := ChangeFileExt(FileName, '.node');
     if fileexists(nodename) then begin
        OpenNode(nodename);
        exit;
     end;
 end;
 if not gNode.LoadEdge(Filename) then exit;
 UpdateToolbar;
 edgeMinEdit.Value := 0;
 edgeMaxEdit.Value := gNode.nodePrefs.maxEdgeAbs;
 OpenDialog.InitialDir:= ExtractFileDir(FileName);
 GLBoxRequestUpdate(nil);
end;

procedure TGLForm1.OpenOverlay(Filename: string);
begin
   if not gMesh.LoadOverlay(FileName, gPrefs.SmoothVoxelwiseData) then begin
     GLBOxRequestUpdate(nil);
     UpdateToolbar;
     exit;
   end;
   gPrefs.PrevOverlayname := FileName;
   OpenDialog.InitialDir:= ExtractFileDir(FileName);
   StringGrid1.RowCount := gMesh.OpenOverlays+1;
   UpdateToolbar;
   UpdateOverlaySpread;
end;

procedure TGLForm1.OpenTrack(Filename: string);
begin
 if gTrack.LoadFromFile(FileName) then begin
    OpenDialog.InitialDir:= ExtractFileDir(FileName);
    gPrefs.PrevTrackname := FileName;
 end;
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
     if not fileexists(filename) then exit;
     AssignFile(f, FileName);
     Reset(f,1);
     FileMode := fmOpenRead;
     szRead := FileSize(f);
     SetLength(Str, szRead);
     BlockRead(f, Str[1],szRead);
     CloseFile(f);
     if (pos('POLYGONS ', Str) > 0) then result := true; //faces
end;

function isGiiMesh (filename: string): boolean;
//returns true if file is a valid mesh (faces+vertices), returns false if overlay map
var
      f: file;
      Str: string;
      szRead: integer;
begin
     result := false;
     if not fileexists(filename) then exit;
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
     if not fileexists(Filename) then exit;
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

procedure TGLForm1.OpenMesh(Filename: string);
var
    curvname, ext: string;
begin
  if not FileExists(Filename) then exit;
  ext := UpperCase(ExtractFileExt(Filename));
  if (ext = '.NII') or (ext = '.HDR')  or (ext = '.GZ') or (ext = '.ANNOT') or (ext = '.W') or (ext = '.CURV')  then begin
    OpenOverlay(Filename);
    exit;
  end else if (ext = '.VTK') and (not isVtkMesh (Filename)) then begin
    OpenTrack(Filename);  //.vtk files can be either meshes or tracks - autodetect
    exit;
  end else if (length(gMesh.Faces) > 0) and (ext = '.MZ3') and (not isMz3Mesh (Filename)) then begin
    OpenOverlay(Filename);  //GIfTI files can be meshes or overlays - autodetect
    exit;
  end else if (length(gMesh.Faces) > 0) and (ext = '.GII') and (not isGiiMesh (Filename)) then begin
    OpenOverlay(Filename);  //GIfTI files can be meshes or overlays - autodetect
    exit;
  end else if (ext = '.TRK') or (ext = '.FIB') then begin
    OpenTrack(Filename);
    exit;
  end else if (ext = '.EDGE') then begin
    OpenEdge(Filename);
    exit;
  end else if (ext = '.NODE') then begin
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
  AddMRU(Filename);
  if gMesh.isFreeSurferMesh then begin
     curvname := changefileext(Filename, '.curv');
     if fileexists(curvname) then
        OpenOverlay(curvname);
  end;
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

procedure TGLForm1.ShaderBoxResize(Sender: TObject);
const
kMinMemoSz= 32;
var
   lDesiredControlSz: integer;
begin
  if not ShaderBox.Visible then exit;
  lDesiredControlSz := ShaderPanelHeight;
  if ShaderBox.Height > (lDesiredControlSz+kMinMemoSz) then begin
    Memo1.Height := ShaderBox.Height - lDesiredControlSz;
    Memo1.visible := true;
  end
  else
     Memo1.visible := false;
  ShaderBox.Refresh;
end;


function ResetIniDefaults : boolean;
begin
     //result := ( GetKeyState(VK_MENU)<> 0) or (GetKeyState(VK_LWIN) <> 0) or (GetKeyState(VK_CONTROL) <> 0)  or (ssShift in KeyDataToShiftState(VK_SHIFT)) ;
     result := ( GetKeyState(VK_MENU)<> 0) or (GetKeyState(VK_LWIN) <> 0)  or (ssShift in KeyDataToShiftState(VK_SHIFT)) ;
end;

procedure TGLForm1.ShaderDropChange(Sender: TObject);
begin
  SetShader(ShaderDir+pathdelim+ShaderDrop.Items[ShaderDrop.ItemIndex]+'.txt');
  ShaderBoxResize(Sender);
  GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.GLboxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if gMouseX < 0 then exit; //mouse is not down
 gElevation := gElevation + (Y - gMouseY);
 gAzimuth := gAzimuth - (X - gMouseX);
 while gAzimuth > 360 do
       gAzimuth := gAzimuth -360;
 while gAzimuth < -360 do
       gAzimuth := gAzimuth + 360;
 gMouseX := X;
 gMouseY := Y;
 GLBox.invalidate;//GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.GLboxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     gMouseX := -1; //released
end;

procedure TGLForm1.GLboxMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
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
 GLBoxRequestUpdate(Sender);
end;

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
  if (Sender <> nil) and (gMesh.OpenOverlays > 0) then begin
    if gMesh.Overlay[1].LUTindex > 14 then
         if MessageDlg('Curvature overlay open', 'Close the FreeSurfer CURV file?', mtConfirmation, [mbYes, mbNo],0) = mrNo then
           exit;
  end;
  gMesh.CloseOverlays;
  GLForm1.SetFocusedControl(nil);
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
  GLboxRequestUpdate(Sender);
end;

procedure TGLForm1.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
 if aRow < 1 then exit;
 if (gMesh.Overlay[aRow].LUTvisible) then exit;
 //make rows of invisible overlays red
 TStringGrid(Sender).Canvas.Font.Color := clRed;
 TStringGrid(Sender).Canvas.TextOut(aRect.Left+2,aRect.Top+2, TStringGrid(Sender).Cells[ACol, ARow]);
end;

procedure TGLForm1.StringGrid1Exit(Sender: TObject);
begin
      ReadCell(gPrevCol,gPrevRow, true);
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

procedure TGLForm1.StringGrid1KeyPress(Sender: TObject; var Key: char);
const
  EnterKey = #13;
  BackspaceKey = #8;
  ControlC = #3;   //  Copy
  ControlV = #22;  //  Paste
var
  ACol,ARow: integer;
  S: string;
begin
ACol := abs(GLForm1.StringGrid1.Selection.Right);
  ARow := abs(GLForm1.StringGrid1.Selection.Top);
  //if ((ACol <> gPrevCol) or (ACol <> gPrevCol)) and    ChangeOverlayUpdate;
  gPrevCol := ACol;
  gPrevRow := ARow;
  if (not (IsDigit (Key) or (Key = DefaultFormatSettings.DecimalSeparator) or (Key = '+') or (Key = '-') or
        (Key = ControlC) or (Key = ControlV) or (Key = BackspaceKey) or
        (Key = EnterKey))) then begin
    Key := #0;
    exit;
  end;
  if (Key = kTab) then begin
    OverlayTimerStart;
    exit;
  end;
  if (Key = kTab) or (Key = kCR) then begin
    ReadCell(gPrevCol,gPrevRow, true);
    OverlayTimerStart;
    exit;
  end;
  gTypeInCell := true;
  OverlayTimerStart;
  if(( GLForm1.StringGrid1.Selection.Top = GLForm1.StringGrid1.Selection.Bottom ) and
		( GLForm1.StringGrid1.Selection.Left = GLForm1.StringGrid1.Selection.Right )) then begin
          if gEnterCell then
             S := ''
          else
	      S := GLForm1.StringGrid1.Cells[ GLForm1.StringGrid1.Selection.Left,GLForm1.StringGrid1.Selection.Top ] ;
          gEnterCell := false;
          if ( ( Key = kDEL ) or ( Key = kBS ) )then begin
              if( length( S ) > 0 ) then begin
                  setlength( S, length( S ) - 1 ) ;
              end;
          end else
	      S := S + Key ;
     {$IFDEF FPC} GLForm1.StringGrid1.Cells[ GLForm1.StringGrid1.Selection.Left,GLForm1.StringGrid1.Selection.Top ] := S;
      {$ENDIF}
  end ;
  ReadCell(gPrevCol,gPrevRow, false);
end;

procedure TGLForm1.UpdateOverlaySpread;// (lIndex: integer);
var
  lIndex: integer;
begin
 GLForm1.LUTdrop.visible := false;
 if gMesh.OpenOverlays < 1 then exit;
 for lIndex := 1 to gMesh.OpenOverlays do begin
   GLForm1.StringGrid1.Cells[kFName, lIndex] := gMesh.Overlay[lIndex].FileName;
   GLForm1.StringGrid1.Cells[kLUT, lIndex] := GLForm1.LutDrop.Items[gMesh.Overlay[lIndex].LUTindex];
    OverlayBox.Height :=  2+ ( (2+gMesh.OpenOverlays)*(StringGrid1.DefaultRowHeight+1));
    StringGrid1.Cells[kMin,lIndex] := FloatToStrF(gMesh.Overlay[lIndex].WindowScaledMin, ffGeneral, 8, 4);
    StringGrid1.Cells[kMax,lIndex] := FloatToStrF(gMesh.Overlay[lIndex].WindowScaledMax, ffGeneral, 8, 4);
  end;
  UpdateImageIntensity;
end;

procedure TGLForm1.OverlayVisible(lOverlay: integer; lVisible: boolean);
begin
  if (lOverlay > gMesh.OpenOverlays) or (lOverlay < 1) then
    exit;
  gMesh.Overlay[lOverlay].LUTvisible := lVisible;
  UpdateOverlaySpread;
end;

procedure TGLForm1.PrefMenuClick(Sender: TObject);
var
  PrefForm: TForm;
  OkBtn: TButton;
  BitmapAlphaCheck, SmoothVoxelwiseDataCheck, TracksAreTubesCheck: TCheckBox; //MultiPassRenderingCheck
  ShaderForBackgroundOnlyCombo, ZDimIsUpCombo, QualityCombo, SaveAsCombo: TComboBox;

  QualityLabel: TLabel;
begin
  PrefForm:=TForm.Create(nil);
  PrefForm.SetBounds(100, 100, 520, 262);
  PrefForm.Caption:='Preferences';
  PrefForm.Position := poScreenCenter;
  PrefForm.BorderStyle := bsDialog;
    //Bitmap Alpha
  BitmapAlphaCheck:=TCheckBox.create(PrefForm);
  BitmapAlphaCheck.Checked := gPrefs.ScreenCaptureTransparentBackground;
  BitmapAlphaCheck.Caption:='Background transparent in bitmaps';
  BitmapAlphaCheck.Left := 8;
  BitmapAlphaCheck.Top := 8;
  BitmapAlphaCheck.Parent:=PrefForm;
  //SmoothVoxelwiseData
  SmoothVoxelwiseDataCheck:=TCheckBox.create(PrefForm);
  SmoothVoxelwiseDataCheck.Checked := gPrefs.SmoothVoxelwiseData;
  SmoothVoxelwiseDataCheck.Caption:='Smooth voxel-based images';
  SmoothVoxelwiseDataCheck.Left := 8;
  SmoothVoxelwiseDataCheck.Top := 38;
  SmoothVoxelwiseDataCheck.Parent:=PrefForm;
  //TracksAreTubes
  TracksAreTubesCheck:=TCheckBox.create(PrefForm);
  TracksAreTubesCheck.Checked := gPrefs.TracksAreTubes;
  TracksAreTubesCheck.Caption:='Better (but slower) tracks';
  TracksAreTubesCheck.Left := 8;
  TracksAreTubesCheck.Top := 68;
  TracksAreTubesCheck.Parent:=PrefForm;
  //ZDimIsUp
  ZDimIsUpCombo := TComboBox.create(PrefForm);
  ZDimIsUpCombo.Items.Add('Z-dimension is up (Neuroimaging/Talairach)');
  ZDimIsUpCombo.Items.Add('Y-dimension is up (Blender/OpenGL)');
  if (gPrefs.ZDimIsUp) then
     ZDimIsUpCombo.ItemIndex := 0
  else
      ZDimIsUpCombo.ItemIndex := 1;
  ZDimIsUpCombo.Left := 8;
  ZDimIsUpCombo.Top := 98;
  ZDimIsUpCombo.Width := PrefForm.Width -16;
  ZDimIsUpCombo.Style := csDropDownList;
  ZDimIsUpCombo.Parent:=PrefForm;
  //ShaderForBackgroundOnly
  ShaderForBackgroundOnlyCombo := TComboBox.create(PrefForm);
  ShaderForBackgroundOnlyCombo.Items.Add('Tracks, nodes and overlays use fixed shader');
  ShaderForBackgroundOnlyCombo.Items.Add('Tracks, nodes and overlays use background shader');
  if (gPrefs.ShaderForBackgroundOnly) then
     ShaderForBackgroundOnlyCombo.ItemIndex := 0
  else
      ShaderForBackgroundOnlyCombo.ItemIndex := 1;
  ShaderForBackgroundOnlyCombo.Left := 8;
  ShaderForBackgroundOnlyCombo.Top := 128;
  ShaderForBackgroundOnlyCombo.Width := PrefForm.Width -16;
  ShaderForBackgroundOnlyCombo.Style := csDropDownList;
  ShaderForBackgroundOnlyCombo.Parent:=PrefForm;

  //SaveAsObj     SaveAsFormat
  SaveAsCombo :=TComboBox.create(PrefForm);
  SaveAsCombo.Left := 8;
  SaveAsCombo.Top := 158;
  SaveAsCombo.Width := PrefForm.Width -16;
  SaveAsCombo.Items.Add('OBJ Format: Export to other programs');
  SaveAsCombo.Items.Add('GII Format: Neuroimaging');
  SaveAsCombo.Items.Add('MZ3 Format: Fast and small');
  SaveAsCombo.ItemIndex:= gPrefs.SaveAsFormat;
  SaveAsCombo.Style := csDropDownList;
  SaveAsCombo.Parent:=PrefForm;
  //SinglePass
  (*MultiPassRenderingCheck:=TCheckBox.create(PrefForm);
  MultiPassRenderingCheck.Checked := gPrefs.MultiPassRendering;
  MultiPassRenderingCheck.Caption:='Better rendering (slower)';
  MultiPassRenderingCheck.Left := 8;
  MultiPassRenderingCheck.Top := 128;
  MultiPassRenderingCheck.Parent:=PrefForm; *)
  //Smooth
  QualityCombo:=TComboBox.create(PrefForm);
  QualityCombo.Left := 8;
  QualityCombo.Top := 188;
  QualityCombo.Width := PrefForm.Width -16;
  QualityCombo.Items.Add('Quality: Poor (old hardware)');
  QualityCombo.Items.Add('Quality: Better');
  //QualityCombo.Items.Add('Quality: Best');
  QualityCombo.ItemIndex:= gPrefs.RenderQuality;
  QualityCombo.Style := csDropDownList;
  QualityCombo.Parent:=PrefForm;
  if not gPrefs.SupportBetterRenderQuality then begin
      QualityCombo.Visible := false;
      QualityLabel:=TLabel.create(PrefForm);
      QualityLabel.Left := 8;
      QualityLabel.Top := 188;
      QualityLabel.Width := PrefForm.Width -16;
      QualityLabel.Caption := 'NOTE: Hardware only supports poor rendering.';
      QualityLabel.Parent:=PrefForm;
  end;
  //SingleShader
 (*   ZDimIsUpCombo := TComboBox.create(PrefForm);
  ZDimIsUpCombo.Items.Add('Z-dimension is up (Neuroimaging/Talairach)');
  ZDimIsUpCombo.Items.Add('Y-dimension is up (Blender/OpenGL)');
  if (gPrefs.ZDimIsUp) then
     ZDimIsUpCombo.ItemIndex := 0
  else
      ZDimIsUpCombo.ItemIndex := 1;
  ZDimIsUpCombo.Left := 8;
  ZDimIsUpCombo.Top := 98;
  ZDimIsUpCombo.Width := PrefForm.Width -16;
  ZDimIsUpCombo.Style := csDropDownList;
  ZDimIsUpCombo.Parent:=PrefForm;  *)

  //OK button
  OkBtn:=TButton.create(PrefForm);
  OkBtn.Caption:='OK';
  OkBtn.Left := PrefForm.Width - 128;
  OkBtn.Top := 228;
  OkBtn.Parent:=PrefForm;
  OkBtn.ModalResult:= mrOK;
  PrefForm.ShowModal;
  if PrefForm.ModalResult <> mrOK then exit; //if user closes window with out pressing "OK"
  gPrefs.ScreenCaptureTransparentBackground :=  BitmapAlphaCheck.Checked;
  gPrefs.SmoothVoxelwiseData := SmoothVoxelwiseDataCheck.Checked;
  if ShaderForBackgroundOnlyCombo.ItemIndex = 1 then
     gPrefs.ShaderForBackgroundOnly := false
  else
      gPrefs.ShaderForBackgroundOnly := true;
  if ZDimIsUpCombo.ItemIndex = 1 then
     gPrefs.ZDimIsUp := false
  else
      gPrefs.ZDimIsUp := true;
  gMesh.isZDimIsUp := gPrefs.ZDimIsUp;
  gNode.isZDimIsUp := gPrefs.ZDimIsUp;

  //gPrefs.SaveAsObj := SaveAsObjCheck.Checked;
  gPrefs.SaveAsFormat := SaveAsCombo.ItemIndex;
  (*if MultiPassRenderingCheck.Checked <> gPrefs.MultiPassRendering then begin
     gPrefs.MultiPassRendering := MultiPassRenderingCheck.Checked;
     MultiPassRenderingToolsUpdate;
  end;*)
  if QualityCombo.ItemIndex <> gPrefs.RenderQuality then begin
     gPrefs.RenderQuality := QualityCombo.ItemIndex;
     MultiPassRenderingToolsUpdate;
     GLBoxRequestUpdate(Sender);
  end;
  if (gPrefs.TracksAreTubes <> TracksAreTubesCheck.Checked) then begin
    gPrefs.TracksAreTubes := TracksAreTubesCheck.Checked;
    gTrack.isTubes:= gPrefs.TracksAreTubes;
    gTrack.isRebuildList:= true;

  end;
  FreeAndNil(PrefForm);
      GLBoxRequestUpdate(Sender);
end; // PrefMenuClick()

procedure TGLForm1.QuickColorClick(Sender: TObject);
begin
  case (sender as TMenuItem).tag of
       1: gPrefs.ObjColor:= RGBToColor(210,148,148); //pink
       2: gPrefs.ObjColor:= RGBToColor(128,162,128); //green
       3: gPrefs.ObjColor:= RGBToColor(167,171,253); //blue
       else gPrefs.ObjColor:= RGBToColor(192,192,192);
  end;
  {$IFDEF COREGL}
  gMesh.isRebuildList := true;
  {$ENDIF}
  GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.ResetMenuClick(Sender: TObject);
begin
     gDistance := 1;
     gElevation := 20;
     gAzimuth := 250;
     gPrefs.AdditiveOverlay:= false;
     gMesh.isAdditiveOverlay:= gPrefs.AdditiveOverlay;
     AdditiveOverlayMenu.Checked:= gPrefs.AdditiveOverlay;
     //set tracks
     TrackLengthTrack.Position:= 20;
     TrackWidthTrack.Position := 2;
     TrackDitherTrack.Position := 3;
     //mesh colors
     MeshSaturationTrack.Position := 100;
     MeshTransparencyTrack.Position:= 100;
     //clipping
     ClipTrack.Position := 0;
     ClipAziTrack.Position := 180;
     ClipElevTrack.Position := 0;
     //set shaders
     OcclusionTrack.Position := 25;
     MeshAlphaTrack.Position := 100;
     MeshBlendTrack.Position:= 0;
     LightElevTrack.Position:= 25;
     LightAziTrack.Position := 0;
     ShaderDrop.ItemIndex:= 0;
     ShaderDropChange(Sender);
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

procedure TGLForm1.StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Row: integer;
begin
  if (gMesh.OpenOverlays < 1) then exit;
  if (X >  (GLForm1.StringGrid1.ColWidths[kFName])) then
    exit; //not one of the first two colums
  Row := GLForm1.StringGrid1.DefaultRowHeight div 2;
  Row := round((Y-Row)/GLForm1.StringGrid1.DefaultRowHeight);
  GLForm1.LUTdrop.visible := false;
  if (Row < 1) or (Row > gMesh.OpenOverlays) then exit;
  If  ((ssRight in Shift) or (ssShift in Shift)) then begin //hide overlay
      OverlayVisible(Row, (not gMesh.Overlay[Row].LUTvisible) );
      OverlayTimerStart;
      exit;
  end;
  if (gMesh.OpenOverlays < 2) then
    exit; //can not shuffle order of a single item!
  //DemoteOrder(Row); //TO DO
end;

procedure TGLForm1.UpdateImageIntensity;
var
  i: integer;
begin
     gTypeInCell := false;
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

procedure TGLForm1.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var R: TRect;
begin
  if (gTypeInCell) then UpdateImageIntensity;
  if (ACol < kLUT) or (ACol > kMax) or (ARow < 1) or (ARow > gMesh.OpenOverlays) then
     exit;
  ReadCell(gPrevCol,gPrevRow, false);
  if (ACol = kLUT) and  (ARow <> 0) then begin
    //Size and position the combo box to fit the cell
    R := StringGrid1.CellRect(ACol, ARow);
    R.Left := R.Left + GLForm1.StringGrid1.Left;
    R.Right := R.Right + GLForm1.StringGrid1.Left;
    R.Top := R.Top + GLForm1.StringGrid1.Top;
    R.Bottom := R.Bottom + GLForm1.StringGrid1.Top;
    //Show the combobox
    with GLForm1.LUTdrop do begin
      Tag := 0;
      Left := R.Left + 1;
      Top := R.Top + 1;
      Width := (R.Right + 1) - R.Left;
      Height := (R.Bottom + 1) - R.Top;
      ItemIndex := Items.IndexOf(GLForm1.StringGrid1.Cells[ACol, ARow]);
      Visible := True;
      //SetFocus;
      Tag := ARow;
    end;
  end else begin
      GLForm1.LUTdrop.visible := false;
      ReadCell(ACol,ARow, false);
      gEnterCell := true;
  end;
  CanSelect := True;
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

procedure TGLForm1.ReadCell (ACol,ARow: integer; Update: boolean);
var
  lF: single;
  lS: string;
begin
  if (ARow < GLForm1.StringGrid1.FixedRows) or (ARow > kMaxOverlays) or (ARow >= GLForm1.StringGrid1.RowCount) then  //2015
    exit;
  if (ACol <> kMin) and (ACol <> kMax) then
    exit;
  lS := StringGrid1.Cells[ACol,ARow];
  if not HasDigit(lS) then
    exit;
  try
    lF := strtofloatDef(lS, 0);
  except
          exit;
  end; {except}
  if ACol = kMin then
    gMesh.Overlay[ARow].WindowScaledMin := lF
  else
    gMesh.Overlay[ARow].WindowScaledMax := lF;
  if Update then UpdateImageIntensity;
end;


procedure TGLForm1.CreateRender(w,h: integer; isToScreen: boolean);
var
  origin: TPoint3f;
  isMultiSample: boolean;
  meshAlpha, meshBlend, ambientOcclusionFrac, scale: single;
begin
  if (h < 1) or (w < 1) then exit;
  if gNode.isBusy or gMesh.isBusy or isBusy then begin //come back later
     UpdateTimer.enabled := true;
     exit;
  end;
  InitGLSL(false);
  origin := GetOrigin(scale);
  //glUseProgram(gShader.program3d);
  {$IFNDEF COREGL}
  SetLighting(gPrefs);
  {$ENDIF}
  //nDrawScene(w, h, true, false, gPrefs, origin, lightPos, ClipPlane, scale, gShader.Distance, gelevation, gazimuth, gMesh,gNode, gTrack);
  if (gPrefs.RenderQuality <> kRenderPoor) and (gPrefs.SupportBetterRenderQuality)  then begin;
    meshAlpha := meshAlphaTrack.position/meshAlphaTrack.max;
    meshBlend := MeshBlendTrack.position/MeshBlendTrack.max;
    ambientOcclusionFrac := occlusionTrack.Position/occlusionTrack.max;

    //first pass: 3D draw all items: framebuffer f1
    isMultiSample := setFrame (w, h, gShader.f1, true );
    DrawScene(w,h, true,isMultiSample, gPrefs, origin, ClipPlane, scale, gDistance, gElevation, gAzimuth, gMesh,gNode, gTrack);
    //second pass: 3D draw overlay items only: framebuffer f2
    isMultiSample := setFrame (w, h, gShader.f2, true );
    DrawScene(w,h, false,isMultiSample, gPrefs, origin,  ClipPlane, scale, gDistance, gElevation, gAzimuth, gMesh,gNode, gTrack);
    if (isToScreen)  then begin
       releaseFrame; //GOOD: multipass, no multisampling
       Set2DDraw (w,h, red(gPrefs.BackColor) ,green(gPrefs.BackColor), blue(gPrefs.BackColor));
       RunAoGLSL( gShader.f1,  gShader.f2, 1,  meshAlpha, meshBlend, ambientOcclusionFrac, gDistance);
    end else begin  //SCREENSHOT - supersampled
        setFrame (w, h, gShader.fScreenShot, true );
        Set2DDraw (w,h, red(gPrefs.BackColor) ,green(gPrefs.BackColor), blue(gPrefs.BackColor));
        RunAoGLSL( gShader.f1,  gShader.f2, kScreenShotZoom,  meshAlpha,meshBlend,ambientOcclusionFrac,gDistance);
    end;
  end else begin //else POOR quality : do not use framebuffers
      //if isToScreen then
         releaseFrame;
      //else
      //    setFrame (w, h, gShader.fScreenShot, true ); //SCREENSHOT - supersampled
      DrawScene(w, h, true, false, gPrefs, origin, ClipPlane, scale, gDistance, gelevation, gazimuth, gMesh,gNode, gTrack);
  end;
  if gPrefs.OrientCube then
     DrawCube (w, h,  gAzimuth, gElevation);


//glViewport( 0, 0, w, h); //required when bitmap zoom <> 1


  if (gPrefs.Colorbar)  then begin
    //RunOffGLSL; //turn off shading
    if gMesh.OpenOverlays > 0 then
       DrawCLUT( gPrefs.ColorBarPos, 0.01, gPrefs, gMesh, w, h) //color bar based on overlays
    else
       DrawCLUT( gPrefs.ColorBarPos, 0.01, gPrefs, gNode, w, h); //color bar based on nodes
  end;
  //TestColorBar(gPrefs, w, h);
  //DrawText (gPrefs, w, h);
  if isToScreen then
     GLbox.SwapBuffers;
  //nDraw;
    isBusy := false;

end;

procedure TGLForm1.GLboxPaint(Sender: TObject);
begin
 CreateRender(GLBox.Width, GLBox.Height, true);
 if UpdateTimer.enabled then
    UpdateTimerTimer(Sender);

end;

function TGLForm1.ScreenShot: TBitmap;
var
  RawImage: TRawImage;
  p: array of byte;
  w, h, x, y, BytePerPixel,trackLineWidth: integer;
  z:longword;
  DestPtr: PInteger;
begin
  if (gTrack.n_count > 0) and (not gTrack.isTubes) then begin  //tracks are drawn in pixels, so zoom appropriately!
     trackLineWidth := gTrack.LineWidth;
     gTrack.LineWidth := 2 * gTrack.LineWidth * kScreenShotZoom;
     gTrack.isRebuildList:= true;
  end;
  if gPrefs.RenderQuality <> kRenderPoor then begin
     w := GLbox.Width * kScreenShotZoom;
     h := GLbox.Height * kScreenShotZoom;
  end else begin //poor does not use frame buffer - assume resolution limited to window
      w := GLbox.Width;
      h := GLbox.Height;
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
  GLBox.MakeCurrent;
  CreateRender(w, h, false); //draw to framebuffer fScreenShot
  {$IFDEF Darwin} //http://lists.apple.com/archives/mac-opengl/2006/Nov/msg00196.html
  glReadPixels(0, 0, w, h, $80E1, $8035, @p[0]); //OSX-Darwin   GL_BGRA = $80E1;  GL_UNSIGNED_INT_8_8_8_8_EXT = $8035;
  {$ELSE}
   {$IFDEF Linux}
     glReadPixels(0, 0, w, h, GL_RGBA, GL_UNSIGNED_BYTE, @p[0]); //Linux-Windows   GL_RGBA = $1908; GL_UNSIGNED_BYTE
   {$ELSE}
    glReadPixels(0, 0, w, h, $80E1, GL_UNSIGNED_BYTE, @p[0]); //Linux-Windows   GL_RGBA = $1908; GL_UNSIGNED_BYTE
   {$ENDIF}
  {$ENDIF}
  setFrame (2, 2, gShader.fScreenShot, false ); // <- release huge framebuffer
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

procedure TGLForm1.AboutMenuClick(Sender: TObject);
const
  kSamp = 36;
var
  s: dword;
  i: integer;
begin
 //showmessage(inttostr(gTrack.utime)); exit;
 s := gettickcount();
 for i := 1 to kSamp do begin
     gAzimuth := (gAzimuth + 10) mod 360;
     GLbox.Repaint;
  end;
  showmessage( 'Surf Ice '+' 1 Jan 2016 '
   {$IFDEF CPU64} + '64-bit'
   {$ELSE} + '32-bit'
   {$ENDIF}
   {$IFDEF DGL} + ' DGL'{$ENDIF}
   +LineEnding+' www.mricro.com :: BSD 2-Clause License (opensource.org/licenses/BSD-2-Clause)'
   +LineEnding+' FPS ' +inttostr(round( (kSamp*1000)/(gettickcount-s)))
   +LineEnding+' Mesh Vertices '+inttostr(length(gMesh.vertices))+' Faces '+  inttostr(length(gMesh.faces)) +' Colors '+  inttostr(length(gMesh.vertexRGBA))
   +LineEnding+' Track Vertices '+inttostr(gTrack.n_vertices)+' Faces '+  inttostr(gTrack.n_faces) +' Count ' +inttostr(gTrack.n_count)
   +LineEnding+' Node Vertices '+inttostr(length(gNode.vertices))+' Faces '+  inttostr(length(gNode.faces))
   +LineEnding+' GPU '+gShader.Vendor);
end;

procedure TGLForm1.AddNodesMenuClick(Sender: TObject);
const
  kNodeFilter = 'BrainNet Node/Edge|*.node;*.edge|Any file|*.*';
var
  ext, f2: string;
begin
     if Fileexists(gPrefs.PrevNodename) then
        OpenDialog.InitialDir := ExtractFileDir(gPrefs.PrevNodename);
     OpenDialog.Filter := kNodeFilter;
     OpenDialog.Title := 'Select Node/Edge file';
     if not OpenDialog.Execute then exit;
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
end;

procedure TGLForm1.AddOverlayMenuClick(Sender: TObject);
const
     kOverlayFilter = 'Mesh or NIfTI|*.*';
begin
  OpenDialog.Filter := kOverlayFilter;
  OpenDialog.Title := 'Select overlay file';
  if Fileexists(gPrefs.PrevOverlayname) then
        OpenDialog.InitialDir := ExtractFileDir(gPrefs.PrevOverlayname);
  if not OpenDialog.Execute then exit;
  //OpenDialog.FileName := ('/Users/rorden/Desktop/Surf_Ice/other/motor_4t95vol.nii.gz');
  //OpenDialog.FileName := ('/Users/rorden/Desktop/Surf_Ice/other/motor_4t95mesh.gii');
  OpenOverlay(OpenDialog.FileName);
end;

procedure TGLForm1.AddTracksMenuClick(Sender: TObject);
const
     kTrackFilter = 'VTK or TrakVis|*.trk;*.fib;*.vtk|Any file|*.*';
begin
 OpenDialog.Filter := kTrackFilter;
 OpenDialog.Title := 'Select track file';
 if Fileexists(gPrefs.PrevTrackname) then
    OpenDialog.InitialDir := ExtractFileDir(gPrefs.PrevTrackname);
 if not OpenDialog.Execute then exit;
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

procedure TGLForm1.CollapseToolPanelBtnClick(Sender: TObject);
begin
  ToolPanel.Visible := not ToolPanel.Visible;
  CollapsedToolPanel.Visible := not CollapsedToolPanel.Visible;
  Self.ActiveControl := nil;
end;

procedure TGLForm1.ColorBarMenuClick(Sender: TObject);
begin
  gPrefs.Colorbar := not gPrefs.Colorbar;
  ColorBarMenu.Checked := gPrefs.Colorbar;
  GLBox.Invalidate;
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
     if ClipTrack.Position > 900 then
        ClipTrack.Position := 0
     else
          ClipTrack.Position := 100 * ((ClipTrack.Position +100) div 100);
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

procedure TGLForm1.GLBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
 if abs(WheelDelta) < 5 then exit;
  if WheelDelta < 0 then
     gDistance := gDistance * 0.9
  else
    gDistance := gDistance * 1.1;
  if gDistance > kMaxDistance then gDistance := kMaxDistance;
  if gDistance < 0.5 then gDistance := 0.5;
  GLBox.Invalidate;
end;

procedure TGLForm1.UpdateLUT(lOverlay,lLUTIndex: integer; lChangeDrop: boolean);
begin
  if gMesh.OpenOverlays > kMaxOverlays then
    exit;
  if lLUTIndex >= LUTdrop.Items.Count then
    gMesh.Overlay[lOverlay].LUTindex:= 0
  else
    gMesh.Overlay[lOverlay].LUTindex:= lLUTIndex;
  if lChangeDrop then begin
    StringGrid1.Cells[kLUT, lOverlay] := LUTdrop.Items[gMesh.Overlay[lOverlay].LUTindex];
    //LUTdrop.ItemIndex := gOverlayImg[lOverlay].LUTindex;
  end;
  gMesh.overlay[lOverlay].LUT := UpdateTransferFunction (gMesh.Overlay[lOverlay].LUTindex);
  //LUTdropLoad(gMesh.Overlay[lOverlay].LUTindex, gMesh.Overlay[lOverlay].LUT, LUTdrop.Items[lLUTindex], gOverlayCLUTrec[lOverlay]);
end;

procedure TGLForm1.LUTdropChange(Sender: TObject);
var intRow: Integer;
begin
  inherited;
  if GLForm1.Lutdrop.Tag < 1 then
     exit;
  intRow := GLForm1.StringGrid1.Row;
  if intRow < 0 then
    intRow := GLForm1.Lutdrop.Tag;
  if (intRow < 1) or (intRow > kMaxOverlays) then
    exit;
  GLForm1.StringGrid1.Cells[kLUT, intRow] := GLForm1.LutDrop.Items[GLForm1.LUTdrop.ItemIndex];
  UpdateLUT(intRow,GLForm1.LUTdrop.ItemIndex,false);
  OverlayTimerStart;
  GLForm1.StringGrid1.Selection:=TGridRect(Rect(-1,-1,-1,-1));
  LutDrop.visible := false;
end;

procedure TGLForm1.NodePrefChange(Sender: TObject);
var
  lo, hi: single;
begin
  gNode.nodePrefs.scaleNodeSize := NodeScaleTrack.Position/5;
  gNode.nodePrefs.nodeLUTindex := LUTdropNode.itemIndex;
  //gNode.nodePrefs.isNodeSizeVaries := NodeSizeVariesCheck.checked;
  gNode.nodePrefs.isEdgeSizeVaries := EdgeSizeVariesCheck.checked;
  gNode.nodePrefs.isNodeColorVaries := NodeColorVariesCheck.checked;
  gNode.nodePrefs.isEdgeColorVaries := EdgeColorVariesCheck.checked;
  lo := nodeMinEdit.Value;
  hi := nodeMaxEdit.value;
  sortsingle(lo, hi);
  gNode.nodePrefs.minNodeThresh := lo;
  gNode.nodePrefs.maxNodeThresh := hi;
  gNode.nodePrefs.edgeLUTindex:= LUTdropEdge.itemIndex;
  gNode.nodePrefs.scaleEdgeSize:= edgeScaleTrack.Position/10;
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
     Memo1.Lines.Add(format('Node size threshold min..max %g..%g',[gNode.NodePrefs.minNodeThresh, gNode.nodePrefs.maxNodeThresh]));
  end else begin
    gNode.nodePrefs.isNodeThresholdBySize := false;
    Memo1.Lines.Add(format('Node color threshold min..max %g..%g',[gNode.NodePrefs.minNodeThresh, gNode.nodePrefs.maxNodeThresh]));
  end;
  Memo1.Lines.Add(format('Edge range min..max %.4g..%.4g',[gNode.NodePrefs.minEdge, gNode.nodePrefs.maxEdge]));
  Memo1.Lines.Add(format('Edge threshold min..max %.4g..%.4g',[gNode.NodePrefs.minEdgeThresh, gNode.nodePrefs.maxEdgeThresh]));
  Memo1.Lines.Add(format('Edge color table %d',[gNode.nodePrefs.edgeLUTindex]) );
  Memo1.Lines.Add(format('Edge scale %.2g',[gNode.nodePrefs.scaleEdgeSize]) );
  gNode.isRebuildList := true;
  GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.OrientCubeMenuClick(Sender: TObject);
begin
 gPrefs.OrientCube := OrientCubeMenu.Checked;
 GLBoxRequestUpdate(Sender);
end;

procedure TGLForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //showmessage(IniName);
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
 OpenDialog.Filter := kMeshFilter;
 OpenDialog.Title := 'Select mesh file';
 if Fileexists(gPrefs.PrevFilename[1]) then
    OpenDialog.InitialDir := ExtractFileDir(gPrefs.PrevFilename[1]);
  if not OpenDialog.Execute then exit;
  OpenMesh(OpenDialog.Filename);
end;

procedure TGLForm1.OverlayBoxCreate;
var
   lSearchRec: TSearchRec;
   lStr: string;
begin
 StringGrid1.Selection := TGridRect(Rect(-1, -1, -1, -1));
 StringGrid1.DefaultRowHeight := LUTdrop.Height+1;
 StringGrid1.DefaultColWidth := (StringGrid1.width div 4)-2;
  {$IFDEF FPC} {$IFNDEF UNIX}
 if Screen.PixelsPerInch <> 96 then begin
     StringGrid1.DefaultColWidth := round(StringGrid1.width* (Screen.PixelsPerInch/96) * 0.25) - 2;
 end;
{$ENDIF}{$ENDIF}
  StringGrid1.Cells[kFname, 0] := 'Name';
  StringGrid1.Cells[kLUT, 0] := 'Color';
  StringGrid1.Cells[kMin, 0] := 'Min';
  StringGrid1.Cells[kMax, 0] := 'Max';
  LUTdrop.Items.Clear;
  LUTdrop.Items.Add('Grayscale');
  LUTdrop.Items.Add('Red-Yellow');
  LUTdrop.Items.Add('Blue-Green');
  LUTdrop.Items.Add('Red');
  LUTdrop.Items.Add('Green');
  LUTdrop.Items.Add('Blue');
  LUTdrop.Items.Add('Violet [r+b]');
  LUTdrop.Items.Add('Yellow [r+g]');
  LUTdrop.Items.Add('Cyan [g+b]');
  LUTdrop.Items.Add('Hot');
  LUTdrop.Items.Add('Bone');
  LUTdrop.Items.Add('Winter');
  LUTdrop.Items.Add('GE');
  LUTdrop.Items.Add('ACTC');
  LUTdrop.Items.Add('X-Rain');
  LUTdrop.Items.Add('FreeSurfer1');
  LUTdrop.Items.Add('FreeSurfer2');
  LUTdrop.Items.Add('FreeSurfer3');
  LUTdrop.Items.Add('FreeSurfer4');
  if DirectoryExists(ClutDir) then  begin
     if FindFirst(CLUTdir+pathdelim+'*.clut', faAnyFile, lSearchRec) = 0 then
	 repeat
               lStr := ChangeFileExt (ExtractFileName (lSearchRec.Name), '');
               if (length(lStr) > 0) and (lStr[1] <> '.') then
                  LUTdrop.Items.Add(lStr);
	 until (FindNext(lSearchRec) <> 0);
     FindClose(lSearchRec);
  end;
  LUTdropNode.Items := LUTdrop.Items;
  LUTdropEdge.Items := LUTdrop.Items;
  LUTdropNode.ItemIndex := 3;
  LUTdropEdge.ItemIndex := 1;
end;

procedure TGLForm1.OverlayTimerTimer(Sender: TObject);
begin
     OverlayTimer.Enabled := false;
     gMesh.isRebuildList:= true;
     gMesh.isAdditiveOverlay := gPrefs.AdditiveOverlay;
     GLbox.Invalidate;
end;

procedure TGLForm1.SaveMenuClick(Sender: TObject);
var
   bmp: TBitmap;
   png: TPortableNetworkGraphic;
begin
  if not SaveDialog1.execute then exit;
  bmp := ScreenShot;
  png := TPortableNetworkGraphic.Create;
  try
    png.Assign(bmp);    //Convert data into png
    png.SaveToFile(SaveDialog1.Filename);
  finally
    png.Free;
  end;
  bmp.Free;
end;

procedure TGLForm1.SaveMesh;
var
   i : integer;
   nam: string;
begin
  if not SaveMeshDialog.Execute then exit;
  gMesh.SaveMz3(SaveMeshDialog.Filename);
  for i :=  1 to gMesh.OpenOverlays do begin
        nam := changefileext(SaveMeshDialog.Filename, '_'+inttostr(i)+extractfileext(SaveMeshDialog.Filename));
        gMesh.SaveOverlay(nam, i);
  end;
end;

procedure TGLForm1.SaveMeshMenuClick(Sender: TObject);
var
   nam: string;
begin
  nam := '';
  if fileexists(gPrefs.PrevFilename[1]) then
    nam := gPrefs.PrevFilename[1];
  if gPrefs.SaveAsFormat = kSaveAsGii then begin
    SaveMeshDialog.Title := 'Save mesh as GIfTI';
    SaveMeshDialog.DefaultExt := '.gii';
    SaveMeshDialog.FileName:= changeFileExt(nam, SaveMeshDialog.DefaultExt);
    if not SaveMeshDialog.Execute then exit;
    gMesh.SaveGii(SaveMeshDialog.Filename);
    exit;
  end;
  if gPrefs.SaveAsFormat = kSaveAsMz3 then begin
    SaveMeshDialog.Title := 'Save mesh as Mz3';
    SaveMeshDialog.DefaultExt := '.mz3';
    SaveMeshDialog.FileName:= changeFileExt(nam, SaveMeshDialog.DefaultExt);
    SaveMesh;
    exit;
  end;
  SaveMeshDialog.Title := 'Save mesh as WaveFront Obj';
  SaveMeshDialog.DefaultExt := '.obj';
  SaveMeshDialog.FileName:= changeFileExt(nam, SaveMeshDialog.DefaultExt);
  if not SaveMeshDialog.Execute then exit;
  gMesh.SaveObj(SaveMeshDialog.Filename);

end;

procedure TGLForm1.SaveTracksMenuClick(Sender: TObject);
begin
 SaveMeshDialog.DefaultExt:= '.vtk';
 SaveMeshDialog.Title := 'Save tracks as VTK';
 if not SaveMeshDialog.Execute then exit;
 gTrack.SaveVtk(SaveMeshDialog.Filename);
end;

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
  GLbox.Invalidate;
end;

//{$DEFINE RELOADTRACK}

procedure TGLForm1.FormCreate(Sender: TObject);
begin
  CreateMRU;
  if not ResetIniDefaults then
    IniFile(true,IniName,gPrefs)
  else begin
    SetDefaultPrefs(gPrefs,true);//reset everything to defaults!
    if MessageDlg('Use advanced graphics? Press "Yes" for better quality. Press "Cancel" for old hardware.', mtConfirmation, [mbYes, mbCancel], 0) = mrCancel then
      gPrefs.RenderQuality:= kRenderPoor;
  end;
  gPrefs.RenderQuality:= kRenderBetter;// kRenderPoor; ;
  //gPrefs.Perspective := false; //optional: enforce perspective settings
  DefaultFormatSettings.DecimalSeparator := '.'; //OBJ/GII/Etc write real numbers as 1.23 not 1,23

  OverlayBoxCreate;//after we read defaults
  {$IFDEF Darwin} Application.OnDropFiles:= AppDropFiles; {$ENDIF}
  {$IFDEF Windows}
  StringGrid1.DefaultRowHeight := 28;
  {$ENDIF}
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
  gTrack.isTubes := gPrefs.TracksAreTubes;
  Application.ShowButtonGlyphs:= sbgNever;
  //GLbox:= TOpenGLControl.Create(GLForm1);
  //GLbox.Align := alClient;
  //GLBox.Parent := GLForm1;
  {$IFDEF COREGL}
  GLbox.OpenGLMajorVersion:= 4;
  {$ELSE}
  GLbox.OpenGLMajorVersion:= 2;
  {$ENDIF}
  GLbox.OpenGLMinorVersion:= 1;

  GLbox.AutoResizeViewport:= true;   // http://www.delphigl.com/forum/viewtopic.php?f=10&t=11311
  GLBox.MultiSampling:= 4;
  GLBox.OnMouseDown := GLboxMouseDown;
  GLBox.OnMouseMove := GLboxMouseMove;
  GLBox.OnMouseUp := GLboxMouseUp;
  //GLBox.OnMouseWheel := GLboxMouseWheel;
  GLBox.OnPaint := GLboxPaint;
  FormCreateShaders;
  UpdateMRU;
  if (gPrefs.OcclusionAmount <> occlusionTrack.Position) and (gPrefs.OcclusionAmount >= 0) and (gPrefs.OcclusionAmount <= 100) then
     occlusionTrack.Position:= gPrefs.OcclusionAmount;
  ColorBarMenu.Checked := gPrefs.Colorbar;
  AdditiveOverlayMenu.Checked := gPrefs.AdditiveOverlay;
  gMesh.isAdditiveOverlay := gPrefs.AdditiveOverlay;
  if (gPrefs.LoadTrackOnLaunch) and fileexists(gPrefs.PrevTrackname) then
    OpenTrack(gPrefs.PrevTrackname)
  else if fileexists(gPrefs.PrevFilename[1]) then
    OpenMesh(gPrefs.PrevFilename[1])
  else
    gMesh.MakePyramid;
  gMesh.isBusy := false;
  isBusy := false;
  {$IFDEF Darwin}
  OpenMenu.ShortCut :=  ShortCut(Word('O'), [ssMeta]);
  SaveMenu.ShortCut :=  ShortCut(Word('S'), [ssMeta]);
  CopyMenu.ShortCut :=  ShortCut(Word('C'), [ssMeta]);
  LeftMenu.ShortCut :=  ShortCut(Word('L'), [ssMeta]);
  RightMenu.ShortCut :=  ShortCut(Word('R'), [ssMeta]);
  AnteriorMenu.ShortCut :=  ShortCut(Word('A'), [ssMeta]);
  PosteriorMenu.ShortCut :=  ShortCut(Word('P'), [ssMeta]);
  InferiorMenu.ShortCut :=  ShortCut(Word('D'), [ssMeta]);
  SuperiorMenu.ShortCut :=  ShortCut(Word('U'), [ssMeta]);
  ExitMenu.Visible:= false;
  HelpMenu.Visible := false;
  {$ELSE}
  AppleMenu.Visible := false;
  {$ENDIF}
  {$IFDEF COREGL} {$IFDEF LCLCarbon} ERROR - Carbon does not support OpenGL core profile: either switch to Cocoa or comment out "COREGL" in opts.inc{$ENDIF} {$ENDIF}
  {$IFDEF LCLCocoa}
  //EditMenu.Visible := false;  //Broken prior to svn 50307
  {$ENDIF}
  OrientCubeMenu.Checked :=  gPrefs.OrientCube;
  GLBox.MakeCurrent(false);
  gPrefs.SupportBetterRenderQuality := InitGLSL(true);
  GLBox.ReleaseContext;
  MultiPassRenderingToolsUpdate;
   ShaderDropChange(sender);
  //AddNodesMenuClick(sender);
  //AddOverlayMenuClick(sender);
  //AddTracksMenuClick(sender);
  //VolumeToMeshMenuClick(sender);
end;

procedure TGLForm1.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
   OpenMesh(Filenames[0]);
end;

procedure TGLForm1.AppDropFiles(Sender: TObject; const FileNames: array of String);
begin
 OpenMesh(Filenames[0]);
end;

end.

