unit textfx;
{$Include opts.inc} //compile for either dglOpenGL or glext
interface
uses
  {$IFDEF DGL} dglOpenGL, {$ELSE} gl, {$IFDEF COREGL}glext,{$ENDIF} {$ENDIF}
  define_types, prefs, classes;

procedure TextArrow (X,Y,Sz: single; NumStr: string; orient: integer;FontColor,ArrowColor: TRGBA);
procedure Enter2D(lPrefs: TPrefs);

implementation

const

  kVert : array [1..50] of tpoint = (
   (X:0;Y:0),(X:0;Y:4),(X:0;Y:8),(X:0;Y:12),(X:0;Y:13),
(X:0;Y:14),(X:0;Y:15),(X:0;Y:16),(X:0;Y:17),(X:0;Y:18),
(X:0;Y:22),(X:0;Y:24),(X:0;Y:28),(X:2;Y:14),(X:4;Y:0),
(X:4;Y:4),(X:4;Y:8),(X:4;Y:11),(X:4;Y:12),(X:4;Y:13),
(X:4;Y:15),(X:4;Y:16),(X:4;Y:17),(X:4;Y:22),(X:4;Y:24),
(X:4;Y:28),(X:8;Y:0),(X:8;Y:14),(X:8;Y:18),(X:8;Y:24),
(X:14;Y:0),(X:14;Y:4),(X:14;Y:12),(X:14;Y:13),(X:14;Y:16),
(X:14;Y:17),(X:14;Y:22),(X:14;Y:24),(X:14;Y:28),(X:16;Y:14),
(X:18;Y:0),(X:18;Y:4),(X:18;Y:11),(X:18;Y:12),(X:18;Y:13),
(X:18;Y:15),(X:18;Y:16),(X:18;Y:22),(X:18;Y:24),(X:18;Y:28)
);

 kStripRaw : array [0..11,1..28] of byte = (
    (16, 12, 2, 25, 15, 16, 31, 32, 42, 38, 49, 39, 25, 26, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (15, 25, 27, 30, 26, 25, 13, 12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (42, 41, 16, 1, 22, 4, 22, 47, 4, 33, 47, 39, 49, 26, 25, 12, 24, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (16, 3, 2, 17, 15, 16, 31, 32, 42, 33, 44, 40, 19, 22, 35, 33, 47, 39, 49, 26, 25, 12, 24, 11, 0, 0, 0, 0 ),
    (13, 26, 7, 21, 18, 46, 43, 50, 41, 39, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (49, 50, 25, 13, 20, 9, 20, 45, 9, 36, 45, 31, 42, 15, 16, 2, 17, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (38, 48, 49, 37, 39, 38, 26, 25, 12, 23, 9, 5, 36, 34, 20, 23, 5, 15, 2, 31, 32, 42, 33, 44, 35, 0, 0, 0 ),
    (11, 13, 24, 25, 13, 50, 25, 49, 38, 27, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (16, 4, 2, 19, 15, 16, 31, 32, 42, 33, 44, 40, 19, 22, 35, 33, 47, 39, 49, 26, 25, 12, 22, 8, 14, 22, 19, 4 ),
    (16, 3, 2, 17, 15, 16, 31, 32, 42, 33, 44, 47, 19, 22, 35, 33, 47, 39, 49, 26, 25, 12, 22, 8, 19, 0, 0, 0 ),
    (1, 15, 2, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (6, 10, 28, 29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    );
 kStripLocal : array [0..11,1..28] of byte = (
 (1, 2, 3, 4, 5, 1, 6, 7, 8, 9, 10, 11, 4, 12, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
   (1, 2, 3, 4, 5, 2, 6, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (1, 2, 3, 4, 5, 6, 5, 7, 6, 8, 7, 9, 10, 11, 12, 13, 14, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (1, 2, 3, 4, 5, 1, 6, 7, 8, 9, 10, 11, 12, 13, 14, 9, 15, 16, 17, 18, 19, 20, 21, 22, 0, 0, 0, 0 ),
    (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (1, 2, 3, 4, 5, 6, 5, 7, 6, 8, 7, 9, 10, 11, 12, 13, 14, 15, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (1, 2, 3, 4, 5, 1, 6, 7, 8, 9, 10, 11, 12, 13, 14, 9, 11, 15, 16, 17, 18, 19, 20, 21, 22, 0, 0, 0 ),
    (1, 2, 3, 4, 2, 5, 4, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (1, 2, 3, 4, 5, 1, 6, 7, 8, 9, 10, 11, 4, 12, 13, 9, 14, 15, 16, 17, 18, 19, 12, 20, 21, 12, 4, 2 ),
    (1, 2, 3, 4, 5, 1, 6, 7, 8, 9, 10, 11, 12, 13, 14, 9, 11, 15, 16, 17, 18, 19, 13, 20, 12, 0, 0, 0 ),
    (1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ),
    (1, 2, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 )
    );
 kStripCount : array [0..11] of byte = (15, 8, 18, 24, 11, 18, 25, 11, 28, 25, 4, 4 );
 kStripWid : array [0..11] of byte = (9, 5, 9, 9, 9, 9, 9, 9, 9, 9, 2, 4 );

procedure Enter2D(lPrefs: TPrefs);
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, lPrefs.window_width, 0, lPrefs.window_height,-1,1);//<- same effect as previous line
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glDisable(GL_DEPTH_TEST);
end;

function PrintHt (Sz: single): single;
begin
  result := Sz * 14;//14-pixel tall font
end;

function Char2Int (c: char): integer;
begin
    result := ord(c)-48;//ascii '0'..'9' = 48..58
    if result = -3 then result := 11; // '-' minus;
    if (result < 0) or (result > 11) then result := 10; //'.'or''
end;

function PrintWid (Sz: single; NumStr: string): single;
var
  i: integer;
begin
  result := 0;
  if length(NumStr) < 1 then
    exit;
  for i := 1 to length(NUmStr) do begin
    result := result + kStripWid[Char2Int(NumStr[i])  ] + 1;  ;
  end;
  if result < 1 then
    exit;
  result := result -1;//fence post: no gap after last gap between character
  result := result * sz;
end;

procedure PrintXY (X,Y,Sz: single; NumStr: string;FontColor: TRGBA);
//draws numerical strong with 18-pixel tall characters. If Sz=2.0 then characters are 36-pixel tall
//Unless you use multisampling, fractional sizes will not look good...
var
  i, j, k: integer;
begin
  if length(NumStr) < 1 then
    exit;
  glLoadIdentity();
  glTranslatef(x,y,0.0); //pixelspace space
  glScalef(Sz ,Sz,0.0);
  glTranslatef(1,0,0.0);//makes a nice border in front, so first and last item effectively have one pixel on outside
  //glColor4f(1,1,1,1);
  glColor4ub (FontColor.r, FontColor.g, FontColor.b, FontColor.a);
  for i := 1 to length(NUmStr) do begin
    j := Char2Int(NumStr[i]);
    glBegin(GL_TRIANGLE_STRIP);
    for k := 1 to kStripCount[j] do
        glVertex2f(0.5*kVert[kStripRaw[j,k]].X,0.5*kVert[kStripRaw[j,k]].Y);
    glEnd;
      glTranslatef(kStripWid[j]+1,0,0)
  end;
end;

procedure TextArrow (X,Y,Sz: single; NumStr: string; orient: integer; FontColor,ArrowColor: TRGBA);
//orient code 1=left,2=top,3=right,4=bottom
var
  lW,lH,lW2,lH2,T: single;
begin
  if NumStr = '' then exit;
  glLoadIdentity();
  lH := PrintHt(Sz);
  lH2 := (lH/2);
  lW := PrintWid(Sz,NumStr);
  lW2 := (lW/2);
  glColor4ub (ArrowColor.r, ArrowColor.g, ArrowColor.b,ArrowColor.a);
  case Orient of
    1: begin
      glBegin(GL_TRIANGLE_STRIP);
        glVertex2f(X-lH2-lW-2*Sz,Y+LH2+Sz);
        glVertex2f(X-lH2-lW-2*Sz,Y-lH2-Sz);
        glVertex2f(X-lH2,Y+lH2+Sz);
        glVertex2f(X-lH2,Y-lH2-Sz);
        glVertex2f(X,Y);
      glEnd;
      PrintXY (X-lW-lH2-1.5*Sz,Y-lH2,Sz, NumStr,FontColor);
    end;
    3: begin
      glBegin(GL_TRIANGLE_STRIP);
        glVertex2f(X+lH2+lW+2*Sz,Y+LH2+Sz);
        glVertex2f(X+lH2+lW+2*Sz,Y-lH2-Sz);
        glVertex2f(X+lH2,Y+lH2+Sz);
        glVertex2f(X+lH2,Y-lH2-Sz);
        glVertex2f(X,Y);
      glEnd;
      PrintXY (X+lH2,Y-lH2,Sz, NumStr,FontColor);
    end;
    4: begin //bottom
    glBegin(GL_TRIANGLE_STRIP);
      glVertex2f(X-lW2-Sz,Y-LH-lH2-2*Sz);//-
      glVertex2f(X-lW2-Sz,Y-lH2);
      glVertex2f(X+lW2+Sz,Y-LH-lH2-2*Sz);//-
      glVertex2f(X+lW2+Sz,Y-lH2);
      glVertex2f(X-lW2-Sz,Y-lH2);
      glVertex2f(X,Y);
    glEnd;
    PrintXY (X-lW2-Sz,Y-lH-LH2,Sz, NumStr,FontColor);
    end;
    else  begin
      if Orient = 5 then
        T := Y-LH-Sz-lH2
      else
        T := Y;
    glBegin(GL_TRIANGLE_STRIP);
      glVertex2f(X-lW2-Sz,T+LH+2*Sz+lH2);
      glVertex2f(X-lW2-Sz,T+lH2);
      glVertex2f(X+lW2+Sz,T+LH+2*Sz+lH2);
      glVertex2f(X+lW2+Sz,T+lH2);
      glVertex2f(X-lW2-Sz,T+lH2);
      glVertex2f(X,T);
    glEnd;
    PrintXY (X-lW2-Sz,T+lH2+Sz,Sz, NumStr,FontColor);
    end;
  end;//case
end;

end.
