unit colorbar2d;
{$D-,L-,O+,Q-,R-,Y-,S-}
{$Include opts.inc} //compile for either dglOpenGL or glext
interface
uses
{$IFDEF DGL} dglOpenGL, {$ELSE} gl, glext, {$ENDIF}
define_types, textfx,  math, prefs, colorTable, mesh, shaderu;

//var
//   WINDOW_HEIGHT, WINDOW_WIDTH: integer;

procedure DrawCLUT ( lU: TUnitRect;lBorder: single; var lPrefs: TPrefs; lMesh: TMesh; window_width, window_height: integer );
function ColorBarPos(var  lU: TUnitRect): integer;

implementation
uses sysutils;

const
  kVertTextLeft = 1;
  kHorzTextBottom = 2;
  kVertTextRight = 3;
  kHorzTextTop = 4;

function ColorBarPos(var  lU: TUnitRect): integer;
begin
   SensibleUnitRect(lU);
   if abs(lU.R-lU.L) > abs(lU.B-lU.T) then begin //wide bars
    if (lU.B+lU.T) >1 then
      result := kHorzTextTop
    else
      result := kHorzTextBottom;
   end else begin //high bars
    if (lU.L+lU.R) >1 then
      result := kVertTextLeft
    else
      result := kVertTextRight;
   end;

end;

procedure DrawColorBarText(lMinIn,lMaxIn: single; var lUin: TUnitRect;lBorder: single; var lPrefs: TPrefs);
var
  lS: string;
  lOrient,lDesiredSteps,lPower,	lSteps,lStep,lDecimals,lStepPosScrn, lTextZoom: integer;
  lBarLength,lScrnL,lScrnT,lStepPos,l1stStep,lMin,lMax,lRange,lStepSize: single;
  lU: TUnitRect;
begin
  lU := lUin;
  lOrient := ColorBarPos(lU);
	 lMin := lMinIn;
	 lMax := lMaxIn;
   if (lMinIn < 0) and (lMaxIn <= 0) then begin
	  lMin := abs(lMinIn);
	  lMax := abs(lMaxIn);
   end;
   sortsingle(lMin,lMax);
   //next: compute increment
   lDesiredSteps := 4;
   lRange := abs(lMax - lMin);
   if lRange < 0.000001 then exit;
   lStepSize := lRange / lDesiredSteps;
   lPower := 0;
   while lStepSize >= 10 do begin
      lStepSize := lStepSize/10;
	    inc(lPower);
   end;
   while lStepSize < 1 do begin
	   lStepSize := lStepSize * 10;
	   dec(lPower);
   end;
   lStepSize := round(lStepSize) *Power(10,lPower);
   if lPower < 0 then
	    lDecimals := abs(lPower)
   else
	    lDecimals := 0;
   l1stStep := trunc((lMin)  / lStepSize)*lStepSize;
   lScrnL := lU.L * lPrefs.window_width;
   if lOrient =  kVertTextRight then
      lScrnL := lU.R * lPrefs.window_width;
   lScrnT := (lU.B) * lPrefs.window_height;
   if lOrient =  kHorzTextTop then
      lScrnT := ((lU.B) * lPrefs.window_height);
   if lOrient =  kHorzTextBottom then
      lScrnT := ((lU.T) * lPrefs.window_height);
   if l1stStep < (lMin) then l1stStep := l1stStep+lStepSize;
    lSteps := trunc( abs((lMax+0.0001)-l1stStep) / lStepSize)+1;
   if (lOrient = kVertTextLeft) or (lOrient = kVertTextRight) then //vertical bars
      lBarLength := lPrefs.window_height * abs(lU.B-lU.T)
   else
      lBarLength := lPrefs.window_width * abs(lU.L-lU.R);
   lTextZoom :=  trunc(lBarLength / 1000) + 1;
   for lStep := 1 to lSteps do begin
      lStepPos := l1stStep+((lStep-1)*lStepSize);
      lStepPosScrn := round( abs(lStepPos-lMin)/lRange*lBarLength);
      lS := realtostr(lStepPos,lDecimals);
      if (lMinIn < 0) and (lMaxIn <= 0) then
        lS := '-'+lS;
      if (lOrient = kVertTextLeft) or  (lOrient = kVertTextRight)  then
         TextArrow (lScrnL,lScrnT+ lStepPosScrn,lTextZoom,lS,lOrient,lPrefs.TextColor, lPrefs.TextBorder)
      else
         TextArrow (lScrnL+ lStepPosScrn,lScrnT,lTextZoom,lS,lOrient,lPrefs.TextColor, lPrefs.TextBorder);
		end;
    glLoadIdentity();
end; //DrawColorBarText

procedure SetOrder (l1,l2: single; var lSmall,lLarge: single);
//set lSmall to be the lesser of l1/l2 and lLarge the greater value of L1/L2
begin
  if l1 < l2 then begin
    lSmall := l1;
    lLarge := l2;
  end else begin
    lSmall := l2;
    lLarge := l1;
  end;
end;

procedure DrawCLUTx (var lCLUT: TLUT; lU: TUnitRect;lPrefs: TPrefs);
var
  lL,lT,lR,lB,lN: single;
  lI: integer;
begin
  SetOrder(lU.L,lU.R,lL,lR);
  SetOrder(lU.T,lU.B,lT,lB);
  lL := lL*lPrefs.window_width;
  lR := lR*lPrefs.window_width;
  lT := lT*lPrefs.window_height;
  lB := lB*lPrefs.window_height;
  if (lR-lL) > (lB-lT) then begin
    lN := lL;
    glBegin(GL_TRIANGLE_STRIP);
     glColor4ub (lCLUT[0].R, lCLUT[0].G,lCLUT[0].B,255);
     glVertex2f(lN,lT);
     glVertex2f(lN,lB);

     for lI := 1 to (255) do begin
        lN := (lI/255 * (lR-lL))+lL;
        glColor4ub (lCLUT[lI].R, lCLUT[lI].G,lCLUT[lI].B,255);
        glVertex2f(lN,lT);
        glVertex2f(lN,lB);

     end;
    glEnd;//STRIP
  end else begin //If WIDE, else TALL
    lN := lT;
    glColor4ub (lCLUT[0].R, lCLUT[0].G,lCLUT[0].B,255);
    glBegin(GL_TRIANGLE_STRIP);
     glVertex2f(lR, lN);
     glVertex2f(lL, lN);
     for lI := 1 to (255) do begin
        lN := (lI/255 * (lB-lT))+lT;
        glColor4ub (lCLUT[lI].R, lCLUT[lI].G,lCLUT[lI].B,255);
        glVertex2f(lR, lN);
        glVertex2f(lL, lN);
     end;
    glEnd;//POLYGON
  end;
end;

procedure DrawBorder (var lU: TUnitRect;lBorder: single; lPrefs: TPrefs);
var
    lL,lT,lR,lB: single;
begin
  if lBorder <= 0 then
    exit;
  SetOrder(lU.L,lU.R,lL,lR);
  SetOrder(lU.T,lU.B,lT,lB);
  glColor4ub(lPrefs.GridAndBorder.r,lPrefs.GridAndBorder.g,lPrefs.GridAndBorder.b,lPrefs.GridAndBorder.a);
  glBegin(GL_TRIANGLE_STRIP);
      glVertex2f((lL-lBorder)*lPrefs.window_width,(lB+lBorder)*lPrefs.window_height);
      glVertex2f((lL-lBorder)*lPrefs.window_width,(lT-lBorder)*lPrefs.window_height);
      glVertex2f((lR+lBorder)*lPrefs.window_width,(lB+lBorder)*lPrefs.window_height);
      glVertex2f((lR+lBorder)*lPrefs.window_width,(lT-lBorder)*lPrefs.window_height);
    glEnd;//In theory, a bit faster than GL_POLYGON
end;

procedure UOffset (var lU: TUnitRect; lX,lY: single);
begin
  lU.L := lU.L+lX;
  lU.T := lU.T+lY;
  lU.R := lU.R+lX;
  lU.B := lU.B+lY;
end;

procedure SetLutFromZero(var lMin,lMax: single);
//if both min and max are positive, returns 0..max
//if both min and max are negative, returns min..0
begin
    SortSingle(lMin,lMax);
    if (lMin > 0) and (lMax > 0) then
      lMin := 0
    else if (lMin < 0) and (lMax < 0) then
      lMax := 0;
end;

procedure DrawCLUT ( lU: TUnitRect;lBorder: single; var lPrefs: TPrefs; lMesh: TMesh; window_width, window_height: integer );
var
  lU2:TUnitRect;
  lX,lY,lMin,lMax: single;
  lIsHorzTop: boolean;
  lIx,lI: integer;

  nLUT : integer;
  LUT: array [1..3] of TLUT;
  mn, mx: array [1..3] of single;
begin
  if (lMesh.OpenOverlays < 1) and (length(lMesh.nodes) < 1) then exit;
  if (lMesh.OpenOverlays > 0) then
    nLUT := lMesh.OpenOverlays
  else begin
    nLUT := 0;
    if (lMesh.nodePrefs.isNodeColorVaries) then begin
       inc(nLUT);
       LUT[nLUT] := UpdateTransferFunction (lMesh.nodePrefs.NodeLUTindex);
       if (lMesh.nodePrefs.isNodeThresholdBySize) then begin
          mn[nLUT] := lMesh.nodePrefs.minNodeColor;
          mx[nLUT] := lMesh.nodePrefs.maxNodeColor;

       end else begin
         mn[nLUT] := lMesh.nodePrefs.minNodeThresh;
         mx[nLUT] := lMesh.nodePrefs.maxNodeThresh;
       end;
       if mn[nLUT] = mx[nLUT] then
         nLUT := nLUT - 1; //no variability!
    end; //nodes
    if (lMesh.nodePrefs.isEdgeColorVaries) and (lMesh.nodePrefs.maxEdge <> lMesh.nodePrefs.minEdge) then begin
      if (lMesh.nodePrefs.maxEdge > 0)  and (not lMesh.nodePrefs.isNoPosEdge) and (lMesh.nodePrefs.minEdgeThresh <> lMesh.nodePrefs.maxEdgeThresh) then begin
         inc(nLUT);
         LUT[nLUT] := UpdateTransferFunction (lMesh.nodePrefs.edgeLUTindex);
         mn[nLUT] := lMesh.nodePrefs.minEdgeThresh;
         mx[nLUT] := lMesh.nodePrefs.maxEdgeThresh;
      end; //positive edges
      if (lMesh.nodePrefs.minEdge < 0)  and (not lMesh.nodePrefs.isNoNegEdge) and (lMesh.nodePrefs.minEdgeThresh <> lMesh.nodePrefs.maxEdgeThresh) then begin
         inc(nLUT);
         lI := lMesh.nodePrefs.edgeLUTindex + 1;
         LUT[nLUT] := UpdateTransferFunction (lI);
         mn[nLUT] := -lMesh.nodePrefs.minEdgeThresh;
         mx[nLUT] := -lMesh.nodePrefs.maxEdgeThresh;
      end; //negative edges
    end;  //edges
  end; //if overlays else edges
  if (nLUT < 1) then exit;
  lPrefs.window_height:= window_height;
  lPrefs.window_width:= window_width;
  lIsHorzTop := false;
  Enter2D(lPrefs);
  glEnable (GL_BLEND);//allow border to be translucent
  glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  //glDisable (GL_BLEND);
  if abs(lU.R-lU.L) > abs(lU.B-lU.T) then begin //wide bars
    lX := 0;
    lY := abs(lU.B-lU.T)+lBorder;
    if (lU.B+lU.T) >1 then
      lY := -lY
    else
      lIsHorzTop := true;
  end else begin //high bars
    lX := abs(lU.R-lU.L)+lBorder;
    lY := 0;
    if (lU.L+lU.R) >1 then
      lX := -lX;
  end;
  //next - draw a border - do this once for all overlays, so
  //semi-transparent regions do not display regions of overlay
  SensibleUnitRect(lU);
  lU2 := lU;
  if nLUT > 1 then begin
    for lI := 2 to nLUT do begin
      if lX < 0 then
        lU2.L := lU2.L + lX
      else
        lU2.R := lU2.R + lX;
      if lY < 0 then
        lU2.B := lU2.B + lY
      else
        lU2.T := lU2.T + lY;
    end;
  end;
  DrawBorder(lU2,lBorder,lPrefs);
  lU2 := lU;
  if (lMesh.OpenOverlays > 0) then begin
    for lI := 1 to lMesh.OpenOverlays do begin
      DrawCLUTx(lMesh.overlay[lI].LUT,lU2,lPrefs);
      UOffset(lU2,lX,lY);
    end;
    lU2 := lU;
    for lI := 1 to lMesh.OpenOverlays do begin
      lMin := lMesh.overlay[lI].WindowScaledMin;
      lMax := lMesh.overlay[lI].WindowScaledMax;
      SortSingle(lMin,lMax);
      DrawColorBarText(lMin,lMax, lU2,lBorder,lPrefs);
      UOffset(lU2,lX,lY);
    end;
  end else if nLUT > 0 then begin
          for lI := 1 to nLUT do begin
            DrawCLUTx(LUT[lI],lU2,lPrefs);
            UOffset(lU2,lX,lY);
          end;
          lU2 := lU;
          for lI := 1 to nLUT do begin
            lMin := mn[lI];
            lMax := mx[lI];
            SortSingle(lMin,lMax);
            DrawColorBarText(lMin,lMax, lU2,lBorder,lPrefs);
            UOffset(lU2,lX,lY);
          end;
  end; //if overlay else edges
glDisable (GL_BLEND);
end;

end.

