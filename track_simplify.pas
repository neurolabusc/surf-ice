unit track_simplify;
{
https://sourceforge.net/p/simdesign-os/code/HEAD/tree/trunk/simlib/geometry/sdSimplifyPolylineDouglasPeucker.pas

Implementation of the famous Douglas-Peucker polyline simplification
  algorithm.

  This file contains a 3D floating point implementation, for spatial
  polylines

  Permalink:
  http://en.wikipedia.org/wiki/Douglas-Peucker_algorithm

  Loosely based on C code from SoftSurfer (www.softsurfer.com)
    https://github.com/jjgreen/dp-simplify/blob/master/dp-simplify.c

  References:
  David Douglas & Thomas Peucker, "Algorithms for the reduction of the number of
  points required to represent a digitized line or its caricature", The Canadian
  Cartographer 10(2), 112-122  (1973)

  Delphi code by Nils Haeck (c) 2003 Simdesign (www.simdesign.nl)
  http://www.simdesign.nl/components/douglaspeucker.html

  ****************************************************************
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at:
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an
  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.
}
interface

uses define_types;


type

  // Generalized float and int types
  TFloat = single;


{ PolySimplifyFloat3D:
  Approximates the polyline with 3D float vertices in Orig, with a simplified
  version that will be returned in Simple. The maximum deviation from the
  original line is given in Tol.
  Input:  Tol      = approximation tolerance
          Orig[]   = polyline array of vertex points
  Output: Simple[] = simplified polyline vertices. This array must initially
                     have the same length as Orig
  Return: the number of points in Simple
}
function PolySimplifyFloat3D(Tol: TFloat; const Orig: array of TPoint3f;
  var Simple: array of TPoint3f): integer;

{ PolySimplifyInt2D:
  Approximates the polyline with 2D integer vertices in Orig, with a simplified
  version that will be returned in Simple. The maximum deviation from the
  original line is given in Tol.
  Input:  Tol      = approximation tolerance
          Orig[]   = polyline array of vertex points
  Output: Simple[] = simplified polyline vertices. This array must initially
                     have the same length as Orig
  Return: the number of points in Simple
}


implementation

function VecMinFloat3D(const A, B: TPoint3f): TPoint3f;
// Result = A - B
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

function DotProdFloat3D(const A, B: TPoint3f): TFloat;
// Dotproduct = A * B
begin
  Result := A.X * B.X + A.Y * B.Y + A.Z * B. Z;
end;

function NormSquaredFloat3D(const A: TPoint3f): TFloat;
// Square of the norm |A|
begin
  Result := A.X * A.X + A.Y * A.Y + A.Z * A.Z;
end;

function DistSquaredFloat3D(const A, B: TPoint3f): TFloat;
// Square of the distance from A to B
begin
  Result := NormSquaredFloat3D(VecMinFloat3D(A, B));
end;

procedure SimplifyFloat3D(var Tol2: TFloat; const Orig: array of TPoint3f;
  var Marker: array of boolean; j, k: integer);
// Simplify polyline in OrigList between j and k. Marker[] will be set to True
// for each point that must be included
var
  i, MaxI: integer; // Index at maximum value
  MaxD2: TFloat;    // Maximum value squared
  CU, CW, B: TFloat;
  DV2: TFloat;
  P0, P1, PB, U, W: TPoint3f;
begin
  // Is there anything to simplify?
  if k <= j + 1 then
    exit;

  P0 := Orig[j];
  P1 := Orig[k];
  U  := VecMinFloat3D(P1, P0); // Segment vector
  CU := DotProdFloat3d(U, U); // Segment length squared
  MaxD2 := 0;
  MaxI  := 0;

  // Loop through points and detect the one furthest away
  for i := j + 1 to k - 1 do
  begin
    W  := VecMinFloat3D(Orig[i], P0);
    CW := DotProdFloat3D(W, U);

    // Distance of point Orig[i] from segment
    if CW <= 0 then
    begin
      // Before segment
      DV2 := DistSquaredFloat3D(Orig[i], P0)
    end else
    begin
      if CW > CU then
      begin
        // Past segment
        DV2 := DistSquaredFloat3D(Orig[i], P1);
      end else
      begin
        // Fraction of the segment
        try
          B := CW / CU;
        except
          B := 0; // in case CU = 0
        end;
        PB.X := P0.X + B * U.X;
        PB.Y := P0.Y + B * U.Y;
        PB.Z := P0.Z + B * U.Z;
        DV2 := DistSquaredFloat3D(Orig[i], PB);
      end;
    end;

    // test with current max distance squared
    if DV2 > MaxD2 then
    begin
      // Orig[i] is a new max vertex
      MaxI  := i;
      MaxD2 := DV2;
    end;
  end;

  // If the furthest point is outside tolerance we must split
  if MaxD2 > Tol2 then
  begin // error is worse than the tolerance

    // split the polyline at the farthest vertex from S
    Marker[MaxI] := True;  // mark Orig[maxi] for the simplified polyline

    // recursively simplify the two subpolylines at Orig[maxi]
    SimplifyFloat3D(Tol2, Orig, Marker, j, MaxI); // polyline Orig[j] to Orig[maxi]
    SimplifyFloat3D(Tol2, Orig, Marker, MaxI, k); // polyline Orig[maxi] to Orig[k]
  end;
end;


function PolySimplifyFloat3D(Tol: TFloat; const Orig: array of TPoint3f;
  var Simple: array of TPoint3f): integer;
var
  i, N: integer;
  Marker: array of boolean;
  Tol2: TFloat;
begin
  Result := 0;
  N := Length(Orig);
  //Simple := Copy(Orig, Low(Orig), Length(Orig));
  if length(Orig) < 2 then
    exit;
  if Tol = 0 then begin
     result := N;
     for i := 0 to N - 1 do
         Simple[i] := Orig[i];
    exit;
  end;
  Tol2 := sqr(Tol);

  // Create a marker array
  SetLength(Marker, N);
  // Include first and last point
  Marker[0]     := True;
  Marker[N - 1] := True;
  // Exclude intermediate for now
  for i := 1 to N - 2 do
    Marker[i] := False;

  // Simplify
  SimplifyFloat3D(Tol2, Orig, Marker, 0, N - 1);

  // Copy to resulting list
  for i := 0 to N - 1 do begin
    if Marker[i] then
    begin
      Simple[Result] := Orig[i];
      inc(Result);
    end;
  end;
  //setlength(Simple,Result);
end;

end.
