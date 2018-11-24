program surfice;

{$mode objfpc}{$H+}

uses
{$IFDEF FPC}Graphics, {$ENDIF}
{$IFDEF UNIX}{$IFDEF UseCThreads}cthreads,{$ENDIF}{$ENDIF}
  Interfaces,
  Forms, lazopenglcontext, pascalscript, mainunit, Shaderu, prefs, nifti_loader,
  uscale, colorTable, track;

{$R *.res}

begin
  Application.Scaled:=True;
  //RequireDerivedFormResource:=True;
  Application.Title:='Surf Ice';
  Application.Initialize;
  Application.CreateForm(TGLForm1, GLForm1);
  ConstrainTrackBars();
  Application.Run;
end.

