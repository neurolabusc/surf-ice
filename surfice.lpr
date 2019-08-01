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
  //ConstrainTrackBars(); //if unpatched: https://bugs.freepascal.org/view.php?id=35861
  Application.Run;
  //Windows: if you get an error "Can't find object file" you can copy the 'static' folder from
  //  https://github.com/synopse/mORMot
  //Alternatively, disable "FastGZ" in opts.inc
end.

