program surfice;

{$mode objfpc}{$H+}

uses
{$IFDEF FPC}{$IFNDEF UNIX}  uscaledpi,{$ENDIF}{$ENDIF}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, pascalscript, mainunit, Shaderu, prefs, nifti_loader,
  colorTable, track, scriptengine;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
    Application.Title:='Surf Ice';

  Application.Initialize;
  Application.CreateForm(TGLForm1, GLForm1);
    Application.CreateForm(TScriptForm, ScriptForm);
  {$IFDEF FPC}{$IFNDEF UNIX}HighDPI(96);{$ENDIF}{$ENDIF}
  Application.Run;
end.

