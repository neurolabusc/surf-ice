program surfice;

{$mode objfpc}{$H+}

uses
{$IFDEF FPC}Graphics, {$ENDIF}
{$IFDEF UNIX}{$IFDEF UseCThreads}cthreads,{$ENDIF}{$ENDIF}
{$IFNDEF Darwin}uscaledpi,{$ENDIF}
  Interfaces,
  Forms, lazopenglcontext, pascalscript, mainunit, Shaderu, prefs, nifti_loader,
  colorTable, track, scriptengine;

{$R *.res}

begin
  //RequireDerivedFormResource:=True;
  Application.Title:='Surf Ice';
  Application.Initialize;
  Application.CreateForm(TGLForm1, GLForm1);
  Application.CreateForm(TScriptForm, ScriptForm);
  //{$IFDEF FPC}{$IFNDEF Darwin}HighDPI(96);{$ENDIF}{$ENDIF}
  {$IFDEF FPC}{$IFDEF LINUX} HighDPILinux(GetFontData(GLForm1.Font.Reference.Handle).Height); {$ENDIF} {$ENDIF}
  {$IFDEF FPC}{$IFNDEF UNIX}HighDPI(96);{$ENDIF}{$ENDIF}
  Application.Run;
end.

