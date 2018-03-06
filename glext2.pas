unit glext2;

{$MACRO ON}
{$MODE Delphi}
{$IFDEF Windows}
  {$DEFINE extdecl:=stdcall }
{$ELSE}
  {$DEFINE extdecl:=cdecl }
{$ENDIF}
interface
uses
  //SysUtils,
  {$IFDEF Windows}Windows,{$ELSE}dynlibs,{$ENDIF}gl;

const
  GL_GEOMETRY_VERTICES_OUT_EXT = $8DDA;

var
  glProgramParameteriEXT: procedure(_program: GLuint; pname: GLenum; value: GLint); extdecl;

implementation

{$IFDEF Windows}
//Declared in Windows unit as well in FPC
// function wglGetProcAddress(proc: PChar): Pointer; extdecl; external 'OpenGL32.dll';
{$ELSE}
function wglGetProcAddress(proc: PChar): Pointer;
begin
  Result := GetProcAddress(LibGL, proc);
end;
{$ENDIF}

initialization
  glProgramParameteriEXT := wglGetProcAddress('glProgramParameteriEXT');
end.
