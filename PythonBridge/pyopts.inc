{$ifdef windows}
  {$define PYTHON_DYNAMIC}
{$endif}
{$ifdef linux}
  //Only for Debian - set in .lpi
  //{$define PYTHON_DYNAMIC}
{$endif}
{$ifdef darwin}
  //{$define PYTHON_DYNAMIC}
{$endif}
