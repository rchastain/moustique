
const
{$if defined(cpu64) and defined(Windows)}
  APPNAME = 'Moustique win64';
{$endif}
{$if defined(cpu86) and defined(Windows)}
  APPNAME = 'Moustique win32';
{$endif}
{$if defined(cpu64) and defined(Linux)}
  APPNAME = 'Moustique lin64';
{$endif}
{$if defined(cpu86) and defined(Linux)}
  APPNAME = 'Moustique lin32';
{$endif}
{$if defined(cpu64) and defined(freebsd)}
  APPNAME = 'Moustique bsd64';
{$endif}
{$if defined(cpu86) and defined(freebsd)}
  APPNAME = 'Moustique bsd32';
{$endif}
{$if defined(linux) and defined(cpuarm)}
  APPNAME = 'Moustique arm';
{$endif}
  APPVERSION = '0.1';
  APPAUTHOR = 'Juergen Schlottke, Valentin Albillo, Roland Chastain';
