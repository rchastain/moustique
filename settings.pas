
unit Settings;

interface

uses
  SysUtils, IniFiles;

var
  vMinDepth, vMaxDepth: integer;

implementation

const
  cSection = 'settings';

var
  vFileName: TFileName;

procedure ReadIniFile;
const
  cDefaultMinDepth = 3;
  cDefaultMaxDepth = 7;
var
  vFile: TIniFile;
begin
  vFile := TIniFile.Create(vFileName);
  try
    vMinDepth := vFile.ReadInteger(cSection, 'mindepth', cDefaultMinDepth);
    vMaxDepth := vFile.ReadInteger(cSection, 'maxdepth', cDefaultMaxDepth);
  finally
    vFile.Free;
  end;
end;

procedure WriteIniFile;
var
  vFile: TIniFile;
begin
  vFile := TIniFile.Create(vFileName);
  try
    vFile.WriteInteger(cSection, 'mindepth', vMinDepth);
    vFile.WriteInteger(cSection, 'maxdepth', vMaxDepth);
  finally
    vFile.Free;
  end;
end;

initialization
  vFileName := ChangeFileExt(ParamStr(0), '.ini');
  ReadIniFile;

finalization
  if not FileExists(vFileName) then
    WriteIniFile;

end.
