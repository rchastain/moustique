
unit Settings;

interface

uses
  SysUtils, IniFiles;

var
  LMinDepth, LMaxDepth: integer;
  LBook: string;
  
implementation

const
  CSection = 'settings';

var
  LFileName: TFileName;

procedure ReadIniFile;
const
  CDefaultMinDepth = 3;
  CDefaultMaxDepth = 7;
  CDefaultBook = 'gm2001.bin';
var
  LFile: TIniFile;
begin
  LFile := TIniFile.Create(LFileName);
  try
    LMinDepth := LFile.ReadInteger(CSection, 'mindepth', CDefaultMinDepth);
    LMaxDepth := LFile.ReadInteger(CSection, 'maxdepth', CDefaultMaxDepth);
    LBook := LFile.ReadString(CSection, 'book', CDefaultBook);
  finally
    LFile.Free;
  end;
end;

procedure WriteIniFile;
var
  LFile: TIniFile;
begin
  LFile := TIniFile.Create(LFileName);
  try
    LFile.WriteInteger(CSection, 'mindepth', LMinDepth);
    LFile.WriteInteger(CSection, 'maxdepth', LMaxDepth);
    LFile.WriteString(CSection, 'book', LBook);
  finally
    LFile.Free;
  end;
end;

initialization
  LFileName := ChangeFileExt(ParamStr(0), '.ini');
  ReadIniFile;

finalization
  if not FileExists(LFileName) then
    WriteIniFile;

end.
