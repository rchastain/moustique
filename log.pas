
unit Log;

interface

uses
  SysUtils;

type
  TLog = class
    class procedure Append(const AStr: string);
  end;

implementation

{$IFDEF DEBUG}
var
  LFile: text;
{$ENDIF}

class procedure TLog.Append(const AStr: string);
begin
{$IFDEF DEBUG}
  WriteLn(LFile, DateTimeToStr(Now) + ' ' + AStr);
  Flush(LFile);
{$ENDIF}
end;

var
  LFileName: string;
  
initialization
  LFileName := ChangeFileExt(ParamStr(0), '.log');
{$IFDEF DEBUG}
  Assign(LFile, LFileName);
  if FileExists(LFileName) then
    Append(LFile)
  else
    Rewrite(LFile);
{$ENDIF}

finalization
{$IFDEF DEBUG}
  Close(LFile);
{$ENDIF}

end.
