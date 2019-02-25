
unit Log;

interface

uses
  SysUtils;

type
  TLog = class
    class procedure Append(const aLine: string);
  end;

implementation

{$IFDEF DEBUG}
var
  vLog: text;
{$ENDIF}

class procedure TLog.Append(const aLine: string);
begin
{$IFDEF DEBUG}
  WriteLn(vLog, DateTimeToStr(Now()) + ' ' + aLine);
  Flush(vLog);
{$ENDIF}
end;

var
  vName: string;
  
initialization
  vName := ChangeFileExt(ParamStr(0), '.log');
{$IFDEF DEBUG}
  Assign(vLog, vName);
  if FileExists(vName) then
    Append(vLog)
  else
    Rewrite(vLog);
{$ENDIF}

finalization
{$IFDEF DEBUG}
  Close(vLog);
{$ENDIF}

end.
