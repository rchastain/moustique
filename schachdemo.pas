
uses
  SysUtils, TypInfo, Schach;

function Clock(): int64;
begin
  result := Trunc(Now() * SecsPerDay * 1000.0);
end;

var
  vClock: int64;
  vMove: string;
  vCount: integer;
  vError: TBestMoveExitCode;
  
begin
  vCount := 0;
  with TChessProgram.Create do
  try
    SetPosition('rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1');
    repeat
      Sleep(10);
      Write(CurrentBoardAsText(false));
      vClock := Clock();
      vMove := BestMove(vError);
      vClock := Clock() - vClock;
      WriteLn(Format(' %s %0.3d ms %s', [vMove, vClock, GetEnumName(TypeInfo(TBestMoveExitCode), Ord(vError))]));
      Inc(vCount);
    until not (vError in [ecSuccess, ecCheck]) or (vCount = 500);
  finally
    Free;
  end;
end.
