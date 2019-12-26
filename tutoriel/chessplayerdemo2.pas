
uses
  SysUtils, TypInfo, ChessPlayer;

function Clock(): int64;
begin
  result := Trunc(1000 * SecsPerDay * Now);
end;

var
  LClock, LMax, LTotal: int64;
  LMove: string;
  LCount: integer;
  LCode: TExitCode;
  
begin
  LMax := 0;
  LTotal := 0;
  LCount := 0;
  with TChessPlayer.Create do
  try
    SetPosition('rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1');
    repeat
      Sleep(10);
      Write(BoardAsText(FALSE));
      LClock := Clock();
      LMove := BestMove(LCode);
      LClock := Clock() - LClock;
      WriteLn(Format(' %s %0.3d ms %s', [LMove, LClock, GetEnumName(TypeInfo(TExitCode), Ord(LCode))]));
      if LClock > LMax then LMax := LClock;
      Inc(LTotal, LClock);
      Inc(LCount);
    until not (LCode in [ecSuccess, ecCheck]) or (LCount = 500);
      WriteLn(Format('Temps maximal : %0.3d ms', [LMax]));
      WriteLn(Format('Temps moyen : %0.3d ms', [LTotal div LCount]));
  finally
    Free;
  end;
end.
