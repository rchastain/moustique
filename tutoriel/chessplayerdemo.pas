
uses
  SysUtils, TypInfo, ChessPlayer;

function Clock(): int64;
begin
  result := Trunc(1000 * SecsPerDay * Now());
end;

var
  vClock, vMax, vTotal: int64;
  vMove: string;
  vCount: integer;
  vCode: TExitCode;
  
begin
  vMax := 0;
  vTotal := 0;
  vCount := 0;
  with TChessPlayer.Create do
  try
    SetPosition('rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1');
    repeat
      Sleep(10);
      Write(BoardAsText(FALSE));
      vClock := Clock();
      vMove := BestMove(vCode);
      vClock := Clock() - vClock;
      WriteLn(Format(' %s %0.3d ms %s', [vMove, vClock, GetEnumName(TypeInfo(TExitCode), Ord(vCode))]));
      if vClock > vMax then vMax := vClock;
      Inc(vTotal, vClock);
      Inc(vCount);
    until not (vCode in [ecSuccess, ecCheck]) or (vCount = 500);
      WriteLn(Format('Temps maximal : %0.3d ms', [vMax]));
      WriteLn(Format('Temps moyen : %0.3d ms', [vTotal div vCount]));
  finally
    Free;
  end;
end.
