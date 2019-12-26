
uses
  SysUtils, ChessPlayer;

function ReadFirstLine(const AFileName: string): string;
var
  LFile: text;
begin
  Assign(LFile, AFileName);
  Reset(LFile);
  ReadLn(LFile, result);
  Close(LFile);
end;
  
var
  LPos: string;
  
begin
  if FileExists(ParamStr(1)) then
    LPos := ReadFirstLine(ParamStr(1))
  else
    //LPos := '8/4k3/1p1p2p1/p1pP2P1/P1P2K2/1P6/8/8 w - -';
    LPos := 'r3k2B/3p1p2/2p1p3/7Q/1P1P4/R1NB4/1P3PPP/4K1NR b Kq - 0 17';
  with TChessPlayer.Create do
  try
    SetPosition(LPos);
    WriteLn(BoardAsText);
    WriteLn(BestMove);
  finally
    Free;
  end;
end.
