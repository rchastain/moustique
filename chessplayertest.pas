
uses
  SysUtils, ChessPlayer;

function ReadFirstLine(const aFileName: string): string;
var
  vText: text;
begin
  Assign(vText, aFileName);
  Reset(vText);
  ReadLn(vText, result);
  Close(vText);
end;
  
var
  vPos: string;
  
begin
  if FileExists(ParamStr(1)) then
    vPos := ReadFirstLine(ParamStr(1))
  else
    vPos := '8/4k3/1p1p2p1/p1pP2P1/P1P2K2/1P6/8/8 w - -';
  with TChessPlayer.Create do
  try
    SetPosition(vPos);
    WriteLn(BoardAsText());
    WriteLn(BestMove());
  finally
    Free;
  end;
end.
