
uses
  SysUtils, ChessPlayerTypes, ChessPlayerCore;

const
  FEN1 = 'rnbqkbnr/pp1ppppp/8/8/2p5/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 1';
  FEN2 = '8/4k3/1p1p2p1/p1pP2P1/P1P2K2/1P6/8/8 w - -';
  
var
  vPos: TChessPosition;
  i: integer;
  
begin
  (* Création et initialisation de l'échiquier. *)
  vPos := TChessPosition.Create(FEN2);
  (* Affichage de l'échiquier. *)
  WriteLn(#13#10, vPos.BoardAsText(TRUE), #13#10);
  (* Évaluation des coups. *)
  WriteLn(vPos.BestEval(vPos.activeColor, 1, 32000));
  (* Affichage des coups dont la note est différente de 0. *)
  for i := 1 to vPos.moveCount do
    with vPos.firstList[i] do
      if v <> 0 then
        WriteLn(Format('  %s %d', [
          MoveToStr(f, t),
          v
        ]));
  
  vPos.Free;
end.

{
  +  a b c d e f g h  +
                       
  8  . . . . . . . .  8
  7  . . . . k . . .  7
  6  . p . p . . p .  6
  5  p . p P . . P .  5
  4  P . P . . K . .  4
  3  . P . . . . . .  3
  2  . . . . . . . .  2
  1  . . . . . . . .  1
                       
  +  a b c d e f g h  +

  0
    b3b4 -2
    f4f5 -32000
    f4e5 -32000
}
