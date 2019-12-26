
uses
  Book;

const
  CStartPosition = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';

begin
  WriteLn(BestMove(CStartPosition, '../gm2001.bin'));
end.
