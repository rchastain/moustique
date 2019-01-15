
uses
  SysUtils, Mater;

type
  TMateProblem = record
    depth: integer;
    all: boolean;
    epd: string;
  end;

const
  PROBLEM: array[1..711] of TMateProblem = ({$I problems.inc});
{
  711 mate problems solved by Mater.
  
  Problems extracted from Franz Huber database.
  
  https://fhub.jimdo.com/
}

procedure TrySolve(const aProblem: TMateProblem; const aIndex: integer);
var
  s: string;
begin
  s := SolveMate(aProblem.epd, aProblem.depth, aProblem.all);
  WriteLn(Format('%s problem=%0.3d depth=%d searchallmoves=%d solution=%s', [TimeToStr(Now), aIndex, aProblem.depth, byte(aProblem.all), s]));
end;

var
  i: integer;
  
begin
  for i := Low(PROBLEM) to High(PROBLEM) do with PROBLEM[i] do
    if
    (
      (depth = 7) // Moves number.
      and not all // Search mode (check only or all moves).
    )
    or
    (
      (depth = 3)
      and all
    )
    then
      TrySolve(PROBLEM[i], i);
end.
