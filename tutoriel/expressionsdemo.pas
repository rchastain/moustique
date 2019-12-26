
uses
  SysUtils, Classes, Expressions;

{$I samples.inc}

var
  i: integer;
  l: TStringList;
  
begin
  for i := Low(CFenSample) to High(CFenSample) do
    WriteLn(IsFEN(CFenSample[i]));
  
  l := Expressions.LMoveList;
  if ExtractMoves(CUciSample[3]) then
    WriteLn(l.Text);
end.
