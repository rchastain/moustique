
uses
  SysUtils, Classes, Expressions;

{$i samples.inc}

var
  i: integer;
  lst: TStringList;
  
begin
  for i := Low(SFEN) to High(SFEN) do
    WriteLn(IsFEN(SFEN[i]));
  
  lst := Expressions.list;
  if ExtractMoves(SCOMMAND[3]) then
    WriteLn(lst.Text);
end.
