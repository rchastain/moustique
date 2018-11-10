
uses
  SysUtils, Classes, Expressions;

var
  list: TStringList;

begin
  list := Expressions.list;
  if ExtractMoves('position startpos moves e2e4') then
    WriteLn(list.Text);
end.
