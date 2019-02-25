
unit Expressions;

interface

uses
  SysUtils, Classes;
  
function IsFEN(const aStr: string): boolean;
function ExtractFEN(const aCommand: string; var aFEN: string): boolean;
function ExtractMoves(const aCommand: string): boolean;

var
  list: TStringList;
  
implementation

uses
  RegExpr;

type
  TValidator = class
    public
      function ExpandEmptySquares(ARegExpr: TRegExpr): string;
      function IsFEN(const aInputStr: string): boolean;
  end;

function IsFEN(const aStr: string): boolean;
var
  vld: TValidator;
begin
  vld := TValidator.Create;
  result := vld.IsFen(aStr);
  vld.Free;
end;

function TValidator.ExpandEmptySquares(aRegExpr: TRegExpr): string;
const
  SYMBOL = '-';
begin
  result := '';
  with aRegExpr do
    result := StringOfChar(SYMBOL, StrToInt(Match[0]));
end;

function TValidator.IsFEN(const aInputStr: string): boolean;
const
  WHITEKING = 'K';
  BLACKKING = 'k';
  PIECES    = '^[1-8BKNPQRbknpqr]+$';
  ACTIVE    = '^[wb]$';
  CASTLING  = '^[KQkq]+$|^[A-Ha-h]+$|^\-$';
  ENPASSANT = '^[a-h][36]$|^\-$';
  HALFMOVE  = '^\d+$';
  FULLMOVE  = '^[1-9]\d*$';
var
  a, b: TStrings;
  i: integer;
  e: TRegExpr;
  s: string;
  
begin
  a := TStringList.Create;
  b := TStringList.Create;
  
  e := TRegExpr.Create;
  e.Expression := '\d';
  
  (*
  ExtractStrings([' '], [], PChar(aInputStr), a);
  ExtractStrings(['/'], [], PChar(a[0]), b);
  *)
  
  SplitRegExpr(' ', aInputStr, a);
  
  result := (a.Count = 6);

  if result then
  begin
    SplitRegExpr('/', a[0], b);
    result := (b.Count = 8);
  end;
  
  if result then
  begin
    result := result and ExecRegExpr(WHITEKING, a[0]);
    result := result and ExecRegExpr(BLACKKING, a[0]);

    for i := 0 to 7 do
    begin
      result := result and ExecRegExpr(PIECES, b[i]);
      if result then
      begin
        s := b[i];
        repeat
          s := e.Replace(s, @ExpandEmptySquares);
        until not ExecRegExpr('\d', s);
        result := result and (Length(s) = 8);
      end;
    end;
    
    result := result and ExecRegExpr(ACTIVE,    a[1]);
    result := result and ExecRegExpr(CASTLING,  a[2]);
    result := result and ExecRegExpr(ENPASSANT, a[3]);
    result := result and ExecRegExpr(HALFMOVE,  a[4]);
    result := result and ExecRegExpr(FULLMOVE,  a[5]);
  end;

  a.Free;
  b.Free;
  e.Free;
end;

const
  PATTERN_PIECES = '[1-8BKNPQRbknpqr]+';
  PATTERN_ACTIVECOLOR = '[wb]';
  PATTERN_CASTLING = '([KQkq]+|-)';
  PATTERN_ENPASSANT = '([a-h][1-8]|-)';
  PATTERN_NUMBER = '\d+';
  PATTERN_MOVE = '\b[a-h][1-8][a-h][1-8][nbrq]?\b';
  
var
  fenPattern: string;
  efen, emove: TRegExpr;

function ExtractFEN(const aCommand: string; var aFEN: string): boolean;
begin
  result := efen.Exec(aCommand);
  if result then
    aFEN := efen.Match[0];
end;

function ExtractMoves(const aCommand: string): boolean;
begin
  list.Clear;
  result := emove.Exec(aCommand);
  if result then 
    repeat 
      list.Append(emove.Match[0]);
    until not emove.ExecNext;
end;

initialization
  fenPattern := Format('%s %s %s %s %s %s', [
    ReplaceRegExpr('x', 'x/x/x/x/x/x/x/x', PATTERN_PIECES, false),
    PATTERN_ACTIVECOLOR,
    PATTERN_CASTLING,
    PATTERN_ENPASSANT,
    PATTERN_NUMBER,
    PATTERN_NUMBER
  ]);
  
  list := TStringList.Create;
  efen := TRegExpr.Create(fenPattern);
  emove := TRegExpr.Create(PATTERN_MOVE);
  
finalization
  list.Free;
  efen.Free;
  emove.Free;
  
end.
