
unit Expressions;

interface

uses
  SysUtils, Classes;
  
function IsFen(const AStr: string): boolean;
function ExtractFen(const ACommand: string; var AFen: string): boolean;
function ExtractMoves(const ACommand: string): boolean;

var
  LMoveList: TStringList;
  
implementation

uses
  RegExpr;

type
  TFenValidator = class
    public
      function ExpandEmptySquares(AExpr: TRegExpr): string;
      function IsFen(const AInputStr: string): boolean;
  end;

function IsFen(const AStr: string): boolean;
var
  LValidator: TFenValidator;
begin
  LValidator := TFenValidator.Create;
  result := LValidator.IsFen(AStr);
  LValidator.Free;
end;

function TFenValidator.ExpandEmptySquares(AExpr: TRegExpr): string;
begin
  result := StringOfChar('-', StrToInt(AExpr.Match[0]));
end;

function TFenValidator.IsFen(const AInputStr: string): boolean;
const
  CWhiteKing = 'K';
  CBlackKing = 'k';
  CPieces    = '^[1-8BKNPQRbknpqr]+$';
  CActive    = '^[wb]$';
  CCastling  = '^[KQkq]+$|^[A-Ha-h]+$|^\-$';
  CEnPassant = '^[a-h][36]$|^\-$';
  CHalfMove  = '^\d+$';
  CFullMove  = '^[1-9]\d*$';
var
  LFields, LPieces: TStringList;
  LExpr: TRegExpr;
  i: integer;
  s: string;
begin
  LFields := TStringList.Create;
  LPieces := TStringList.Create;
  LExpr := TRegExpr.Create('\d');
  (*
  ExtractStrings([' '], [], PChar(AInputStr), LFields);
  ExtractStrings(['/'], [], PChar(LFields[0]), LPieces);
  *)
  SplitRegExpr(' ', AInputStr, LFields);
  result := (LFields.Count = 6);

  if result then
  begin
    SplitRegExpr('/', LFields[0], LPieces);
    result := (LPieces.Count = 8);
  end;
  
  if result then
  begin
    result := result and ExecRegExpr(CWhiteKing, LFields[0]);
    result := result and ExecRegExpr(CBlackKing, LFields[0]);

    for i := 0 to 7 do
    begin
      result := result and ExecRegExpr(CPieces, LPieces[i]);
      if result then
      begin
        s := LPieces[i];
        repeat
          s := LExpr.Replace(s, @ExpandEmptySquares);
        until not ExecRegExpr('\d', s);
        result := result and (Length(s) = 8);
      end;
    end;
    
    result := result and ExecRegExpr(CActive,    LFields[1]);
    result := result and ExecRegExpr(CCastling,  LFields[2]);
    result := result and ExecRegExpr(CEnPassant, LFields[3]);
    result := result and ExecRegExpr(CHalfMove,  LFields[4]);
    result := result and ExecRegExpr(CFullMove,  LFields[5]);
  end;

  LFields.Free;
  LPieces.Free;
  LExpr.Free;
end;

const
  CPatternPieces = '[1-8BKNPQRbknpqr]+';
  CPatternActiveColor = '[wb]';
  CPatternCastling = '([KQkq]+|-)';
  CPatternEnPassant = '([a-h][1-8]|-)';
  CPatternNumber = '\d+';
  CPatternMove = '\b[a-h][1-8][a-h][1-8][nbrq]?\b';
  
var
  LFenPattern: string;
  LExprFen, LExprMove: TRegExpr;

function ExtractFen(const ACommand: string; var AFen: string): boolean;
begin
  result := LExprFen.Exec(ACommand);
  if result then
    AFen := LExprFen.Match[0];
end;

function ExtractMoves(const ACommand: string): boolean;
begin
  LMoveList.Clear;
  result := LExprMove.Exec(ACommand);
  if result then 
    repeat 
      LMoveList.Append(LExprMove.Match[0]);
    until not LExprMove.ExecNext;
end;

initialization
  LFenPattern := Format('%s %s %s %s %s %s', [
    ReplaceRegExpr('x', 'x/x/x/x/x/x/x/x', CPatternPieces, false),
    CPatternActiveColor,
    CPatternCastling,
    CPatternEnPassant,
    CPatternNumber,
    CPatternNumber
  ]);
  
  LMoveList := TStringList.Create;
  LExprFen := TRegExpr.Create(LFenPattern);
  LExprMove := TRegExpr.Create(CPatternMove);
  
finalization
  LMoveList.Free;
  LExprFen.Free;
  LExprMove.Free;
  
end.
