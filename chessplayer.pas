
{ Autor:   Jürgen Schlottke, Schönaich-C.-Str. 46, D-W 2200 Elmshorn
           Tel. 04121/63109
  Zweck  : Demonstration der Schachprogrammierung unter Turbo-Pascal
  Datum  : irgendwann 1991, als PD freigegeben am 18.01.93
  Version: ohne Versionsnummer
}

unit ChessPlayer;

interface

uses
  ChessPlayerCore;

type
  TExitCode = (ecSuccess, ecCheck, ecCheckmate, ecStalemate, ecError);
  
  TChessPlayer = class
  strict private
    FIniPos,
    FCurPos: TChessPosition;
    FHalfmovesCount: integer;
    FSecondList: TMoveList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetPosition(const APos: string);
    function PlayMove(const AMove: string): boolean;
    function BestMoveIndex(const ABestValue: integer): integer;
    function BestMove(out ACode: TExitCode): string; overload;
    function BestMove: string; overload;
    function BoardAsText(const APretty: boolean = TRUE): string;
    function FEN: string;
    function SetPositionFromMoves(const APos: string; const AMoves: array of string): string;
  end;

implementation

uses
  Classes, SysUtils, TypInfo, ChessPlayerTypes, Log;

const
  STARTPOS = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';

constructor TChessPlayer.Create;
begin
  inherited Create;
  FIniPos := TChessPosition.Create(STARTPOS);
  FCurPos := TChessPosition.Create;
end;

destructor TChessPlayer.Destroy;
begin
  FCurPos.Free;
  FIniPos.Free;
  inherited Destroy;
end;

procedure TChessPlayer.SetPosition(const APos: string);

  function GetMoveCount(const APos: string): integer;
  var
    LFields: TStringList;
  begin
    LFields := TStringList.Create;
    ExtractStrings([' '], [], pchar(APos), LFields);
    if LFields.Count = 6 then
      result := 2 * Pred(StrToInt(LFields[5])) + Ord(LFields[1] = 'b')
    else
      result := 0;
    LFields.Free;
  end;

begin
  FCurPos.SetPosition(APos);
  FHalfmovesCount := GetMoveCount(APos);
end;

function TChessPlayer.PlayMove(const AMove: string): boolean;
var
  LAuxPos: TChessPosition;
  LMove, LPromo, LFrom, LTo: integer;
begin
  MoveToInt(AMove, LMove, LPromo);
  LFrom := LMove div 100;
  LTo := LMove mod 100;
  FCurPos.GenerateMoves;
  result := FCurPos.IsLegal(LMove);
  if result then
  begin
    LAuxPos := TChessPosition.Create;
    LAuxPos.SetPosition(FCurPos);
    LAuxPos.MovePiece(LFrom, LTo, LPromo);
    LAuxPos.activeColor := CBlack * LAuxPos.activeColor;
    if LAuxPos.Check then
      result := FALSE
    else
    begin
      FCurPos.MovePiece(LFrom, LTo, LPromo);
      Inc(FHalfmovesCount);
    end;
    LAuxPos.Free;
  end;
end;

function TChessPlayer.BestMoveIndex(const ABestValue: integer): integer;
var
  LAuxPos: TChessPosition;
  LMaxValue: integer;
  LSecondListCount: integer;
  i, j: integer;
  LLine1, LLine2: string;
begin
  LAuxPos := TChessPosition.Create;

  with FCurPos do
  begin
    LSecondListCount := 0;
    for i := 1 to moveCount do
      if firstList[i].FValue = ABestValue then
      begin
        Inc(LSecondListCount);
        FSecondList[LSecondListCount].FFrom := firstList[i].FFrom;
        FSecondList[LSecondListCount].FTo := firstList[i].FTo;
        FSecondList[LSecondListCount].FValue := 0
      end;
  end;

  LMaxValue := Low(integer);
  result := 0;
  LLine1 := '';
  LLine2 := '';
  
  for i := 1 to LSecondListCount do
  begin
    LAuxPos.SetPosition(FCurPos);

    with LAuxPos do
    begin
      if chessboard[FSecondList[i].FFrom] = FIniPos.chessboard[FSecondList[i].FFrom] then
      begin
        Inc(FSecondList[i].FValue, 5);
        if chessboard[FSecondList[i].FFrom] * activeColor = CPawn then
          Inc(FSecondList[i].FValue, 2);
      end;

      if chessboard[FSecondList[i].FFrom] * activeColor = CKing then
        if Abs(FSecondList[i].FTo - FSecondList[i].FFrom) = 20 then
          Inc(FSecondList[i].FValue, 20)
        else
          Dec(FSecondList[i].FValue, 10);

      if (FHalfmovesCount < 32) and (chessboard[FSecondList[i].FFrom] * activeColor in [CPawn, CBishop, CKnight]) then
        Inc(FSecondList[i].FValue, 20);

      if (FSecondList[i].FFrom div 10 = 1)
      or (FSecondList[i].FFrom div 10 = 8)
      or (FSecondList[i].FFrom mod 10 = 1)
      or (FSecondList[i].FFrom mod 10 = 8) then
        Inc(FSecondList[i].FValue, 2);

      if (FSecondList[i].FTo div 10 = 1)
      or (FSecondList[i].FTo div 10 = 8)
      or (FSecondList[i].FTo mod 10 = 1)
      or (FSecondList[i].FTo mod 10 = 8) then
        Dec(FSecondList[i].FValue, 2);
    end;

    LAuxPos.SetPosition(FCurPos);
    LAuxPos.MovePiece(FSecondList[i].FFrom, FSecondList[i].FTo, CQueen);

    if LAuxPos.chessboard[FSecondList[i].FTo] = FIniPos.chessboard[FSecondList[i].FTo] then
      Dec(FSecondList[i].FValue, 10);

    LAuxPos.activeColor := CBlack * LAuxPos.activeColor;
    LAuxPos.GenerateSimpleMoves;

    with LAuxPos do
      for j := 1 to moveCount do
      begin
        Inc(FSecondList[i].FValue);
        if chessboard[firstList[j].FTo] <> CNil then
          Inc(FSecondList[i].FValue);
      end;

    LAuxPos.activeColor := CBlack * LAuxPos.activeColor;
    LAuxPos.GenerateSimpleMoves;

    with LAuxPos do
      for j := 1 to moveCount do
      begin
        Dec(FSecondList[i].FValue);
        if chessboard[firstList[j].FTo] <> CNil then
          Dec(FSecondList[i].FValue);
      end;
{$IFDEF DEBUG}
    with FSecondList[i] do
    begin
      LLine1 := LLine1 + Format('%6s', [MoveToStr(100 * FFrom + FTo)]);
      LLine2 := LLine2 + Format('%6d', [FValue]);
    end;
{$ENDIF}
    if FSecondList[i].FValue >= LMaxValue then
    begin
      LMaxValue := FSecondList[i].FValue;
      result := i;
    end;
  end;
{$IFDEF DEBUG}
  TLog.Append(LLine1);
  TLog.Append(LLine2);
{$ENDIF}
  LAuxPos.Free;
end;

function TChessPlayer.BestMove(out ACode: TExitCode): string;
const
  CCodeIllegal: array[boolean] of TExitCode = (ecStalemate, ecCheckmate);
  CCodeLegal: array[boolean] of TExitCode = (ecSuccess, ecCheck);
var
  i: integer;
  LCheckBefore, LPromo: boolean;
begin
  result := '0000';
  i := FCurPos.BestEval(FCurPos.activeColor, 1, 32000);
  i := BestMoveIndex(i);
  if i > 0 then
  begin
    Inc(FHalfmovesCount);
    LCheckBefore := FCurPos.Check;
    with FSecondList[i] do
      LPromo := FCurPos.MovePiece(FFrom, FTo, CQueen);
    FCurPos.activeColor := CBlack * FCurPos.activeColor;
    if FCurPos.Check then
      ACode := CCodeIllegal[LCheckBefore]
    else
    begin
      with FSecondList[i] do
        result := MoveToStr(100 * FFrom + FTo);
      if LPromo then
        result := result + 'q';
      FCurPos.activeColor := CBlack * FCurPos.activeColor;
      ACode := CCodeLegal[FCurPos.Check];
    end;
  end else
    ACode := ecError;
  if not (ACode in [ecSuccess, ecCheck]) then
    TLog.Append(Format('bestmove function exit code %d', [ACode]));
end;

function TChessPlayer.BestMove: string;
var
  LCode: TExitCode;
begin
  result := BestMove(LCode);
end;

function TChessPlayer.BoardAsText(const APretty: boolean): string;
begin
  result := FCurPos.BoardAsText(APretty);
end;

function TChessPlayer.FEN: string;
begin
  result := FCurPos.FEN;
end;

function TChessPlayer.SetPositionFromMoves(const APos: string; const AMoves: array of string): string;
var
  i: integer;
begin
  SetPosition(APos);
  for i := Low(AMoves) to High(AMoves) do
    PlayMove(AMoves[i]);
  result := FEN;
end;

end.
