
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
    procedure SetPosition(const aPos: string);
    function PlayMove(const aMove: string): boolean;
    function BestMoveIndex(const aBestValue: integer): integer;
    function BestMove(out aCode: TExitCode): string; overload;
    function BestMove(): string; overload;
    function BoardAsText(const aPretty: boolean = TRUE): string;
    function FEN(): string;
    function SetPosition_(const aPos: string; const aMoves: array of string): string;
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

procedure TChessPlayer.SetPosition(const aPos: string);

  function GetMoveCount(const aPos: string): integer;
  var
    lst: TStringList;
  begin
    lst := TStringList.Create;
    ExtractStrings([' '], [], pchar(aPos), lst);
    if lst.Count = 6 then
      result := 2 * Pred(StrToInt(lst[5])) + Ord(lst[1] = 'b')
    else
      result := 0;
    lst.Free;
  end;

begin
  FCurPos.SetPosition(aPos);
  FHalfmovesCount := GetMoveCount(aPos);
end;

function TChessPlayer.PlayMove(const aMove: string): boolean;
var
  aux: TChessPosition;
  iMove, iPromo, iFromSquare, iToSquare: integer;
begin
  MoveToInt(aMove, iMove, iPromo);
  iFromSquare := iMove div 100;
  iToSquare := iMove mod 100;
  FCurPos.GenerateMoves;
  result := FCurPos.IsLegal(iMove);
  if result then
  begin
    aux := TChessPosition.Create;
    aux.SetPosition(FCurPos);
    aux.MovePiece(iFromSquare, iToSquare, iPromo);
    aux.activeColor := cBlack * aux.activeColor;
    if aux.Check then
      result := FALSE
    else
    begin
      FCurPos.MovePiece(iFromSquare, iToSquare, iPromo);
      Inc(FHalfmovesCount);
    end;
    aux.Free;
  end;
end;

function TChessPlayer.BestMoveIndex(const aBestValue: integer): integer;
var
  aux: TChessPosition;
  i, j: integer;
  maxValue: integer;
  secondListCount: integer;
begin
  aux := TChessPosition.Create;

  with FCurPos do
  begin
    secondListCount := 0;
    for i := 1 to moveCount do
      if firstList[i].v = aBestValue then
      begin
        Inc(secondListCount);
        FSecondList[secondListCount].f := firstList[i].f;
        FSecondList[secondListCount].t := firstList[i].t;
        FSecondList[secondListCount].v := 0
      end;
  end;

  maxValue := Low(integer);
  result := 0;

  for i := 1 to secondListCount do
  begin
    aux.SetPosition(FCurPos);

    with aux do
    begin
      if chessboard[FSecondList[i].f] = FIniPos.chessboard[FSecondList[i].f] then
      begin
        Inc(FSecondList[i].v, 5);
        if chessboard[FSecondList[i].f] * activeColor = cPawn then
          Inc(FSecondList[i].v, 2);
      end;

      if chessboard[FSecondList[i].f] * activeColor = cKing then
        if Abs(FSecondList[i].t - FSecondList[i].f) = 20 then
          Inc(FSecondList[i].v, 20)
        else
          Dec(FSecondList[i].v, 10);

      if (FHalfmovesCount < 32) and (chessboard[FSecondList[i].f] * activeColor in [cPawn, cBishop, cKnight]) then
        Inc(FSecondList[i].v, 20);

      if (FSecondList[i].f div 10 = 1)
      or (FSecondList[i].f div 10 = 8)
      or (FSecondList[i].f mod 10 = 1)
      or (FSecondList[i].f mod 10 = 8) then
        Inc(FSecondList[i].v, 2);

      if (FSecondList[i].t div 10 = 1)
      or (FSecondList[i].t div 10 = 8)
      or (FSecondList[i].t mod 10 = 1)
      or (FSecondList[i].t mod 10 = 8) then
        Dec(FSecondList[i].v, 2);
    end;

    aux.SetPosition(FCurPos);
    aux.MovePiece(FSecondList[i].f, FSecondList[i].t, cQueen);

    if aux.chessboard[FSecondList[i].t] = FIniPos.chessboard[FSecondList[i].t] then
      Dec(FSecondList[i].v, 10);

    aux.activeColor := cBlack * aux.activeColor;
    aux.GenerateSimpleMoves;

    with aux do
      for j := 1 to moveCount do
      begin
        Inc(FSecondList[i].v);
        if chessboard[firstList[j].t] <> cNil then
          Inc(FSecondList[i].v);
      end;

    aux.activeColor := cBlack * aux.activeColor;
    aux.GenerateSimpleMoves;

    with aux do
      for j := 1 to moveCount do
      begin
        Dec(FSecondList[i].v);
        if chessboard[firstList[j].t] <> cNil then
          Dec(FSecondList[i].v);
      end;
{$IFDEF DEBUG}
      with FSecondList[i] do Write(MoveToStr(100 * f + t), '(', v, ') ');
{$ENDIF}
    if FSecondList[i].v >= maxValue then
    begin
      maxValue := FSecondList[i].v;
      result := i;
    end;
  end;
{$IFDEF DEBUG}
  WriteLn;
{$ENDIF}
  aux.Free;
end;

function TChessPlayer.BestMove(out aCode: TExitCode): string;
const
  CCodeIllegalMove: array[boolean] of TExitCode = (ecStalemate, ecCheckmate);
  CCodeLegalMove: array[boolean] of TExitCode = (ecSuccess, ecCheck);
var
  i: integer;
  checkBefore, prom: boolean;
begin
  result := '0000';
  i := FCurPos.BestEval(FCurPos.activeColor, 1, 32000);
  i := BestMoveIndex(i);
  if i > 0 then
  begin
    Inc(FHalfmovesCount);
    checkBefore := FCurPos.Check;
    with FSecondList[i] do
      prom := FCurPos.MovePiece(f, t, cQueen);
    FCurPos.activeColor := cBlack * FCurPos.activeColor;
    if FCurPos.Check then
      aCode := CCodeIllegalMove[checkBefore]
    else
    begin
      with FSecondList[i] do
        result := MoveToStr(100 * f + t);
      if prom then result := result + 'q';
      FCurPos.activeColor := cBlack * FCurPos.activeColor;
      aCode := CCodeLegalMove[FCurPos.Check];
    end;
  end else
    aCode := ecError;
  if not (aCode in [ecSuccess, ecCheck]) then
    TLog.Append(Format('bestmove exit code %d'#13#10'%s', [aCode, BoardAsText(TRUE)]));
end;

function TChessPlayer.BestMove(): string;
var
  _: TExitCode;
begin
  result := BestMove(_);
end;

function TChessPlayer.BoardAsText(const aPretty: boolean): string;
begin
  result := FCurPos.BoardAsText(aPretty);
end;

function TChessPlayer.FEN(): string;
begin
  result := FCurPos.FEN();
end;

function TChessPlayer.SetPosition_(const aPos: string; const aMoves: array of string): string;
var
  vIndex: integer;
begin
  SetPosition(aPos);
  for vIndex := Low(aMoves) to High(aMoves) do
    PlayMove(aMoves[vIndex]);
  result := FEN();
end;

end.
