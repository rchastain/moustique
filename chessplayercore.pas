
{ Autor:   Jürgen Schlottke, Schönaich-C.-Str. 46, D-W 2200 Elmshorn
           Tel. 04121/63109
  Zweck  : Demonstration der Schachprogrammierung unter Turbo-Pascal
  Datum  : irgendwann 1991, als PD freigegeben am 18.01.93
  Version: ohne Versionsnummer
}

unit ChessPlayerCore;

interface

uses
  ChessPlayerTypes;

const
  CListSize = 100;
  
type
  TCastling = (g1, c1, g8, c8);

  TMove = record
    FFrom, FTo: shortint;
    FValue: integer;
  end;

  TMoveList = array[1..CListSize] of TMove;

  TChessPosition = class
  strict private
    FBoard: TChessboard;
    FActive: shortint;
    FCastling: array[TCastling] of boolean;
    FEnPassant: array[CBlack..CWhite] of shortint;
    FMoveList: TMoveList;
    FMoveCount: byte;
    FBalance: integer;
    FTwoKings: boolean;
  public
    constructor Create(const APos: string); overload;
    procedure SetPosition(const APos: string); overload;
    procedure SetPosition(const APos: TChessPosition); overload;
    function Eval(const AColor: shortint): integer;
    function MovePiece(AFrom, ATo: shortint; const APromo: shortint): boolean;
    procedure AppendMove(const AFrom, ATo: shortint; const ACondition: boolean = TRUE);
    procedure GenerateSimpleMoves;
    function Check: boolean;
    function CastlingCheck(const AKSquare, AStep: shortint): boolean;
    procedure GenerateCastling(const AKSquare, AStep, ARSquare: shortint);
    procedure GenerateMoves;
    function BestEval(const AColor, ADepth, AAlpha: integer): integer;
    function IsLegal(const AMove: integer): boolean;
    function BoardAsText(const APretty: boolean): string;
    function FEN: string;
    property activeColor: shortint read FActive write FActive;
    property moveCount: byte read FMoveCount;
    property firstList: TMoveList read FMoveList;
    property chessboard: TChessboard read FBoard;
  end;

implementation

uses
  Classes, SysUtils, TypInfo, Log, Settings;

const
  cVecPawn  : array[1..4] of shortint = ( 01,  02, -09,  11);
  cVecBishop: array[1..4] of shortint = ( 11, -11,  09, -09);
  cVecRook  : array[1..4] of shortint = (-01,  01, -10,  10);
  cVecKnight: array[1..8] of shortint = ( 12,  21,  19,  08, -12, -21, -19, -08);
  cVecKing  : array[1..8] of shortint = (-01,  01, -10,  10,  11, -11,  09, -09);

constructor TChessPosition.Create(const APos: string);
begin
  inherited Create;
  SetPosition(APos);
end;

procedure TChessPosition.SetPosition(const APos: string);
var
  LFields: TStringList;
  x, y: integer;
  i, j: integer;
begin
  LFields := TStringList.Create;
  ExtractStrings([' '], [], pchar(APos), LFields);
  Assert(LFields.Count in [4, 6]);

  for i := Low(FBoard) to High(FBoard) do
    FBoard[i] := CBorder;

  x := 1;
  y := 8;
  for i := 1 to Length(LFields[0]) do
  begin
    case LFields[0][i] of
      '/':
        begin
          x := 1;
          Dec(y);
        end;
      '1'..'8':
        begin
          j := RowToInt(LFields[0][i]);
          while j > 0 do
          begin
            FBoard[10 * x + y] := CNil;
            Inc(x);
            Dec(j);
          end;
        end;
      'P', 'N', 'B', 'R', 'Q', 'K',
      'p', 'n', 'b', 'r', 'q', 'k':
        begin
          FBoard[10 * x + y] := PieceToInt(LFields[0][i]);
          Inc(x);
        end;
    end;
  end;

  if LFields[1] = 'w' then
    FActive := CWhite
  else
    FActive := CBlack;

  FCastling[g1] := Pos('K', LFields[2]) > 0;
  FCastling[c1] := Pos('Q', LFields[2]) > 0;
  FCastling[g8] := Pos('k', LFields[2]) > 0;
  FCastling[c8] := Pos('q', LFields[2]) > 0;

  if LFields[3] <> '-' then
    FEnPassant[FActive] := 10 * ColToInt(LFields[3][1]) + RowToInt(LFields[3][2]);
  FEnPassant[CBlack * FActive] := CNil;

  FBalance := 0;
  for x := 1 to 8 do
    for y := 1 to 8 do
      Inc(
        FBalance,
        FBoard[10 * x + y]
      );

  FTwoKings := (Pos('K', LFields[0]) > 0) and (Pos('k', LFields[0]) > 0);

  LFields.Free;
end;

procedure TChessPosition.SetPosition(const APos: TChessPosition);
begin
  FBoard     := APos.FBoard;
  FActive    := APos.FActive;
  FCastling  := APos.FCastling;
  FEnPassant := APos.FEnPassant;
  FMoveList  := APos.FMoveList;
  FMoveCount := APos.FMoveCount;
  FBalance   := APos.FBalance;
  FTwoKings  := APos.FTwoKings;
end;

function TChessPosition.Eval(const AColor: shortint): integer;
begin
  result := FBalance * AColor;
end;

function TChessPosition.MovePiece(AFrom, ATo: shortint; const APromo: shortint): boolean;
begin
  result := FALSE;
  if FBoard[AFrom] * FActive = CKing then
    case AFrom of
      51:
        begin
          FCastling[g1] := FALSE;
          FCastling[c1] := FALSE;
        end;
      58:
        begin
          FCastling[g8] := FALSE;
          FCastling[c8] := FALSE;
        end;
    end;

  if FBoard[AFrom] * FActive = CRook then
    case AFrom of
      81: FCastling[g1] := FALSE;
      11: FCastling[c1] := FALSE;
      88: FCastling[g8] := FALSE;
      18: FCastling[c8] := FALSE;
    end;

  if ((ATo - AFrom) * (ATo - AFrom) = 4) and (FBoard[AFrom] * FActive = CPawn) then
    FEnPassant[FActive] := AFrom + cVecPawn[1] * FActive
  else
    FEnPassant[FActive] := CNil;

  if (ATo = FEnPassant[CBlack * FActive]) and (FBoard[AFrom] * FActive = CPawn) then
  begin
    FEnPassant[FActive] := CNil;
    MovePiece(AFrom, (ATo div 10) * 10 + AFrom mod 10, CQueen);
    FActive := CBlack * FActive;
    AFrom := (ATo div 10) * 10 + AFrom mod 10;
  end;

  if (AFrom in [51, 58]) and (ATo in [71, 31, 78, 38]) and (FBoard[AFrom] * FActive = CKing) then
  begin
    FBoard[ATo] := FBoard[AFrom];
    FBoard[AFrom] := CNil;
    if ATo div 10 = 7 then
    begin
      AFrom := AFrom mod 10 + 80;
      ATo := AFrom - 20;
    end else
    begin
      AFrom := AFrom mod 10 + 10;
      ATo := AFrom + 30;
    end;
    FBoard[ATo] := FBoard[AFrom];
    FBoard[AFrom] := CNil;
  end else
  begin
    FBalance := FBalance - FBoard[ATo];

    if FBoard[ATo] * CBlack * FActive = CKing then
      FTwoKings := FALSE;

    FBoard[ATo] := FBoard[AFrom];
    FBoard[AFrom] := CNil;
    if ((ATo mod 10 = 1) or (ATo mod 10 = 8)) and (FBoard[ATo] * FActive = CPawn) then
    begin
      result := TRUE;
      FBoard[ATo] := APromo * FActive;
      FBalance := FBalance + (APromo - CPawn) * FActive;
    end;
  end;

  FActive := CBlack * FActive;
end;

procedure TChessPosition.AppendMove(const AFrom, ATo: shortint; const ACondition: boolean = TRUE);
begin
  if not (ACondition and FTwoKings) then
    Exit;
  if FMoveCount < CListSize then
  begin
    Inc(FMoveCount);
    FMoveList[FMoveCount].FFrom := AFrom;
    FMoveList[FMoveCount].FTo := ATo;
  end;
end;

procedure TChessPosition.GenerateSimpleMoves;
var
  f, t, x, y, i: integer;
begin
  FMoveCount := 0;
  for x := 1 to 8 do
    for y := 1 to 8 do
    begin
      f := 10 * x + y;
      if FBoard[f] <> CNil then
      begin
        case FBoard[f] * FActive of
          CPawn:
            begin
              t := f + cVecPawn[1] * FActive;
              if FBoard[t] = CNil then
              begin
                AppendMove(f, t);
                if (FActive = CWhite) and (y = 2) or (FActive = CBlack) and (y = 7) then
                begin
                  t := f + cVecPawn[2] * FActive;
                  AppendMove(f, t, FBoard[t] = CNil);
                end;
              end;
              for i := 3 to 4 do
              begin
                t := f + cVecPawn[i] * FActive;
                AppendMove(f, t, (CBlack * FBoard[t] * FActive in [CPawn..CKing]) or (t = FEnPassant[CBlack * FActive]));
              end;
            end;
          CKnight:
            for i := 1 to 8 do
            begin
              t := f + cVecKnight[i];
              AppendMove(f, t, CBlack * FBoard[t] * FActive in [CNil..CKing]);
            end;
          CBishop:
            for i := 1 to 4 do
            begin
              t := f;
              repeat
                Inc(t, cVecBishop[i]);
                AppendMove(f, t, CBlack * FBoard[t] * FActive in [CNil..CKing]);
              until FBoard[t] <> CNil;
            end;
          CRook:
            for i := 1 to 4 do
            begin
              t := f;
              repeat
                Inc(t, cVecRook[i]);
                AppendMove(f, t, CBlack * FBoard[t] * FActive in [CNil..CKing]);
              until FBoard[t] <> 0;
            end;
          CKing:
            for i := 1 to 8 do
            begin
              t := f + cVecKing[i];
              AppendMove(f, t, CBlack * FBoard[t] * FActive in [CNil, CPawn, CBishop, CKnight, CRook, CQueen, CKing]);
            end;
          CQueen:
            for i := 1 to 8 do
            begin
              t := f;
              repeat
                Inc(t, cVecKing[i]);
                AppendMove(f, t, CBlack * FBoard[t] * FActive in [CNil..CKing]);
              until FBoard[t] <> CNil;
            end;
        end;
      end;
    end;
end;

function TChessPosition.Check: boolean;
var
  i: integer;
begin
  result := FALSE;
  FActive := CBlack * FActive;

  GenerateSimpleMoves;
  FActive := CBlack * FActive;

  for i := 1 to FMoveCount do
    if FBoard[FMoveList[i].FTo] * FActive = CKing then
    begin
      result := TRUE;
      Exit;
    end;
end;

function TChessPosition.CastlingCheck(const AKSquare, AStep: shortint): boolean;
var
  i: integer;
begin
  result := FALSE;
  FActive := CBlack * FActive;
  GenerateSimpleMoves;
  FActive := CBlack * FActive;
  for i := 1 to FMoveCount do
    with FMoveList[i] do
    if (FTo mod 10 = AKSquare mod 10)
    and ((FTo - AKSquare) div AStep >= 0)
    and ((FTo - AKSquare) div AStep <= 2) then
      Exit(TRUE);
end;

procedure TChessPosition.GenerateCastling(const AKSquare, AStep, ARSquare: shortint);
var
  LPos: TChessPosition;
  square: shortint;
begin
  if FBoard[ARSquare] * FActive <> CRook then
    Exit;
  square := AKSquare + AStep;
  repeat
    if FBoard[square] <> CNil then
      Exit;
    Inc(square, AStep);
  until square = ARSquare;
  LPos := TChessPosition.Create;
  LPos.SetPosition(self);
  if not LPos.CastlingCheck(AKSquare, AStep) then
    AppendMove(AKSquare, AKSquare + 2 * AStep);
  LPos.Free;
end;

procedure TChessPosition.GenerateMoves;
begin
  GenerateSimpleMoves;
  if FActive = CWhite then begin
    if FCastling[g1] then GenerateCastling(51, +10, 81);
    if FCastling[c1] then GenerateCastling(51, -10, 11);
  end else begin
    if FCastling[g8] then GenerateCastling(58, +10, 88);
    if FCastling[c8] then GenerateCastling(58, -10, 18);
  end;
end;

function TChessPosition.BestEval(const AColor, ADepth, AAlpha: integer): integer;
var
  LValue, LBeta, i: integer;
  LPos: TChessPosition;
  LStop: boolean;
  LLine1, LLine2: string;
begin
  GenerateMoves;
  i := 0;
  LBeta := -32000 * FActive * AColor;
  LPos := TChessPosition.Create;
  LStop := FALSE;
  LLine1 := '';
  LLine2 := '';
  while (i < FMoveCount) and not LStop do
  begin
    Inc(i);
    LPos.SetPosition(self);
    with LPos.FMoveList[i] do LPos.MovePiece(FFrom, FTo, CQueen);
    if (ADepth >= LMinDepth) and (FBoard[FMoveList[i].FTo] = CNil) or (ADepth = LMaxDepth) then
      LValue := LPos.Eval(AColor)
    else
      LValue := LPos.BestEval(AColor, ADepth + 1, LBeta);
    if FActive = AColor then
    begin
      if LValue > LBeta then
        LBeta := LValue;
      if LBeta > AAlpha then
        LStop := TRUE;
    end else
    begin
      if LValue < LBeta then
        LBeta := LValue;
      if LBeta < AAlpha then
        LStop := TRUE;
    end;
    FMoveList[i].FValue := LValue;
{$IFDEF DEBUG}
    if ADepth = 1 then
      with FMoveList[i] do
      begin
        LLine1 := LLine1 + Format('%6s', [MoveToStr(100 * FFrom + FTo)]);
        LLine2 := LLine2 + Format('%6d', [FValue]);
      end;
{$ENDIF}
  end;
{$IFDEF DEBUG}
  if ADepth = 1 then
  begin
    TLog.Append(LLine1);
    TLog.Append(LLine2);
  end;
{$ENDIF}
  result := LBeta;
  LPos.Free;
end;

function TChessPosition.IsLegal(const AMove: integer): boolean;
var
  i: integer;
begin
  result := FALSE;
  i := Low(TMoveList);
  while (i <= FMoveCount) and not result do
    with FMoveList[i] do
      if AMove = 100 * FFrom + FTo then
        result := TRUE
      else
        Inc(i);
end;

function TChessPosition.BoardAsText(const APretty: boolean): string;
var
  i: integer;
  LText, LLine: string;
  x, y: integer;
begin
  SetLength(LText, 120);
  for i := 1 to 120 do
    LText[i] := PieceToChar(FBoard[i - 11]);
  
  if APretty then
  begin
    result := '+  a b c d e f g h  +'#10;
    result := result + '                     '#10;
    for y := 8 downto 1 do
    begin
      LLine := Format('%d                   %d', [y, y]);
      for x := 1 to 8 do
        LLine[2 * x + 2] := LText[10 * x + y + 11];
      result := result + LLine + #10;
    end;
    result := result + '                     '#10;
    result := result + '+  a b c d e f g h  +';
  end else
    result := LText;
end;

function TChessPosition.FEN: string;
var
  x, y, n: integer;
  LActiveColor, LCastling, LEnPassant: string;
begin
  result := '';
  x := 1;
  y := 8;
  while y >= 1 do
  begin
    if FBoard[10 * x + y] = CNil then
    begin
      n := 0;
      while (x + n <= 8) and (FBoard[10 * (x + n) + y] = CNil) do
        Inc(n);
      result := Concat(result, IntToStr(n));
      Inc(x, n);
    end else
    begin
      result := Concat(result, PieceToChar(FBoard[10 * x + y]));
      Inc(x);
    end;
    if x > 8 then
    begin
      if y > 1 then
        result := Concat(result, '/');
      x := 1;
      Dec(y);
    end;
  end;

  if FActive = CWhite then
    LActiveColor := 'w'
  else
    LActiveColor := 't';

  LCastling := '';
  if FCastling[g1] then
    LCastling := Concat(LCastling, 'K');
  if FCastling[c1] then
    LCastling := Concat(LCastling, 'Q');
  if FCastling[g8] then
    LCastling := Concat(LCastling, 'k');
  if FCastling[c8] then
    LCastling := Concat(LCastling, 'q');
  if LCastling = '' then
    LCastling := '-';

  if FEnPassant[FActive] = CNil then
    LEnPassant := '-'
  else
  begin
    LEnPassant := IntToStr(FEnPassant[FActive]);
    LEnPassant[1] := DigitToRow(LEnPassant[1]);
  end;

  result := Format(
    '%s %s %s %s %d %d',
    [
    result,
      LActiveColor,
      LCastling,
      LEnPassant,
      0,
      1
    ]
  );
end;

end.
