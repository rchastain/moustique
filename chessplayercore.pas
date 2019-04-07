
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

type
  TCastling = (g1, c1, g8, c8);

  TMoveValue = record
    f, t: shortint;
    v: integer;
  end;

  TMoveList = array[1..100] of TMoveValue;

  TChessPosition = class
  strict private
    board: TChessboard;
    active: shortint;
    castling: array[TCastling] of boolean;
    enPassant: array[cBlack..cWhite] of shortint;
    moveList: TMoveList;
    count: byte;
    balance: integer;
    twoKings: boolean;
  public
    constructor Create(const aPos: string); overload;
    procedure SetPosition(const aPos: string); overload;
    procedure SetPosition(const aPos: TChessPosition); overload;
    function Eval(const aColor: shortint): integer;
    function MovePiece(f, t: shortint; const aPromo: shortint): boolean;
    procedure AppendMove(const f, t: shortint; const cond: boolean = TRUE);
    procedure GenerateSimpleMoves;
    function Check(): boolean;
    function CastlingCheck(const aKSquare, aStep: shortint): boolean;
    procedure GenerateCastling(const aKSquare, aStep, aRSquare: shortint);
    procedure GenerateMoves;
    function BestEval(const aColor, aDepth, aAlpha: integer): integer;
    function IsLegal(const aMove: integer): boolean;
    function BoardAsText(const aPretty: boolean): string;
    function FEN(): string;
    property activeColor: shortint read active write active;
    property moveCount: byte read count;
    property firstList: TMoveList read moveList;
    property chessboard: TChessboard read board;
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

constructor TChessPosition.Create(const aPos: string);
begin
  inherited Create;
  SetPosition(aPos);
end;

procedure TChessPosition.SetPosition(const aPos: string);
var
  lst: TStringList;
  x, y, i, j: integer;
begin
  lst := TStringList.Create;
  ExtractStrings([' '], [], pchar(aPos), lst);
  Assert(lst.Count in [4, 6]);

  for i := Low(board) to High(board) do
    board[i] := cBorder;

  x := 1;
  y := 8;
  for i := 1 to Length(lst[0]) do
  begin
    case lst[0][i] of
      '/':
        begin
          x := 1;
          Dec(y);
        end;
      '1'..'8':
        begin
          j := RowToInt(lst[0][i]);
          while j > 0 do
          begin
            board[10 * x + y] := cNil;
            Inc(x);
            Dec(j);
          end;
        end;
      'P', 'N', 'B', 'R', 'Q', 'K', 'p', 'n', 'b', 'r', 'q', 'k':
        begin
          board[10 * x + y] := PieceToInt(lst[0][i]);
          Inc(x);
        end;
    end;
  end;

  if lst[1] = 'w' then
    active := cWhite
  else
    active := cBlack;

  castling[g1] := Pos('K', lst[2]) > 0;
  castling[c1] := Pos('Q', lst[2]) > 0;
  castling[g8] := Pos('k', lst[2]) > 0;
  castling[c8] := Pos('q', lst[2]) > 0;

  if lst[3] <> '-' then
    enPassant[active] := 10 * ColToInt(lst[3][1]) + RowToInt(lst[3][2]);
  enPassant[cBlack * active] := cNil;

  balance := 0;
  for x := 1 to 8 do
    for y := 1 to 8 do
      Inc(
        balance,
        board[10 * x + y]
      );

  twoKings := (Pos('K', lst[0]) > 0) and (Pos('k', lst[0]) > 0);

  lst.Free;
end;

procedure TChessPosition.SetPosition(const aPos: TChessPosition);
begin
  board := aPos.board;
  active := aPos.active;
  castling := aPos.castling;
  enPassant := aPos.enPassant;
  moveList := aPos.moveList;
  count := aPos.count;
  balance := aPos.balance;
  twoKings := aPos.twoKings;
end;

function TChessPosition.Eval(const aColor: shortint): integer;
begin
  result := balance * aColor;
end;

function TChessPosition.MovePiece(f, t: shortint; const aPromo: shortint): boolean;
begin
  result := FALSE;
  if board[f] * active = cKing then
    case f of
      51:
        begin
          castling[g1] := FALSE;
          castling[c1] := FALSE;
        end;
      58:
        begin
          castling[g8] := FALSE;
          castling[c8] := FALSE;
        end;
    end;

  if board[f] * active = cRook then
    case f of
      81: castling[g1] := FALSE;
      11: castling[c1] := FALSE;
      88: castling[g8] := FALSE;
      18: castling[c8] := FALSE;
    end;

  if ((t - f) * (t - f) = 4) and (board[f] * active = cPawn) then
    enPassant[active] := f + cVecPawn[1] * active
  else
    enPassant[active] := cNil;

  if (t = enPassant[cBlack * active]) and (board[f] * active = cPawn) then
  begin
    enPassant[active] := cNil;
    MovePiece(f, (t div 10) * 10 + f mod 10, cQueen);
    active := cBlack * active;
    f := (t div 10) * 10 + f mod 10;
  end;

  if (f in [51, 58]) and (t in [71, 31, 78, 38]) and (board[f] * active = cKing) then
  begin
    board[t] := board[f];
    board[f] := cNil;
    if t div 10 = 7 then
    begin
      f := f mod 10 + 80;
      t := f - 20;
    end else
    begin
      f := f mod 10 + 10;
      t := f + 30;
    end;
    board[t] := board[f];
    board[f] := cNil;
  end else
  begin
    balance := balance - board[t];

    if board[t] * cBlack * active = cKing then
      twoKings := FALSE;

    board[t] := board[f];
    board[f] := cNil;
    if ((t mod 10 = 1) or (t mod 10 = 8)) and (board[t] * active = cPawn) then
    begin
      result := TRUE;
      board[t] := aPromo * active;
      balance := balance + (aPromo - cPawn) * active;
    end;
  end;

  active := cBlack * active;
end;

procedure TChessPosition.AppendMove(const f, t: shortint; const cond: boolean = TRUE);
begin
  if not (cond and twoKings) then
    exit;
  Inc(count);
  moveList[count].f := f;
  moveList[count].t := t;
end;

procedure TChessPosition.GenerateSimpleMoves;
var
  f, t, x, y, i: integer;
begin
  count := 0;
  for x := 1 to 8 do
    for y := 1 to 8 do
    begin
      f := 10 * x + y;
      if board[f] <> cNil then
      begin
        case board[f] * active of
          cPawn:
            begin
              t := f + cVecPawn[1] * active;
              if board[t] = cNil then
              begin
                AppendMove(f, t);
                if (active = cWhite) and (y = 2) or (active = cBlack) and (y = 7) then
                begin
                  t := f + cVecPawn[2] * active;
                  AppendMove(f, t, board[t] = cNil);
                end;
              end;
              for i := 3 to 4 do
              begin
                t := f + cVecPawn[i] * active;
                AppendMove(f, t, (cBlack * board[t] * active in [cPawn..cKing]) or (t = enPassant[cBlack * active]));
              end;
            end;
          cKnight:
            for i := 1 to 8 do
            begin
              t := f + cVecKnight[i];
              AppendMove(f, t, cBlack * board[t] * active in [cNil..cKing]);
            end;
          cBishop:
            for i := 1 to 4 do
            begin
              t := f;
              repeat
                Inc(t, cVecBishop[i]);
                AppendMove(f, t, cBlack * board[t] * active in [cNil..cKing]);
              until board[t] <> cNil;
            end;
          cRook:
            for i := 1 to 4 do
            begin
              t := f;
              repeat
                Inc(t, cVecRook[i]);
                AppendMove(f, t, cBlack * board[t] * active in [cNil..cKing]);
              until board[t] <> 0;
            end;
          cKing:
            for i := 1 to 8 do
            begin
              t := f + cVecKing[i];
              AppendMove(f, t, cBlack * board[t] * active in [cNil, cPawn, cBishop, cKnight, cRook, cQueen, cKing]);
            end;
          cQueen:
            for i := 1 to 8 do
            begin
              t := f;
              repeat
                Inc(t, cVecKing[i]);
                AppendMove(f, t, cBlack * board[t] * active in [cNil..cKing]);
              until board[t] <> cNil;
            end;
        end;
      end;
    end;
end;

function TChessPosition.Check(): boolean;
var
  i: integer;
begin
  result := FALSE;
  active := cBlack * active;

  GenerateSimpleMoves;
  active := cBlack * active;

  for i := 1 to count do
    if board[moveList[i].t] * active = cKing then
    begin
      result := TRUE;
      exit;
    end;
end;

function TChessPosition.CastlingCheck(const aKSquare, aStep: shortint): boolean;
var
  i: integer;
begin
  result := FALSE;
  active := cBlack * active;
  GenerateSimpleMoves;
  active := cBlack * active;
  for i := 1 to count do
    with moveList[i] do
    if (t mod 10 = aKSquare mod 10)
    and ((t - aKSquare) div aStep >= 0)
    and ((t - aKSquare) div aStep <= 2) then
      exit(TRUE);
end;

procedure TChessPosition.GenerateCastling(const aKSquare, aStep, aRSquare: shortint);
var
  vPos: TChessPosition;
  square: shortint;
begin
  if board[aRSquare] * active <> cRook then
    exit;
  square := aKSquare + aStep;
  repeat
    if board[square] <> cNil then
      exit;
    Inc(square, aStep);
  until square = aRSquare;
  vPos := TChessPosition.Create;
  vPos.SetPosition(self);
  if not vPos.CastlingCheck(aKSquare, aStep) then
    AppendMove(aKSquare, aKSquare + 2 * aStep);
  vPos.Free;
end;

procedure TChessPosition.GenerateMoves;
begin
  GenerateSimpleMoves;
  if active = cWhite then begin
    if castling[g1] then GenerateCastling(51, +10, 81);
    if castling[c1] then GenerateCastling(51, -10, 11);
  end else begin
    if castling[g8] then GenerateCastling(58, +10, 88);
    if castling[c8] then GenerateCastling(58, -10, 18);
  end;
end;

function TChessPosition.BestEval(const aColor, aDepth, aAlpha: integer): integer;
var
  vValue, vBeta, i: integer;
  vPos: TChessPosition;
  vStop: boolean;
begin
  GenerateMoves;
  i := 0;
  vBeta := -32000 * active * aColor;
  vPos := TChessPosition.Create;
  vStop := FALSE;
  while (i < count) and not vStop do
  begin
    Inc(i);
    vPos.SetPosition(self);
    with vPos.moveList[i] do vPos.MovePiece(f, t, cQueen);
    if (aDepth >= vMinDepth) and (board[moveList[i].t] = cNil) or (aDepth = vMaxDepth) then
      vValue := vPos.Eval(aColor)
    else
      vValue := vPos.BestEval(aColor, aDepth + 1, vBeta);
    if active = aColor then
    begin
      if vValue > vBeta then
        vBeta := vValue;
      if vBeta > aAlpha then
        vStop := TRUE;
    end else
    begin
      if vValue < vBeta then
        vBeta := vValue;
      if vBeta < aAlpha then
        vStop := TRUE;
    end;
    moveList[i].v := vValue;
{$IFDEF DEBUG}
    if aDepth = 1 then with moveList[i] do Write(MoveToStr(100 * f + t), '(', v, ') ');
{$ENDIF}
  end;
{$IFDEF DEBUG}
  if aDepth = 1 then WriteLn;
{$ENDIF}
  result := vBeta;
  vPos.Free;
end;

function TChessPosition.IsLegal(const aMove: integer): boolean;
var
  i: integer;
begin
  result := FALSE;
  i := Low(TMoveList);
  while (i <= count) and not result do
    with moveList[i] do
      if aMove = 100 * f + t then
        result := TRUE
      else
        Inc(i);
end;

function TChessPosition.BoardAsText(const aPretty: boolean): string;
var
  i: integer;
  vText, vLine: string;
  x, y: integer;
begin
  SetLength(vText, 120);
  for i := 1 to 120 do
    vText[i] := PieceToChar(board[i - 11]);
  
  if aPretty then
  begin
    result := '+  a b c d e f g h  +'#10;
    result := result + '                     '#10;
    for y := 8 downto 1 do
    begin
      vLine := Format('%d                   %d', [y, y]);
      for x := 1 to 8 do
        vLine[2 * x + 2] := vText[10 * x + y + 11];
      result := result + vLine + #10;
    end;
    result := result + '                     '#10;
    result := result + '+  a b c d e f g h  +';
  end else
    result := vText;
end;

function TChessPosition.FEN(): string;
var
  x, y, n: integer;
  act, castl, pass: string;
begin
  result := '';
  x := 1;
  y := 8;
  while y >= 1 do
  begin
    if board[10 * x + y] = cNil then
    begin
      n := 0;
      while (x + n <= 8) and (board[10 * (x + n) + y] = cNil) do
        Inc(n);
      result := Concat(result, IntToStr(n));
      Inc(x, n);
    end else
    begin
      result := Concat(result, PieceToChar(board[10 * x + y]));
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

  if active = cWhite then
    act := 'w'
  else
    act := 't';

  castl := '';
  if castling[g1] then
    castl := Concat(castl, 'K');
  if castling[c1] then
    castl := Concat(castl, 'Q');
  if castling[g8] then
    castl := Concat(castl, 'k');
  if castling[c8] then
    castl := Concat(castl, 'q');
  if castl = '' then
    castl := '-';

  if enPassant[active] = cNil then
    pass := '-'
  else
  begin
    pass := IntToStr(enPassant[active]);
    pass[1] := DigitToRow(pass[1]);
  end;

  result := Format(
    '%s %s %s %s %d %d',
    [
    result,
      act,
      castl,
      pass,
      0,
      1
    ]
  );
end;

end.
