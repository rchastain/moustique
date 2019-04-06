
{ Autor:   Jürgen Schlottke, Schönaich-C.-Str. 46, D-W 2200 Elmshorn
           Tel. 04121/63109
  Zweck  : Demonstration der Schachprogrammierung unter Turbo-Pascal
  Datum  : irgendwann 1991, als PD freigegeben am 18.01.93
  Version: ohne Versionsnummer
}

unit ChessPlayerTypes;

interface

type
  TChessboard = array[-10..109] of shortint;

const
  cNil    =   0;
  cWhite  =   1;
  cBlack  =  -1;
  cPawn   =   2;
  cBishop =   6;
  cKnight =   7;
  cRook   =  10;
  cQueen  =  19;
  cKing   = 126;
  cBorder = 127;
  (*
  cColToInt: array['a'..'h'] of integer = (1, 2, 3, 4, 5, 6, 7, 8);
  cRowToInt: array['1'..'8'] of integer = (1, 2, 3, 4, 5, 6, 7, 8);
  cDigitToRow: array['1'..'8'] of char = ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h');
  cRowToDigit: array['a'..'h'] of char = ('1', '2', '3', '4', '5', '6', '7', '8');
  *)
  
function ColToInt(const c: char): integer; 
function RowToInt(const c: char): integer;
function DigitToRow(const c: char): char;
procedure MoveToInt(const m: string; var i, p: integer);
function MoveToStr(const iMove: integer): string;
function PieceToInt(const p: char): shortint;
function PieceToChar(const p: shortint): char;

implementation

uses
  SysUtils;
  
function ColToInt(const c: char): integer;
begin
  result := Ord(c) - Ord('a') + 1;
end;

function RowToInt(const c: char): integer;
begin
  result := Ord(c) - Ord('1') + 1;
end;

function DigitToRow(const c: char): char;
begin
  result := Chr(Ord(c) - Ord('1') + Ord('a'));
end;

function RowToDigit(const c: char): char;
begin
  result := Chr(Ord(c) - Ord('a') + Ord('1'));
end;

function MoveToStr(const iMove: integer): string;
begin
  result := IntToStr(iMove);
  result[1] := DigitToRow(result[1]);
  result[3] := DigitToRow(result[3]);
end;

procedure MoveToInt(const m: string; var i, p: integer);
var
  s: string;
begin
  s := Copy(m, 1, 4);
  s[1] := RowToDigit(s[1]);
  s[3] := RowToDigit(s[3]);
  i := StrToInt(s);
  p := 0;
  if Length(m) > 4 then
    case m[5] of
      'b': p := cBishop;
      'n': p := cKnight;
      'r': p := cRook;
      'q': p := cQueen;
    end;
end;

function PieceToInt(const p: char): shortint;
begin
  case p of
    'P': result := cWhite * cPawn;
    'N': result := cWhite * cKnight;
    'B': result := cWhite * cBishop;
    'R': result := cWhite * cRook;
    'Q': result := cWhite * cQueen;
    'K': result := cWhite * cKing;
    'p': result := cBlack * cPawn;
    'n': result := cBlack * cKnight;
    'b': result := cBlack * cBishop;
    'r': result := cBlack * cRook;
    'q': result := cBlack * cQueen;
    'k': result := cBlack * cKing;
  else
    result := cNil;
  end;
end;

function PieceToChar(const p: shortint): char;
begin
  case p of
    cWhite * cPawn  : result := 'P';
    cWhite * cKnight: result := 'N';
    cWhite * cBishop: result := 'B';
    cWhite * cRook  : result := 'R';
    cWhite * cQueen : result := 'Q';
    cWhite * cKing  : result := 'K';
    cBlack * cPawn  : result := 'p';
    cBlack * cKnight: result := 'n';
    cBlack * cBishop: result := 'b';
    cBlack * cRook  : result := 'r';
    cBlack * cQueen : result := 'q';
    cBlack * cKing  : result := 'k';
    cNil            : result := '.';
    cBorder         : result := '#'
    else              result := '?';
  end;
end;

end.
