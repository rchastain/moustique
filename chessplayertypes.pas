
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
  CNil    =   0;
  CWhite  =   1;
  CBlack  =  -1;
  CPawn   =   2;
  CBishop =   6;
  CKnight =   7;
  CRook   =  10;
  CQueen  =  19;
  CKing   = 126;
  CBorder = 127;
  (*
  CColToInt: array['a'..'h'] of integer = (1, 2, 3, 4, 5, 6, 7, 8);
  CRowToInt: array['1'..'8'] of integer = (1, 2, 3, 4, 5, 6, 7, 8);
  CDigitToRow: array['1'..'8'] of char = ('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h');
  CRowToDigit: array['a'..'h'] of char = ('1', '2', '3', '4', '5', '6', '7', '8');
  *)
  
function ColToInt(const c: char): integer; 
function RowToInt(const c: char): integer;
function DigitToRow(const c: char): char;
procedure MoveToInt(const m: string; var i, p: integer);
function MoveToStr(const m: integer): string;
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

function MoveToStr(const m: integer): string;
begin
  result := IntToStr(m);
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
      'b': p := CBishop;
      'n': p := CKnight;
      'r': p := CRook;
      'q': p := CQueen;
    end;
end;

function PieceToInt(const p: char): shortint;
begin
  case p of
    'P': result := CWhite * CPawn;
    'N': result := CWhite * CKnight;
    'B': result := CWhite * CBishop;
    'R': result := CWhite * CRook;
    'Q': result := CWhite * CQueen;
    'K': result := CWhite * CKing;
    'p': result := CBlack * CPawn;
    'n': result := CBlack * CKnight;
    'b': result := CBlack * CBishop;
    'r': result := CBlack * CRook;
    'q': result := CBlack * CQueen;
    'k': result := CBlack * CKing;
  else
    result := CNil;
  end;
end;

function PieceToChar(const p: shortint): char;
begin
  case p of
    CWhite * CPawn  : result := 'P';
    CWhite * CKnight: result := 'N';
    CWhite * CBishop: result := 'B';
    CWhite * CRook  : result := 'R';
    CWhite * CQueen : result := 'Q';
    CWhite * CKing  : result := 'K';
    CBlack * CPawn  : result := 'p';
    CBlack * CKnight: result := 'n';
    CBlack * CBishop: result := 'b';
    CBlack * CRook  : result := 'r';
    CBlack * CQueen : result := 'q';
    CBlack * CKing  : result := 'k';
    CNil            : result := '.';
    CBorder         : result := '#'
    else              result := '?';
  end;
end;

end.
