
program Moustique;

uses
{$IFDEF UNIX}
  CThreads, CWString,
{$ENDIF}
  Classes, SysUtils, ChessPlayer, Uci, Expressions, Log;

{$INCLUDE version.inc}

var
  vProgram: TChessPlayer;

function GetPlayerMove(const aFENRecord: string; const aMovesArray: array of string): string;
var
  _: string;
begin
  result := 'a1a1';
  if not IsFEN(aFENRecord) then
    exit;
  _ := vProgram.SetPosition_(aFENRecord, aMovesArray);
  result := vProgram.BestMove;
end;

var
  vPosition: string;
  vMovesArray: array of string;

type
  TBestMoveThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TBestMoveThread.Execute;
var
  vMove: string;
begin
  vMove := GetPlayerMove(vPosition, vMovesArray);
  SetLength(vMovesArray, 0);
  WriteLn(output, Format('bestmove %s', [vMove]));
  Flush(output);
end;

const
  CONVENTSTARTPOS = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';

var
  vParser: TUCICommandParser;
  vCommand: string;
  vStop: boolean = FALSE;
  vUciCommand: TUCICommand;
  vThread: TBestMoveThread;
  vIndex: integer;

begin
  TLog.Append(Format('Moustique %s %s %s Free Pascal %s', [APPVERSION, {$I %DATE%}, {$I %TIME%}, {$I %FPCVERSION%}]));
  
  vProgram := TChessPlayer.Create;
  vParser := TUCICommandParser.Create;
  vPosition := CONVENTSTARTPOS;
  SetLength(vMovesArray, 0);

  repeat
    Sleep(10);
    ReadLn(input, vCommand);
    TLog.Append('>>> ' + vCommand);

    vUciCommand := vParser.ParseCommand(vCommand);
    case vUciCommand of
      cmdUCI:
        begin
          WriteLn(output, 'id name ' + APPNAME + ' ' + APPVERSION);
          WriteLn(output, 'id author ' + APPAUTHOR);
          WriteLn(output, 'uciok');
          Flush(output);
        end;
      cmdQuit:
        vStop := TRUE;
      cmdNewGame:
        vPosition := CONVENTSTARTPOS;
      cmdPositionFen:
        vPosition := vParser.position;
      cmdPositionStartPos:
        begin
          vPosition := CONVENTSTARTPOS;
          SetLength(vMovesArray, vParser.moves.Count);
          for vIndex := 0 to Pred(vParser.moves.Count) do
            vMovesArray[vIndex] := vParser.moves[vIndex];
        end;
      cmdGo:
        begin
          vThread := TBestMoveThread.Create(TRUE);
          vThread.FreeOnTerminate := TRUE;
          vThread.Priority := tpHigher;
          vThread.Start;
        end;
      cmdIsReady:
        begin
          WriteLn(output, 'readyok');
          Flush(output);
        end;
      cmdStop:
        begin
        end;
      cmdUnknown:
        begin
        end;
    end;
  until vStop;
  
  vProgram.Free;
  vParser.Free;
end.
