
program Moustique;

uses
{$IFDEF UNIX}
  CThreads, CWString,
{$ENDIF}
  Classes, SysUtils, ChessPlayer, Uci, Expressions, Log, Settings, Book;

{$INCLUDE version.inc}

var
  LChessPlayer: TChessPlayer;

function GetPlayerMove(const AFenRecord: string; const AMovesArray: array of string): string;
var
  LResultFen: string;
begin
  result := 'a1a1';
  if not IsFen(AFenRecord) then
    Exit;
  LResultFen := LChessPlayer.SetPositionFromMoves(AFenRecord, AMovesArray);
  result := Book.BestMove(LResultFen, LBook);
  if result <> '' then
    TLog.Append(Format('book move %s', [result]))
  else
    result := LChessPlayer.BestMove;
end;

var
  LPosition: string;
  LMovesArray: array of string;

type
  TBestMoveThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TBestMoveThread.Execute;
var
  LMove: string;
begin
  LMove := GetPlayerMove(LPosition, LMovesArray);
  SetLength(LMovesArray, 0);
  WriteLn(output, Format('bestmove %s', [LMove]));
  Flush(output);
end;

const
  CTraditionalStartPos = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';

var
  LParser: TUciCommandParser;
  LCommand: string;
  LUciCommand: TUciCommand;
  LThread: TBestMoveThread;
  LIndex: integer;

begin
  TLog.Append(Format('Moustique %s %s %s Free Pascal %s', [CAppVersion, {$I %DATE%}, {$I %TIME%}, {$I %FPCVERSION%}]));
  
  LChessPlayer := TChessPlayer.Create;
  LParser := TUciCommandParser.Create;
  LPosition := CTraditionalStartPos;
  SetLength(LMovesArray, 0);

  while not Eof do
  begin
    ReadLn(input, LCommand);
    TLog.Append('> ' + LCommand);

    LUciCommand := LParser.ParseCommand(LCommand);
    case LUciCommand of
      cmdUci:
        begin
          WriteLn(output, 'id name ' + CAppName + ' ' + CAppVersion);
          WriteLn(output, 'id author ' + CAppAuthor);
          WriteLn(output, 'uciok');
          Flush(output);
        end;
      cmdQuit:
        Break;
      cmdNewGame:
        LPosition := CTraditionalStartPos;
      cmdPositionFen:
        LPosition := LParser.Position;
      cmdPositionStartPos:
        begin
          LPosition := CTraditionalStartPos;
          SetLength(LMovesArray, LParser.Moves.Count);
          for LIndex := 0 to Pred(LParser.Moves.Count) do
            LMovesArray[LIndex] := LParser.Moves[LIndex];
        end;
      cmdGo:
        begin
          LThread := TBestMoveThread.Create(TRUE);
          LThread.FreeOnTerminate := TRUE;
          LThread.Priority := tpHigher;
          LThread.Start;
        end;
      cmdIsReady:
        begin
          WriteLn(output, 'readyok');
          Flush(output);
        end;
      cmdStop:
        begin
          WriteLn(output, Format('bestmove %s', ['a1a1']));
          Flush(output);
        end;
      cmdUnknown:
        begin
          TLog.Append(Format('Unknown command: %s', [LCommand]));
        end;
    end;
  end;
  
  LChessPlayer.Free;
  LParser.Free;
end.
