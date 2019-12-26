
unit Uci;

interface

uses
  Classes;

type
  TUciCommand =
  (
    cmdUci,
    cmdQuit,
    cmdNewGame,
    cmdPositionFen,
    cmdPositionStartPos,
    cmdGo,
    cmdIsReady,
    cmdStop,
    cmdUnknown
  );

  TUciCommandParser = class
    private
      FMoves: TStringList;
      FPosition: string;
    public
      constructor Create;
      destructor Destroy; override;
      function ParseCommand(const ACommand: string): TUciCommand;
      property Moves: TStringList read FMoves;
      property Position: string read FPosition;
  end;

implementation

uses
  Expressions;

constructor TUciCommandParser.Create;
begin
  FMoves := Expressions.LMoveList;
end;

destructor TUciCommandParser.Destroy;
begin
  inherited Destroy;
end;

function TUciCommandParser.ParseCommand(const ACommand: string): TUciCommand;
begin
  if ACommand = 'uci' then
    result := cmdUci
  else
    if ACommand = 'quit' then
      result := cmdQuit
    else
      if ACommand = 'ucinewgame' then
        result := cmdNewGame
      else
        if (Pos('position fen', ACommand) = 1)
        and ExtractFen(ACommand, FPosition) then
          result := cmdPositionFen
        else
          if (Pos('position startpos', ACommand) = 1) then
          begin
            result := cmdPositionStartPos;
            ExtractMoves(ACommand);
          end else
            if Pos('go', ACommand) = 1 then
              result := cmdGo
            else
              if ACommand = 'isready' then
                result := cmdIsReady
              else
                if ACommand = 'stop' then
                  result := cmdStop
                else
                  result := cmdUnknown;
end;

end.
