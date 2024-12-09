unit EventSubscriberHandlerList;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

type
  //Обработчик события
  TEventHandler = procedure of object;


  //Список обработчиков
  TEventSubscriberHandlerList = class
  private
    FCount: Integer;
    FList: array of TEventHandler;

    function GetItem(Index: Integer): TEventHandler;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;
    procedure Add(Handler: TEventHandler);

    property Count: Integer read FCount;
    property Item[Index: Integer]: TEventHandler read GetItem;
  end;


implementation


function TEventSubscriberHandlerList.GetItem(Index: Integer): TEventHandler;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt('Index out of bounds %s', [Index]);

  Result := FList[Index];
end;


constructor TEventSubscriberHandlerList.Create;
begin
  FCount := 0;
end;


destructor TEventSubscriberHandlerList.Destroy;
begin
  Clear;
end;


procedure TEventSubscriberHandlerList.Clear;
begin
  FCount := 0;
  SetLength(FList, FCount);
end;


procedure TEventSubscriberHandlerList.Add(Handler: TEventHandler);
begin
  SetLength(FList, FCount + 1);
  FList[FCount] := Handler;
  Inc(FCount);
end;



end.

