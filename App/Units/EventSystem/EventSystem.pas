unit EventSystem;

{$mode ObjFPC}{$H+}

interface

uses
  Contnrs,
  EventSubscriberHandlerList;

const
  esMountUnmountWorkDrive = 'Event.WorkDrive';


type
  TEventSystem = class
  private
    FSubscribers: TFPHashObjectList;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;
    procedure Subscribe(EventName: String; Handler: TEventHandler);
    procedure Publish(EventName: String);
  end;


implementation


constructor TEventSystem.Create;
begin
  FSubscribers := TFPHashObjectList.Create(True);
end;


destructor TEventSystem.Destroy;
begin
  FSubscribers.Free;
end;


procedure TEventSystem.Clear;
begin
  FSubscribers.Clear;
end;


procedure TEventSystem.Subscribe(EventName:String; Handler: TEventHandler);
var
  SubList: TEventSubscriberHandlerList;
begin
  SubList := TEventSubscriberHandlerList(FSubscribers.Find(EventName));
  if SubList = nil then
  begin
    SubList := TEventSubscriberHandlerList.Create;
    FSubscribers.Add(EventName, SubList);
  end;

  SubList.Add(Handler);
end;


procedure TEventSystem.Publish(EventName:String);
var
  SubList: TEventSubscriberHandlerList;
  i: Integer;
  Handler: TEventHandler;
begin
  SubList := TEventSubscriberHandlerList(FSubscribers.Find(EventName));
  if SubList = nil then
    Exit;

  for i := 0 to SubList.Count - 1 do
  begin
    Handler := SubList.Item[i];
    Handler;
  end;
end;



end.

