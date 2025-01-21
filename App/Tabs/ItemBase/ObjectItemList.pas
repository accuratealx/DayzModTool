unit ObjectItemList;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LazUTF8, shlwapi;

type
  TObjectItem = class
  public
    ObjName: String;
    ObjTitle: String;
    ObjDescription: String;

    constructor Create;
    constructor Create(Str: String);

    function Copy: TObjectItem;

    procedure FromString(Str: String);
    function  ToString: String; reintroduce;
  end;


  TObjectItemListSortType = (
    stName,
    stTitle,
    stDescription
  );


  TObjectItemListSortDirection = (
    sdForward,
    sdBackward
  );


  TObjectItemList = class
  private
    FItems: array of TObjectItem;
    FCount: Integer;

    procedure AddFromStringList(AList: TStringList);

    function GetItem(Index: Integer): TObjectItem;
  public
    constructor Create;
    constructor Create(FileName: String);
    destructor  Destroy; override;

    procedure Clear;
    procedure Add(AItem: TObjectItem);
    function  IndexOfObject(AObjName: String): Integer;
    procedure Delete(Index: Integer);

    function  Add(AName, ATitle, ADescription: String): TObjectItem;

    procedure LoadFromFile(FileName: String);
    procedure SaveToFile(FileName: String);

    procedure AddFromFile(FileName: String);

    procedure Sort(SortType: TObjectItemListSortType = stName; SortDirection: TObjectItemListSortDirection = sdForward);

    property Count: Integer read FCount;
    property Item[Index: Integer]: TObjectItem read GetItem;
  end;


implementation

const
  SEPARATOR = ';;;';


constructor TObjectItem.Create;
begin

end;


constructor TObjectItem.Create(Str: String);
begin
  FromString(Str);
end;


function TObjectItem.Copy: TObjectItem;
begin
  Result := TObjectItem.Create(ToString);
end;


procedure TObjectItem.FromString(Str: String);
var
  List: TStringList;
begin
  List := TStringList.Create;
  List.LineBreak := SEPARATOR;
  List.Text := Str;

  try
    if List.Count > 0 then
      ObjName := List.Strings[0];

    if List.Count > 1 then
      ObjTitle := List.Strings[1];

    if List.Count > 2 then
      ObjDescription := List.Strings[2];

  finally
    List.Free;
  end;
end;


function TObjectItem.ToString: String;
begin
  Result := ObjName + SEPARATOR + ObjTitle + SEPARATOR + ObjDescription;
end;


procedure TObjectItemList.AddFromStringList(AList: TStringList);
var
  i: Integer;
  s: String;
  AItem: TObjectItem;
begin
  for i := 0 to AList.Count - 1 do
  begin
    s := Trim(AList.Strings[i]);

    if s = '' then
      Continue;

    AItem := TObjectItem.Create(s);

    //Если такой объект уже есть, то не грузим
    if IndexOfObject(AItem.ObjName) <> -1 then
    begin
      AItem.Free;
      Continue;
    end;

    Add(AItem);
  end;
end;


function TObjectItemList.GetItem(Index: Integer): TObjectItem;
begin
  if (Index < 0) or (Index > FCount - 1) then
    raise Exception.CreateFmt('Index out of bounds %d', [Index]);

  Result := FItems[Index];
end;


constructor TObjectItemList.Create;
begin

end;


constructor TObjectItemList.Create(FileName: String);
begin
  LoadFromFile(FileName);
end;


destructor TObjectItemList.Destroy;
begin
  Clear;
end;


procedure TObjectItemList.Clear;
var
  i: Integer;
begin
  for i := 0 to Length(FItems) - 1 do
    FItems[i].Free;

  SetLength(FItems, 0);
  FCount := 0;
end;


procedure TObjectItemList.Add(AItem: TObjectItem);
begin
  SetLength(FItems, FCount + 1);
  FItems[FCount] := AItem;
  Inc(FCount);
end;


function TObjectItemList.IndexOfObject(AObjName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  AObjName := Trim(LowerCase(AObjName));
  for i := 0 to FCount - 1 do
    if Trim(LowerCase(FItems[i].ObjName)) = AObjName then
      Exit(i);
end;


procedure TObjectItemList.Delete(Index: Integer);
var
  c, i: Integer;
begin
  c := Fcount - 1;
  if (Index < 0) or (Index > FCount) then
    raise Exception.CreateFmt('Index out of bounds %d', [Index]);

  FItems[Index].Free;

  for i := Index to c - 1 do
    FItems[i] := FItems[i + 1];

  SetLength(FItems, c);
  Dec(FCount);
end;


function TObjectItemList.Add(AName, ATitle, ADescription: String): TObjectItem;
var
  AItem: TObjectItem;
begin
  AItem := TObjectItem.Create;
  AItem.ObjName := AName;
  AItem.ObjTitle := ATitle;
  AItem.ObjDescription := ADescription;

  Add(AItem);

  Result := AItem;
end;


procedure TObjectItemList.LoadFromFile(FileName: String);
begin
  Clear;
  AddFromFile(FileName);
end;


procedure TObjectItemList.SaveToFile(FileName: String);
var
  List: TStringList;
  i: Integer;
begin
  List := TStringList.Create;
  List.LineBreak := sLineBreak;
  try

    for i := 0 to FCount - 1 do
      List.Add(FItems[i].ToString);

    List.SaveToFile(FileName);

  finally
    List.Free;
  end;
end;


procedure TObjectItemList.AddFromFile(FileName: String);
var
  List: TStringList;
begin
  List := TStringList.Create;
  List.LineBreak := sLineBreak;
  try
    List.LoadFromFile(FileName);
    AddFromStringList(List);

  finally
    List.Free;
  end;
end;


procedure TObjectItemList.Sort(SortType: TObjectItemListSortType; SortDirection: TObjectItemListSortDirection);

  function GetCompareData(AItem: TObjectItem): String;
  begin
    case SortType of
      stName:
        Result := AItem.ObjName;
      stTitle:
        Result := AItem.ObjTitle;
      stDescription:
        Result := AItem.ObjDescription;
    end;
  end;

  function ACompareString(Sa, Sb: String): Integer;
  begin
    Result := StrCmpLogicalW(PWideChar(UTF8Decode(Sa)), PWideChar(UTF8Decode(Sb)));
  end;

var
  i, j, ci, cj: Integer;
  s1, s2: String;
  AItem: TObjectItem;
begin
  ci := FCount - 1;
  cj := ci - 1;
  for i := 0 to ci do
  begin
    for j := 0 to cj - i do
    begin
      s1 := GetCompareData(FItems[j]);
      s2 := GetCompareData(FItems[j + 1]);

      if ACompareString(s1, s2) > 0 then
      begin
        AItem := FItems[j];
        FItems[j] := FItems[j + 1];
        FItems[j + 1] := AItem;
      end;
    end;
  end;

  ci := FCount - 1;
  if (SortDirection = sdBackward) and (ci > 0) then
  begin
    cj := ci div 2;
    for i := 0 to cj do
    begin
      AItem := FItems[i];
      FItems[i] := FItems[ci - i];
      FItems[ci - i] := AItem;
    end;
  end;
end;



end.

