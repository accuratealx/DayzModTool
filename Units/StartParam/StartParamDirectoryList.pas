unit StartParamDirectoryList;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTemplateCollection,
  StartParamSimple;

type
  //Один мод
  TStartParamDirectoryListItem = class
  private
    FChecked: Boolean;  //Выбрано или нет
    FValue: String;     //Имя/Путь
  public
    constructor Create;
    constructor Create(Checked: Boolean; Value: String);

    function  ValueToString: String;
    procedure ValueFromString(const Str: String);

    property Checked: Boolean read FChecked write FChecked;
    property Value: String read FValue write FValue;
  end;


  TStartParamDirectoryListItemArray = class(specialize TsgeTemplateCollection<TStartParamDirectoryListItem>)
  protected
    function IndexOfName(AName: String): Integer;
  public
    function GetCommandLine(Prefix: String = ''): String;

    function  ValueToString: String;
    procedure ValueFromString(const Str: String);
  end;


  TStartParamDirectoryList = class(TStartParamSimple)
  protected
    FDirectoryList: TStartParamDirectoryListItemArray;
    FValue : String;
    FFullPath: Boolean;

    function  GetType: String; override;
    procedure LoadModList(const ADirectory: String);

    procedure SetValue(AValue: String);
  public
    constructor Create(const AName, APrefix, ADescription: String; AFullPath: Boolean = True; AValue: String = '');
    destructor  Destroy; override;

    function  ValueToString: String; override;
    procedure ValueFromString(const Str: String); override;

    function GetCommandLine: String; override;

    property DirectoryList: TStartParamDirectoryListItemArray read FDirectoryList;
    property Value: String read FValue write SetValue;
    property FullPath: Boolean read FFullPath write FFullPath;
  end;


implementation

uses
  SysUtils, Classes;


constructor TStartParamDirectoryListItem.Create;
begin
  FChecked := True;
  FValue := '';
end;


constructor TStartParamDirectoryListItem.Create(Checked: Boolean; Value: String);
begin
  FChecked := Checked;
  FValue := Value;
end;


function TStartParamDirectoryListItem.ValueToString: String;
begin
  //1:DreamCore
  Result := BoolToStr(FChecked, False) + ':' + FValue;
end;


procedure TStartParamDirectoryListItem.ValueFromString(const Str: String);
var
  List: TStringList;
begin
  //1:DreamCore
  List := TStringList.Create;
  List.LineBreak := ':';
  List.Text := Str;

  //Активность
  if List.Count > 0 then
    if not TryStrToBool(List.Strings[0], FChecked) then
      FChecked := False;

  //Значение
  if List.Count > 1 then
    FValue := List.Strings[1];

  List.Free;
end;


function TStartParamDirectoryListItemArray.IndexOfName(AName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FCount - 1 do
    if FList[i].FValue = AName then
      Exit(i);
end;


function TStartParamDirectoryListItemArray.GetCommandLine(Prefix: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FCount - 1 do
  begin
    if not FList[i].FChecked then
      Continue;

    if Result <> '' then
      Result := Result + ';';

    Result := Result + Prefix + FList[i].FValue;
  end;
end;


function TStartParamDirectoryListItemArray.ValueToString: String;
var
  i: Integer;
begin
  //1:DreamCore|0:DreamConfig
  Result := '';
  for i := 0 to FCount - 1 do
  begin
    Result := Result + FList[i].ValueToString;
    if i <> Fcount - 1 then
      Result := Result + '|';
  end;
end;


procedure TStartParamDirectoryListItemArray.ValueFromString(const Str: String);
var
  List, Line: TStringList;
  i, Index: Integer;
begin
  //1:DreamCore|0:DreamConfig
  List := TStringList.Create;
  List.LineBreak := '|';
  List.Text := Str;

  Line := TStringList.Create;
  LIne.LineBreak := ':';
  try

    for i := 0 to List.Count - 1 do
    begin
      //Разбить строку на части
      Line.Text := List.Strings[i];
      if Line.Count <> 2 then
        Continue;

      //Найти индекс Мода по имени
      Index := IndexOfName(Line.Strings[1]);

      //Если есть такой мод, то загрузить параметры
      if Index <> -1 then
        FList[Index].ValueFromString(List.Strings[i]);
    end;

  finally
    Line.Free;
    List.Free;
  end;
end;


function TStartParamDirectoryList.GetType: String;
begin
  Result := 'DirectoryList';
end;


procedure TStartParamDirectoryList.LoadModList(const ADirectory: String);

  procedure Add(Rec: TSearchRec);
  begin
    if (Rec.Attr and faDirectory) <> faDirectory then
      Exit;
    if (Rec.Name = '.') or (Rec.Name = '..') then
      Exit;
    DirectoryList.Add(TStartParamDirectoryListItem.Create(True, Rec.Name));
  end;

var
  o: TSearchRec;
begin
  FDirectoryList.Clear;

  if ADirectory = '' then
    Exit;

  if FindFirst(ADirectory + '\*', faAnyFile, o) = 0  then
    Add(o);
  while FindNext(o) = 0 do
    Add(o);
  FindClose(o);
end;


procedure TStartParamDirectoryList.SetValue(AValue: String);
begin
  FValue := AValue;

  LoadModList(FValue);
end;


constructor TStartParamDirectoryList.Create(const AName, APrefix, ADescription: String; AFullPath: Boolean; AValue: String);
begin
  inherited Create(AName, APrefix, ADescription);

  FDirectoryList := TStartParamDirectoryListItemArray.Create(True);

  FValue := AValue;
  FFullPath := AFullPath;
end;


destructor TStartParamDirectoryList.Destroy;
begin
  FDirectoryList.Free;
  inherited Destroy;
end;


function TStartParamDirectoryList.ValueToString: String;
begin
  Result := inherited ValueToString;
  Result := Result + ';' + BoolToStr(FFullPath, False);
  Result := Result + ';' + FValue;
  Result := Result + ';' + FDirectoryList.ValueToString;
end;


procedure TStartParamDirectoryList.ValueFromString(const Str: String);
var
  List: TStringList;
begin
  List := TStringList.Create;
  List.LineBreak := ';';
  List.Text := Str;

  //Активность, Полный путь, Каталог
  if List.Count > 2 then
  begin
    inherited ValueFromString(List.Strings[0]);
    FFullPath := StrToBool(List.Strings[1]);
    Value := List.Strings[2];
  end;

  //Значение
  if List.Count > 3 then
    FDirectoryList.ValueFromString(List.Strings[3]);

  List.Free;
end;


function TStartParamDirectoryList.GetCommandLine: String;
var
  APrefix: String;
begin
  if FFullPath then
    APrefix := IncludeTrailingBackslash(FValue)
  else
    APrefix := '';

  Result := '"' + inherited GetCommandLine + '=' + FDirectoryList.GetCommandLine(APrefix) + '"';
end;



end.

