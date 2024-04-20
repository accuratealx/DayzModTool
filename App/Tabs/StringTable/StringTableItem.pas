unit StringTableItem;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Classes;

type
  //Типы языков
  TStringTableLanguageTypes = (
    ltEnglish,
    ltCzech,
    ltGerman,
    ltRussian,
    ltPolish,
    ltHungarian,
    ltItalian,
    ltSpanish,
    ltFrench,
    ltChinese,
    ltJapanese,
    ltPortuguese,
    ltChineseSimp
  );


const
  //Имена языков
  StringTableLanguageNames: array[TStringTableLanguageTypes] of string = (
    'Английский',
    'Чешский',
    'Немецкий',
    'Русский',
    'Польский',
    'Венгерский',
    'Итальянский',
    'Эспанский',
    'Французский',
    'Китайский',
    'Японский',
    'Португальский',
    'Китайский (упрощенный)'
  );


type
  //Таблица с переводами языков
  TStringTableLanguageTable = array[TStringTableLanguageTypes] of string;


  //Элемент для хранения одной языковой записи
  TStringTableItem = class
  private
    FID: String;
    FTable: TStringTableLanguageTable;

    procedure SetLocalizedText(Language: TStringTableLanguageTypes; Str: String);
    function  GetLocalizedText(Language: TStringTableLanguageTypes): String;
  public
    constructor Create;
    constructor Create(Str: String);

    function  ToString: String; reintroduce;
    procedure FromString(str: String);

    procedure ClearTable;
    procedure CopyFrom(Item: TStringTableItem);

    property ID: String read FID write FID;
    property LocalizedText[Language: TStringTableLanguageTypes]: String read GetLocalizedText write SetLocalizedText;
    property Table: TStringTableLanguageTable read FTable;
  end;


implementation

uses
  sgeSimpleCommand;


procedure TStringTableItem.SetLocalizedText(Language: TStringTableLanguageTypes; Str: String);
begin
  FTable[Language] := Str;
end;


function TStringTableItem.GetLocalizedText(Language: TStringTableLanguageTypes): String;
begin
  Result := FTable[Language];
end;


constructor TStringTableItem.Create;
begin
  //Заглушка
end;


constructor TStringTableItem.Create(Str: String);
begin
  FromString(Str);
end;

//'"Language","original",
// "english","czech","german","russian","polish","hungarian","italian","spanish","french","chinese","japanese","portuguese","chinesesimp",'
function TStringTableItem.ToString: String;
const
  STR_FORMAT = '"%s",';
var
  i: TStringTableLanguageTypes;
begin
  //Добавим ID строки
  Result := Format(STR_FORMAT, [FID]);

  //Добавим Original
  Result := Result + Format(STR_FORMAT, [FTable[ltEnglish]]);

  //Добавить таблицу
  for i := Low(TStringTableLanguageTypes) to High(TStringTableLanguageTypes) do
    Result := Result + Format(STR_FORMAT, [FTable[i]]);
end;


//'"Language","original",
// "english","czech","german","russian","polish","hungarian","italian","spanish","french","chinese","japanese","portuguese","chinesesimp",'
procedure TStringTableItem.FromString(str: String);
var
  Cmd: TsgeSimpleCommand;
  i: TStringTableLanguageTypes;
begin
  Cmd := TsgeSimpleCommand.Create(str, True, ',', '"');
  try
    //Прочитаем ID
    FID := Cmd.GetPartSafe(0, 'ERROR');

    for i := Low(TStringTableLanguageTypes) to High(TStringTableLanguageTypes) do
      FTable[i] := Cmd.GetPartSafe(Ord(i) + 2, '');

  finally
    Cmd.Free;
  end;
end;


procedure TStringTableItem.ClearTable;
var
  i: TStringTableLanguageTypes;
begin
  for i := Low(TStringTableLanguageTypes) to High(TStringTableLanguageTypes) do
    FTable[i] := '';
end;


procedure TStringTableItem.CopyFrom(Item: TStringTableItem);
begin
  FID := Item.FID;
  FTable := Item.FTable;
end;



end.

