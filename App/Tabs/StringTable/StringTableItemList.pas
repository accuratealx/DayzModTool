unit StringTableItemList;

{$mode ObjFPC}{$H+}

interface

uses
  sgeTemplateCollection,
  StringTableItem;

type
  TStringTableItemList = class(specialize TsgeTemplateCollection<TStringTableItem>)
  public
    constructor Create; reintroduce;

    procedure LoadFromFile(FileName: String);
    procedure SaveToFile(FileName: String);
  end;


implementation

uses
  Classes, SysUtils;


constructor TStringTableItemList.Create;
begin
  inherited Create(True);
end;


procedure TStringTableItemList.LoadFromFile(FileName: String);
var
  List: TStringList;
  i: Integer;
  s: String;
  tItem: TStringTableItem;
begin
  List := TStringList.Create;
  try
    //Загрузить из файла
    List.LoadFromFile(FileName);

    //Почистить таблицу
    Clear;

    //Пропуск первой строки заголовка
    for i := 1 to List.Count - 1 do
    begin
      s := Trim(List.Strings[i]);

      //Пропуск пустых строк
      if s = '' then
        Continue;

      //Добавить элемент
      tItem := TStringTableItem.Create(s);
      Add(tItem);
    end;

  finally
    List.Free;
  end;
end;


procedure TStringTableItemList.SaveToFile(FileName: String);
const
  FILE_HEADER = '"Language","original","english","czech","german","russian","polish","hungarian","italian","spanish","french","chinese","japanese","portuguese","chinesesimp",';
var
  List: TStringList;
  i: Integer;
begin
  List := TStringList.Create;
  try
    //Добавить заголовок
    List.Add(FILE_HEADER);

    for i := 0 to FCount - 1 do
      List.Add(FList[i].ToString);

    List.SaveToFile(FileName);

  finally
    List.Free;
  end;
end;



end.

