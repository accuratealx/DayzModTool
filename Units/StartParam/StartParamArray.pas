unit StartParamArray;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  sgeTemplateCollection,
  StartParamSimple;

type
  TStartParamArray = class(specialize TsgeTemplateCollection<TStartParamSimple>)
  private

  public
    constructor Create;

    function GetCommandLine: String; virtual;

    function  ValueToString: String;
    procedure ValueFromString(const Str: String);

    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);

    property Count: Integer read FCount;
    property Item[Index: Integer]: TStartParamSimple read GetItem;
  end;


implementation

uses
  Classes,
  sgeSimpleContainer, sgeSimpleParameters,
  StartParamInteger, StartParamString, StartParamDirectory, StartParamFile, StartParamDirectoryList;


constructor TStartParamArray.Create;
begin
  inherited Create(True);
end;


function TStartParamArray.GetCommandLine: String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FCount - 1 do
  begin
    if not FList[i].Enable then
      Continue;

    if Result <> '' then
      Result := Result + ' ';
    Result := Result + FList[i].GetCommandLine;
  end;

end;


function TStartParamArray.ValueToString: String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FCount - 1 do
  begin
    Result := Result + FList[i].ValueToString;
    if i <> FCount - 1 then
      Result := Result + '[SEPARATOR]';
  end;
end;


procedure TStartParamArray.ValueFromString(const Str: String);
var
  List: TStringList;
  i: Integer;
begin
  List := TStringList.Create;
  List.LineBreak := '[SEPARATOR]';
  List.Text := Str;

  for i := 0 to FCount - 1 do
  begin
    FList[i].ValueFromString(List.Strings[i]);
  end;

  List.Free;
end;


procedure TStartParamArray.LoadFromFile(const FileName: String);
var
  Container: TsgeSimpleContainer;
  Section: TsgeSimpleParameters;
  SectionName, ParamName: String;
  AItem: TStartParamSimple;
  i: Integer;
begin
  Container := TsgeSimpleContainer.Create;
  Section := TsgeSimpleParameters.Create;
  try

    //Загрузить данные из файла
    Container.LoadFromFile(FileName);

    //Просмотреть секции
    for i := 0 to Container.Count - 1 do
    begin
      //Имя секции
      SectionName := Container.Section[i].Name;

      //Получить секцию
      Container.GetSectionParameters(SectionName, Section);

      //Получить тип параметра
      ParamName := Section.GetValue('Type', '');

      //Создать элемент в зависимости от типа
      case ParamName of
        'Simple':
        begin
          AItem := TStartParamSimple.Create(
            SectionName,
            Section.GetValue('Prefix', ''),
            Section.GetValue('Description', '')
          );
          Add(AItem);
        end;

        'Integer':
        begin
          AItem := TStartParamInteger.Create(
            SectionName,
            Section.GetValue('Prefix', ''),
            Section.GetValue('Description', ''),
            0,
            Section.GetValue('Default', 0)
          );
          Add(AItem);
        end;

        'String':
        begin
          AItem := TStartParamString.Create(
            SectionName,
            Section.GetValue('Prefix', ''),
            Section.GetValue('Description', ''),
            '',
            Section.GetValue('Default', '')
          );
          Add(AItem);
        end;

        'Directory':
        begin
          AItem := TStartParamDirectory.Create(
            SectionName,
            Section.GetValue('Prefix', ''),
            Section.GetValue('Description', ''),
            '',
            Section.GetValue('Default', '')
          );
          Add(AItem);
        end;

        'File':
        begin
          AItem := TStartParamFile.Create(
            SectionName,
            Section.GetValue('Prefix', ''),
            Section.GetValue('Description', ''),
            '',
            Section.GetValue('Default', '')
          );
          Add(AItem);
        end;

        'DirectoryList':
        begin
          AItem := TStartParamDirectoryList.Create(
            SectionName,
            Section.GetValue('Prefix', ''),
            Section.GetValue('Description', ''),
            True,
            ''
          );

          Add(AItem);
        end;
      end;

    end;

  finally
    Section.Free;
    Container.Free;
  end;
end;


procedure TStartParamArray.SaveToFile(const FileName: String);
begin

end;



end.

