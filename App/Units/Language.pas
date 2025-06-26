unit Language;

{$mode ObjFPC}{$H+}

interface

uses
  Generics.Collections;

type
  TLanguage = class
  private
    const
      PARAM_APP_VERSION = 'Info.Version';
      PARAM_FILE_VERSION = 'Info.FileVersion';
      PARAM_AUTOR = 'Info.Autor';
      PARAM_DATE_CREATED = 'Info.DateCreated';
      PARAM_COMMENT = 'Info.Comment';
  private
    FAppVersion: String;
    FFileVersion: String;
    FAutor: String;
    FDateCreated: String;
    FComment: String;

  private
    type
      TStringMap = specialize TDictionary<String, String>;
    var
      FDictionary: TStringMap;

      procedure FromString(Str: String);
      function GetValue(Name: String; Default: String): String;
  public
    constructor Create;
    constructor Create(const FileName: String);
    destructor  Destroy; override;

    procedure LoadFromFile(const FileName: String);
    function  GetLocalizedString(const StringID: String; DefaultValue: String = ''): String;

    property AppVersion: String read FAppVersion write FAppVersion;
    property FileVersion: String read FFileVersion write FFileVersion;
    property Autor: String read FAutor write FAutor;
    property DateCreated: String read FDateCreated write FDateCreated;
    property Comment: String read FComment write FComment;
  end;


implementation

uses
  SysUtils, sgeStringList, sgeSystemUtils, sgeFileUtils, sgeFile;


procedure TLanguage.FromString(Str: String);
const
  //LineSeparator = #13#10;
  Separator     = '=';
  Commentary    = '#';
var
  List: TsgeStringList;
  i, c, j, k: Integer;
  S, sParam, sValue: String;
  IsName: Boolean;
  Symbol: Char;
begin
  //Очистить список
  FDictionary.Clear;

  //Разбить на строки
  List := TsgeStringList.Create;
  List.FromString(Str);

  //Обработать строки
  c := List.Count - 1;
  for i := 0 to c do
  begin
    S := sgeTrim(List.Part[i]);

    if S = '' then
      Continue;

    if S[1] = Commentary then
      Continue;


    //Подготовить переменные
    sParam := '';
    sValue := '';
    IsName := True;

    //Цикл по символам
    k := Length(S);
    for j := 1 to k do
    begin
      //Выделить символ
      Symbol := S[j];

      //Проверить на разделитель
      if (Symbol = Separator) and IsName then
      begin
        IsName := False;
        Continue;
      end;

      //Добавить символ
      case IsName of
        True:
          sParam := sParam + Symbol;

        False:
          sValue := sValue + Symbol;
      end;

    end;

    //Обработать параметр
    sParam := sgeTrim(sParam);
    if sParam = '' then
      Continue;

    sValue := sgeTrim(sValue);
    FDictionary.AddOrSetValue(sParam, sValue);
  end;

  List.Free;
end;


function TLanguage.GetValue(Name: String; Default: String): String;
begin
  if not FDictionary.TryGetValue(Name, Result) then
    Result := Default;
end;


constructor TLanguage.Create;
begin
  FDictionary := TStringMap.Create;
end;


constructor TLanguage.Create(const FileName: String);
begin
  Create;
  LoadFromFile(FileName);
end;


destructor TLanguage.Destroy;
begin
  FDictionary.Free;
end;


procedure TLanguage.LoadFromFile(const FileName: String);
var
  F: TsgeFile;
  S: String;
  Size: Integer;
begin
  if not FileExists(FileName) then
    raise Exception.Create(Format('Cant load file %s', [FileName]));

  //Прочитать файл в строку
  try
    try
      F := TsgeFile.Create(FileName, fmRead, False);
      Size := F.Size;
      SetLength(S, Size);
      F.Read(S[1], Size);
    except
      raise Exception.Create(Format('Cant read file %s', [FileName]));
    end;

  finally
    F.Free;
  end;

  //Преобразовать строку в параметры
  FromString(S);

  //Найти параметры
  FAppVersion := GetValue(PARAM_APP_VERSION, '');
  FFileVersion := GetValue(PARAM_FILE_VERSION, '');
  FAutor := GetValue(PARAM_AUTOR, '');
  FDateCreated := GetValue(PARAM_DATE_CREATED, '');
  FComment := GetValue(PARAM_COMMENT, '');
end;


function TLanguage.GetLocalizedString(const StringID: String; DefaultValue: String): String;
var
  s: String;
begin
  Result := DefaultValue;

  s := Trim(GetValue(StringID, DefaultValue));
  if s <> '' then
    Result := s;
end;



end.

