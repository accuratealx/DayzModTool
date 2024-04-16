unit Language;

{$mode ObjFPC}{$H+}

interface

uses
  sgeSimpleParameters;

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

    FDictionary: TsgeSimpleParameters;
  public
    constructor Create;
    constructor Create(const FileName: String);
    destructor  Destroy; override;

    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);

    function GetLocalizedString(const StringID: String; DefaultValue: String = ''): String;

    property AppVersion: String read FAppVersion write FAppVersion;
    property FileVersion: String read FFileVersion write FFileVersion;
    property Autor: String read FAutor write FAutor;
    property DateCreated: String read FDateCreated write FDateCreated;
    property Comment: String read FComment write FComment;
  end;


implementation

uses
  SysUtils;


constructor TLanguage.Create;
begin
  FDictionary := TsgeSimpleParameters.Create;
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
begin
  //Прочитать из файла
  FDictionary.LoadFromFile(FileName);

  //Найти параметры
  FAppVersion := FDictionary.GetValue(PARAM_APP_VERSION, '');
  FFileVersion := FDictionary.GetValue(PARAM_FILE_VERSION, '');
  FAutor := FDictionary.GetValue(PARAM_AUTOR, '');
  FDateCreated := FDictionary.GetValue(PARAM_DATE_CREATED, '');
  FComment := FDictionary.GetValue(PARAM_COMMENT, '');
end;


procedure TLanguage.SaveToFile(const FileName: String);
begin
  //Обновить параметры
  FDictionary.SetValue(PARAM_APP_VERSION, FAppVersion);
  FDictionary.SetValue(PARAM_FILE_VERSION, FFileVersion);
  FDictionary.SetValue(PARAM_AUTOR, FAutor);
  FDictionary.SetValue(PARAM_DATE_CREATED, FDateCreated);
  FDictionary.SetValue(PARAM_COMMENT, FComment);

  //Записать в файл
  FDictionary.SaveToFile(FileName);
end;


function TLanguage.GetLocalizedString(const StringID: String; DefaultValue: String): String;
var
  s: String;
begin
  Result := DefaultValue;

  s := Trim(FDictionary.GetValue(StringID, DefaultValue));
  if s <> '' then
    Result := s;
end;



end.

