unit TabParameters;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  Language;

type
  TTabParameters = class
    Language: TLanguage;        //Ссылка на текущий перевод
    TabDirectory: String;       //Каталог закладок
    IconDirectory: String;      //Каталог иконок
    SettingsDirectory: String;  //Каталог хранения настроек

    constructor Create;
    constructor Create(ALanguage: TLanguage; ATabDirectory, AIconDirectory, ASettingsDirectory: String);

    procedure CopyFrom(AParameters: TTabParameters);
  end;

implementation


constructor TTabParameters.Create;
begin
  //Заглушка
end;


constructor TTabParameters.Create(ALanguage: TLanguage; ATabDirectory, AIconDirectory, ASettingsDirectory: String);
begin
  Language := ALanguage;
  TabDirectory := ATabDirectory;
  IconDirectory := AIconDirectory;
  SettingsDirectory := ASettingsDirectory;
end;


procedure TTabParameters.CopyFrom(AParameters: TTabParameters);
begin
  Language := AParameters.Language;
  TabDirectory := IncludeTrailingBackslash(AParameters.TabDirectory);
  IconDirectory := IncludeTrailingBackslash(AParameters.IconDirectory);
  SettingsDirectory := IncludeTrailingBackslash(AParameters.SettingsDirectory);
end;



end.

