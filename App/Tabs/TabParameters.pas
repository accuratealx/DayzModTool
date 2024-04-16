unit TabParameters;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  Language;

type
  TTabParameters = class
    Language: TLanguage;        //Ссылка на текущий перевод
    DataDirectory: String;      //Каталог данных
    SettingsDirectory: String;  //Каталог хранения настроек

    constructor Create;
    constructor Create(ALanguage: TLanguage; ADataDirectory, ASettingsDirectory: String);

    procedure CopyFrom(AParameters: TTabParameters);
  end;

implementation


constructor TTabParameters.Create;
begin
  //Заглушка
end;


constructor TTabParameters.Create(ALanguage: TLanguage; ADataDirectory, ASettingsDirectory: String);
begin
  Language := ALanguage;
  DataDirectory := ADataDirectory;
  SettingsDirectory := ASettingsDirectory;
end;


procedure TTabParameters.CopyFrom(AParameters: TTabParameters);
begin
  Language := AParameters.Language;
  DataDirectory := IncludeTrailingBackslash(AParameters.DataDirectory);
  SettingsDirectory := IncludeTrailingBackslash(AParameters.SettingsDirectory);
end;



end.

