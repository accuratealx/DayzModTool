unit TabParameters;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  Language, ObjectItemList;

type
  TTabParameters = class
    Language: TLanguage;        //Ссылка на текущий перевод
    ItemBase: TObjectItemList;  //База предметов
    TabDirectory: String;       //Каталог закладок
    IconDirectory: String;      //Каталог иконок
    SettingsDirectory: String;  //Каталог хранения настроек
  end;

implementation



end.

