unit TabParameters;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  Language, ObjectItemList, EventSystem;

type
  TTabParameters = class
    Language: TLanguage;        //Ссылка на текущий перевод
    EventSystem: TEventSystem;  //Ссылка на систему событий
    ItemBase: TObjectItemList;  //База предметов
    TabDirectory: String;       //Каталог закладок
    IconDirectory: String;      //Каталог иконок
    SettingsDirectory: String;  //Каталог хранения настроек
  end;

implementation



end.

