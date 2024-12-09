unit DialogParameters;

{$mode ObjFPC}{$H+}

interface

uses
  Language, EventSystem;

type
  TDialogParameters = class
    Language: TLanguage;        //Ссылка на перевод
    EventSystem: TEventSystem;  //Ссылка на систему событий
  end;

implementation

end.

