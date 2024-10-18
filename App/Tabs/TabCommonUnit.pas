unit TabCommonUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  TabParameters;

type
  TTabCommonFrame = class(TFrame)
  protected
    FParams: TTabParameters;  //Ссылка на параметры для закладок

  public
    constructor Create(Parameters: TTabParameters; AParent: TWinControl); reintroduce;

    procedure ApplyLanguage; virtual;
    procedure SaveSettings; virtual;
    procedure LoadSettings; virtual;
  end;


implementation

{$R *.lfm}


constructor TTabCommonFrame.Create(Parameters: TTabParameters; AParent: TWinControl);
begin
  inherited Create(nil);
  Parent := AParent;

  //Подготовить параметры
  FParams := Parameters;
end;


procedure TTabCommonFrame.ApplyLanguage;
begin
  //Переопределяется в потомках
end;


procedure TTabCommonFrame.SaveSettings;
begin
  //Переопределяется в потомках
end;


procedure TTabCommonFrame.LoadSettings;
begin
  //Переопределяется в потомках
end;



end.

