unit BuilderUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  TabParameters, TabCommonUnit;

type
  TBuilderFrame = class(TTabCommonFrame)
  private
    procedure SaveSettings(const FileName: String);
    procedure LoadSettings(const FileName: String);

  public
    constructor Create(Parameters: TTabParameters); reintroduce;
    destructor  Destroy; override;

    procedure ApplyLanguage; override;
    procedure SaveSettings;
    procedure LoadSettings;
  end;


implementation

{$R *.lfm}


procedure TBuilderFrame.SaveSettings(const FileName: String);
begin

end;


procedure TBuilderFrame.LoadSettings(const FileName: String);
begin

end;


constructor TBuilderFrame.Create(Parameters: TTabParameters);
begin
  inherited Create(Parameters);

  //Загрузить настройки
  //LoadSettings(FSettingsFile);
end;


destructor TBuilderFrame.Destroy;
begin
  //Сохранить настройки
  //SaveSettings(FSettingsFile);

  inherited Destroy;
end;


procedure TBuilderFrame.ApplyLanguage;
begin

end;


procedure TBuilderFrame.SaveSettings;
begin
  //SaveSettings();
end;


procedure TBuilderFrame.LoadSettings;
begin
  //LoadSettings();
end;



end.

