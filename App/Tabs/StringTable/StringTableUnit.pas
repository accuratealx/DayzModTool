unit StringTableUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, IniFiles, Buttons,
  TabParameters, TabCommonUnit;

type
  TStringTableFrame = class(TTabCommonFrame)
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    btnDown: TSpeedButton;
    btnEdit: TSpeedButton;
    btnExplore: TSpeedButton;
    btnUp: TSpeedButton;
    pnlTools: TPanel;
    sbContent: TScrollBox;
  private
    const
      LANGUAGE_PREFIX = 'StringTable.';
  private
    FSettingsFile: String;

    procedure SaveSettings(const FileName: String);
    procedure LoadSettings(const FileName: String);

  public
    constructor Create(Parameters: TTabParameters); reintroduce;

    destructor  Destroy; override;

    procedure ApplyLanguage; override;

  end;


implementation

{$R *.lfm}

procedure TStringTableFrame.SaveSettings(const FileName: String);
var
  F: TIniFile;
begin
  F := TIniFile.Create(FSettingsFile);
  try

  finally
    F.Free;
  end;
end;


procedure TStringTableFrame.LoadSettings(const FileName: String);
var
  F: TIniFile;
begin
  F := TIniFile.Create(FSettingsFile);
  try

  finally
    F.Free;
  end;
end;


constructor TStringTableFrame.Create(Parameters: TTabParameters);
begin
  inherited Create(Parameters);

  //Определить файл настроек
  FSettingsFile := FParams.SettingsDirectory + '\StringTable.ini';

  LoadSettings(FSettingsFile);
end;


destructor TStringTableFrame.Destroy;
begin
  SaveSettings(FSettingsFile);

  inherited Destroy;
end;


procedure TStringTableFrame.ApplyLanguage;
begin
  //Пеервод
  //btnAdd.Caption := FLanguage.GetLocalizedString(PREFIX_TAB_DIRECTORY + 'Add', 'Добавить');
  //btnEdit.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_DIRECTORY + 'Edit', 'Изменить');
end;



end.

