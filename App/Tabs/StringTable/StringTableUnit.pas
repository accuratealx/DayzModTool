unit StringTableUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, IniFiles, Buttons,
  Language;

type
  TStringTableFrame = class(TFrame)
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
      PREFIX_TAB_STRING_TABLE = 'StringTable.';
  private
    FSettingsFile: String;
    FLanguage: TLanguage;

    procedure SaveSettings(const FileName: String);
    procedure LoadSettings(const FileName: String);

  public
    constructor Create(const SettingsFile: String); reintroduce;
    destructor  Destroy; override;

    procedure ChangeLanguage(Language: TLanguage);

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


constructor TStringTableFrame.Create(const SettingsFile: String);
begin
  inherited Create(nil);

  FSettingsFile := SettingsFile;

  LoadSettings(FSettingsFile);
end;


destructor TStringTableFrame.Destroy;
begin
  SaveSettings(FSettingsFile);

  inherited Destroy;
end;


procedure TStringTableFrame.ChangeLanguage(Language: TLanguage);
begin
  FLanguage := Language;

  //Пеервод
  //btnAdd.Caption := FLanguage.GetLocalizedString(PREFIX_TAB_DIRECTORY + 'Add', 'Добавить');
  //btnEdit.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_DIRECTORY + 'Edit', 'Изменить');
end;



end.

