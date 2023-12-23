unit MainUnit;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Windows,
  ExtCtrls, ComCtrls,
  LaunchExecutableUnit;

const
  VERSION = '0.1';

type
  TMainForm = class(TForm)
    ilTray: TImageList;
    ilMainMenu: TImageList;
    MainMenu: TMainMenu;
    miMainHide: TMenuItem;
    miMainSeparator1: TMenuItem;
    miMainExit: TMenuItem;
    miMainFIle: TMenuItem;
    miTrayShowHide: TMenuItem;
    miTraySeparator1: TMenuItem;
    miTrayExit: TMenuItem;
    PageControl: TPageControl;
    tabLaunchClient: TTabSheet;
    tabDedicatedServer: TTabSheet;
    TrayMenu: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miMainHideClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miTrayShowHideClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure TrayMenuPopup(Sender: TObject);
  private
    FCloseApplication: Boolean;

    //Каталоги
    FMainDir: String;
    FSettingsDir: String;

    //Фреймы
    FDedicatedServerFrame: TLaunchExecutableFrame;

    procedure Init;
    procedure Done;
    procedure SaveFormSettings;
    procedure LoadFormSettings;
    procedure SaveSettings;
    procedure LoadSettings;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  IniFiles;

const
  CONFIG_FILE_NAME = 'DayZConfig.ini';
  CONFIG_SECTION_MAIN_FORM = 'MainForm';


procedure TMainForm.FormCreate(Sender: TObject);
begin
  Init;
end;


procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FCloseApplication then
    CloseAction := caFree
  else
    CloseAction := caHide;
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Done;
end;


procedure TMainForm.miMainHideClick(Sender: TObject);
begin
  Close;
end;


procedure TMainForm.miExitClick(Sender: TObject);
begin
  FCloseApplication := True;
  Close;
end;


procedure TMainForm.miTrayShowHideClick(Sender: TObject);
begin
  if MainForm.Visible then
    Hide
  else
    Show;
end;


procedure TMainForm.TrayIconDblClick(Sender: TObject);
begin
  miTrayShowHide.Click;
end;


procedure TMainForm.TrayMenuPopup(Sender: TObject);
begin
  if MainForm.Visible then
  begin
    miTrayShowHide.Caption := 'Скрыть';
    miTrayShowHide.ImageIndex := 1;
  end
  else
  begin
    miTrayShowHide.Caption := 'Показать';
    miTrayShowHide.ImageIndex := 2;
  end;
end;


procedure TMainForm.Init;
begin
  Caption := 'DayZ Mod Tool (v' + VERSION + ')';

  FMainDir := ExtractFilePath(ParamStr(0));
  FSettingsDir := FMainDir + '\Settings\';
  ForceDirectories(FSettingsDir);
  LoadSettings;

  //Запуск сервера
  FDedicatedServerFrame := TLaunchExecutableFrame.Create(
    FMainDir + 'StartParameters\DedicatedServer.cfg',
    FSettingsDir + 'DedicatedServer.ini'
  );
  FDedicatedServerFrame.Parent := tabDedicatedServer;
end;


procedure TMainForm.Done;
begin
  FDedicatedServerFrame.Free;

  SaveSettings;
end;


procedure TMainForm.SaveFormSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FMainDir + CONFIG_FILE_NAME);
  try

    Ini.WriteInteger(CONFIG_SECTION_MAIN_FORM, 'Top', Self.Top);
    Ini.WriteInteger(CONFIG_SECTION_MAIN_FORM, 'Left', Self.Left);
    Ini.WriteInteger(CONFIG_SECTION_MAIN_FORM, 'Width', Self.Width);
    Ini.WriteInteger(CONFIG_SECTION_MAIN_FORM, 'Height', Self.Height);

    Ini.WriteBool(CONFIG_SECTION_MAIN_FORM, 'Visible', IsWindowVisible(Handle));

  finally
    Ini.Free;
  end;
end;


procedure TMainForm.LoadFormSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FMainDir + CONFIG_FILE_NAME);
  try

    Self.Top := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, 'Top', 100);
    Self.Left := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, 'Left', 100);
    Self.Width := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, 'Width', 300);
    Self.Height := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, 'Height', 300);

    Application.ShowMainForm := Ini.ReadBool(CONFIG_SECTION_MAIN_FORM, 'Visible', True);

  finally
    Ini.Free;
  end;
end;


procedure TMainForm.SaveSettings;
begin
  SaveFormSettings;
end;


procedure TMainForm.LoadSettings;
begin
  LoadFormSettings;
end;



end.

