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
    ilTab: TImageList;
    MainMenu: TMainMenu;
    miMainHide: TMenuItem;
    miMainSeparator1: TMenuItem;
    miMainExit: TMenuItem;
    miMainFIle: TMenuItem;
    miTrayShowHide: TMenuItem;
    miTraySeparator1: TMenuItem;
    miTrayExit: TMenuItem;
    PageControl: TPageControl;
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
    FLaunchDir: String;

    procedure Init;
    procedure Done;
    procedure SaveFormSettings;
    procedure LoadFormSettings;
    procedure SaveSettings;
    procedure LoadSettings;

    procedure CreateLaunchFrames(const SettingsFile: String);
    procedure DestroyFrames;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  IniFiles,
  TabCommonUnit;

const
  CONFIG_FILE_NAME = 'DayZConfig.ini';
  CONFIG_SECTION_MAIN_FORM = 'MainForm';
  CONFIG_PARAM_LEFT = 'Left';
  CONFIG_PARAM_TOP = 'Top';
  CONFIG_PARAM_WIDTH = 'Width';
  CONFIG_PARAM_HEIGHT = 'Height';
  CONFIG_PARAM_VISIBLE = 'Visible';
  CONFIG_PARAM_TAB_INDEX = 'TabIndex';


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
  FSettingsDir := FMainDir + 'Settings\';
  ForceDirectories(FSettingsDir);
  FLaunchDir := FMainDir + 'Tabs\Launch\';
  ForceDirectories(FLaunchDir);

  CreateLaunchFrames(FLaunchDir + 'List.ini');

  LoadSettings;
end;


procedure TMainForm.Done;
begin
  DestroyFrames;

  SaveSettings;
end;


procedure TMainForm.SaveFormSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FSettingsDir + CONFIG_FILE_NAME);
  try

    Ini.WriteInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_TOP, Self.Top);
    Ini.WriteInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_LEFT, Self.Left);
    Ini.WriteInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_WIDTH, Self.Width);
    Ini.WriteInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_HEIGHT, Self.Height);

    Ini.WriteBool(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_VISIBLE, IsWindowVisible(Handle));

    Ini.WriteInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_TAB_INDEX, PageControl.TabIndex);

  finally
    Ini.Free;
  end;
end;


procedure TMainForm.LoadFormSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FSettingsDir + CONFIG_FILE_NAME);
  try

    Self.Top := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_TOP, 100);
    Self.Left := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_LEFT, 100);
    Self.Width := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_WIDTH, 300);
    Self.Height := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_HEIGHT, 300);

    Application.ShowMainForm := Ini.ReadBool(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_VISIBLE, True);

    PageControl.TabIndex := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_TAB_INDEX, -1);

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


procedure TMainForm.CreateLaunchFrames(const SettingsFile: String);
var
  F: TIniFile;
  SectionList: TStringList;
  i: Integer;
  FrameCaption, FrameParams, FrameSettings, FrameName, FrameIconName: String;
  Frame: TLaunchExecutableFrame;
  Tab: TTabSheet;
  FrameIcon: TIcon;
  IconIndex: Integer;
begin
  F := TIniFile.Create(SettingsFile);
  SectionList := TStringList.Create;
  FrameIcon := TIcon.Create;
  try

    //Прочитать список секций
    F.ReadSections(SectionList);

    //Создать фреймы запуска
    for i := 0 to SectionList.Count - 1 do
    begin
      //Прочитать параметры фрейма
      FrameCaption := SectionList.Strings[i];
      FrameParams := FLaunchDir + F.ReadString(FrameCaption, 'ParamFile', '');
      FrameSettings := FSettingsDir + F.ReadString(FrameCaption, 'SettingsFile', '');
      FrameName := F.ReadString(FrameCaption, 'InnerName', '');
      FrameIconName := FLaunchDir + F.ReadString(FrameCaption, 'Icon', '');

      //Если нет файла с настройками параметров, то пропуск
      if not FileExists(FrameParams) then
        Continue;

      //Грузим иконку
      try
        if FileExists(FrameIconName) then
          FrameIcon.LoadFromFile(FrameIconName)
        else
          FrameIcon.Assign(Icon);
      except
        FrameIcon.Assign(Icon);
      end;

      //Добавим в список иконок
      IconIndex := ilTab.Count;
      ilTab.AddIcon(FrameIcon);

      //Создать фрейм
      Frame := TLaunchExecutableFrame.Create(FrameParams, FrameSettings);

      //Создать закладку
      Tab := TTabSheet.Create(PageControl);
      Tab.Name := FrameName;
      Tab.Caption := FrameCaption;
      Tab.ImageIndex := IconIndex;
      Tab.Parent := PageControl;

      //Прикрепить фрейм к закладке
      Frame.Parent := Tab;
    end;

  finally
    FrameIcon.Free;
    SectionList.Free;
    F.Free;
  end;
end;


procedure TMainForm.DestroyFrames;
var
  i: Integer;
  Tab: TTabSheet;
begin
  //Просмотрим все закладки
  for i := 0 to PageControl.ControlCount - 1 do
  begin
    //Пропуск компонентов не Tab
    if not (PageControl.Controls[i] is TTabSheet) then
      Continue;

    //Ссылка на закладку
    Tab := PageControl.Controls[i] as TTabSheet;

    //Найти первый элемент. Всегда должен быть только один
    if Tab.ControlCount = 1 then
    begin
      if Tab.Controls[0] is TTabCommonFrame then
        Tab.Controls[0].Free;
    end;
  end;
end;



end.

