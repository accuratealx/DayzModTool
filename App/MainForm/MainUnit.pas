unit MainUnit;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Windows,
  ExtCtrls, ComCtrls,
  LaunchExecutableUnit, DirectoryUnit, DirectoryItemUnit, YesNoQuestionDialogUnit;

const
  VERSION = '0.5';

type
  TMainForm = class(TForm)
    ilTray: TImageList;
    ilMainMenu: TImageList;
    ilTab: TImageList;
    ilTrayLaunch: TImageList;
    ilTrayDirectory: TImageList;
    MainMenu: TMainMenu;
    miMainTabDirectoryEraseIncorrect: TMenuItem;
    miMainTabDirectory: TMenuItem;
    miTrayDirectory: TMenuItem;
    miMainTabLaunchFindExecutables: TMenuItem;
    miMainSeparator2: TMenuItem;
    miMainTabLaunchCollapseAll: TMenuItem;
    miMainTabLaunchExpandAll: TMenuItem;
    miMainTabLaunch: TMenuItem;
    miMainTabs: TMenuItem;
    miTrayStop: TMenuItem;
    miTrayLaunch: TMenuItem;
    miTraySeparator2: TMenuItem;
    miMainHide: TMenuItem;
    miMainSeparator1: TMenuItem;
    miMainExit: TMenuItem;
    miMainSystem: TMenuItem;
    miTrayShowHide: TMenuItem;
    miTraySeparator1: TMenuItem;
    miTrayExit: TMenuItem;
    PageControl: TPageControl;
    sbLaunch: TScrollBox;
    tabLaunch: TTabSheet;
    tabDirectory: TTabSheet;
    TrayMenu: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miMainHideClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miMainTabDirectoryEraseIncorrectClick(Sender: TObject);
    procedure miMainTabLaunchCollapseAllClick(Sender: TObject);
    procedure miMainTabLaunchExpandAllClick(Sender: TObject);
    procedure miMainTabLaunchFindExecutablesClick(Sender: TObject);
    procedure miTrayShowHideClick(Sender: TObject);
    procedure sbLaunchResize(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure TrayMenuPopup(Sender: TObject);
  private
    FCloseApplication: Boolean;

    //Каталоги
    FMainDir: String;
    FSettingsDir: String;
    FLaunchDir: String;
    FDirectoryDir: String;

    //Закладки
    FDirectoryFrame: TDirectoryFrame;

    procedure Init;
    procedure Done;
    procedure SaveFormSettings;
    procedure LoadFormSettings;

    function  GetLaunchFrameList: TLaunchExecutableFrameList;
    procedure CreateLaunchFrame(const SettingsFile: String);
    procedure DestroyLaunchFrame;
    procedure ArrangeLaunchTabItems;
    procedure FindExecutables;
    procedure SetLaunchFrameCollapsed(ACollapsed: Boolean);
    procedure OnChangeLaunchContentHeight(Sender: TObject);
    procedure DeleteIncorrectDirectory;

    procedure CreateTrayMenuLaunchItems;
    procedure CorrectTrayMenuLaunchItems;
    procedure miTrayLaunchClick(Sender: TObject);
    procedure miTrayStopClick(Sender: TObject);

    procedure CreateTrayMenuDirectoryItems;
    procedure DestroyTrayMenuDirectoryItems;
    procedure miTrayDirectoryClick(Sender: TObject);
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


procedure TMainForm.miMainTabDirectoryEraseIncorrectClick(Sender: TObject);
begin
  DeleteIncorrectDirectory;
end;


procedure TMainForm.miMainTabLaunchCollapseAllClick(Sender: TObject);
begin
  SetLaunchFrameCollapsed(True);
end;


procedure TMainForm.miMainTabLaunchExpandAllClick(Sender: TObject);
begin
  SetLaunchFrameCollapsed(False);
end;


procedure TMainForm.miMainTabLaunchFindExecutablesClick(Sender: TObject);
begin
  FindExecutables;
end;


procedure TMainForm.miTrayShowHideClick(Sender: TObject);
begin
  if MainForm.Visible then
    Hide
  else
    Show;
end;


procedure TMainForm.sbLaunchResize(Sender: TObject);
begin
  ArrangeLaunchTabItems;
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

  //Поправить кнопки запуска / останова
  CorrectTrayMenuLaunchItems;

  //Подготовить кнопки открытия каталогов
  CreateTrayMenuDirectoryItems;
end;


procedure TMainForm.Init;
begin
  Caption := 'DayZ Mod Tool  v' + VERSION;

  TrayIcon.Hint := Caption;

  FMainDir := ExtractFilePath(ParamStr(0));
  FSettingsDir := FMainDir + 'Settings\';
  ForceDirectories(FSettingsDir);
  FLaunchDir := FMainDir + 'Tabs\Launch\';
  ForceDirectories(FLaunchDir);
  FDirectoryDir := FMainDir + 'Tabs\Directory\';
  ForceDirectories(FDirectoryDir);

  //Создать закладки запуска
  CreateLaunchFrame(FLaunchDir + 'List.ini');

  //Закладка с каталогами
  FDirectoryFrame := TDirectoryFrame.Create(FSettingsDir + 'Directory.ini', FDirectoryDir);
  FDirectoryFrame.Parent := tabDirectory;

  //Создать элементы запуска/останова приложений
  CreateTrayMenuLaunchItems;

  LoadFormSettings;
end;


procedure TMainForm.Done;
begin
  FDirectoryFrame.Free;

  DestroyLaunchFrame;

  SaveFormSettings;
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
    Self.Left := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_LEFT, 300);
    Self.Width := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_WIDTH, Self.Width);
    Self.Height := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_HEIGHT, Self.Height);
    Application.ShowMainForm := Ini.ReadBool(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_VISIBLE, True);
    PageControl.TabIndex := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_TAB_INDEX, 0);

  finally
    Ini.Free;
  end;
end;


function TMainForm.GetLaunchFrameList: TLaunchExecutableFrameList;
var
  i, c: Integer;
begin
  Result := nil;

  //Просмотрим все закладки
  for i := 0 to sbLaunch.ControlCount - 1 do
  begin
    if sbLaunch.Controls[i] is TLaunchExecutableFrame then
    begin
      c := Length(Result);
      SetLength(Result, c + 1);
      Result[c] := sbLaunch.Controls[i] as TLaunchExecutableFrame;
    end;
  end;
end;


procedure TMainForm.CreateLaunchFrame(const SettingsFile: String);
var
  F: TIniFile;
  SectionList: TStringList;
  i: Integer;
  FrameCaption, FrameParams, FrameSettings, FrameIconName, FrameRelativeFilename: String;
  Frame: TLaunchExecutableFrame;
  FrameIcon: TIcon;
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
      FrameIconName := FLaunchDir + F.ReadString(FrameCaption, 'Icon', '');
      FrameRelativeFilename := F.ReadString(FrameCaption, 'RelativeFileName', '');

      //Если нет файла с настройками параметров, то пропуск
      if not FileExists(FrameParams) then
        Continue;

      //Грузим иконку
      try
        if FileExists(FrameIconName) then
          FrameIcon.LoadFromFile(FrameIconName)
        else
          FrameIcon.Assign(Application.Icon);
      except
        FrameIcon.Assign(Icon);
      end;

      //Создать фрейм
      Frame := TLaunchExecutableFrame.Create(FrameCaption, FrameIcon, FrameParams, FrameSettings, FrameRelativeFilename);

      //Настроить выделение
      Frame.Higlight := not Odd(i);

      //Установить обработчик изменения высоты
      Frame.OnHeightChange := @OnChangeLaunchContentHeight;

      //Прикрепить фрейм к закладке
      Frame.Parent := sbLaunch;
    end;

  finally
    FrameIcon.Free;
    SectionList.Free;
    F.Free;
  end;
end;


procedure TMainForm.DestroyLaunchFrame;
var
  i: Integer;
begin
  for i := sbLaunch.ControlCount - 1 downto 0 do
  begin
    if sbLaunch.Controls[i] is TLaunchExecutableFrame then
      (sbLaunch.Controls[i] as TLaunchExecutableFrame).Free;
  end;
end;


procedure TMainForm.ArrangeLaunchTabItems;
var
  i, Y: Integer;
  Frame: TLaunchExecutableFrame;
begin
  Y := 0;

  for i := 0 to sbLaunch.ControlCount - 1 do
  begin
    if not (sbLaunch.Controls[i] is TLaunchExecutableFrame) then
      Continue;

    Frame := sbLaunch.Controls[i] as TLaunchExecutableFrame;
    Frame.Left := 0;
    Frame.Width := sbLaunch.ClientWidth;
    Frame.Top := Y;

    Inc(Y, Frame.Height);
  end;
end;


procedure TMainForm.FindExecutables;
var
  FrameList: TLaunchExecutableFrameList;
  i: Integer;
begin
  FrameList := GetLaunchFrameList;

  //В каждом фрейме вызвать поиск приложения
  for i := 0 to Length(FrameList) - 1 do
    FrameList[i].FindExecutable;
end;


procedure TMainForm.SetLaunchFrameCollapsed(ACollapsed: Boolean);
var
  FrameList: TLaunchExecutableFrameList;
  i: Integer;
begin
  FrameList := GetLaunchFrameList;
  for i := Length(FrameList) - 1 downto 0 do
    FrameList[i].Collapsed := ACollapsed;
end;


procedure TMainForm.OnChangeLaunchContentHeight(Sender: TObject);
begin
  ArrangeLaunchTabItems;
end;


procedure TMainForm.DeleteIncorrectDirectory;
begin
  if YesNoQuestionDialogExecute('Вопрос', 'Удалить несуществующие каталоги?') then
    FDirectoryFrame.DeleteIncorrectDirectory;
end;


procedure TMainForm.CreateTrayMenuLaunchItems;

  procedure AddMenu(RootMenu: TMenuItem; Frame: TLaunchExecutableFrame; IconIndex: Integer; Proc: TNotifyEvent);
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(RootMenu);
    MenuItem.Caption := Frame.Caption;
    MenuItem.ImageIndex := IconIndex;
    MenuItem.OnClick := Proc;
    MenuItem.Tag := PtrUInt(Frame);

    RootMenu.Add(MenuItem);
  end;

var
  FrameList: TLaunchExecutableFrameList;
  c, i, IconIndex: Integer;
begin
  FrameList := GetLaunchFrameList;

  c := Length(FrameList) - 1;
  for i := 0 to c do
  begin
    IconIndex := ilTrayLaunch.Count;
    ilTrayLaunch.AddIcon(FrameList[i].Icon);

    AddMenu(miTrayLaunch, FrameList[i], IconIndex, @miTrayLaunchClick);
    AddMenu(miTrayStop, FrameList[i], IconIndex, @miTrayStopClick);
  end;
end;


procedure TMainForm.CorrectTrayMenuLaunchItems;

  procedure SetMenuItemsEnabled(Root: TMenuItem);
  var
    Frame: TLaunchExecutableFrame;
    i: Integer;
  begin
    for i := 0 to Root.Count - 1 do
    begin
      Frame := TLaunchExecutableFrame(Root.Items[i].Tag);
      Root.Items[i].Enabled := Frame.ExecatableEnable;
    end;
  end;

begin
  SetMenuItemsEnabled(miTrayLaunch);
  SetMenuItemsEnabled(miTrayStop);
end;


procedure TMainForm.miTrayLaunchClick(Sender: TObject);
var
  Frame: TLaunchExecutableFrame;
begin
  if not (Sender is TMenuItem) then
    Exit;

  Frame := TLaunchExecutableFrame((Sender as TMenuItem).Tag);
  Frame.Launch;
end;


procedure TMainForm.miTrayStopClick(Sender: TObject);
var
  Frame: TLaunchExecutableFrame;
begin
  if not (Sender is TMenuItem) then
    Exit;

  Frame := TLaunchExecutableFrame((Sender as TMenuItem).Tag);
  Frame.Stop;
end;


procedure TMainForm.CreateTrayMenuDirectoryItems;

  procedure AddMenu(RootMenu: TMenuItem; Frame: TDirectoryItemFrame; IconIndex: Integer);
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(RootMenu);
    MenuItem.Caption := Frame.Caption;
    MenuItem.Enabled := Frame.IsPathCorrect;
    MenuItem.ImageIndex := IconIndex;
    MenuItem.OnClick := @miTrayDirectoryClick;
    MenuItem.Tag := PtrUInt(Frame);

    RootMenu.Add(MenuItem);
  end;

var
  i: Integer;
begin
  DestroyTrayMenuDirectoryItems;
  ilTrayDirectory.Clear;

  for i := 0 to Length(FDirectoryFrame.Frames) - 1 do
  begin
    ilTrayDirectory.AddIcon(FDirectoryFrame.Frames[i].Icon);
    AddMenu(miTrayDirectory, FDirectoryFrame.Frames[i], i);
  end;
end;


procedure TMainForm.DestroyTrayMenuDirectoryItems;
begin
  miTrayDirectory.Clear;
end;


procedure TMainForm.miTrayDirectoryClick(Sender: TObject);
var
  Frame: TDirectoryItemFrame;
begin
  if not (Sender is TMenuItem) then
    Exit;

  Frame := TDirectoryItemFrame((Sender as TMenuItem).Tag);
  Frame.OpenDirectory;
end;



end.

