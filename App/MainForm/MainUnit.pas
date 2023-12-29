unit MainUnit;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, Windows,
  ExtCtrls, ComCtrls,
  LaunchExecutableUnit;

const
  VERSION = '0.3';

type
  TMainForm = class(TForm)
    ilTray: TImageList;
    ilMainMenu: TImageList;
    ilTab: TImageList;
    ilTrayLaunch: TImageList;
    MainMenu: TMainMenu;
    miTrayStop: TMenuItem;
    miTrayLaunch: TMenuItem;
    miTraySeparator2: TMenuItem;
    miMainHide: TMenuItem;
    miMainSeparator1: TMenuItem;
    miMainExit: TMenuItem;
    miMainFIle: TMenuItem;
    miTrayShowHide: TMenuItem;
    miTraySeparator1: TMenuItem;
    miTrayExit: TMenuItem;
    PageControl: TPageControl;
    sbLaunch: TScrollBox;
    tabLaunch: TTabSheet;
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

    function  GetLaunchFrameList: TLaunchExecutableFrameList;
    procedure CreateLaunchFrame(const SettingsFile: String);
    procedure DestroyLaunchFrame;
    procedure CreateTrayMenuLaunchItems;

    procedure miTrayLaunchClick(Sender: TObject);
    procedure miTrayStopClick(Sender: TObject);
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
  Caption := 'DayZ Mod Tool  v' + VERSION;

  FMainDir := ExtractFilePath(ParamStr(0));
  FSettingsDir := FMainDir + 'Settings\';
  ForceDirectories(FSettingsDir);
  FLaunchDir := FMainDir + 'Tabs\Launch\';
  ForceDirectories(FLaunchDir);

  //Создать закладки запуска
  CreateLaunchFrame(FLaunchDir + 'List.ini');

  //Создать элементы запуска/останова приложений
  CreateTrayMenuLaunchItems;

  LoadFormSettings;
end;


procedure TMainForm.Done;
begin
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
  FrameCaption, FrameParams, FrameSettings, FrameIconName: String;
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
    for i := SectionList.Count - 1 downto 0 do
    begin
      //Прочитать параметры фрейма
      FrameCaption := SectionList.Strings[i];
      FrameParams := FLaunchDir + F.ReadString(FrameCaption, 'ParamFile', '');
      FrameSettings := FSettingsDir + F.ReadString(FrameCaption, 'SettingsFile', '');
      FrameIconName := FLaunchDir + F.ReadString(FrameCaption, 'Icon', '');

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
      Frame := TLaunchExecutableFrame.Create(FrameCaption, FrameIcon, FrameParams, FrameSettings);

      //Настроить выделение
      Frame.Higlight := not Odd(i);

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
  for i := c downto 0 do
  begin
    IconIndex := ilTrayLaunch.Count;
    ilTrayLaunch.AddIcon(FrameList[i].Icon);

    AddMenu(miTrayLaunch, FrameList[i], IconIndex, @miTrayLaunchClick);
    AddMenu(miTrayStop, FrameList[i], IconIndex, @miTrayStopClick);
  end;
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



end.

