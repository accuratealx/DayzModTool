unit MainUnit;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, windows, IniFiles, LCLIntf,
  sgeStringList, EventSystem, GitVersion,
  Language, TabParameters, TabCommonUnit,
  LaunchUnit, LaunchItemUnit, DirectoryUnit, DirectoryItemUnit, StringTableUnit, BuilderUnit,
  WorkDriveUnit, TrashCleanerUnit, ItemBaseUnit, NoteUnit;

const
  VERSION = '0.14';

type
  TMainForm = class(TForm)
    ilTray: TImageList;
    ilTab: TImageList;
    ilTrayLaunch: TImageList;
    ilTrayDirectory: TImageList;
    ilLanguages: TImageList;
    MainMenu: TMainMenu;
    miMainTabBuilderEraseAll: TMenuItem;
    miMainTabBuilderSeparator2: TMenuItem;
    miMainTabStringTableSort: TMenuItem;
    miMainTabDirectorySort: TMenuItem;
    miMainTabBuilderSort: TMenuItem;
    miMainTabBuilderSeparator1: TMenuItem;
    miMainInfoCheckVersion:TMenuItem;
    miMainTabItemBaseFitColomns: TMenuItem;
    miMainTabItemBaseSort: TMenuItem;
    miMainTabItemBase: TMenuItem;
    miMainToolsOpenSettings: TMenuItem;
    miMainToolsImportSettings: TMenuItem;
    miMainToolsExportSettings: TMenuItem;
    miMainToolsGroup: TMenuItem;
    miMainTabBuilderCollapseAll: TMenuItem;
    miMainTabBuilderExpandAll: TMenuItem;
    miMainTabBuilder: TMenuItem;
    miMainToolsTimeCalculator: TMenuItem;
    miMainToolsTrashCleaner: TMenuItem;
    miTraySeparator3: TMenuItem;
    miTrayDonate: TMenuItem;
    miMainToolsSeparator1: TMenuItem;
    miMainTabStringTableFitColumns: TMenuItem;
    miMainTabStringTable: TMenuItem;
    miMainInfoDonate: TMenuItem;
    miMainInfo: TMenuItem;
    miMainToolsExtractData: TMenuItem;
    miMainToolsWorkDrive: TMenuItem;
    miMainTools: TMenuItem;
    miMainLanguage: TMenuItem;
    miTrayHide: TMenuItem;
    miMainTabDirectorySeparator1: TMenuItem;
    miMainTabDirectoryEraseAll: TMenuItem;
    miMainTabDirectoryAddDefault: TMenuItem;
    miMainTabDirectoryEraseIncorrect: TMenuItem;
    miMainTabDirectory: TMenuItem;
    miTrayDirectory: TMenuItem;
    miMainTabLaunchFindExecutables: TMenuItem;
    miMainTabLaunchSeparator1: TMenuItem;
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
    miTrayShow: TMenuItem;
    miTraySeparator1: TMenuItem;
    miTrayExit: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    SaveDialog: TSaveDialog;
    miMainToolsGroupSeparator1: TMenuItem;
    tabLaunch: TTabSheet;
    tabDirectory: TTabSheet;
    tabBuilder: TTabSheet;
    tabItemBase: TTabSheet;
    tabNote: TTabSheet;
    tabStringTable: TTabSheet;
    TrayMenu: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miMainHideClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miMainInfoCheckVersionClick(Sender:TObject);
    procedure miMainInfoDonateClick(Sender: TObject);
    procedure miMainTabBuilderCollapseAllClick(Sender: TObject);
    procedure miMainTabBuilderEraseAllClick(Sender: TObject);
    procedure miMainTabBuilderExpandAllClick(Sender: TObject);
    procedure miMainTabBuilderSortClick(Sender: TObject);
    procedure miMainTabDirectoryAddDefaultClick(Sender: TObject);
    procedure miMainTabDirectoryEraseAllClick(Sender: TObject);
    procedure miMainTabDirectoryEraseIncorrectClick(Sender: TObject);
    procedure miMainTabDirectorySortClick(Sender: TObject);
    procedure miMainTabItemBaseFitColomnsClick(Sender: TObject);
    procedure miMainTabItemBaseSortClick(Sender: TObject);
    procedure miMainTabLaunchCollapseAllClick(Sender: TObject);
    procedure miMainTabLaunchExpandAllClick(Sender: TObject);
    procedure miMainTabLaunchFindExecutablesClick(Sender: TObject);
    procedure miMainTabStringTableFitColumnsClick(Sender: TObject);
    procedure miMainTabStringTableSortClick(Sender: TObject);
    procedure miMainToolsExportSettingsClick(Sender: TObject);
    procedure miMainToolsExtractDataClick(Sender: TObject);
    procedure miMainToolsImportSettingsClick(Sender: TObject);
    procedure miMainToolsOpenSettingsClick(Sender: TObject);
    procedure miMainToolsTimeCalculatorClick(Sender: TObject);
    procedure miMainToolsTrashCleanerClick(Sender: TObject);
    procedure miMainToolsWorkDriveClick(Sender: TObject);
    procedure miTrayDonateClick(Sender: TObject);
    procedure miTrayHideClick(Sender: TObject);
    procedure miTrayShowClick(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure TrayMenuPopup(Sender: TObject);
  private
    type
      //Типы вкладок
      TTabFrameType = (
        tftLaunch,
        tftDirectory,
        tftBuilder,
        tftStringTable,
        tftItemBase,
        tftNote
      );
    const
      WM_AFTER_SHOW = WM_USER + 1;
    var
      FIsFirstRun: Boolean;
      procedure AfterShowHandler(var Msg: TMessage); message WM_AFTER_SHOW;
  private
    const
      CONFIG_FILE_NAME = 'DayZModTool.ini';

      CONFIG_SECTION_MAIN_FORM = 'MainForm';
      CONFIG_SECTION_SYSTEM = 'System';

      CONFIG_PARAM_LEFT = 'Left';
      CONFIG_PARAM_TOP = 'Top';
      CONFIG_PARAM_WIDTH = 'Width';
      CONFIG_PARAM_HEIGHT = 'Height';
      CONFIG_PARAM_VISIBLE = 'Visible';
      CONFIG_PARAM_TAB_INDEX = 'TabIndex';
      CONFIG_PARAM_LANGUAGE = 'Language';
  private
    FCloseApplication: Boolean;

    //Каталоги
    FMainDir: String;           //Каталог запуска приложения
    FSettingsDir: String;       //Каталог настроек
    FLanguageDir: String;       //Каталог языков

    //Язык
    FCurrentLanguage: String;
    FLanguage: TLanguage;
    FLanguageFileList: TsgeStringList;

    //Закладки
    FTabParams: TTabParameters;
    FFrames: array[TTabFrameType] of TTabCommonFrame;

    //Система событий
    FEventSystem: TEventSystem;

    procedure Init;
    procedure Done;
    procedure SaveSettings;
    procedure LoadSettings;

    procedure OpenDonationSite;

    procedure LoadLanguage(LanguageName: String);
    procedure ApplyLanguage;
    procedure CreateLanguageFileList;
    procedure ChangeLanguage(LanguageName: String);

    procedure CreateTabs(AParams: TTabParameters);
    procedure DestroyTabs;

    procedure CreateTrayMenuLaunchItems;
    procedure CorrectTrayMenuLaunchItems;
    procedure miTrayLaunchClick(Sender: TObject);
    procedure miTrayStopClick(Sender: TObject);

    procedure CreateTrayMenuDirectoryItems;
    procedure DestroyTrayMenuDirectoryItems;
    procedure miTrayDirectoryClick(Sender: TObject);

    procedure CreateLanguageMenuItems;
    procedure CorrectLanguageMenuItemSelected;
    procedure miLanguageMenuItemClick(Sender: TObject);

    //Методы для импорта/экспорта настроек
    procedure GlobalSaveSettings;
    procedure GlobalLoadSettings;
  end;


function GetAppTitle: String;


var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  sgeFileUtils, DayZUtils, SettingsManager,
  DataExtractorUnit,
  YesNoQuestionDialogUnit, MessageDialogUnit, TimecalCulatorUnit;


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


procedure TMainForm.FormShow(Sender: TObject);
begin
  if FIsFirstRun then
  begin
    FIsFirstRun := False;
    if not FileExists(FSettingsDir + CONFIG_FILE_NAME) then
      PostMessage(Handle, WM_AFTER_SHOW, 0, 0);
  end;
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


procedure TMainForm.miMainInfoCheckVersionClick(Sender:TObject);
const
  vError = 0;
  vNoUpdate = 1;
  vUpdate = 2;
var
  ver: TGitHubVersion;
  mode: Byte;
begin
  mode := vError;

  ver := GetGithubVersion;
  if ver.Version = '' then
    mode := vError
  else
  begin
    if VERSION < ver.Version then
      mode := vUpdate
    else
      mode := vNoUpdate;
  end;

  case mode of
    vError:
    begin
      MessageDialogExecute(
        FLanguage,
        FLanguage.GetLocalizedString('MainForm.UpdateError', 'Ошибка обновления, попробуйте позже')
      );
    end;

    vNoUpdate:
    begin
      MessageDialogExecute(
        FLanguage,
        FLanguage.GetLocalizedString('MainForm.NoUpdate', 'Обновлений не найдено')
      );
    end;

    vUpdate:
    begin
      //Есть обновление, открыть ссылку
      if YesNoQuestionDialogExecute(
        FLanguage,
        Format(
          FLanguage.GetLocalizedString('MainForm.Update', 'Обнаружена новая версия %s, открыть ссылку?'),
          [ver.Version]
        )
      ) then
        if ver.URL <> '' then
          OpenURL(ver.URL);
    end;
  end;
end;


procedure TMainForm.miMainInfoDonateClick(Sender: TObject);
begin
  OpenDonationSite;
end;


procedure TMainForm.miMainTabBuilderCollapseAllClick(Sender: TObject);
begin
  (FFrames[tftBuilder] as TBuilderFrame).SetCollapset(True);
end;


procedure TMainForm.miMainTabBuilderEraseAllClick(Sender: TObject);
begin
  (FFrames[tftBuilder] as TBuilderFrame).Clear;
end;


procedure TMainForm.miMainTabBuilderExpandAllClick(Sender: TObject);
begin
  (FFrames[tftBuilder] as TBuilderFrame).SetCollapset(False);
end;


procedure TMainForm.miMainTabBuilderSortClick(Sender: TObject);
begin
  (FFrames[tftBuilder] as TBuilderFrame).Sort;
end;


procedure TMainForm.miMainTabDirectoryAddDefaultClick(Sender: TObject);
begin
  (FFrames[tftDirectory] as TDirectoryFrame).AddStandartDirectory;
end;


procedure TMainForm.miMainTabDirectoryEraseAllClick(Sender: TObject);
begin
  (FFrames[tftDirectory] as TDirectoryFrame).DeleteAll;
end;


procedure TMainForm.miMainTabDirectoryEraseIncorrectClick(Sender: TObject);
begin
  (FFrames[tftDirectory] as TDirectoryFrame).DeleteIncorrectDirectory;
end;


procedure TMainForm.miMainTabDirectorySortClick(Sender: TObject);
begin
  (FFrames[tftDirectory] as TDirectoryFrame).Sort;
end;


procedure TMainForm.miMainTabItemBaseFitColomnsClick(Sender: TObject);
begin
  (FFrames[tftItemBase] as TItemBaseFrame).FitColumns;
end;


procedure TMainForm.miMainTabItemBaseSortClick(Sender: TObject);
begin
  (FFrames[tftItemBase] as TItemBaseFrame).Sort;
end;


procedure TMainForm.miMainTabLaunchCollapseAllClick(Sender: TObject);
begin
  (FFrames[tftLaunch] as TLaunchFrame).SetCollapset(True);
end;


procedure TMainForm.miMainTabLaunchExpandAllClick(Sender: TObject);
begin
  (FFrames[tftLaunch] as TLaunchFrame).SetCollapset(False);
end;


procedure TMainForm.miMainTabLaunchFindExecutablesClick(Sender: TObject);
begin
  (FFrames[tftLaunch] as TLaunchFrame).FindExecutables;
end;


procedure TMainForm.miMainTabStringTableFitColumnsClick(Sender: TObject);
begin
  (FFrames[tftStringTable] as TStringTableFrame).FitColumns;
end;


procedure TMainForm.miMainTabStringTableSortClick(Sender: TObject);
begin
  (FFrames[tftStringTable] as TStringTableFrame).Sort;
end;


procedure TMainForm.miMainToolsExportSettingsClick(Sender: TObject);
var
  Fn: String;
begin
  fn := Format('DayzModTool Settings v%s %s.dzmtcb', [VERSION, FormatDateTime('yyyy.mm.dd-hh.nn.ss', Now)]);
  SaveDialog.FileName := Fn;
  if SaveDialog.Execute then
  begin
    try
      //Обновим настройки на диске, что бы экспортировать по факту
      GlobalSaveSettings;

      //Экспорт
      SettingsManager_Export(FSettingsDir, SaveDialog.FileName);

      //Показать что все создано успешно
      MessageDialogExecute(
        FLanguage,
        FLanguage.GetLocalizedString('MainForm.ExportConfigSuccess', 'Настройки успешно экспортированы')
      );
    except
      MessageDialogExecute(
        FLanguage,
        FLanguage.GetLocalizedString('MainForm.ExportConfigError', 'Произошла ошибка при экспорте настроек') + sLineBreak + Exception(ExceptObject).Message
      );
    end;
  end;
end;


procedure TMainForm.miMainToolsExtractDataClick(Sender: TObject);
begin
  DataExtractorExecute(FLanguage, FSettingsDir + CONFIG_FILE_NAME);
end;


procedure TMainForm.miMainToolsImportSettingsClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    try
      //Импорт настроек
      SettingsManager_Import(FSettingsDir, OpenDialog.FileName);

      //Перезагрузить настройки
      GlobalLoadSettings;

      //Показать что все успешно
      MessageDialogExecute(
        FLanguage,
        FLanguage.GetLocalizedString('MainForm.ImportConfigSuccess', 'Настройки успешно импортированы')
      );
    except
      MessageDialogExecute(
        FLanguage,
        FLanguage.GetLocalizedString('MainForm.ImportConfigError', 'Произошла ошибка при импорте настроек') + sLineBreak + Exception(ExceptObject).Message
      );
    end;
  end;
end;


procedure TMainForm.miMainToolsOpenSettingsClick(Sender: TObject);
begin
  OpenFolderInExplorer(FSettingsDir);
end;


procedure TMainForm.miMainToolsTimeCalculatorClick(Sender: TObject);
begin
  TimeCalculatorExecute(FLanguage, FSettingsDir + 'TimeCalculator.ini');
end;


procedure TMainForm.miMainToolsTrashCleanerClick(Sender: TObject);
begin
  TrashCleanerExecute(FLanguage);
end;


procedure TMainForm.miMainToolsWorkDriveClick(Sender: TObject);
begin
  WorkDriveExecute(FLanguage, FEventSystem, FSettingsDir + CONFIG_FILE_NAME);
end;


procedure TMainForm.miTrayDonateClick(Sender: TObject);
begin
  OpenDonationSite;
end;


procedure TMainForm.miTrayHideClick(Sender: TObject);
begin
  Hide;
end;


procedure TMainForm.miTrayShowClick(Sender: TObject);
begin
  Show;
end;


procedure TMainForm.TrayIconClick(Sender: TObject);
begin
  if not Visible then
    miTrayShow.Click;

  //Активировать главную форму
  SetForegroundWindow(Handle);
end;


procedure TMainForm.TrayMenuPopup(Sender: TObject);
begin
  //Показать / Скрыть
  miTrayShow.Visible := not MainForm.Visible;
  miTrayHide.Visible := MainForm.Visible;

  //Поправить кнопки запуска / останова
  CorrectTrayMenuLaunchItems;

  //Подготовить кнопки открытия каталогов
  CreateTrayMenuDirectoryItems;
end;


procedure TMainForm.AfterShowHandler(var Msg: TMessage);
begin
  //Проверим, если нет файла настроек, значит это первый запуск
  if not FileExists(FSettingsDir + CONFIG_FILE_NAME) then
  begin
    if YesNoQuestionDialogExecute(
      FLanguage,
      FLanguage.GetLocalizedString('MainForm.FirstLaunchQuestion', format('Не обнаружены настройки приложения%sПровести автоматическую настройку?', [sLineBreak]))
    ) then
    begin
      //Поищем каталоги
      miMainTabDirectoryAddDefault.Click;

      //Найдем приложения
      miMainTabLaunchFindExecutables.Click;
    end;
  end;
end;


procedure TMainForm.Init;
begin
  Caption := GetAppTitle;
  TrayIcon.Hint := Caption;

  FIsFirstRun := True;

  //Система событий
  FEventSystem := TEventSystem.Create;

  //Объекты
  FLanguage := TLanguage.Create;
  FLanguageFileList := TsgeStringList.Create;

  //Каталог запуска
  FMainDir := ExtractFilePath(ParamStr(0));

  //Каталог хранения настроек пользователей
  FSettingsDir := IncludeTrailingBackslash(SysUtils.GetEnvironmentVariable('APPDATA')) + 'DayZModTool\';
  ForceDirectories(FSettingsDir);

  //Языки
  FLanguageDir := FMainDir + 'Languages\';
  ForceDirectories(FLanguageDir);

  //Параметры закладок
  FTabParams := TTabParameters.Create;
  FTabParams.Language := FLanguage;
  FTabParams.EventSystem := FEventSystem;
  FTabParams.TabDirectory := FMainDir + 'Data\Tabs\';
  FTabParams.IconDirectory := FMainDir + 'Data\Icons\';
  FTabParams.SettingsDirectory := FSettingsDir;
  ForceDirectories(FTabParams.IconDirectory);

  //Создать закладки
  CreateTabs(FTabParams);

  //Создать элементы запуска/останова приложений
  CreateTrayMenuLaunchItems;

  //Прочитать параметры
  LoadSettings;

  //Получить список файлов с языками
  CreateLanguageFileList;

  //Поправить пункт перевода
  miMainLanguage.Visible := (FLanguageFileList.Count > 0);

  //Создать элементы языка
  CreateLanguageMenuItems;

  //Загрузить язык
  ChangeLanguage(FCurrentLanguage);
end;


procedure TMainForm.Done;
begin
  SaveSettings;

  //Удалить настройки закладок
  FTabParams.Free;

  //Удалить закладки
  DestroyTabs;

  //Почистить объекты
  FLanguageFileList.Free;
  FLanguage.Free;

  //Система событий
  FEventSystem.Free;
end;


procedure TMainForm.SaveSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FSettingsDir + CONFIG_FILE_NAME);
  try
    //Форма
    Ini.WriteInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_TOP, Self.Top);
    Ini.WriteInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_LEFT, Self.Left);
    Ini.WriteInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_WIDTH, Self.Width);
    Ini.WriteInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_HEIGHT, Self.Height);
    Ini.WriteBool(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_VISIBLE, IsWindowVisible(Handle));
    Ini.WriteInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_TAB_INDEX, PageControl.TabIndex);

    //Язык
    Ini.WriteString(CONFIG_SECTION_SYSTEM, CONFIG_PARAM_LANGUAGE, FCurrentLanguage);

  finally
    Ini.Free;
  end;
end;


procedure TMainForm.LoadSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FSettingsDir + CONFIG_FILE_NAME);
  try
    //Форма
    Self.Top := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_TOP, 100);
    Self.Left := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_LEFT, 300);
    Self.Width := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_WIDTH, Self.Width);
    Self.Height := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_HEIGHT, Self.Height);
    Application.ShowMainForm := Ini.ReadBool(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_VISIBLE, True);
    PageControl.TabIndex := Ini.ReadInteger(CONFIG_SECTION_MAIN_FORM, CONFIG_PARAM_TAB_INDEX, 0);

    //Язык
    FCurrentLanguage := Ini.ReadString(CONFIG_SECTION_SYSTEM, CONFIG_PARAM_LANGUAGE, 'Русский');

  finally
    Ini.Free;
  end;
end;


procedure TMainForm.OpenDonationSite;
begin
  OpenURL('https://donation.ntlab.su');
end;


procedure TMainForm.LoadLanguage(LanguageName: String);
var
  Fn: String;
begin
  Fn := FLanguageDir + LanguageName + '.Language';
  if FileExists(Fn) then
    FLanguage.LoadFromFile(Fn);
end;


procedure TMainForm.ApplyLanguage;

  procedure TranslateMenu(RootItem: TMenuItem; Prefix: String = '');
  var
    i: Integer;
  begin
    //Переводим себя
    RootItem.Caption := FLanguage.GetLocalizedString(Prefix + RootItem.Name, RootItem.Caption);

    //Перводим детей
    for i := 0 to RootItem.Count - 1 do
      TranslateMenu(RootItem.Items[i], Prefix);
  end;

  procedure TranslateTabsCaption;
  const
    PREFIX = 'MainForm.Tabs.';
  var
    i: Integer;
    Tab: TTabSheet;
  begin
    for i := 0 to PageControl.ControlCount - 1 do
    begin
      Tab := PageControl.Controls[i] as TTabSheet;
      Tab.Caption := FLanguage.GetLocalizedString(PREFIX + Tab.Name, Tab.Caption);
    end;
  end;

  procedure TranslateTabs;
  var
    i: TTabFrameType;
  begin
    for i := Low(TTabFrameType) to High(TTabFrameType) do
      FFrames[i].ApplyLanguage;
  end;

const
  LANGUAGE_PREFIX = 'MainForm.';
begin
  //Меню
  TranslateMenu(MainMenu.Items, 'MainForm.MainMenu.');
  TranslateMenu(TrayMenu.Items, 'MainForm.TrayMenu.');

  //Имена закладок
  TranslateTabsCaption;

  //Закладки
  TranslateTabs;

  //Пересоздать пункты меню в трее
  CreateTrayMenuLaunchItems;

  //Главная форма
  OpenDialog.Filter := Format('%s (*.dzmtcb)|*.dzmtcb', [FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'ConfigBackup', 'Файлы бэкапа настроек')]);
  SaveDialog.Filter := OpenDialog.Filter;
end;


procedure TMainForm.CreateLanguageFileList;
begin
  if DirectoryExists(FLanguageDir) then
    sgeFindFilesInFolderByExt(FLanguageDir, FLanguageFileList, '.Language');
end;


procedure TMainForm.ChangeLanguage(LanguageName: String);
begin
  FCurrentLanguage := LanguageName;
  LoadLanguage(FCurrentLanguage);

  //Поправить текущий язык
  ApplyLanguage;
  CorrectLanguageMenuItemSelected;
end;


procedure TMainForm.CreateTabs(AParams: TTabParameters);
begin
  FFrames[tftLaunch] := TLaunchFrame.Create(AParams, tabLaunch);
  FFrames[tftDirectory] := TDirectoryFrame.Create(AParams, tabDirectory);
  FFrames[tftBuilder] := TBuilderFrame.Create(AParams, tabBuilder);
  FFrames[tftStringTable] := TStringTableFrame.Create(AParams, tabStringTable);
  FFrames[tftItemBase] := TItemBaseFrame.Create(AParams, tabItemBase);
  FFrames[tftNote] := TNoteFrame.Create(AParams, tabNote);
end;


procedure TMainForm.DestroyTabs;
var
  i: TTabFrameType;
begin
  for i := Low(TTabFrameType) to High(TTabFrameType) do
  begin
    FFrames[i].Free;
    FFrames[i] := nil;
  end;
end;


procedure TMainForm.CreateTrayMenuLaunchItems;

  procedure AddMenu(RootMenu: TMenuItem; Frame: TLaunchItemFrame; IconIndex: Integer; Proc: TNotifyEvent);
  var
    MenuItem: TMenuItem;
  begin
    MenuItem := TMenuItem.Create(RootMenu);
    MenuItem.Caption := Frame.LocaleCaption;
    MenuItem.ImageIndex := IconIndex;
    MenuItem.OnClick := Proc;
    MenuItem.Tag := PtrUInt(Frame);

    RootMenu.Add(MenuItem);
  end;

var
  c, i, IconIndex: Integer;
  Frame: TLaunchFrame;
begin
  //Почистить лишнее
  miTrayLaunch.Clear;
  miTrayStop.Clear;
  ilTrayLaunch.Clear;

  //Построить меню
  Frame := FFrames[tftLaunch] as TLaunchFrame;
  c := Length(Frame.Items) - 1;
  for i := 0 to c do
  begin
    IconIndex := ilTrayLaunch.Count;
    ilTrayLaunch.AddIcon(Frame.Items[i].Icon);

    AddMenu(miTrayLaunch, Frame.Items[i], IconIndex, @miTrayLaunchClick);
    AddMenu(miTrayStop, Frame.Items[i], IconIndex, @miTrayStopClick);
  end;
end;


procedure TMainForm.CorrectTrayMenuLaunchItems;

  procedure SetMenuItemsEnabled(Root: TMenuItem);
  var
    Frame: TLaunchItemFrame;
    i: Integer;
  begin
    for i := 0 to Root.Count - 1 do
    begin
      Frame := TLaunchItemFrame(Root.Items[i].Tag);
      Root.Items[i].Enabled := Frame.ExecatableEnable;
    end;
  end;

begin
  SetMenuItemsEnabled(miTrayLaunch);
  SetMenuItemsEnabled(miTrayStop);
end;


procedure TMainForm.miTrayLaunchClick(Sender: TObject);
var
  Frame: TLaunchItemFrame;
begin
  if not (Sender is TMenuItem) then
    Exit;

  Frame := TLaunchItemFrame((Sender as TMenuItem).Tag);
  Frame.Launch;
end;


procedure TMainForm.miTrayStopClick(Sender: TObject);
var
  Frame: TLaunchItemFrame;
begin
  if not (Sender is TMenuItem) then
    Exit;

  Frame := TLaunchItemFrame((Sender as TMenuItem).Tag);
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
  Frame: TDirectoryFrame;
begin
  DestroyTrayMenuDirectoryItems;
  ilTrayDirectory.Clear;

  Frame := FFrames[tftDirectory] as TDirectoryFrame;
  for i := 0 to Length(Frame.Frames) - 1 do
  begin
    ilTrayDirectory.AddIcon(Frame.Frames[i].Icon);
    AddMenu(miTrayDirectory, Frame.Frames[i], i);
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


procedure TMainForm.CreateLanguageMenuItems;
var
  i, IconIndex: Integer;
  Fn, Cpt: String;
  Item: TMenuItem;
  Icn: TIcon;
begin
  for i := 0 to FLanguageFileList.Count - 1 do
  begin
    Cpt := ChangeFileExt(FLanguageFileList.Part[i], '');

    Item := TMenuItem.Create(MainMenu);
    Item.Caption := Cpt;
    Item.Tag := i;
    Item.OnClick := @miLanguageMenuItemClick;

    //Иконка
    Fn := FLanguageDir + ChangeFileExt(FLanguageFileList.Part[i], '.ico');
    if FileExists(fn) then
    begin
      Icn := TIcon.Create;
      Icn.LoadFromFile(fn);
      ilLanguages.AddIcon(Icn);
      Icn.Free;
      IconIndex := ilLanguages.Count - 1;
    end
    else
      IconIndex := 0;
    Item.ImageIndex := IconIndex;

    miMainLanguage.Add(Item);
  end;

  CorrectLanguageMenuItemSelected;
end;


procedure TMainForm.CorrectLanguageMenuItemSelected;
var
  i: Integer;
  Item: TMenuItem;
begin
  for i := 0 to miMainLanguage.Count - 1 do
  begin
    Item := miMainLanguage.Items[i];
    Item.Checked := LowerCase(FCurrentLanguage) = LowerCase(ChangeFileExt(FLanguageFileList.Part[i], ''));
  end;
end;


procedure TMainForm.miLanguageMenuItemClick(Sender: TObject);
var
  LngName: String;
begin
  LngName := FLanguageFileList.Part[(Sender as TMenuItem).Tag];
  ChangeLanguage(ChangeFileExt(LngName, ''));
end;


procedure TMainForm.GlobalSaveSettings;
var
  i: TTabFrameType;
begin
  //Основная форма
  SaveSettings;

  //Фреймы
  for i := Low(TTabFrameType) to High(TTabFrameType) do
    FFrames[i].SaveSettings;
end;


procedure TMainForm.GlobalLoadSettings;
var
  i: TTabFrameType;
begin
  //Основная форма
  LoadSettings;

  //Фреймы
  for i := Low(TTabFrameType) to High(TTabFrameType) do
    FFrames[i].LoadSettings;
end;


function GetAppTitle: String;
begin
  Result := 'DayZ Mod Tool  v' + VERSION;
end;



end.

