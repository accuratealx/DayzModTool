unit LaunchItemUnit;

{$mode ObjFPC}{$H+}
{$ModeSwitch duplicatelocals}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Graphics, IniFiles,
  Language,
  StartParamArray, sgeStringList, Types;

type
  TLaunchItemFrame = class(TFrame)
    btnSaveProfile: TSpeedButton;
    btnClearValue: TSpeedButton;
    btnClearJournalDir: TSpeedButton;
    btnRenameProfile: TSpeedButton;
    btnExecutableOpenDirectory: TSpeedButton;
    btnDeleteProfile: TSpeedButton;
    btnAddProfile: TSpeedButton;
    btnStop: TSpeedButton;
    btnOpenJournalDirectory: TSpeedButton;
    cbEraseJournalDirBeforeRun: TCheckBox;
    cbSubdirectories: TCheckBox;
    cbProfile: TComboBox;
    edExecutable: TEdit;
    edAdditionalCommandLine: TEdit;
    edJournalDir: TEdit;
    ilCollapse: TImageList;
    imgCollapse: TImage;
    imgIcon: TImage;
    lblTitle: TLabel;
    OpenDialog: TOpenDialog;
    pnlCollapse: TPanel;
    btnSelectJournalDirectory: TSpeedButton;
    ContentScrollBox: TScrollBox;
    pnlContent: TPanel;
    pnlButton: TPanel;
    btnLaunch: TSpeedButton;
    btnShowCommandLine: TSpeedButton;
    btnSelectExecutable: TSpeedButton;
    procedure btnAddProfileClick(Sender: TObject);
    procedure btnClearJournalDirClick(Sender: TObject);
    procedure btnClearValueClick(Sender: TObject);
    procedure btnDeleteProfileClick(Sender: TObject);
    procedure btnLaunchClick(Sender: TObject);
    procedure btnExecutableOpenDirectoryClick(Sender: TObject);
    procedure btnOpenJournalDirectoryClick(Sender: TObject);
    procedure btnRenameProfileClick(Sender: TObject);
    procedure btnSaveProfileClick(Sender: TObject);
    procedure btnShowCommandLineClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure cbProfileMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure cbProfileSelect(Sender: TObject);
    procedure edExecutableChange(Sender: TObject);
    procedure btnSelectJournalDirectoryClick(Sender: TObject);
    procedure btnSelectExecutableClick(Sender: TObject);
    procedure btnCollapseClick(Sender: TObject);
  private
    const
      COLLAPSED_HEIGHT = 45;

      EXT_PROFILE = 'ini';

      SETTINGS_FILE = 'Config.Settings';

      SECTION_SETTINGS = 'Settings';
      SECTION_PARAMS = 'Params';
      SECTION_EXECUTABLE = 'Executable';

      PARAM_CURRENT_PROFILE = 'CurrentProfile';
      PARAM_APP = 'Application';
      PARAM_CMD_LINE = 'CmdLine';
      PARAM_ERASE_JOURNAL = 'EraseJournalBeforeRun';
      PARAM_ERASE_JOURNAL_DIRECTORY = 'JournalDirectory';
      PARAM_ERASE_JOURNAL_SUBDERICTORIES = 'Subdirectories';
      PARAM_COLLAPSED = 'Collapsed';

      LANGUAGE_PREFIX = 'TabLaunch.';
  private
    FCaption: String;
    FIcon: TIcon;
    FItems: TStartParamArray;
    FParameterFile: String;
    FSettingsDirectory: String;
    FRelativeFileName: String;
    FLocaleID: String;
    FCollapsed: Boolean;
    FHighlight: Boolean;
    FOnHeightChange: TNotifyEvent;
    FLanguage: TLanguage;

    procedure ClearInterface;
    procedure PrepareInterface(Items: TStartParamArray);
    procedure ArrangeContentPanelItems;
    function  GetTotalFrameHeight: Integer;
    function  GetCommandLine: String;

    procedure OnChangeContentHeight(Sender: TObject);
    procedure DoHeightChange;
    procedure ClearDirectory;

    procedure SetIcon(AIcon: TIcon);
    procedure SetCollapsed(ACollapsed: Boolean);
    procedure SetHighlight(AHighlight: Boolean);
    procedure SetValue(AValue: String);
    function  GetLocaleCaption: String;

    procedure SaveFrameSettings;
    procedure LoadFrameSettings;
  private
    FProfileList: TsgeStringList; //Список профилей
    FCurrentProfile: String;      //Имя текущего профиля

    function  GetSettingsFileName: String;
    function  GetProfileFileName(ProfileName: String): String;
    function  GetProfileDefaultName: String;

    procedure RenameProfile(Old, New: String);
    procedure DeleteProfile(ProfileName: String);
    function  IsProfileExist(ProfileName: String): Boolean;
    procedure ApplyProfile(ProfileName: String);
    procedure SaveProfile(ProfileName: String);
    procedure LoadProfile(ProfileName: String);

    procedure ReadProfileList;
    procedure FillProfileBox;
  public
    constructor Create(Language: TLanguage; const ACaption: string; AIcon: TIcon; const ParameterFile: String; const SettingsDirectory: String; RelativeFileName: String; LocaleID: String); reintroduce;
    destructor Destroy; override;

    procedure Launch;
    procedure Stop;
    procedure FindExecutable;
    function  ExecatableEnable: Boolean;
    procedure ChangeLanguage(Language: TLanguage);
    procedure SaveSettings;
    procedure LoadSettings;

    property Icon: TIcon read FIcon write SetIcon;
    property Caption: String read FCaption write FCaption;
    property LocaleCaption: String read GetLocaleCaption;
    property Items: TStartParamArray read FItems;
    property ParameterFile: String read FParameterFile;
    property RelativeFileName: String read FRelativeFileName write FRelativeFileName;
    property LocaleID: String read FLocaleID write FLocaleID;
    property Collapsed: Boolean read FCollapsed write SetCollapsed;
    property Highlight: Boolean read FHighlight write SetHighlight;
    property OnHeightChange: TNotifyEvent read FOnHeightChange write FOnHeightChange;
  end;

  TLaunchItemFrameList = array of TLaunchItemFrame;


implementation

{$R *.lfm}

uses
  DayZUtils, SteamUtils, sgeFileUtils,
  MemoDialogUnit, SelectDirectoryDialogUnit, YesNoQuestionDialogUnit, InputDialogUnit,
  MessageDialogUnit,
  StartParamSimple,
  ParamFrameSimpleUnit, ParamFrameIntegerUnit, ParamFrameStringUnit,
  ParamFrameDirectoryUnit, ParamFrameFileUnit, ParamFrameDirectoryListUnit;


procedure TLaunchItemFrame.btnShowCommandLineClick(Sender: TObject);
var
  ParamStr, Cmd, Fn: String;
begin
  ParamStr := '';

  //Приложение
  Fn := Trim(edExecutable.Text);
  if FileExists(Fn) then
    ParamStr := Format('"%s"', [Fn])
  else
    ParamStr := '';

  //Параметры
  Cmd := GetCommandLine;
  if Cmd <> '' then
  begin
    if ParamStr <> '' then
      ParamStr := ParamStr + ' ' + Cmd
    else
      ParamStr := Cmd;
  end;

  //Показать диалог
  MemoDialogExecute(
    FLanguage,
    FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'MemoCaption', 'Параметры запуска'),
    ParamStr
  );
end;


procedure TLaunchItemFrame.btnStopClick(Sender: TObject);
var
  Fn: String;
begin
  //Убить процесс по имени
  Fn := Trim(edExecutable.Text);
  KillProcess(ExtractFileName(Fn));
end;


procedure TLaunchItemFrame.cbProfileMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
end;


procedure TLaunchItemFrame.cbProfileSelect(Sender: TObject);
var
  s: String;
begin
  //Получить имя текущего профиля
  s := cbProfile.Text;

  //Сохраним текущее имя профиля
  FCurrentProfile := s;

  //Загрузить настройки профиля
  LoadProfile(s);
end;


procedure TLaunchItemFrame.edExecutableChange(Sender: TObject);
begin
  SetValue(edExecutable.Text);
end;


procedure TLaunchItemFrame.btnLaunchClick(Sender: TObject);
begin
  //Удалить содержимое каталога если включена настройка
  if cbEraseJournalDirBeforeRun.Checked then
    ClearDirectory;

  //Launch application
  ExecuteFile(Trim(edExecutable.Text), GetCommandLine);
end;


procedure TLaunchItemFrame.btnClearValueClick(Sender: TObject);
begin
  edJournalDir.Text := '';
end;


procedure TLaunchItemFrame.btnDeleteProfileClick(Sender: TObject);
var
  s: String;
begin
  //Проверить что бы не удалился последний профиль
  if FProfileList.Count <= 1 then
  begin
    MessageDialogExecute(
      FLanguage,
      FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'CantDeleteProfile', 'Невозможно удалить единственный профиль')
    );
    Exit;
  end;

  //Спросим можно ли удалить текущий профиль
  s := FCurrentProfile;
  if YesNoQuestionDialogExecute(
    FLanguage,
    Format(FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'DeleteProfileQuestion', 'Удалить профиль "%s"?'), [s])
  ) then
  begin
    //Удалим профиль
    DeleteProfile(s);

    //Перечитаем список профилей с диска
    ReadProfileList;

    //Заполним список профилей
    FillProfileBox;

    //Установим новый профиль
    s := FProfileList.Part[0];
    ApplyProfile(s);
  end;
end;


procedure TLaunchItemFrame.btnClearJournalDirClick(Sender: TObject);
begin
  ClearDirectory;
end;


procedure TLaunchItemFrame.btnAddProfileClick(Sender: TObject);
var
  AName: String;
  Overwrite: Boolean;
begin
  AName := '';
  if InputDialogExecute(
    FLanguage,
    FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'InputProfileName', 'Введите имя профиля'),
    AName) then
  begin
    Overwrite := True;

    //Проверим нет ли такого профиля на диске
    if IsProfileExist(AName) then
    begin
      if not YesNoQuestionDialogExecute(
        FLanguage,
        Format(FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'OverwriteProfile', 'Профиль "%s" существует, переписать?'), [AName])
      ) then
      Overwrite := False;
    end;

    //Выход, если не перетирать
    if not Overwrite then
      Exit;

    //Сохраним новый профиль
    SaveProfile(AName);

    //Перечитаем список профилей с диска
    ReadProfileList;

    //Заполним список профилей
    FillProfileBox;

    //Установим новый профиль
    ApplyProfile(AName);
  end;
end;


procedure TLaunchItemFrame.btnExecutableOpenDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := ExtractFilePath(Trim(edExecutable.Text));
  if DirectoryExists(Dir) then
    OpenFolderInExplorer(Dir);
end;


procedure TLaunchItemFrame.btnOpenJournalDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := edJournalDir.Text;
  if DirectoryExists(Dir) then
    OpenFolderInExplorer(Dir);
end;


procedure TLaunchItemFrame.btnRenameProfileClick(Sender: TObject);
var
  AName: String;
  Overwrite: Boolean;
begin
  AName := cbProfile.Text;
  if InputDialogExecute(
    FLanguage,
    FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'InputProfileName', 'Введите имя профиля'),
    AName) then
  begin
    Overwrite := True;

    //Проверим нет ли такого профиля на диске
    if IsProfileExist(AName) then
    begin
      if not YesNoQuestionDialogExecute(
        FLanguage,
        Format(FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'OverwriteProfile', 'Профиль "%s" существует, переписать?'), [AName])
      ) then
        Overwrite := False;
    end;

    //Выход, если не перетирать
    if not Overwrite then
      Exit;

    //Переименуем профиль
    RenameProfile(cbProfile.Text, AName);

    //Перечитаем список профилей с диска
    ReadProfileList;

    //Заполним список профилей
    FillProfileBox;

    //Установим новый профиль
    ApplyProfile(AName);
  end;
end;


procedure TLaunchItemFrame.btnSaveProfileClick(Sender: TObject);
begin
  //Обновим профиль на диске
  SaveProfile(FCurrentProfile);
end;


procedure TLaunchItemFrame.btnSelectJournalDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := edJournalDir.Text;

  if SelectDirectoryDialogExecute(FLanguage, Dir) then
    edJournalDir.Text := Dir;
end;


procedure TLaunchItemFrame.btnSelectExecutableClick(Sender: TObject);
var
  Dir, Fn: String;
begin
  Dir := ExtractFilePath(edExecutable.Text);
  Fn := ExtractFileName(edExecutable.Text);

  OpenDialog.InitialDir := Dir;
  OpenDialog.FileName := Fn;
  if OpenDialog.Execute then
    SetValue(OpenDialog.FileName);
end;


procedure TLaunchItemFrame.btnCollapseClick(Sender: TObject);
begin
  Collapsed := not Collapsed;
end;


procedure TLaunchItemFrame.ClearInterface;
var
  i: Integer;
begin
  for i := pnlContent.ControlCount - 1 downto 0 do
    pnlContent.Controls[i].Free;
end;


procedure TLaunchItemFrame.PrepareInterface(Items: TStartParamArray);
var
  i: Integer;
  Item: TStartParamSimple;
  FrameItem: TParamFrameSimpleFrame;
begin
  imgIcon.Picture.Assign(Icon);
  lblTitle.Caption := Caption;

  for i := 0 to Items.Count - 1 do
  begin
    //Ссылка на параметр
    Item := Items.Item[i];

    FrameItem := nil;

    //Создать фрейм
    case Item.&Type of
      'Simple':
        FrameItem := TParamFrameSimpleFrame.Create(Item);

      'Integer':
        FrameItem := TParamFrameIntegerFrame.Create(Item);

      'String':
        FrameItem := TParamFrameStringFrame.Create(Item);

      'Directory':
        FrameItem := TParamFrameDirectoryFrame.Create(Item);

      'File':
        FrameItem := TParamFrameFileFrame.Create(Item);

      'DirectoryList':
      begin
        FrameItem := TParamFrameDirectoryListFrame.Create(Item);
        (FrameItem as TParamFrameDirectoryListFrame).OnHeightChange := @OnChangeContentHeight;
      end

      else
        ShowMessage(Format('Неизвестный тип фрейма %s', [Item.&Type]));
    end;

    //Заполнить фрейм если создан
    if FrameItem <> nil then
    begin
      FrameItem.Parent := pnlContent;
      FrameItem.Anchors := [akTop, akLeft, akRight];
    end;
  end;

  //Расположить элементы на панели и выставить правильную высоту
  ArrangeContentPanelItems;

  //Поправить высоту основного фрейма
  Height := GetTotalFrameHeight;
end;


procedure TLaunchItemFrame.ArrangeContentPanelItems;
var
  FrameItem: TParamFrameSimpleFrame;
  i, Y: Integer;
begin
  Y := 0;

  for i := 0 to pnlContent.ControlCount - 1 do
  begin
    if not (pnlContent.Controls[i] is TParamFrameSimpleFrame) then
      Continue;

    FrameItem := pnlContent.Controls[i] as TParamFrameSimpleFrame;
    FrameItem.Left := 0;
    FrameItem.Width := pnlContent.Width;
    FrameItem.Top := Y;

    Inc(Y, FrameItem.Height);
  end;

  pnlContent.Height := Y;
end;


function TLaunchItemFrame.GetTotalFrameHeight: Integer;
begin
  Result := pnlButton.Height + pnlContent.Height;
end;


function TLaunchItemFrame.GetCommandLine: String;
var
  Cmd: String;
begin
  //Список параметров во фреймах
  Result := FItems.GetCommandLine;

  //Строка дополнительных параметров
  Cmd := Trim(edAdditionalCommandLine.Text);
  if Cmd <> '' then
  Result := Result + ' ' + Cmd;
end;


constructor TLaunchItemFrame.Create(Language: TLanguage; const ACaption: string; AIcon: TIcon; const ParameterFile: String; const SettingsDirectory: String; RelativeFileName: String; LocaleID: String);
begin
  inherited Create(nil);

  //Создать объекты
  DoubleBuffered := True;
  FProfileList := TsgeStringList.Create;

  FCaption := ACaption;
  FIcon := TIcon.Create;
  FIcon.Assign(AIcon);
  FLanguage := Language;
  FParameterFile := ParameterFile;
  FSettingsDirectory := IncludeTrailingBackslash(SettingsDirectory);
  FRelativeFileName := RelativeFileName;
  FLocaleID := LocaleID;

  //Создаем фреймы с параметрами запуска
  FItems := TStartParamArray.Create;
  FItems.LoadFromFile(FParameterFile);
  PrepareInterface(FItems);

  //Поиск профилей в каталоге
  ReadProfileList;

  //Заполним список профилей
  FillProfileBox;

  //Загрузим настройки фрейма
  LoadFrameSettings;

  //Установим профиль
  ApplyProfile(FCurrentProfile);
end;


destructor TLaunchItemFrame.Destroy;
begin
  //Сохранить настройки
  SaveProfile(FCurrentProfile);
  SaveFrameSettings;

  //Почистим память
  FProfileList.Free;
  FItems.Free;

  ClearInterface;

  FIcon.Free;
  inherited Destroy;
end;


procedure TLaunchItemFrame.Launch;
begin
  btnLaunch.Click;
end;


procedure TLaunchItemFrame.Stop;
begin
  btnStop.Click;
end;


procedure TLaunchItemFrame.FindExecutable;
var
  Fn: String;
  DirSteam, DirTools: String;
begin
  //Не искать, если уже все найдено
  if FileExists(Trim(edExecutable.Text)) then
    Exit;

  //Основные каталоги стима
  DirSteam := GetSteamInstallPathFromRegistry;
  DirTools := GetDayZToolsInstallPathFromRegistry;

  //Смотрим в папку с инструментами
  Fn := DirTools + FRelativeFileName;
  if FileExists(Fn) then
  begin
    edExecutable.Text := Fn;
    Exit;
  end;

  //Смотрим в папку common steam
  Fn := DirSteam + FRelativeFileName;
  if FileExists(Fn) then
  begin
    edExecutable.Text := Fn;
    Exit;
  end;

  //Ничего не нашли, спросить пользователя
  if YesNoQuestionDialogExecute(
    FLanguage,
    Format(FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'CantDeterminePath', 'Не удалось автоматически определить путь для "%s"%sУказать вручную?'), [Caption, sLineBreak])
  ) then
    btnSelectExecutable.Click;
end;


function TLaunchItemFrame.ExecatableEnable: Boolean;
begin
  Result := btnLaunch.Enabled;
end;


procedure TLaunchItemFrame.ChangeLanguage(Language: TLanguage);
var
  i: Integer;
begin
  FLanguage := Language;

  //Интерфейс
  lblTitle.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'Application.' + FLocaleID, FCaption);
  btnLaunch.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'Launch', 'Запустить');
  btnStop.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'Stop', 'Остановить');
  btnShowCommandLine.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'ShowCommandLine', 'Показать параметры запуска');
  btnSaveProfile.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'SaveProfile', 'Сохранить профиль');
  btnAddProfile.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'AddProfile', 'Добавить профиль');
  btnRenameProfile.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'RenameProfile', 'Переименовать профиль');
  btnDeleteProfile.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'DeleteProfile', 'Удалить профиль');
  btnExecutableOpenDirectory.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'ExecutableOpenDirectory', 'Открыть каталог в проводнике');
  btnSelectExecutable.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'SelectExecutable', 'Выбрать файл для запуска');
  btnOpenJournalDirectory.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'OpenJournalDirectory', 'Открыть каталог в проводнике');
  btnSelectJournalDirectory.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'SelectJournalDirectory', 'Выбрать каталог журналов');
  btnClearValue.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'ClearValue', 'Очистить значение');
  btnClearJournalDir.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'ClearJournalDir', 'Очистить каталог');
  edExecutable.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'Executable', 'Файл для запуска');
  edAdditionalCommandLine.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'AdditionalCommandLine', 'Дополнительные параметры запуска');
  edJournalDir.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'JournalDir', 'Каталог с журналами');
  cbEraseJournalDirBeforeRun.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'EraseJournalDirBeforeRun', 'Очищать каталог перед запуском');
  cbSubdirectories.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'Subdirectories', 'Подкаталоги');
  OpenDialog.Filter := Format('%s (*.exe)|*.exe', [FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'Applications', 'Приложения')]);

  //Перевести редакторы параметров
  for i := 0 to pnlContent.ControlCount - 1 do
  begin
    if pnlContent.Controls[i] is TParamFrameSimpleFrame then
      (pnlContent.Controls[i] as TParamFrameSimpleFrame).ChangeLanguage(FLanguage);
  end;
end;


procedure TLaunchItemFrame.SaveSettings;
begin
  SaveProfile(FCurrentProfile);
  SaveFrameSettings;
end;


procedure TLaunchItemFrame.LoadSettings;
begin
  LoadFrameSettings;
  ApplyProfile(FCurrentProfile);
end;


procedure TLaunchItemFrame.OnChangeContentHeight(Sender: TObject);
begin
  //Поправить элементы внутри
  ArrangeContentPanelItems;

  //Поправить общую высоту
  Height := GetTotalFrameHeight;

  //Вызвать обработчик изменения размеров
  DoHeightChange;
end;


procedure TLaunchItemFrame.DoHeightChange;
begin
  if Assigned(FOnHeightChange) then
    FOnHeightChange(Self);
end;


procedure TLaunchItemFrame.ClearDirectory;
var
  dir: String;
begin
  //Кталог для удаления
  Dir := Trim(edJournalDir.Text);
  if dir = '' then
    Exit;

  if cbSubdirectories.Checked then
    DeleteFolderToRecycle(Dir)
  else
    DeleteFilesToRecycle(Dir);
end;


procedure TLaunchItemFrame.SetIcon(AIcon: TIcon);
begin
  FIcon.Assign(AIcon);
end;


procedure TLaunchItemFrame.SetCollapsed(ACollapsed: Boolean);
begin
  FCollapsed := ACollapsed;

  if FCollapsed then
  begin
    Height := COLLAPSED_HEIGHT;
    imgCollapse.ImageIndex := 0;
  end
  else
  begin
    Height := GetTotalFrameHeight;
    imgCollapse.ImageIndex := 1;
  end;

  //Вызвать обработчик изменения высоты
  DoHeightChange;
end;


procedure TLaunchItemFrame.SetHighlight(AHighlight: Boolean);
begin
  if FHighlight = AHighlight then
    Exit;

  FHighlight := AHighlight;

  //Изменить фоновый цвет
  Self.ParentColor := not FHighlight;
  Self.ParentBackground := not FHighlight;

  if FHighlight then
    Self.Color := cl3DLight
  else
    Self.Color := clDefault;
end;


procedure TLaunchItemFrame.SetValue(AValue: String);
var
  IsEnable: Boolean;
begin
  edExecutable.Text := Trim(AValue);

  IsEnable := FileExists(edExecutable.Text);
  btnLaunch.Enabled := IsEnable;
  btnStop.Enabled := IsEnable;
end;


function TLaunchItemFrame.GetLocaleCaption: String;
begin
  Result := lblTitle.Caption;
end;


function TLaunchItemFrame.GetSettingsFileName: String;
begin
  Result := Format('%s%s', [FSettingsDirectory, SETTINGS_FILE]);
end;


function TLaunchItemFrame.GetProfileFileName(ProfileName: String): String;
begin
  Result := Format('%s%s.%s', [FSettingsDirectory, ProfileName, EXT_PROFILE]);
end;


function TLaunchItemFrame.GetProfileDefaultName: String;
begin
  Result := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'DefaultProfileName', 'Профиль по умолчанию');
end;


procedure TLaunchItemFrame.RenameProfile(Old, New: String);
begin
  Old := GetProfileFileName(Old);
  New := GetProfileFileName(New);
  DeleteFile(New);
  RenameFile(Old, New);
end;


procedure TLaunchItemFrame.DeleteProfile(ProfileName: String);
begin
  ProfileName := GetProfileFileName(ProfileName);
  DeleteFile(ProfileName);
end;


function TLaunchItemFrame.IsProfileExist(ProfileName: String): Boolean;
begin
  ProfileName := GetProfileFileName(ProfileName);
  Result := FileExists(ProfileName);
end;


procedure TLaunchItemFrame.ReadProfileList;
var
  i: Integer;
  s: String;
begin
  //Ищем профили в каталоге настроек
  sgeFindFilesInFolderByExt(FSettingsDirectory, FProfileList, EXT_PROFILE);

  //Отпилить расширение
  for i := 0 to FProfileList.Count - 1 do
  begin
    s := FProfileList.Part[i];
    s := StringReplace(s, '.' + EXT_PROFILE, '', [rfIgnoreCase, rfReplaceAll]);
    FProfileList.Part[i] := s;
  end;

  //Если нет ни одного профиля, то создадим профиль по умолчанию
  if FProfileList.Count = 0 then
  begin
    s := GetProfileDefaultName;
    SaveProfile(s);
    FProfileList.Add(s);
  end;
end;


procedure TLaunchItemFrame.ApplyProfile(ProfileName: String);
var
  idx: Integer;
begin
  //Поищем индекс профиля
  idx := FProfileList.IndexOf(ProfileName);

  if idx <> -1 then
    cbProfile.ItemIndex := idx
  else
    cbProfile.ItemIndex := 0;

  //Выберем профиль
  cbProfileSelect(cbProfile);
end;


procedure TLaunchItemFrame.LoadFrameSettings;
var
  F: TIniFile;
begin
  F := TIniFile.Create(GetSettingsFileName);
  try
    FCurrentProfile := F.ReadString(SECTION_SETTINGS, PARAM_CURRENT_PROFILE, '');
    Collapsed := F.ReadBool(SECTION_SETTINGS, PARAM_COLLAPSED, True);

  finally
    F.Free;
  end;
end;


procedure TLaunchItemFrame.SaveFrameSettings;
var
  F: TIniFile;
begin
  F := TIniFile.Create(GetSettingsFileName);
  try
    F.WriteString(SECTION_SETTINGS, PARAM_CURRENT_PROFILE, FCurrentProfile);
    F.WriteBool(SECTION_SETTINGS, PARAM_COLLAPSED, Collapsed);

  finally
    F.Free;
  end;
end;


procedure TLaunchItemFrame.SaveProfile(ProfileName: String);
var
  F: TIniFile;
  i: Integer;
begin
  F := TIniFile.Create(GetProfileFileName(ProfileName));
  try
    //Настройки запуска
    F.WriteString(SECTION_EXECUTABLE, PARAM_APP, edExecutable.Text);
    F.WriteString(SECTION_EXECUTABLE, PARAM_CMD_LINE, edAdditionalCommandLine.Text);

    //Очистка каталога перед запуском
    F.WriteBool(SECTION_EXECUTABLE, PARAM_ERASE_JOURNAL, cbEraseJournalDirBeforeRun.Checked);
    F.WriteBool(SECTION_EXECUTABLE, PARAM_ERASE_JOURNAL_SUBDERICTORIES, cbSubdirectories.Checked);
    F.WriteString(SECTION_EXECUTABLE, PARAM_ERASE_JOURNAL_DIRECTORY, edJournalDir.Text);

    //Записать параметры
    for i := 0 to FItems.Count - 1 do
      F.WriteString(SECTION_PARAMS, FItems.Item[i].Name, FItems.Item[i].ValueToString);

  finally
    F.Free;
  end;
end;


procedure TLaunchItemFrame.LoadProfile(ProfileName: String);
var
  F: TIniFile;
  i: Integer;
begin
  F := TIniFile.Create(GetProfileFileName(ProfileName));

  try
    //Читаем настройки парметров
    for i := 0 to FItems.Count - 1 do
      FItems.Item[i].ValueFromString(F.ReadString(SECTION_PARAMS, FItems.Item[i].Name, ''));


    //Настройки запуска
    SetValue(F.ReadString(SECTION_EXECUTABLE, PARAM_APP, ''));
    edAdditionalCommandLine.Text := F.ReadString(SECTION_EXECUTABLE, PARAM_CMD_LINE, '');

    //Очистка каталога перед запуском
    cbEraseJournalDirBeforeRun.Checked := F.ReadBool(SECTION_EXECUTABLE, PARAM_ERASE_JOURNAL, False);
    cbSubdirectories.Checked := F.ReadBool(SECTION_EXECUTABLE, PARAM_ERASE_JOURNAL_SUBDERICTORIES, False);
    edJournalDir.Text := F.ReadString(SECTION_EXECUTABLE, PARAM_ERASE_JOURNAL_DIRECTORY, '');

    //Чтение параметров
    for i := 0 to pnlContent.ControlCount - 1 do
    begin
      if not (pnlContent.Controls[i] is TParamFrameSimpleFrame) then
        Continue;

      //Поправить интерфейс
      (pnlContent.Controls[i] as TParamFrameSimpleFrame).UpdateInterface;
    end;

    //Расположить элементы на панели и выставить правильную высоту
    ArrangeContentPanelItems;

    //Поправить высоту основного фрейма в зависимости от состояния
    if FCollapsed then
      Height := COLLAPSED_HEIGHT
    else
      Height := GetTotalFrameHeight;

  finally
    F.Free;
  end;
end;


procedure TLaunchItemFrame.FillProfileBox;
var
  i: Integer;
begin
  cbProfile.Clear;
  for i := 0 to FProfileList.Count - 1 do
    cbProfile.Items.Add(FProfileList.Part[i]);
end;



end.


