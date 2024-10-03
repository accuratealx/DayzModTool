unit LaunchItemUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Graphics, IniFiles,
  Language,
  StartParamArray;

type
  TLaunchItemFrame = class(TFrame)
    btnClearValue: TSpeedButton;
    btnClearJournalDir: TSpeedButton;
    btnExecutableOpenDirectory: TSpeedButton;
    btnStop: TSpeedButton;
    btnOpenJournalDirectory: TSpeedButton;
    cbEraseJournalDirBeforeRun: TCheckBox;
    cbSubdirectories: TCheckBox;
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
    procedure btnClearJournalDirClick(Sender: TObject);
    procedure btnClearValueClick(Sender: TObject);
    procedure btnLaunchClick(Sender: TObject);
    procedure btnExecutableOpenDirectoryClick(Sender: TObject);
    procedure btnOpenJournalDirectoryClick(Sender: TObject);
    procedure btnShowCommandLineClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure edExecutableChange(Sender: TObject);
    procedure btnSelectJournalDirectoryClick(Sender: TObject);
    procedure btnSelectExecutableClick(Sender: TObject);
    procedure btnCollapseClick(Sender: TObject);
  private
    const
      SECTION_PARAMS = 'Params';
      SECTION_EXECUTABLE = 'Executable';
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
    FSettingsFile: String;
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

    procedure SaveSettings(const FileName: String);
    procedure LoadSettings(const FileName: String);
    procedure LoadItemsSettings(const FileName: String);

    procedure OnChangeContentHeight(Sender: TObject);
    procedure DoHeightChange;
    procedure ClearDirectory;

    procedure SetIcon(AIcon: TIcon);
    procedure SetCollapsed(ACollapsed: Boolean);
    procedure SetHighlight(AHighlight: Boolean);
    procedure SetValue(AValue: String);
    function  GetLocaleCaption: String;
  public
    constructor Create(const ACaption: string; AIcon: TIcon; const ParameterFile: String; const SettingsFile: String; RelativeFileName: String; LocaleID: String); reintroduce;
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
    property SettingsFile: String read FSettingsFile;
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
  DayZUtils, SteamUtils,
  MemoDialogUnit, SelectDirectoryDialogUnit, YesNoQuestionDialogUnit,
  StartParamSimple,
  ParamFrameCommonUnit,
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


procedure TLaunchItemFrame.btnClearJournalDirClick(Sender: TObject);
begin
  ClearDirectory;
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
  FrameItem: TParamFrameCommonFrame;
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
  FrameItem: TParamFrameCommonFrame;
  i, Y: Integer;
begin
  Y := 0;

  for i := 0 to pnlContent.ControlCount - 1 do
  begin
    if not (pnlContent.Controls[i] is TParamFrameCommonFrame) then
      Continue;

    FrameItem := pnlContent.Controls[i] as TParamFrameCommonFrame;

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


constructor TLaunchItemFrame.Create(const ACaption: string; AIcon: TIcon; const ParameterFile: String; const SettingsFile: String; RelativeFileName: String; LocaleID: String);
begin
  inherited Create(nil);
  FCaption := ACaption;
  FIcon := TIcon.Create;
  FIcon.Assign(AIcon);

  DoubleBuffered := True;

  FParameterFile := ParameterFile;
  FSettingsFile := SettingsFile;
  FRelativeFileName := RelativeFileName;
  FLocaleID := LocaleID;

  FItems := TStartParamArray.Create;
  FItems.LoadFromFile(FParameterFile);

  LoadItemsSettings(FSettingsFile);
  PrepareInterface(FItems);
  LoadSettings(FSettingsFile);
end;


destructor TLaunchItemFrame.Destroy;
begin
  SaveSettings(FSettingsFile);

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

  lblTitle.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'Application.' + FLocaleID, FCaption);
  btnLaunch.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'Launch', 'Запустить');
  btnStop.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'Stop', 'Остановить');
  btnShowCommandLine.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'ShowCommandLine', 'Показать параметры запуска');
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
  SaveSettings(FSettingsFile);
end;


procedure TLaunchItemFrame.LoadSettings;
begin
  LoadItemsSettings(FSettingsFile);
  LoadSettings(FSettingsFile);
end;


procedure TLaunchItemFrame.SaveSettings(const FileName: String);
var
  F: TIniFile;
  i: Integer;
begin
  F := TIniFile.Create(FileName);
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

    //Минимальный вид
    F.WriteBool(SECTION_EXECUTABLE, PARAM_COLLAPSED, Collapsed);

  finally
    F.Free;
  end;
end;


procedure TLaunchItemFrame.LoadSettings(const FileName: String);
var
  F: TIniFile;
  i: Integer;
begin
  F := TIniFile.Create(FileName);
  try
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

    //Поправить высоту основного фрейма
    Height := GetTotalFrameHeight;

    //Минимальный вид
    Collapsed := F.ReadBool(SECTION_EXECUTABLE, PARAM_COLLAPSED, True);

  finally
    F.Free;
  end;
end;


procedure TLaunchItemFrame.LoadItemsSettings(const FileName: String);
var
  F: TIniFile;
  i: Integer;
begin
  F := TIniFile.Create(FileName);
  try
    for i := 0 to FItems.Count - 1 do
      FItems.Item[i].ValueFromString(F.ReadString(SECTION_PARAMS, FItems.Item[i].Name, ''));

  finally
    F.Free;
  end;
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
    Height := 45;
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



end.


