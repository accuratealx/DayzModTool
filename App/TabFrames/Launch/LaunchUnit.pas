unit LaunchUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Graphics, IniFiles,
  Language,
  StartParamArray;

type
  TLaunchFrame = class(TFrame)
    btnClearJournalDir: TSpeedButton;
    btnExecutableOpenDirectory: TSpeedButton;
    btnStop: TSpeedButton;
    btnOpenJournalDirectory: TSpeedButton;
    cbEraseJournalDirBeforeRun: TCheckBox;
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
      PARAM_COLLAPSED = 'Collapsed';

      PREFIX_TAB_LAUNCH = 'TabLaunch.';
  private
    FCaption: String;
    FIcon: TIcon;
    FItems: TStartParamArray;
    FParameterFile: String;
    FSettingsFile: String;
    FRelativeFileName: String;
    FLocaleID: String;
    FCollapsed: Boolean;
    FHiglight: Boolean;
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
    procedure ClearLogDirectory;

    procedure SetIcon(AIcon: TIcon);
    procedure SetCollapsed(ACollapsed: Boolean);
    procedure SetHighlight(AHighlight: Boolean);
    procedure SetValue(AValue: String);
  public
    constructor Create(const ACaption: string; AIcon: TIcon; const ParameterFile: String; const SettingsFile: String; RelativeFileName: String; LocaleID: String); reintroduce;
    destructor Destroy; override;

    procedure Launch;
    procedure Stop;
    procedure FindExecutable;
    function  ExecatableEnable: Boolean;
    procedure ChangeLanguage(Language: TLanguage);

    property Icon: TIcon read FIcon write SetIcon;
    property Caption: String read FCaption write FCaption;
    property Items: TStartParamArray read FItems;
    property ParameterFile: String read FParameterFile;
    property SettingsFile: String read FSettingsFile;
    property RelativeFileName: String read FRelativeFileName write FRelativeFileName;
    property LocaleID: String read FLocaleID write FLocaleID;
    property Collapsed: Boolean read FCollapsed write SetCollapsed;
    property Higlight: Boolean read FHiglight write SetHighlight;
    property OnHeightChange: TNotifyEvent read FOnHeightChange write FOnHeightChange;
  end;

  TLaunchExecutableFrameList = array of TLaunchFrame;


implementation

{$R *.lfm}

uses
  DayZUtils, SteamUtils,
  MemoDialogUnit, SelectDirectoryDialogUnit, YesNoQuestionDialogUnit,
  StartParamSimple,
  ParamFrameCommonUnit,
  ParamFrameSimpleUnit, ParamFrameIntegerUnit, ParamFrameStringUnit,
  ParamFrameDirectoryUnit, ParamFrameFileUnit, ParamFrameDirectoryListUnit;


procedure TLaunchFrame.btnShowCommandLineClick(Sender: TObject);
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
    FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'MemoCaption', 'Параметры запуска'),
    ParamStr
  );
end;


procedure TLaunchFrame.btnStopClick(Sender: TObject);
var
  Fn: String;
begin
  //Убить процесс по имени
  Fn := Trim(edExecutable.Text);
  KillProcess(ExtractFileName(Fn));
end;


procedure TLaunchFrame.edExecutableChange(Sender: TObject);
begin
  SetValue(edExecutable.Text);
end;


procedure TLaunchFrame.btnLaunchClick(Sender: TObject);
begin
  //Удалить содержимое каталога
  ClearLogDirectory;

  //Launch application
  ExecuteFile(Trim(edExecutable.Text), GetCommandLine);
end;


procedure TLaunchFrame.btnClearJournalDirClick(Sender: TObject);
begin
  ClearLogDirectory;
end;


procedure TLaunchFrame.btnExecutableOpenDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := ExtractFilePath(Trim(edExecutable.Text));
  if DirectoryExists(Dir) then
    OpenFolderInExplorer(Dir);
end;


procedure TLaunchFrame.btnOpenJournalDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := edJournalDir.Text;
  if DirectoryExists(Dir) then
    OpenFolderInExplorer(Dir);
end;


procedure TLaunchFrame.btnSelectJournalDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := edJournalDir.Text;

  if SelectDirectoryDialogExecute(FLanguage, Dir) then
    edJournalDir.Text := Dir;
end;


procedure TLaunchFrame.btnSelectExecutableClick(Sender: TObject);
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


procedure TLaunchFrame.btnCollapseClick(Sender: TObject);
begin
  Collapsed := not Collapsed;
end;


procedure TLaunchFrame.ClearInterface;
var
  i: Integer;
begin
  for i := pnlContent.ControlCount - 1 downto 0 do
    pnlContent.Controls[i].Free;
end;


procedure TLaunchFrame.PrepareInterface(Items: TStartParamArray);
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


procedure TLaunchFrame.ArrangeContentPanelItems;
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


function TLaunchFrame.GetTotalFrameHeight: Integer;
begin
  Result := pnlButton.Height + pnlContent.Height;
end;


function TLaunchFrame.GetCommandLine: String;
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


constructor TLaunchFrame.Create(const ACaption: string; AIcon: TIcon; const ParameterFile: String; const SettingsFile: String; RelativeFileName: String; LocaleID: String);
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


destructor TLaunchFrame.Destroy;
begin
  SaveSettings(FSettingsFile);

  FItems.Free;

  ClearInterface;

  FIcon.Free;
  inherited Destroy;
end;


procedure TLaunchFrame.Launch;
begin
  btnLaunch.Click;
end;


procedure TLaunchFrame.Stop;
begin
  btnStop.Click;
end;


procedure TLaunchFrame.FindExecutable;
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
    Format(FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'CantDeterminePath', 'Не удалось автоматически определить путь для "%s"%sУказать вручную?'), [Caption, sLineBreak])
  ) then
    btnSelectExecutable.Click;
end;


function TLaunchFrame.ExecatableEnable: Boolean;
begin
  Result := btnLaunch.Enabled;
end;


procedure TLaunchFrame.ChangeLanguage(Language: TLanguage);
var
  i: Integer;
begin
  FLanguage := Language;

  lblTitle.Caption := FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'Application.' + FLocaleID, FCaption);
  btnLaunch.Caption := FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'Launch', 'Запустить');
  btnStop.Caption := FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'Stop', 'Остановить');
  btnShowCommandLine.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'ShowCommandLine', 'Показать параметры запуска');
  btnExecutableOpenDirectory.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'ExecutableOpenDirectory', 'Открыть каталог в проводнике');
  btnSelectExecutable.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'SelectExecutable', 'Выбрать файл для запуска');
  btnOpenJournalDirectory.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'OpenJournalDirectory', 'Открыть каталог в проводнике');
  btnSelectJournalDirectory.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'SelectJournalDirectory', 'Выбрать каталог журналов');
  btnClearJournalDir.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'ClearJournalDir', 'Очистить каталог');
  edExecutable.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'Executable', 'Файл для запуска');
  edAdditionalCommandLine.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'AdditionalCommandLine', 'Дополнительные параметры запуска');
  edJournalDir.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'JournalDir', 'Каталог с журналами');
  cbEraseJournalDirBeforeRun.Caption := FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'EraseJournalDirBeforeRun', 'Очищать каталог перед запуском');
  OpenDialog.Filter := Format('%s (*.exe)|*.exe', [FLanguage.GetLocalizedString(PREFIX_TAB_LAUNCH + 'Applications', 'Приложения')]);

  //Перевести редакторы параметров
  for i := 0 to pnlContent.ControlCount - 1 do
  begin
    if pnlContent.Controls[i] is TParamFrameSimpleFrame then
      (pnlContent.Controls[i] as TParamFrameSimpleFrame).ChangeLanguage(FLanguage);
  end;
end;


procedure TLaunchFrame.SaveSettings(const FileName: String);
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


procedure TLaunchFrame.LoadSettings(const FileName: String);
var
  F: TIniFile;
  i: Integer;
begin
  F := TIniFile.Create(FileName);
  try
    //Настройки запуска
    SetValue(F.ReadString(SECTION_Executable, PARAM_APP, ''));
    edAdditionalCommandLine.Text := F.ReadString(SECTION_Executable, PARAM_CMD_LINE, '');

    //Очистка каталога перед запуском
    cbEraseJournalDirBeforeRun.Checked := F.ReadBool(SECTION_Executable, PARAM_ERASE_JOURNAL, False);
    edJournalDir.Text := F.ReadString(SECTION_Executable, PARAM_ERASE_JOURNAL_DIRECTORY, '');

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


procedure TLaunchFrame.LoadItemsSettings(const FileName: String);
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


procedure TLaunchFrame.OnChangeContentHeight(Sender: TObject);
begin
  //Поправить элементы внутри
  ArrangeContentPanelItems;

  //Поправить общую высоту
  Height := GetTotalFrameHeight;

  //Вызвать обработчик изменения размеров
  DoHeightChange;
end;


procedure TLaunchFrame.DoHeightChange;
begin
  if Assigned(FOnHeightChange) then
    FOnHeightChange(Self);
end;


procedure TLaunchFrame.ClearLogDirectory;
var
  Dir: String;
begin
  Dir := Trim(edJournalDir.Text);

  //Удалить содержимое каталога
  if Dir <> '' then
    DeleteFolderToRecycle(Dir);
end;


procedure TLaunchFrame.SetIcon(AIcon: TIcon);
begin
  FIcon.Assign(AIcon);
end;


procedure TLaunchFrame.SetCollapsed(ACollapsed: Boolean);
begin
  if FCollapsed = ACollapsed then
    Exit;

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


procedure TLaunchFrame.SetHighlight(AHighlight: Boolean);
begin
  if FHiglight = AHighlight then
    Exit;

  FHiglight := AHighlight;

  //Изменить фоновый цвет
  Self.ParentColor := not FHiglight;
  Self.ParentBackground := not FHiglight;

  if FHiglight then
    Color := cl3DLight
  else
    Color := clDefault;
end;


procedure TLaunchFrame.SetValue(AValue: String);
var
  IsEnable: Boolean;
begin
  edExecutable.Text := Trim(AValue);

  IsEnable := FileExists(edExecutable.Text);
  btnLaunch.Enabled := IsEnable;
  btnStop.Enabled := IsEnable;
end;



end.


