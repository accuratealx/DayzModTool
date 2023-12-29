unit LaunchExecutableUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Graphics, IniFiles,
  TabCommonUnit, StartParamArray;

type
  TLaunchExecutableFrame = class(TTabCommonFrame)
    btnClearJournal: TSpeedButton;
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
    sbSelectJournalDirectory: TSpeedButton;
    ContentScrollBox: TScrollBox;
    pnlContent: TPanel;
    pnlButton: TPanel;
    btnLaunch: TSpeedButton;
    btnShowCommandLine: TSpeedButton;
    sbSelectExecutable: TSpeedButton;
    procedure btnClearJournalClick(Sender: TObject);
    procedure btnLaunchClick(Sender: TObject);
    procedure btnExecutableOpenDirectoryClick(Sender: TObject);
    procedure btnOpenJournalDirectoryClick(Sender: TObject);
    procedure btnShowCommandLineClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure sbSelectJournalDirectoryClick(Sender: TObject);
    procedure sbSelectExecutableClick(Sender: TObject);
    procedure btnCollapseClick(Sender: TObject);
  private
    FItems: TStartParamArray;
    FParameterFile: String;
    FSettingsFile: String;
    FCollapsed: Boolean;
    FHiglight: Boolean;

    procedure ClearInterface;
    procedure PrepareInterface(Items: TStartParamArray);
    procedure ArrangeContentPanelItems;
    function  GetTotalFrameHeight: Integer;
    function  GetCommandLine: String;
    function  IsExecutableReady: Boolean;

    procedure SaveSettings(const FileName: String);
    procedure LoadSettings(const FileName: String);
    procedure LoadItemsSettings(const FileName: String);

    procedure OnChangeContentHeight(Sender: TObject);

    procedure SetCollapsed(ACollapsed: Boolean);
    procedure SetHighlight(AHighlight: Boolean);
  public
    constructor Create(const ACaption: string; AIcon: TIcon; const ParameterFile: String; const SettingsFile: String); reintroduce;
    destructor Destroy; override;

    procedure Launch;
    procedure Stop;

    property Items: TStartParamArray read FItems;
    property ParameterFile: String read FParameterFile;
    property SettingsFile: String read FSettingsFile;
    property Collapsed: Boolean read FCollapsed write SetCollapsed;
    property Higlight: Boolean read FHiglight write SetHighlight;
  end;

  TLaunchExecutableFrameList = array of TLaunchExecutableFrame;


implementation

{$R *.lfm}

uses
  DayZUtils,
  MemoDialogUnit, SelectDirectoryDialogUnit,
  StartParamSimple,
  ParamFrameCommonUnit,
  ParamFrameSimpleUnit, ParamFrameIntegerUnit, ParamFrameStringUnit,
  ParamFrameDirectoryUnit, ParamFrameFileUnit, ParamFrameDirectoryListUnit;

const
  SECTION_PARAMS = 'Params';
  SECTION_EXECUTABLE = 'Executable';
  PARAM_APP = 'Application';
  PARAM_CMD_LINE = 'CmdLine';
  PARAM_ERASE_JOURNAL = 'EraseJournalBeforeRun';
  PARAM_ERASE_JOURNAL_DIRECTORY = 'JournalDirectory';
  PARAM_COLLAPSED = 'Collapsed';


procedure TLaunchExecutableFrame.btnShowCommandLineClick(Sender: TObject);
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
  MemoDialogExecute('Параметры запуска',  ParamStr);
end;


procedure TLaunchExecutableFrame.btnStopClick(Sender: TObject);
var
  Fn: String;
begin
  //Проверка на отсутствие файла
  if not IsExecutableReady then
    Exit;

  //Убить процесс по имени
  Fn := Trim(edExecutable.Text);
  KillProcess(ExtractFileName(Fn));
end;


procedure TLaunchExecutableFrame.btnLaunchClick(Sender: TObject);
var
  Dir: String;
begin
  //Проверка на отсутствие файла
  if not IsExecutableReady then
    Exit;

  Dir := Trim(edJournalDir.Text);

  //Удалить содержимое каталога
  if cbEraseJournalDirBeforeRun.Checked and (Dir <> '') then
    DeleteFolderToRecycle(Dir);

  //Launch application
  ExecuteFile(Trim(edExecutable.Text), GetCommandLine);
end;


procedure TLaunchExecutableFrame.btnClearJournalClick(Sender: TObject);
begin
  edJournalDir.Text := '';
end;


procedure TLaunchExecutableFrame.btnExecutableOpenDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := ExtractFilePath(Trim(edExecutable.Text));
  if DirectoryExists(Dir) then
    OpenFolderInExplorer(Dir);
end;


procedure TLaunchExecutableFrame.btnOpenJournalDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := edJournalDir.Text;
  if DirectoryExists(Dir) then
    OpenFolderInExplorer(Dir);
end;


procedure TLaunchExecutableFrame.sbSelectJournalDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := edJournalDir.Text;

  if SelectDirectoryDialogExecute('Выберите каталог', Dir) then
  begin
    edJournalDir.Text := Dir;
  end;
end;


procedure TLaunchExecutableFrame.sbSelectExecutableClick(Sender: TObject);
var
  Dir, Fn: String;
begin
  Dir := ExtractFilePath(edExecutable.Text);
  Fn := ExtractFileName(edExecutable.Text);

  OpenDialog.InitialDir := Dir;
  OpenDialog.FileName := Fn;
  if OpenDialog.Execute then
  begin
    edExecutable.Text := OpenDialog.FileName;
  end;
end;


procedure TLaunchExecutableFrame.btnCollapseClick(Sender: TObject);
begin
  Collapsed := not Collapsed;
end;


procedure TLaunchExecutableFrame.ClearInterface;
var
  i: Integer;
begin
  for i := pnlContent.ControlCount - 1 downto 0 do
    pnlContent.Controls[i].Free;
end;


procedure TLaunchExecutableFrame.PrepareInterface(Items: TStartParamArray);
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


procedure TLaunchExecutableFrame.ArrangeContentPanelItems;
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


function TLaunchExecutableFrame.GetTotalFrameHeight: Integer;
begin
  Result := pnlButton.Height + pnlContent.Height;
end;


function TLaunchExecutableFrame.GetCommandLine: String;
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


function TLaunchExecutableFrame.IsExecutableReady: Boolean;
begin
  Result := False;

  if not FileExists(Trim(edExecutable.Text)) then
  begin
    //Открыть диалог выбора файла
    sbSelectExecutable.Click;

    //Если после действий пользователя файл все таки выбрали
    if FileExists(Trim(edExecutable.Text)) then
      Result := True;
  end
  else
    Result := True;
end;


constructor TLaunchExecutableFrame.Create(const ACaption: string; AIcon: TIcon; const ParameterFile: String; const SettingsFile: String);
begin
  inherited Create(ACaption, AIcon);

  FParameterFile := ParameterFile;
  FSettingsFile := SettingsFile;

  FItems := TStartParamArray.Create;
  FItems.LoadFromFile(FParameterFile);

  LoadItemsSettings(FSettingsFile);

  PrepareInterface(FItems);

  LoadSettings(FSettingsFile);


  FCollapsed := True;
  imgCollapse.ImageIndex := 0;
  Height := 45;
end;


destructor TLaunchExecutableFrame.Destroy;
begin
  SaveSettings(FSettingsFile);

  FItems.Free;

  ClearInterface;

  inherited Destroy;
end;


procedure TLaunchExecutableFrame.Launch;
begin
  if btnLaunch.Enabled then
    btnLaunch.Click;
end;


procedure TLaunchExecutableFrame.Stop;
begin
  btnStop.Click;
end;


procedure TLaunchExecutableFrame.SaveSettings(const FileName: String);
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


procedure TLaunchExecutableFrame.LoadSettings(const FileName: String);
var
  F: TIniFile;
  i: Integer;
begin
  F := TIniFile.Create(FileName);
  try

    //Настройки запуска
    edExecutable.Text := F.ReadString(SECTION_Executable, PARAM_APP, '');
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
    Collapsed := F.ReadBool(SECTION_EXECUTABLE, PARAM_COLLAPSED, False);

  finally
    F.Free;
  end;
end;


procedure TLaunchExecutableFrame.LoadItemsSettings(const FileName: String);
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


procedure TLaunchExecutableFrame.OnChangeContentHeight(Sender: TObject);
begin
  ArrangeContentPanelItems;
  Height := GetTotalFrameHeight;
end;


procedure TLaunchExecutableFrame.SetCollapsed(ACollapsed: Boolean);
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
end;


procedure TLaunchExecutableFrame.SetHighlight(AHighlight: Boolean);
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



end.

