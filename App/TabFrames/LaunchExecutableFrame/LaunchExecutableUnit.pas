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
    OpenDialog: TOpenDialog;
    sbSelectJournalDirectory: TSpeedButton;
    ScrollBox1: TScrollBox;
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
    procedure edExecutableChange(Sender: TObject);
    procedure sbSelectJournalDirectoryClick(Sender: TObject);
    procedure sbSelectExecutableClick(Sender: TObject);
  private
    FItems: TStartParamArray;
    FParameterFile: String;
    FSettingsFile: String;

    procedure ClearInterface;
    procedure PrepareInterface(Items: TStartParamArray);

    function  GetCommandLine: String;

    procedure SaveSettings(const FileName: String);
    procedure LoadSettings(const FileName: String);
  public
    constructor Create(const ACaption: string; AIcon: TIcon; const ParameterFile: String; const SettingsFile: String); reintroduce;
    destructor Destroy; override;

    procedure Launch;
    procedure Stop;

    property Items: TStartParamArray read FItems;
    property ParameterFile: String read FParameterFile;
    property SettingsFile: String read FSettingsFile;
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


procedure TLaunchExecutableFrame.btnShowCommandLineClick(Sender: TObject);
begin
  MemoDialogExecute('Параметры запуска', GetCommandLine);
end;


procedure TLaunchExecutableFrame.btnStopClick(Sender: TObject);
var
  Fn: String;
begin
  Fn := Trim(edExecutable.Text);
  if FileExists(Fn) then
    KillProcess(ExtractFileName(Fn));
end;


procedure TLaunchExecutableFrame.btnLaunchClick(Sender: TObject);
var
  Dir: String;
begin
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


procedure TLaunchExecutableFrame.edExecutableChange(Sender: TObject);
begin
  btnLaunch.Enabled := FileExists(edExecutable.Text);
  btnStop.Enabled := FileExists(edExecutable.Text);
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
  for i := Items.Count - 1 downto 0 do
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
        FrameItem := TParamFrameDirectoryListFrame.Create(Item);

      else
        ShowMessage(Format('Неизвестный тип фрейма %s', [Item.&Type]));
    end;

    //Заполнить фрейм если создан
    if FrameItem <> nil then
    begin
      FrameItem.Parent := pnlContent;
      FrameItem.Align := alTop;
    end;
  end;
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


constructor TLaunchExecutableFrame.Create(const ACaption: string; AIcon: TIcon; const ParameterFile: String; const SettingsFile: String);
begin
  inherited Create(ACaption, AIcon);

  FParameterFile := ParameterFile;
  FSettingsFile := SettingsFile;

  FItems := TStartParamArray.Create;
  FItems.LoadFromFile(FParameterFile);

  PrepareInterface(FItems);

  LoadSettings(FSettingsFile);
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
  Item: TStartParamSimple;
begin
  F := TIniFile.Create(FileName);
  try

    //Настройки запуска
    F.WriteString(SECTION_Executable, PARAM_APP, edExecutable.Text);
    F.WriteString(SECTION_Executable, PARAM_CMD_LINE, edAdditionalCommandLine.Text);

    //Очистка каталога перед запуском
    F.WriteBool(SECTION_Executable, PARAM_ERASE_JOURNAL, cbEraseJournalDirBeforeRun.Checked);
    F.WriteString(SECTION_Executable, PARAM_ERASE_JOURNAL_DIRECTORY, edJournalDir.Text);

    //Записать параметры
    for i := 0 to pnlContent.ControlCount - 1 do
    begin
      if not (pnlContent.Controls[i] is TParamFrameSimpleFrame) then
        Continue;

      Item := (pnlContent.Controls[i] as TParamFrameSimpleFrame).Item;
      F.WriteString(SECTION_PARAMS, Item.Name, Item.ValueToString);
    end;

  finally
    F.Free;
  end;
end;


procedure TLaunchExecutableFrame.LoadSettings(const FileName: String);
var
  F: TIniFile;
  i: Integer;
  Item: TStartParamSimple;
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

      Item := (pnlContent.Controls[i] as TParamFrameSimpleFrame).Item;
      Item.ValueFromString(F.ReadString(SECTION_PARAMS, Item.Name, ''));

      //Поправить интерфейс
      (pnlContent.Controls[i] as TParamFrameSimpleFrame).UpdateInterface;
    end;

  finally
    F.Free;
  end;
end;



end.

