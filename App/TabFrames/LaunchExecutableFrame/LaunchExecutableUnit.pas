unit LaunchExecutableUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, IniFiles,
  TabCommonUnit, StartParamArray;

type
  TLaunchExecutableFrame = class(TTabCommonFrame)
    btnOpenDirectory: TSpeedButton;
    edExecutable: TEdit;
    edAdditionalCommandLine: TEdit;
    OpenDialog: TOpenDialog;
    ScrollBox1: TScrollBox;
    pnlContent: TPanel;
    pnlButton: TPanel;
    btnLaunch: TSpeedButton;
    btnShowCommandLine: TSpeedButton;
    sbSelectExecutable: TSpeedButton;
    procedure btnShowCommandLineClick(Sender: TObject);
    procedure edExecutableChange(Sender: TObject);
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
    constructor Create(ParameterFile: String; SettingsFile: String); reintroduce;
    destructor Destroy; override;

    property Items: TStartParamArray read FItems;
    property ParameterFile: String read FParameterFile;
    property SettingsFile: String read FSettingsFile;
  end;
  TLaunchExecutableFrameClass = class of TLaunchExecutableFrame;


implementation

{$R *.lfm}

uses
  MemoDialogUnit,
  StartParamSimple,
  ParamFrameCommonUnit,
  ParamFrameSimpleUnit, ParamFrameIntegerUnit, ParamFrameStringUnit,
  ParamFrameDirectoryUnit, ParamFrameFileUnit, ParamFrameDirectoryListUnit;

const
  SECTION_PARAMS = 'Params';
  SECTION_EXECUTABLE = 'Executable';
  PARAM_APP = 'Application';
  PARAM_CMD_LINE = 'CmdLine';


procedure TLaunchExecutableFrame.btnShowCommandLineClick(Sender: TObject);
begin
  MemoDialogExecute('Параметры запуска', GetCommandLine);
end;


procedure TLaunchExecutableFrame.edExecutableChange(Sender: TObject);
begin
  btnLaunch.Enabled := FileExists(edExecutable.Text);
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

    case Item.&Type of

      'Simple':
      begin
        FrameItem := TParamFrameSimpleFrame.Create(Item);
        FrameItem.Parent := pnlContent;
        FrameItem.Align := alTop;
      end;

      'Integer':
      begin
        FrameItem := TParamFrameIntegerFrame.Create(Item);
        FrameItem.Parent := pnlContent;
        FrameItem.Align := alTop;
      end;

      'String':
      begin
        FrameItem := TParamFrameStringFrame.Create(Item);
        FrameItem.Parent := pnlContent;
        FrameItem.Align := alTop;
      end;

      'Directory':
      begin
        FrameItem := TParamFrameDirectoryFrame.Create(Item);
        FrameItem.Parent := pnlContent;
        FrameItem.Align := alTop;
      end;

      'File':
      begin
        FrameItem := TParamFrameFileFrame.Create(Item);
        FrameItem.Parent := pnlContent;
        FrameItem.Align := alTop;
      end;

      'DirectoryList':
      begin
        FrameItem := TParamFrameDirectoryListFrame.Create(Item);
        FrameItem.Parent := pnlContent;
        FrameItem.Align := alTop;
      end;

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


constructor TLaunchExecutableFrame.Create(ParameterFile: String; SettingsFile: String);
begin
  inherited Create(nil);

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

