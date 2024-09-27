unit DataExtractorUnit;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Controls, Graphics, Dialogs, Buttons, ComCtrls, windows,
  Language, ExtractWorker,
  DialogCommonUnit, Forms, StdCtrls;

type
  TDataExtractorForm = class(TDialogCommonForm)
    btnClose: TSpeedButton;
    btnExtract: TSpeedButton;
    lblProgressInfo: TLabel;
    pbExtract: TProgressBar;
    procedure btnCloseClick(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormResize(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'Tools.DataExtractor.';

      SECTION_SYSTEM = 'System';
      PARAM_PROJECT_DIRECTORY = 'ProjectDirectory';
  private
    type
      TButtonType = (btExtract, btStop);
      TWorkMode = (wmExtract, wmBreak);
      TExtractStage = (esPBO, esBin, esRvmat);
  private
    procedure MessageHook(var Msg: TMessage); message WM_EXTRACT_WORKER;
  private
    FWorkMode: TWorkMode;
    FExtractor: TExtractWorker;
    FCurrentExtractStageName: String;

    function  GetProjectFolder: String;
    procedure RunExtract;
    procedure SetButtonCaption(AType: TButtonType);
    procedure CorrectCurrentStage(AStage: TExtractStage);
    procedure CorrectExtractLabel;
  protected
    procedure PrepareInterface; override;
    procedure SetLanguage; override;
  end;


procedure DataExtractorExecute(Language: TLanguage; const SettingsFile: String);


implementation

{$R *.lfm}

uses
  IniFiles,
  DialogParameters, SteamUtils,
  YesNoQuestionDialogUnit, MessageDialogUnit, SelectDirectoryDialogUnit;

type
  TDataExtractorParameters = class(TDialogParameters)
    SettingsFile: String;
  end;


procedure DataExtractorExecute(Language: TLanguage; const SettingsFile: String);
var
  Params: TDataExtractorParameters;
begin
  Params := TDataExtractorParameters.Create;
  Params.Language := Language;
  Params.SettingsFile := SettingsFile;
  try
    with TDataExtractorForm.Create(Params) do
    begin
      ShowModal;
      Free;
    end;

  finally
    Params.Free;
  end;
end;

procedure TDataExtractorForm.FormResize(Sender: TObject);
const
  SPACE_CLOSE = 10;
var
  W: Integer;
begin
  W := btnExtract.Width + btnClose.Width + SPACE_CLOSE;

  btnExtract.Left := (pnlButton.Width - W) div 2;
  btnClose.Left := btnExtract.Left + btnExtract.Width + SPACE_CLOSE;
end;


procedure TDataExtractorForm.MessageHook(var Msg: TMessage);
begin
  case Msg.wParam of
    WORKER_PBO_ERROR:
    begin
      FreeAndNil(FExtractor);
      btnExtract.Enabled := True;
      SetButtonCaption(btExtract);
      pbExtract.Position := 0;
      FWorkMode := wmExtract;
      lblProgressInfo.Visible := False;
      MessageDialogExecute(
        FParameters.Language,
        FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ExtractError', 'Произошла ошибка при рапаковке')
      );
    end;

    WORKER_PBO_COUNT:
    begin
      pbExtract.Max := Msg.lParam + 1;
      lblProgressInfo.Visible := True;
      CorrectCurrentStage(esPBO);
      CorrectExtractLabel;
    end;

    WORKER_PBO_STEP:
    begin
      pbExtract.StepIt;
      CorrectExtractLabel;
    end;


    WORKER_BIN_ERROR:
    begin
      FreeAndNil(FExtractor);
      btnExtract.Enabled := True;
      SetButtonCaption(btExtract);
      pbExtract.Position := 0;
      FWorkMode := wmExtract;
      lblProgressInfo.Visible := False;
      MessageDialogExecute(
        FParameters.Language,
        FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ConvertError', 'Произошла ошибка конвертировании')
      );
    end;

    WORKER_BIN_COUNT:
    begin
      pbExtract.Position := 0;
      pbExtract.Max := Msg.lParam + 1;
      lblProgressInfo.Visible := True;
      CorrectCurrentStage(esBin);
      CorrectExtractLabel;
    end;

    WORKER_BIN_STEP:
    begin
      pbExtract.StepIt;
      CorrectExtractLabel;
    end;


    WORKER_RVMAT_ERROR:
    begin
      FreeAndNil(FExtractor);
      btnExtract.Enabled := True;
      SetButtonCaption(btExtract);
      pbExtract.Position := 0;
      FWorkMode := wmExtract;
      lblProgressInfo.Visible := False;
      MessageDialogExecute(
        FParameters.Language,
        FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ConvertError', 'Произошла ошибка конвертировании')
      );
    end;

    WORKER_RVMAT_COUNT:
    begin
      pbExtract.Position := 0;
      pbExtract.Max := Msg.lParam + 1;
      lblProgressInfo.Visible := True;
      CorrectCurrentStage(esRvmat);
      CorrectExtractLabel;
    end;

    WORKER_RVMAT_STEP:
    begin
      pbExtract.StepIt;
      CorrectExtractLabel;
    end;


    WORKER_FINISH:
    begin
      FreeAndNil(FExtractor);
      btnExtract.Enabled := True;
      SetButtonCaption(btExtract);
      pbExtract.Position := 0;
      FWorkMode := wmExtract;
      lblProgressInfo.Visible := False;
      MessageDialogExecute(
        FParameters.Language,
        FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Success', 'Данные успешно распакованы')
      );
    end;

    WORKER_CANCEL:
    begin
      FreeAndNil(FExtractor);
      btnExtract.Enabled := True;
      SetButtonCaption(btExtract);
      pbExtract.Position := 0;
      FWorkMode := wmExtract;
      lblProgressInfo.Visible := False;
      MessageDialogExecute(
        FParameters.Language,
        FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Cancel', 'Прервано пользователем')
      );
    end;
  end;
end;


function TDataExtractorForm.GetProjectFolder: String;
var
  F: TIniFile;
begin
  F := TIniFile.Create((FParameters as TDataExtractorParameters).SettingsFile);
  try
    Result := F.ReadString(SECTION_SYSTEM, PARAM_PROJECT_DIRECTORY, '');
  finally
    F.Free;
  end;
end;


procedure TDataExtractorForm.RunExtract;
var
  GameFolder, ProjectFolder, ExtractTool, CfgConverterTool: String;
begin
  //Найдем утилиту распаковки
  ExtractTool := GetDayZToolsInstallPathFromRegistry + 'Bin\PboUtils\BankRev.exe';
  if not FileExists(ExtractTool) then
  begin
    MessageDialogExecute(
      FParameters.Language,
      Format(
        FParameters.Language.GetLocalizedString(
          LANGUAGE_PREFIX + 'CantFindExtractTool',
          'Не найдена утилита распаковки. Продолжение невозможно%sУстановите DayzTools и повторите действие.'
        ),
        [sLineBreak]
      )
    );
    Exit;
  end;

  //Найдем утилиту конвертирования Bin -> Cpp
  CfgConverterTool := GetDayZToolsInstallPathFromRegistry + 'Bin\CfgConvert\CfgConvert.exe';
  if not FileExists(CfgConverterTool) then
  begin
    MessageDialogExecute(
      FParameters.Language,
      Format(
        FParameters.Language.GetLocalizedString(
          LANGUAGE_PREFIX + 'CantFindCfgConverterTool',
          'Не найдена утилита конвертирования Bin в Cpp. Продолжение невозможно%sУстановите DayzTools и повторите действие.'
        ),
        [sLineBreak]
      )
    );
    Exit;
  end;

  //Проверим каталог с игрой
  GameFolder := GetSteamInstallPathFromRegistry + 'DayZ';
  if not DirectoryExists(GameFolder) then
  begin
    case YesNoQuestionDialogExecute(
      FParameters.Language,
      FParameters.Language.GetLocalizedString(
        LANGUAGE_PREFIX + 'CantFindGameFolder',
        'Не найден каталог с игрой. Укажите вручную?')
    ) of
      True:
      begin
        if not SelectDirectoryDialogExecute(FParameters.Language, GameFolder) then
          Exit;
      end;

      False:
        Exit;
    end;
  end;


  //Найдем куда распаковывать
  ProjectFolder := GetProjectFolder;
  if not DirectoryExists(ProjectFolder) then
  begin
    case YesNoQuestionDialogExecute(
      FParameters.Language,
      FParameters.Language.GetLocalizedString(
        LANGUAGE_PREFIX + 'CantFindProjectFolder',
        'Не найден каталог проекта. Укажите вручную?')
    ) of
      True:
      begin
        if not SelectDirectoryDialogExecute(FParameters.Language, GameFolder) then
          Exit;
      end;

      False:
        Exit;
    end;
  end;

  //Создать джамшута
  FExtractor := TExtractWorker.Create(ExtractTool, CfgConverterTool, ProjectFolder, GameFolder, Handle);
  FExtractor.Resume;
end;


procedure TDataExtractorForm.SetButtonCaption(AType: TButtonType);
var
  s: String;
begin
  s := '';

  case AType of
    btExtract:
      s := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Extract', 'Распаковать');

    btStop:
      s := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Stop', 'Остановить');
  end;

  btnExtract.Caption := s;
end;


procedure TDataExtractorForm.CorrectCurrentStage(AStage: TExtractStage);
begin
  case AStage of
    esPBO:
      FCurrentExtractStageName := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ExtractPBO', 'Распаковка PBO');

    esBin:
      FCurrentExtractStageName := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ConvertBin', 'Конвертирование Bin -> Cpp');

    esRvmat:
      FCurrentExtractStageName := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ConvertRvmat', 'Конвертирование Rvmat');
  end;
end;


procedure TDataExtractorForm.CorrectExtractLabel;
begin
  lblProgressInfo.Caption := Format('%s [%d/%d]', [FCurrentExtractStageName, pbExtract.Position + 1, pbExtract.Max]);
end;


procedure TDataExtractorForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TDataExtractorForm.btnExtractClick(Sender: TObject);

begin
  case FWorkMode of
    wmExtract:
    begin
      RunExtract;
      FWorkMode := wmBreak;

      //Поправить интерфейс
      SetButtonCaption(btStop);
      pbExtract.Min := 0;
      pbExtract.Position := 0;
      pbExtract.Max := 1;
    end;

    wmBreak:
    begin
      FExtractor.Cancel := True;
      btnExtract.Enabled := False;
    end;
  end;
end;


procedure TDataExtractorForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not Assigned(FExtractor);
end;


procedure TDataExtractorForm.PrepareInterface;
begin
  FWorkMode := wmExtract;
end;


procedure TDataExtractorForm.SetLanguage;
begin
  Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Caption', 'Извлечь данные');
  btnClose.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Close', 'Закрыть');
  SetButtonCaption(btExtract);
end;



end.

