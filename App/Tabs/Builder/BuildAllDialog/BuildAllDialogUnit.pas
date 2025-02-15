unit BuildAllDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, ComCtrls, ExtCtrls, StdCtrls, SysUtils, Forms, Controls, Classes,
  Graphics, Dialogs, windows,
  Language, DialogParameters, DialogCommonUnit, BuilderUtils, BuilderTask;

type
  TBuildAllDialogForm = class(TDialogCommonForm)
    btnClose: TSpeedButton;
    btnSkip: TSpeedButton;
    btnErrorInfo: TSpeedButton;
    btnStartStop: TSpeedButton;
    ilButton: TImageList;
    imTitle: TImage;
    lblProgressInfo: TLabel;
    lblTitle: TLabel;
    pbProgress: TProgressBar;
    Timer: TTimer;
    procedure btnCloseClick(Sender: TObject);
    procedure btnErrorInfoClick(Sender: TObject);
    procedure btnSkipClick(Sender: TObject);
    procedure btnStartStopClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormResize(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'TabBuilder.BuildAllDialog.';

    procedure CorrectTaskInfo(Index: Integer);
    procedure StartTask(Index: Integer);
    procedure CorrectProgressLabel(Index: Integer);

    procedure WM_BuilderEvent(var Msg: TMessage); message WM_BUILDER_EVENT;
  private
    FCurrentTask: TBuilderTask;
    FCurrentIndex: Integer;
    FCancel: Boolean;
    FWorking: Boolean;
    FErrorList: TStringList;

    procedure CreateTask(Data: TBuilderItemBuildData);
    procedure DestroyTask;
    procedure SetError(str: String);
    function  GetErrorWithoutEmptyLines: String;
  private
    type
      TButtonState = (
        bsInit,       //Начальное состояние
        bsWork,       //Работа сборщика
        bsStopping,   //Остановка
        bsBuildError, //Ошибки сборки
        bsDone        //Все собрано без ошибок
      );

    procedure SetButtonState(State: TButtonState);
  protected
    procedure PrepareInterface; override;
    procedure SetLanguage; override;

  public
    constructor Create(Parameters: TDialogParameters); reintroduce;
    destructor  Destroy; override;
  end;


procedure BuildAllDialogExecute(Language: TLanguage; ABuildParams: TBuilderItemBuildDataList);


implementation

{$R *.lfm}

type
  TBuildAllDialogParameters = class(TDialogParameters)
    BuildParams: TBuilderItemBuildDataList;
  end;


procedure BuildAllDialogExecute(Language: TLanguage; ABuildParams: TBuilderItemBuildDataList);
var
  Params: TBuildAllDialogParameters;
begin
  Params := TBuildAllDialogParameters.Create;
  Params.Language := Language;
  Params.BuildParams := ABuildParams;
  try
    with TBuildAllDialogForm.Create(Params) do
    begin
      ShowModal;
      Free;
    end;

  finally
    Params.Free;
  end;
end;


procedure TBuildAllDialogForm.FormResize(Sender: TObject);
const
  INDENT = 10;
var
  w: Integer;
begin
  w := (btnClose.Width + INDENT + btnStartStop.Width);
  btnStartStop.Left := (pnlButton.Width - w) div 2;
  btnClose.Left := btnStartStop.Left + btnStartStop.Width + INDENT;
  btnSkip.Left := btnStartStop.Left - INDENT - btnSkip.Width;
end;


procedure TBuildAllDialogForm.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;

  btnStartStopClick(Self);
end;


procedure TBuildAllDialogForm.btnStartStopClick(Sender: TObject);
begin
  if FWorking then
  begin
    FCancel := True;
    SetButtonState(bsStopping);
  end
  else
  begin
    StartTask(FCurrentIndex);
  end;
end;


procedure TBuildAllDialogForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not FWorking;
end;


procedure TBuildAllDialogForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TBuildAllDialogForm.btnErrorInfoClick(Sender: TObject);
begin
  ShowMessage(GetErrorWithoutEmptyLines);
end;


procedure TBuildAllDialogForm.btnSkipClick(Sender: TObject);
var
  Params: TBuildAllDialogParameters;
begin
  Params := FParameters as TBuildAllDialogParameters;

  //Пропустим текущую настройку
  Inc(FCurrentIndex);

  //Если это конец, то завершить работу
  if FCurrentIndex = Length(Params.BuildParams) - 1 then
  begin
    SetButtonState(bsDone);
    Exit;
  end;

  StartTask(FCurrentIndex);
end;


procedure TBuildAllDialogForm.CorrectTaskInfo(Index: Integer);
const
  INDENT = 10;
var
  Params: TBuildAllDialogParameters;
  Data: TBuilderItemBuildData;
  x, tw: Integer;
begin
  Params := FParameters as TBuildAllDialogParameters;
  Data := Params.BuildParams[Index];

  //Прогресс
  pbProgress.Position := Index + 1;

  //Надпись
  CorrectProgressLabel(Index + 1);

  //Иконка
  imTitle.Picture.Assign(Data.Icon.Picture);
  imTitle.Visible := True;

  //Текст
  lblTitle.Caption := Data.Title;
  lblTitle.Visible := True;

  //Подвинуть текст и иконку
  tw := lblTitle.Canvas.GetTextWidth(Data.Title);
  x := (pnlContent.Width - (imTitle.Width + INDENT + tw)) div 2;
  imTitle.Left := x;
  lblTitle.Left := imTitle.Left + imTitle.Width + INDENT;
end;


procedure TBuildAllDialogForm.StartTask(Index: Integer);
var
  Params: TBuildAllDialogParameters;
  Data: TBuilderItemBuildData;
begin
  Params := FParameters as TBuildAllDialogParameters;
  Data := Params.BuildParams[Index];

  FWorking := True;
  CorrectTaskInfo(Index);
  SetButtonState(bsWork);
  CreateTask(Data);
end;


procedure TBuildAllDialogForm.CorrectProgressLabel(Index: Integer);
var
  Params: TBuildAllDialogParameters;
begin
  Params := FParameters as TBuildAllDialogParameters;

  //Надпись
  lblProgressInfo.Caption := Format('%d/%d', [Index, Length(Params.BuildParams)]);
end;


procedure TBuildAllDialogForm.WM_BuilderEvent(var Msg: TMessage);
var
  Params: TBuildAllDialogParameters;
begin
  Params := FParameters as TBuildAllDialogParameters;

  //Проверить состояние задачи
  case FCurrentTask.Result of
    btrBuildFail:
    begin
      FWorking := False;

      SetError(FCurrentTask.Output.Text);
      SetButtonState(bsBuildError);
    end;

    btrCantCreateIncFile:
    begin
      FWorking := False;

      SetError(FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'CantCreateExtFile', 'Не удалось создать файл с расширениями'));
      SetButtonState(bsBuildError);
    end;

    btrCantFindBuildTool:
    begin
      FWorking := False;

      SetError(FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'CantFindAddonBuilder', 'Не найден Addon Builder. Переустановите Dayz Tools.'));
      SetButtonState(bsBuildError);
    end;

    btrSourceDirectoryNotExist:
    begin
      FWorking := False;

      SetError(FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'NoSrcDir', 'Нет исходного каталога'));
      SetButtonState(bsBuildError);
    end;

    btrDestinationDirectoryNotExist:
    begin
      FWorking := False;

      SetError(FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'NoDstDir', 'Нет каталога назначения'));
      SetButtonState(bsBuildError);
    end;


    btrOK:
    begin
      FWorking := False;

      //Если это конец, то завершить работу
      if FCurrentIndex = Length(Params.BuildParams) - 1 then
      begin
        SetButtonState(bsDone);
        Exit;
      end;

      //Если прервана работа по кнопке
      if FCancel then
      begin
        FCancel := False;
        SetButtonState(bsInit);
        Exit;
      end;

      //Не конец, перейти на следующую задачу
      Inc(FCurrentIndex);
      StartTask(FCurrentIndex);
    end;
  end;
end;


procedure TBuildAllDialogForm.CreateTask(Data: TBuilderItemBuildData);
begin
  DestroyTask;

  FCurrentTask := TBuilderTask.Create(Handle, Data);
  FCurrentTask.Resume;
end;


procedure TBuildAllDialogForm.DestroyTask;
begin
  if Assigned(FCurrentTask) then
    FreeAndNil(FCurrentTask);
end;


procedure TBuildAllDialogForm.SetError(str: String);
begin
  FErrorList.Text := str;
end;


function TBuildAllDialogForm.GetErrorWithoutEmptyLines: String;
var
  List: TStringList;
  i: Integer;
  s: String;
begin
  List := TStringList.Create;
  List.LineBreak := sLineBreak;
  try
    for i := 0 to FErrorList.Count - 1 do
    begin
      s := Trim(FErrorList.Strings[i]);
      if s <> '' then
        List.Add(s);
    end;

    Result := List.Text;
  finally
    List.Free;
  end;
end;


procedure TBuildAllDialogForm.SetButtonState(State: TButtonState);
begin
  case State of
    bsInit:
    begin
      btnClose.Enabled := True;
      btnSkip.Visible := False;
      btnErrorInfo.Visible := False;
      btnStartStop.Visible := True;
      btnStartStop.Enabled := True;
      btnStartStop.ImageIndex := 0;
      btnStartStop.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Build', 'Собрать');
    end;

    bsWork:
    begin
      btnClose.Enabled := False;
      btnSkip.Visible := False;
      btnErrorInfo.Visible := False;
      btnStartStop.Visible := True;
      btnStartStop.Enabled := True;
      btnStartStop.ImageIndex := 1;
      btnStartStop.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Stop', 'Остановить');
    end;

    bsStopping:
    begin
      btnClose.Enabled := False;
      btnSkip.Visible := False;
      btnErrorInfo.Visible := False;
      btnStartStop.Visible := True;
      btnStartStop.Enabled := False;
      btnStartStop.ImageIndex := 0;
      btnStartStop.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Build', 'Собрать');
    end;

    bsBuildError:
    begin
      btnClose.Enabled := True;
      btnSkip.Visible := True;
      btnErrorInfo.Visible := True;
      btnStartStop.Visible := True;
      btnStartStop.Enabled := True;
      btnStartStop.ImageIndex := 0;
      btnStartStop.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Build', 'Собрать');
    end;

    bsDone:
    begin
      btnClose.Enabled := True;
      btnSkip.Visible := False;
      btnErrorInfo.Visible := False;
      btnStartStop.Visible := False;
    end;
  end;
end;


procedure TBuildAllDialogForm.PrepareInterface;
var
  Params: TBuildAllDialogParameters;
  DataList: TBuilderItemBuildDataList;
begin
  Params := FParameters as TBuildAllDialogParameters;
  DataList := Params.BuildParams;

  //Настроим прогресс
  pbProgress.Min := 0;
  pbProgress.Max := Length(DataList);
  CorrectProgressLabel(0);

  //Флаги
  FCancel := False;
  FWorking := False;

  //Кнопки
  SetButtonState(bsInit);
end;


procedure TBuildAllDialogForm.SetLanguage;
begin
  Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Title', 'Сборка');
  btnClose.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Close', 'Закрыть');
  btnSkip.Hint := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Skip', 'Пропустить');
  btnErrorInfo.Hint := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Details', 'Подробности');
end;


constructor TBuildAllDialogForm.Create(Parameters: TDialogParameters);
begin
  FErrorList := TStringList.Create;

  inherited Create(Parameters);
end;


destructor TBuildAllDialogForm.Destroy;
begin
  //Почистим
  DestroyTask;
  FErrorList.Free;

  inherited Destroy;
end;





end.

