unit TimecalCulatorUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Buttons,
  Language, DialogParameters, DialogCommonUnit;

type

  { TTimeCalculatorForm }

  TTimeCalculatorForm = class(TDialogCommonForm)
    btnClose: TSpeedButton;
    btnCopyContent: TSpeedButton;
    lblDay: TLabel;
    lblNight: TLabel;
    mResult: TMemo;
    teDay: TTimeEdit;
    teNight: TTimeEdit;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyContentClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure teDayAcceptTime(Sender: TObject; var ATime: TDateTime; var AcceptTime: Boolean);
    procedure teDayChange(Sender: TObject);
    procedure teDayEditingDone(Sender: TObject);
    procedure teNightAcceptTime(Sender: TObject; var ATime: TDateTime; var AcceptTime: Boolean);
    procedure teNightChange(Sender: TObject);
    procedure teNightEditingDone(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'Tools.TimeCalculator.';

      SECTION_SYSTEM = 'System';
      PARAM_DAY = 'Day';
      PARAM_NIGHT = 'Night';
  private
    function FitValueToRange(Value: Single; Min: Single = 0.1; Max: Single = 64): Single;

    function TimeToFloatCoefficient(Time: TDateTime): Single;
    function FloatCoefficientToTime(coeff: Single): TDateTime;

    //День
    function FloatCoefficientToDayParameter(Coeff: Single): Single;
    function DayParameterToFloatCoefficient(DayParameter: Single): Single;
    function TimeToDayParameter(Time: TDateTime): Single;
    function DayParameterToTime(DayParameter: Single): TDateTime;

    //Ночь
    function FloatCoefficientToNightParameter(Coeff: Single; DayParameter: Single): Single;
    function NightParameterToFloatCoefficient(NightParameter, DayParameter: Single): Single;
    function TimeToNightParameter(Time: TDateTime; DayParameter: Single): Single;
    function NightParameterToTime(NightParameter, DayParameter: Single): TDateTime;

    procedure UpdateParams;
    procedure UpdateResultMemo(Day, Night: Single);
    procedure DisableTimeEditActions(ADisable: Boolean);

    procedure LoadSettings;
    procedure SaveSettings;
  protected
    procedure SetLanguage; override;
    procedure PrepareInterface; override;
  public
    constructor Create(Parameters: TDialogParameters); reintroduce;
    destructor  Destroy; override;
  end;


procedure TimeCalculatorExecute(Language: TLanguage; const SettingsFile: String);


implementation

{$R *.lfm}

uses
  Math, Clipbrd, IniFiles;

type
  TTimeCalculatorParameters = class(TDialogParameters)
    SettingsFile: String;
  end;

procedure TimeCalculatorExecute(Language: TLanguage; const SettingsFile: String);
var
  Params: TTimeCalculatorParameters;
begin
  Params := TTimeCalculatorParameters.Create;
  Params.Language := Language;
  Params.SettingsFile := SettingsFile;
  try
    with TTimeCalculatorForm.Create(Params) do
    begin
      ShowModal;
      Free;
    end;

  finally
    Params.Free;
  end;
end;


procedure TTimeCalculatorForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TTimeCalculatorForm.btnCopyContentClick(Sender: TObject);
begin
  Clipboard.AsText := mResult.Text;
end;


procedure TTimeCalculatorForm.FormResize(Sender: TObject);
begin
  btnClose.Left := (pnlButton.Width - btnClose.Width) div 2;
end;


procedure TTimeCalculatorForm.teDayAcceptTime(Sender: TObject; var ATime: TDateTime; var AcceptTime: Boolean);
begin
  UpdateParams;
end;


procedure TTimeCalculatorForm.teDayChange(Sender: TObject);
begin
  UpdateParams;
end;


procedure TTimeCalculatorForm.teDayEditingDone(Sender: TObject);
begin
	UpdateParams;
end;


procedure TTimeCalculatorForm.teNightAcceptTime(Sender: TObject; var ATime: TDateTime; var AcceptTime: Boolean);
begin
  UpdateParams;
end;


procedure TTimeCalculatorForm.teNightChange(Sender: TObject);
begin
  UpdateParams;
end;


procedure TTimeCalculatorForm.teNightEditingDone(Sender: TObject);
begin
  UpdateParams;
end;


function TTimeCalculatorForm.FitValueToRange(Value: Single; Min: Single; Max: Single): Single;
begin
  Result := Value;

  if Result < Min then
    Result := Min;
  if Result > Max then
    Result := Max;
end;


function TTimeCalculatorForm.TimeToFloatCoefficient(Time: TDateTime): Single;
var
  H, M, S, Ms: Word;
begin
  DecodeTime(Time, H, M, S, Ms);
  Result := H + (1 / 60 * M);
  Result := FitValueToRange(Result, 0.01, MaxSingle);
end;


function TTimeCalculatorForm.FloatCoefficientToTime(coeff: Single): TDateTime;
var
  H, M: Word;
begin
  H := Trunc(coeff);
  M := Round(Frac(coeff) / (1 / 60));
  Result := EncodeTime(H, M, 0, 0);
end;


function TTimeCalculatorForm.FloatCoefficientToDayParameter(Coeff: Single): Single;
begin
  Result := 12 / Coeff;
  Result := FitValueToRange(Result);
end;


function TTimeCalculatorForm.FloatCoefficientToNightParameter(Coeff: Single; DayParameter: Single): Single;
begin
  Result := 12 / Coeff / DayParameter;
  Result := FitValueToRange(Result);
end;


function TTimeCalculatorForm.DayParameterToFloatCoefficient(DayParameter: Single): Single;
begin
  Result := 12 / DayParameter;
end;


function TTimeCalculatorForm.TimeToDayParameter(Time: TDateTime): Single;
begin
  Result := FloatCoefficientToDayParameter(TimeToFloatCoefficient(Time));
end;


function TTimeCalculatorForm.DayParameterToTime(DayParameter: Single): TDateTime;
begin
  Result := FloatCoefficientToTime(DayParameterToFloatCoefficient(DayParameter));
end;


function TTimeCalculatorForm.NightParameterToFloatCoefficient(NightParameter, DayParameter: Single): Single;
begin
  Result := 12 / NightParameter / DayParameter;
end;


function TTimeCalculatorForm.TimeToNightParameter(Time: TDateTime; DayParameter: Single): Single;
begin
  Result := FloatCoefficientToNightParameter(TimeToFloatCoefficient(Time), DayParameter);
end;


function TTimeCalculatorForm.NightParameterToTime(NightParameter, DayParameter: Single): TDateTime;
begin
  Result := FloatCoefficientToTime(NightParameterToFloatCoefficient(NightParameter, DayParameter));
end;


procedure TTimeCalculatorForm.UpdateParams;
var
  DayP, NightP: Float;
  TimeD, TimeN: TDateTime;
begin
  //Получим параметр дня
  DayP := TimeToDayParameter(teDay.Time);

  //Получим время из параметра дня
  TimeD := DayParameterToTime(DayP);

  //Если время не совпадает, то поправить ввод
  if teDay.Time <> TimeD then
  begin
    //Поправим поле ввода
  	DisableTimeEditActions(True);
  	teDay.Time := TimeD;
  	DisableTimeEditActions(False);

    //Пересчитаем параметр дня, потому что время не совпало
    DayP := TimeToDayParameter(TimeD);
  end;

  //Получим параметр ночи
  NightP := TimeToNightParameter(teNight.Time, DayP);

  //Получим время из параметра ночи
  TimeN := NightParameterToTime(NightP, DayP);

  //Если время не совпало, то поправить ввод
  if teNight.Time <> TimeN then
  begin
    DisableTimeEditActions(True);
    teNight.Time := TimeN;
    DisableTimeEditActions(False);

    //Пересчитаем параметр ночи, потому что время не совпало
    NightP := TimeToNightParameter(TimeN, DayP);
  end;

  //Обновим результат
  UpdateResultMemo(DayP, NightP);
end;


procedure TTimeCalculatorForm.UpdateResultMemo(Day, Night: Single);
const
  FORMAT_STR = '%s = %f';
  DAY_PARAM = 'serverTimeAcceleration';
  NIGHT_PARAM = 'serverNightTimeAcceleration';
begin
  mResult.Clear;
  mResult.Lines.Add(Format(FORMAT_STR, [DAY_PARAM, Day]));
  mResult.Lines.Add(Format(FORMAT_STR, [NIGHT_PARAM, Night]));
  mResult.SelStart := 0;
end;


procedure TTimeCalculatorForm.DisableTimeEditActions(ADisable: Boolean);
begin
  if ADisable then
  begin
    teDay.OnEditingDone := @teDayEditingDone;
    teNight.OnEditingDone := @teNightEditingDone;
  end
  else
  begin
    teDay.OnEditingDone := nil;
    teNight.OnEditingDone := nil;
  end;
end;


procedure TTimeCalculatorForm.LoadSettings;
var
  F: TIniFile;
begin
  F := TIniFile.Create((FParameters as TTimeCalculatorParameters).SettingsFile);

  teDay.Text := F.ReadString(SECTION_SYSTEM, PARAM_DAY, '12:00');
  teNight.Text := F.ReadString(SECTION_SYSTEM, PARAM_NIGHT, '1:00');

  F.Free;
end;


procedure TTimeCalculatorForm.SaveSettings;
var
  F: TIniFile;
begin
  F := TIniFile.Create((FParameters as TTimeCalculatorParameters).SettingsFile);

  F.WriteString(SECTION_SYSTEM, PARAM_DAY, teDay.Text);
  F.WriteString(SECTION_SYSTEM, PARAM_NIGHT, teNight.Text);

  F.Free;
end;


procedure TTimeCalculatorForm.SetLanguage;
begin
  Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Caption', 'Калькулятор времени');
  btnCopyContent.Hint := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'CopyContent', 'Скопировать параметры в буфер обмена');;
  btnClose.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Close', 'Закрыть');
  lblDay.Caption:= FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'DayLength', 'Длительность дня');
  lblNight.Caption:= FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'NightLength', 'Длительность ночи');
end;


procedure TTimeCalculatorForm.PrepareInterface;
begin
  UpdateParams;
end;


constructor TTimeCalculatorForm.Create(Parameters: TDialogParameters);
begin
  inherited Create(Parameters);

  LoadSettings;
end;


destructor TTimeCalculatorForm.Destroy;
begin
  SaveSettings;

  inherited Destroy;
end;



end.

