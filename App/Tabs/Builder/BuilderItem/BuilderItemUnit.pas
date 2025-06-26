unit BuilderItemUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, Graphics,
  Dialogs, Language, BuilderUtils, Types;

type
  TBuilderItemFrame = class(TFrame)
    btnBuild: TSpeedButton;
    btnShowCommandLine: TSpeedButton;
    btnVersionClearValue: TSpeedButton;
    btnPrivateKeyClearValue: TSpeedButton;
    btnPrivateKeyOpenDirectory: TSpeedButton;
    btnPrefixClearValue: TSpeedButton;
    btnFileExtensionsClearValue: TSpeedButton;
    btnPrivateKeySelect: TSpeedButton;
    btnSourceDirectoryClearValue: TSpeedButton;
    btnDestinationDirectoryClearValue: TSpeedButton;
    btnSourceDirectoryOpenDir: TSpeedButton;
    btnDestinationDirectoryOpenDir: TSpeedButton;
    btnSourceDirectorySelect: TSpeedButton;
    btnDestinationDirectorySelect: TSpeedButton;
    cbSign: TCheckBox;
    cbDriveLetters: TComboBox;
    edVersion: TEdit;
    edPrivateKey: TEdit;
    edPrefix: TEdit;
    edFileExtensions: TEdit;
    edSourceDirectory: TEdit;
    edDestinationDirectory: TEdit;
    ilCollapse: TImageList;
    ilEnableBuild: TImageList;
    imgEnabledBuild: TImage;
    imgCollapse: TImage;
    imgIcon: TImage;
    lblVersion: TLabel;
    lblPrivateKey: TLabel;
    lblPrefix: TLabel;
    lblFileExtensions: TLabel;
    lblSourceDirectory: TLabel;
    lblDestinationDirectory: TLabel;
    lblTitle: TLabel;
    lblDriveLetter: TLabel;
    OpenDialog: TOpenDialog;
    pnlCollapse: TPanel;
    procedure btnBuildClick(Sender: TObject);
    procedure btnDestinationDirectoryClearValueClick(Sender: TObject);
    procedure btnDestinationDirectoryOpenDirClick(Sender: TObject);
    procedure btnDestinationDirectorySelectClick(Sender: TObject);
    procedure btnFileExtensionsClearValueClick(Sender: TObject);
    procedure btnPrefixClearValueClick(Sender: TObject);
    procedure btnPrivateKeyClearValueClick(Sender: TObject);
    procedure btnPrivateKeyOpenDirectoryClick(Sender: TObject);
    procedure btnPrivateKeySelectClick(Sender: TObject);
    procedure btnShowCommandLineClick(Sender: TObject);
    procedure btnSourceDirectoryClearValueClick(Sender: TObject);
    procedure btnSourceDirectoryOpenDirClick(Sender: TObject);
    procedure btnSourceDirectorySelectClick(Sender: TObject);
    procedure btnVersionClearValueClick(Sender: TObject);
    procedure cbDriveLettersKeyPress(Sender: TObject; var Key: char);
    procedure cbDriveLettersMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure edPrivateKeyChange(Sender: TObject);
    procedure edEditChange(Sender: TObject);
    procedure edVersionKeyPress(Sender: TObject; var Key: char);
    procedure FrameClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure imgEnabledBuildClick(Sender: TObject);
    procedure pnlCollapseClick(Sender: TObject);
    procedure pnlCollapsePaint(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'TabBuilder.';
  private
    FLanguage: TLanguage;
    FIconDirectory: String;
    FDriveLetters: TStringList; //Ссылка на список букв дисководов

    FIconName: String;
    FCollapsed: Boolean;
    FHighlight: Boolean;
    FSelected: Boolean;
    FEnabledBuild: Boolean;

    FOnHeightChange: TNotifyEvent;
    FOnSelect: TNotifyEvent;

    function  GetTotalFrameHeight: Integer;

    procedure SetTitle(ATitle: String);
    function  GetTitle: String;
    procedure SetSelected(ASelected: Boolean);
    procedure SetHighlight(AHighlight: Boolean);
    procedure SetCollapsed(ACollapsed: Boolean);
    procedure SetIconName(AIconName: String);
    procedure SetEnableBuild(AEnable: Boolean);

    procedure DoOnSelect;
    procedure DoHeightChange;

  public
    constructor Create(AIconDirectory: String; Letters: TStringList); reintroduce;
    constructor Create(AIconDirectory: String; Letters: TStringList; ASettings: String);

    procedure ValueFromString(const AValue: String);
    function  ValueToString: String;

    procedure CorrectButtonVisible;
    procedure CorrectDriveLetterBox;
    procedure ChangeLanguage(Language: TLanguage);

    function GetBuildData: TBuilderItemBuildData;

    property Collapsed: Boolean read FCollapsed write SetCollapsed;
    property Selected: Boolean read FSelected write SetSelected;
    property Highlight: Boolean read FHighlight write SetHighlight;
    property Title: String read GetTitle write SetTitle;
    property IconName: String read FIconName write SetIconName;
    property EnableBuild: Boolean read FEnabledBuild write SetEnableBuild;

    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnHeightChange: TNotifyEvent read FOnHeightChange write FOnHeightChange;
  end;

  TBuilderItemFrameList = array of TBuilderItemFrame;


implementation

{$R *.lfm}

uses
  DayZUtils, SteamUtils,
  SelectDirectoryDialogUnit, BuildDialogUnit, MessageDialogUnit, MemoDialogUnit;

const
  SEPARATOR = ';;;';


procedure TBuilderItemFrame.pnlCollapseClick(Sender: TObject);
begin
  if not FSelected then
  begin
    Selected := True;
    Exit;
  end;

  Collapsed := not Collapsed;
end;


procedure TBuilderItemFrame.pnlCollapsePaint(Sender: TObject);
begin
  if FSelected then
  begin
    pnlCollapse.Canvas.Pen.Color := clHighlight;
    pnlCollapse.Canvas.Pen.Style := psSolid;
    pnlCollapse.Canvas.Pen.Width := 5;
    pnlCollapse.Canvas.Line(0, 0, 0, pnlCollapse.Height);
  end;
end;


procedure TBuilderItemFrame.FrameClick(Sender: TObject);
begin
  Selected := True;
end;


procedure TBuilderItemFrame.FrameResize(Sender: TObject);
begin
  //Костыль, потому что панель не изменяет свои размеры через свойство, скорее всего из-за невидимости TControl
  pnlCollapse.Width := imgEnabledBuild.Left - 10;
  lblTitle.Width := pnlCollapse.Width - lblTitle.Left;
end;


procedure TBuilderItemFrame.imgEnabledBuildClick(Sender: TObject);
begin
  EnableBuild := not EnableBuild;
end;


procedure TBuilderItemFrame.btnSourceDirectoryOpenDirClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := edSourceDirectory.Text;
  if DirectoryExists(Dir) then
    OpenFolderInExplorer(Dir);
end;


procedure TBuilderItemFrame.btnSourceDirectorySelectClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := edSourceDirectory.Text;

  if SelectDirectoryDialogExecute(FLanguage, Dir) then
  begin
    edSourceDirectory.Text := '';
    edSourceDirectory.Text := Dir;
  end;
end;


procedure TBuilderItemFrame.btnVersionClearValueClick(Sender: TObject);
begin
  edVersion.Text := '';
end;


procedure TBuilderItemFrame.cbDriveLettersKeyPress(Sender: TObject; var Key: char);
const
  VALID_CHARS = ['a'..'z', 'A'..'Z', #8];
begin
  if not (Key in VALID_CHARS) then
    Key := #0;

  //Костыль, при CharCase = UpperCase этот метод срабатывает при нажатии кнопки только вместе с Shift
  //if Key <> #0 then
    Key := UpperCase(Key)[1];
end;


procedure TBuilderItemFrame.cbDriveLettersMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
end;


procedure TBuilderItemFrame.edPrivateKeyChange(Sender: TObject);
begin
  cbSign.Enabled := FileExists(edPrivateKey.Text);
end;


procedure TBuilderItemFrame.edEditChange(Sender: TObject);
begin
  btnBuild.Enabled := DirectoryExists(edSourceDirectory.Text) and
                      DirectoryExists(edDestinationDirectory.Text) and
                      (Trim(edFileExtensions.Text) <> '') and
                      FEnabledBuild;
end;


procedure TBuilderItemFrame.edVersionKeyPress(Sender: TObject; var Key: char);
const
  VALID_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', #8];
begin
  if not (Key in VALID_CHARS) then
    Key := #0;
end;


procedure TBuilderItemFrame.btnSourceDirectoryClearValueClick(Sender: TObject);
begin
  edSourceDirectory.Text := '';
end;


procedure TBuilderItemFrame.btnDestinationDirectoryOpenDirClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := edDestinationDirectory.Text;
  if DirectoryExists(Dir) then
    OpenFolderInExplorer(Dir);
end;


procedure TBuilderItemFrame.btnDestinationDirectoryClearValueClick(Sender: TObject);
begin
  edDestinationDirectory.Text := '';
end;


procedure TBuilderItemFrame.btnBuildClick(Sender: TObject);
var
  exe, params, s, IncFile: String;
begin
  exe := GetPackToolFile;
  if not FileExists(exe) then
  begin
    MessageDialogExecute(
      FLanguage,
      FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'CantFindAddonBuilder', 'Не найден Addon Builder. Переустановите Dayz Tools.')
    );
    Exit;
  end;

  IncFile := GetIncludeFilePath;
  if not CreateIncludeFile(IncFile, Trim(edFileExtensions.Text)) then
  begin
    MessageDialogExecute(
      FLanguage,
      Format(FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'CantCreateIncFile', 'Ошибка создания файла "%s"'), [IncFile])
    );
    Exit;
  end;

  if cbSign.Checked then
    s := Trim(edPrivateKey.Text);

  //Параметры сборки
  params := GetBuildParamString(
    Trim(edSourceDirectory.Text),
    Trim(edDestinationDirectory.Text),
    Trim(edPrefix.Text),
    s,
    IncFile,
    cbDriveLetters.Text,
    Trim(edVersion.Text)
  );

  //Показать диалог сборки
  BuildDialogExecute(FLanguage, GetTitle, exe, params);
end;


procedure TBuilderItemFrame.btnDestinationDirectorySelectClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := edDestinationDirectory.Text;

  if SelectDirectoryDialogExecute(FLanguage, Dir) then
  begin
    edDestinationDirectory.Text := '';
    edDestinationDirectory.Text := Dir;
  end;
end;


procedure TBuilderItemFrame.btnFileExtensionsClearValueClick(Sender: TObject);
begin
  edFileExtensions.Text := '';
end;


procedure TBuilderItemFrame.btnPrefixClearValueClick(Sender: TObject);
begin
  edPrefix.Text := '';
end;


procedure TBuilderItemFrame.btnPrivateKeyClearValueClick(Sender: TObject);
begin
  edPrivateKey.Text := '';
end;


procedure TBuilderItemFrame.btnPrivateKeyOpenDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := ExtractFilePath(edPrivateKey.Text);
  if DirectoryExists(Dir) then
    OpenFolderInExplorer(Dir);
end;


procedure TBuilderItemFrame.btnPrivateKeySelectClick(Sender: TObject);
var
  Dir, Fn: String;
begin
  Dir := ExtractFilePath(edPrivateKey.Text);
  Fn := ExtractFileName(edPrivateKey.Text);

  OpenDialog.InitialDir := Dir;
  OpenDialog.FileName := Fn;
  if OpenDialog.Execute then
    edPrivateKey.Text := OpenDialog.FileName;
end;


procedure TBuilderItemFrame.btnShowCommandLineClick(Sender: TObject);
var
  Param: String;
begin
  //Параметры сборки
  Param := GetBuildParamString(
    Trim(edSourceDirectory.Text),
    Trim(edDestinationDirectory.Text),
    Trim(edPrefix.Text),
    edPrivateKey.Text,
    GetIncludeFilePath,
    cbDriveLetters.Text,
    Trim(edVersion.Text)
  );

  //Показать диалог
  MemoDialogExecute(
    FLanguage,
    FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'MemoCaption', 'Параметры запуска'),
    GetPackToolFile + ' ' + Param
  );
end;


function TBuilderItemFrame.GetTotalFrameHeight: Integer;
begin
  Result := cbSign.Top + cbSign.Height + 10;
end;


procedure TBuilderItemFrame.SetTitle(ATitle: String);
begin
  lblTitle.Caption := ATitle;
end;


function TBuilderItemFrame.GetTitle: String;
begin
  Result := Trim(lblTitle.Caption);
end;


procedure TBuilderItemFrame.SetSelected(ASelected: Boolean);
begin
  if FSelected = ASelected then
    Exit;

  FSelected := ASelected;

  pnlCollapse.Repaint;

  if FSelected then
    DoOnSelect;
end;


procedure TBuilderItemFrame.SetHighlight(AHighlight: Boolean);
begin
  if FHighlight = AHighlight then
    Exit;

  FHighlight := AHighlight;

  Self.ParentColor := not FHighlight;
  Self.ParentBackground := not FHighlight;

  if FHighlight then
    Self.Color := cl3DLight
  else
    Self.Color := Parent.Color;

  //Почему-то панелька меняет свой цвет после изменения цвета родителя
  pnlCollapse.ParentBackground := True;
  pnlCollapse.Color := Self.Color;
end;


procedure TBuilderItemFrame.SetCollapsed(ACollapsed: Boolean);
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


procedure TBuilderItemFrame.SetIconName(AIconName: String);
var
  AIcon: TIcon;
  IcoFile: String;
begin
  IcoFile := FIconDirectory + AIconName;

  if FileExists(IcoFile) then
  begin
    FIconName := AIconName;

    AIcon := TIcon.Create;
    try
      AIcon.LoadFromFile(IcoFile);
      imgIcon.Picture.Assign(AIcon);

    finally
      AIcon.Free;
    end;
  end;
end;


procedure TBuilderItemFrame.SetEnableBuild(AEnable: Boolean);
begin
  FEnabledBuild := AEnable;
  imgEnabledBuild.ImageIndex := Ord(not FEnabledBuild);

  //Поправить кнопку
  CorrectButtonVisible;
end;


procedure TBuilderItemFrame.DoOnSelect;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;


procedure TBuilderItemFrame.DoHeightChange;
begin
  if Assigned(FOnHeightChange) then
    FOnHeightChange(Self);
end;


constructor TBuilderItemFrame.Create(AIconDirectory: String; Letters: TStringList);
begin
  inherited Create(nil);
  FIconDirectory := AIconDirectory;
  FEnabledBuild := True;
  FDriveLetters := Letters;

  //Заполнить список дисководов
  CorrectDriveLetterBox;
end;


constructor TBuilderItemFrame.Create(AIconDirectory: String; Letters: TStringList; ASettings: String);
begin
  Create(AIconDirectory, Letters);
  ValueFromString(ASettings);
end;


procedure TBuilderItemFrame.ValueFromString(const AValue: String);
var
  List: TStringList;
begin
  List := TStringList.Create;
  List.LineBreak := SEPARATOR;
  List.Text := AValue;
  try
    if List.Count > 0 then
      Collapsed := StrToBool(List.Strings[0]);

    if List.Count > 1 then
      SetIconName(List.Strings[1]);

    if List.Count > 2 then
      SetTitle(List.Strings[2]);

    if List.Count > 3 then
      edSourceDirectory.Text := Trim(List.Strings[3]);

    if List.Count > 4 then
      edDestinationDirectory.Text := Trim(List.Strings[4]);

    if List.Count > 5 then
      edPrefix.Text := Trim(List.Strings[5]);

    if List.Count > 6 then
      edFileExtensions.Text := Trim(List.Strings[6]);

     if List.Count > 7 then
      edVersion.Text := Trim(List.Strings[7]);

    if List.Count > 8 then
      edPrivateKey.Text := Trim(List.Strings[8]);

    if List.Count > 9 then
      cbSign.Checked := StrToBool(List.Strings[9]);

    if List.Count > 10 then
      EnableBuild := StrToBool(List.Strings[10]);

    if List.Count > 11 then
      cbDriveLetters.Text := List.Strings[11];

  finally
    List.Free;
  end;
end;


function TBuilderItemFrame.ValueToString: String;
begin
  Result :=
    BoolToStr(FCollapsed) + SEPARATOR +
    FIconName + SEPARATOR +
    lblTitle.Caption + SEPARATOR +
    edSourceDirectory.Text + SEPARATOR +
    edDestinationDirectory.Text + SEPARATOR +
    edPrefix.Text + SEPARATOR +
    edFileExtensions.Text + SEPARATOR +
    edVersion.Text + SEPARATOR +
    edPrivateKey.Text + SEPARATOR +
    BoolToStr(cbSign.Checked) + SEPARATOR +
    BoolToStr(EnableBuild) + SEPARATOR +
    cbDriveLetters.Text;
end;


procedure TBuilderItemFrame.CorrectButtonVisible;
begin
  edEditChange(nil);
end;


procedure TBuilderItemFrame.CorrectDriveLetterBox;
begin
  cbDriveLetters.Items.AddStrings(FDriveLetters, True);
end;


procedure TBuilderItemFrame.ChangeLanguage(Language: TLanguage);
begin
  FLanguage := Language;

  //Элементы управления
  btnBuild.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'Build', 'Собрать');

  lblSourceDirectory.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'SourceDirectory', 'Исходный каталог');
  btnSourceDirectoryOpenDir.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'OpenDirectory', 'Открыть каталог в проводнике');
  btnSourceDirectorySelect.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'SelectDirectory', 'Выбрать каталог');
  btnSourceDirectoryClearValue.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'ClearValue', 'Очистить значение');
  imgEnabledBuild.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'EnabledBuild', 'Доступность сборки');
  btnShowCommandLine.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'ShowCommandLine', 'Показать параметры запуска');


  lblDestinationDirectory.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'DestinationDirectory', 'Выходной каталог');
  btnDestinationDirectoryOpenDir.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'OpenDirectory', 'Открыть каталог в проводнике');
  btnDestinationDirectorySelect.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'SelectDirectory', 'Выбрать каталог');
  btnDestinationDirectoryClearValue.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'ClearValue', 'Очистить значение');

  lblPrefix.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'Prefix', 'Префикс');
  btnPrefixClearValue.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'ClearValue', 'Очистить значение');

  lblFileExtensions.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'FileExtensions', 'Типы файлов');
  btnFileExtensionsClearValue.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'ClearValue', 'Очистить значение');

  lblVersion.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'Version', 'Версия');
  btnVersionClearValue.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'ClearValue', 'Очистить значение');

  lblDriveLetter.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'DriveLetter', 'Диск проекта');

  lblPrivateKey.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'PrivateKey', 'Приватный ключ');
  btnPrivateKeyOpenDirectory.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'OpenDirectory', 'Открыть каталог в проводнике');
  btnPrivateKeySelect.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'SelectKey', 'Выбрать ключ');
  btnPrivateKeyClearValue.Hint := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'ClearValue', 'Очистить значение');

  cbSign.Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'Sign', 'Подписывать PBO');
  OpenDialog.Filter := Format('%s (*.biprivatekey)|*.biprivatekey', [FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'PrivateKeys', 'Приватные ключи')]);
end;


function TBuilderItemFrame.GetBuildData: TBuilderItemBuildData;
begin
  Result.Icon := imgIcon;
  Result.Title := GetTitle;
  Result.Extensions := Trim(edFileExtensions.Text);
  Result.SourceDirectory := Trim(edSourceDirectory.Text);
  Result.DestinationDirectory := Trim(edDestinationDirectory.Text);
  Result.ProjectDrive := cbDriveLetters.Text;
  Result.Prefix := Trim(edPrefix.Text);
  Result.Version := Trim(edVersion.Text);

  if cbSign.Checked then
    Result.Sign := Trim(edPrivateKey.Text);
end;



end.

