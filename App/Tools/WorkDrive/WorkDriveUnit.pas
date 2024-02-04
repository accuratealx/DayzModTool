unit WorkDriveUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  Language, DialogParameters, DialogCommonUnit;

type
  TWorkDriveForm = class(TDialogCommonForm)
    btnClose: TSpeedButton;
    btnMount: TSpeedButton;
    btnOpenDirectory: TSpeedButton;
    btnSelectDirectory: TSpeedButton;
    btnUnMount: TSpeedButton;
    cbLetters: TComboBox;
    edDirectory: TEdit;
    lblDriveLetter: TLabel;
    lblDirectory: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure btnMountClick(Sender: TObject);
    procedure btnOpenDirectoryClick(Sender: TObject);
    procedure btnSelectDirectoryClick(Sender: TObject);
    procedure btnUnMountClick(Sender: TObject);
    procedure cbLettersChange(Sender: TObject);
    procedure edDirectoryChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    const
      PREFIX_TOOLS = 'Tools.WorkDrive.';

      SECTION_SYSTEM = 'System';
      PARAM_LETTER = 'DriveLetter';
      PARAM_DIRECTORY = 'ProjectDirectory';
  private
    FAvailableDriveLetters: TStringList;

    procedure LoadSettings;
    procedure SaveSettings;

    procedure LoadAvailableDriveLetters;
    procedure FillLetterBox;
    procedure CorrectButtons;
  protected
    procedure PrepareInterface; override;
    procedure SetLanguage; override;
  public
    constructor Create(Parameters: TDialogParameters); reintroduce;
    destructor  Destroy; override;
  end;


procedure WorkDriveExecute(Language: TLanguage; const SettingsFile: String);


implementation

{$R *.lfm}

uses
  IniFiles,
  DayZUtils, DosDevices,
  SelectDirectoryDialogUnit;

type
  TWorkDriveParameters = class(TDialogParameters)
    SettingsFile: String;
  end;


procedure WorkDriveExecute(Language: TLanguage; const SettingsFile: String);
var
  Params: TWorkDriveParameters;
begin
  Params := TWorkDriveParameters.Create;
  Params.Language := Language;
  Params.SettingsFile := SettingsFile;
  try
    with TWorkDriveForm.Create(Params) do
    begin
      ShowModal;
      Free;
    end;

  finally
    Params.Free;
  end;
end;


procedure TWorkDriveForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TWorkDriveForm.btnMountClick(Sender: TObject);
begin
  try
    MapDrive(cbLetters.Text[1], edDirectory.Text);
  except
    ShowMessage('Ошибка' + sLineBreak + Exception(ExceptObject).Message);
  end;

  CorrectButtons;
end;


procedure TWorkDriveForm.btnOpenDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := Trim(edDirectory.Text);
  if DirectoryExists(Dir) then
    OpenFolderInExplorer(Dir);
end;


procedure TWorkDriveForm.btnSelectDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := edDirectory.Text;
  if SelectDirectoryDialogExecute(FParameters.Language, Dir) then
    edDirectory.Text := Dir;
end;


procedure TWorkDriveForm.btnUnMountClick(Sender: TObject);
begin
  try
    UnMapDrive(cbLetters.Text[1]);
  except
    ShowMessage('Ошибка' + sLineBreak + Exception(ExceptObject).Message);
  end;

  CorrectButtons;
end;


procedure TWorkDriveForm.cbLettersChange(Sender: TObject);
begin
  CorrectButtons;
end;


procedure TWorkDriveForm.edDirectoryChange(Sender: TObject);
begin
  CorrectButtons;
end;


procedure TWorkDriveForm.FormResize(Sender: TObject);
const
  SPACE_MOUNT = 10;
  SPACE_CLOSE = 40;
var
  W: Integer;
begin
  W := btnMount.Width + btnUnMount.Width + btnClose.Width + SPACE_MOUNT + SPACE_CLOSE;

  btnMount.Left := (pnlButton.Width - W) div 2;
  btnUnMount.Left := btnMount.Left + btnMount.Width + SPACE_MOUNT;
  btnClose.Left := btnUnMount.Left + btnUnMount.Width + SPACE_CLOSE;
end;


procedure TWorkDriveForm.LoadSettings;
var
  F: TIniFile;
begin
  F := TIniFile.Create((FParameters as TWorkDriveParameters).SettingsFile);

  cbLetters.ItemIndex := cbLetters.Items.IndexOf(F.ReadString(SECTION_SYSTEM, PARAM_LETTER, ''));
  edDirectory.Text := F.ReadString(SECTION_SYSTEM, PARAM_DIRECTORY, '');

  F.Free;
end;


procedure TWorkDriveForm.SaveSettings;
var
  F: TIniFile;
begin
  F := TIniFile.Create((FParameters as TWorkDriveParameters).SettingsFile);

  F.WriteString(SECTION_SYSTEM, PARAM_LETTER, cbLetters.Text);
  F.WriteString(SECTION_SYSTEM, PARAM_DIRECTORY, edDirectory.Text);

  F.Free;
end;


procedure TWorkDriveForm.LoadAvailableDriveLetters;
begin
  GetAvailableDriveLetters(FAvailableDriveLetters);
end;


procedure TWorkDriveForm.FillLetterBox;
var
  I: Char;
  j: Integer;
  List: TStringList;
  s: String;
begin
  List := TStringList.Create;
  try
    //Отсеим занятые буквы
    for I := 'C' to 'Z' do
    begin
      if FAvailableDriveLetters.IndexOf(I) = -1 then
        List.Add(I);
    end;

    //Добавим примапленные
    for j := 0 to FAvailableDriveLetters.Count - 1 do
    begin
      i := FAvailableDriveLetters.Strings[j][1];
      s := GetDrivePath(I);
      if s <> '' then
        List.Add(I);
    end;

    //Упорядочить
    List.Sort;

    //Добавить в комбобокс
    cbLetters.Clear;
    cbLetters.Items.AddStrings(List);

  finally
    List.Free;
  end;
end;


procedure TWorkDriveForm.CorrectButtons;
var
  Letter: Char;
  MountPath: String;
  PathExist: Boolean;
begin
  if cbLetters.Text <> '' then
    Letter := cbLetters.Text[1]
  else
    Letter := #0;

  //Проверим не привязан ли диск
  MountPath := GetDrivePath(Letter);

  //Проверим существование пути
  PathExist := DirectoryExists(edDirectory.Text);

  //Поправим кнопки
  btnMount.Enabled := (MountPath = '') and (PathExist) and (cbLetters.Text <> '');
  btnUnMount.Enabled := (MountPath <> '') and (cbLetters.Text <> '');
  btnOpenDirectory.Enabled := PathExist;
end;


procedure TWorkDriveForm.PrepareInterface;
begin
  //Получить список доступных букв дисководов
  LoadAvailableDriveLetters;

  //Заполнить список доступными буквами дисков
  FillLetterBox;
end;


procedure TWorkDriveForm.SetLanguage;
begin
  Caption := FParameters.Language.GetLocalizedString(PREFIX_TOOLS + 'Caption', 'Рабочий каталог');
  btnClose.Caption := FParameters.Language.GetLocalizedString(PREFIX_TOOLS + 'Close', 'Закрыть');
  btnMount.Caption := FParameters.Language.GetLocalizedString(PREFIX_TOOLS + 'Mount', 'Привязать');
  btnUnMount.Caption := FParameters.Language.GetLocalizedString(PREFIX_TOOLS + 'Unmount', 'Отвязать');
  btnOpenDirectory.Hint := FParameters.Language.GetLocalizedString(PREFIX_TOOLS + 'OpenDirectory', 'Открыть каталог в проводнике');
  btnSelectDirectory.Hint := FParameters.Language.GetLocalizedString(PREFIX_TOOLS + 'SelectDirectory', 'Выбрать каталог');
  lblDriveLetter.Caption := FParameters.Language.GetLocalizedString(PREFIX_TOOLS + 'DriveLetter', 'Буква диска');
  lblDirectory.Caption := FParameters.Language.GetLocalizedString(PREFIX_TOOLS + 'Directory', 'Каталог проекта');
end;


constructor TWorkDriveForm.Create(Parameters: TDialogParameters);
begin
  FAvailableDriveLetters := TStringList.Create;
  inherited Create(Parameters);

  //Загрузить настройки
  LoadSettings;

  //Поправить кнопки
  CorrectButtons;
end;


destructor TWorkDriveForm.Destroy;
begin
  //Сохранить настройки
  SaveSettings;

  FAvailableDriveLetters.Free;
  inherited Destroy;
end;



end.


