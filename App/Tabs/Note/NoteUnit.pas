unit NoteUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  SynEdit, SynHighlighterAny,
  TabCommonUnit, TabParameters;

type
  TNoteFrame = class(TTabCommonFrame)
    btnClear: TSpeedButton;
    btnSave: TSpeedButton;
    Panel1: TPanel;
    pnlTools: TPanel;
    mNote: TSynEdit;
    procedure btnClearClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'TabNote.';
  private
    FNoteFile: String;

  public
    constructor Create(Parameters: TTabParameters; AParent: TWinControl); reintroduce;
    destructor  Destroy; override;

    procedure ApplyLanguage; override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;
  end;


implementation

{$R *.lfm}

uses
  YesNoQuestionDialogUnit, MessageDialogUnit;


procedure TNoteFrame.btnClearClick(Sender: TObject);
begin
  if YesNoQuestionDialogExecute(
    FParams.Language,
    FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ProceedClear', 'Заметки будут очищены, продолжить?')
  ) then
  begin
    //Очистить заметки
    mNote.Clear;

    //Сохранить на диске
    SaveSettings;
  end;
end;


procedure TNoteFrame.btnSaveClick(Sender: TObject);
begin
  SaveSettings;
end;


constructor TNoteFrame.Create(Parameters: TTabParameters; AParent: TWinControl);
var
  Dir: String;
begin
  inherited Create(Parameters, AParent);

  //Создать каталог
  Dir := FParams.SettingsDirectory + 'Note\';
  ForceDirectories(Dir);

   //Определить файл настроек
  FNoteFile := Dir + 'Note.txt';

  //Загрузить настройки
  LoadSettings;
end;


destructor TNoteFrame.Destroy;
begin
  //Сохранить настройки
  SaveSettings;

  inherited Destroy;
end;


procedure TNoteFrame.ApplyLanguage;
begin
  //Пеервод
  btnClear.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Clear', 'Очистить');
  btnSave.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Save', 'Сохранить');
end;


procedure TNoteFrame.SaveSettings;
begin
  try
    mNote.Lines.SaveToFile(FNoteFile);
  except
    MessageDialogExecute(
      FParams.Language,
      Format(FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'FileAccessError', 'Произошла ошибка при доступе к файлу "%s" '), [FNoteFile])
      );
  end;
end;


procedure TNoteFrame.LoadSettings;
begin
  try
    if FileExists(FNoteFile) then
      mNote.Lines.LoadFromFile(FNoteFile);
  except
    MessageDialogExecute(
      FParams.Language,
      Format(FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'FileAccessError', 'Произошла ошибка при доступе к файлу "%s" '), [FNoteFile])
      );
  end;
end;



end.

