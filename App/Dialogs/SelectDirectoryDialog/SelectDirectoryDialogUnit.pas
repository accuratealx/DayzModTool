unit SelectDirectoryDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Controls, Graphics, Dialogs, ShellCtrls, Buttons, StdCtrls,
  Language,
  DialogCommonUnit, Classes;

type
  TSelectDirectoryDialogForm = class(TDialogCommonForm)
    btnClose: TSpeedButton;
    btnSelect: TSpeedButton;
    btnAddDirectory: TSpeedButton;
    edCurrentPath: TEdit;
    stvDirectoryTree: TShellTreeView;
    procedure btnAddDirectoryClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure stvDirectoryTreeClick(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'Dialogs.SelectDirectory.';
  private
    procedure SetPath(APath: String);
  protected
    procedure PrepareInterface; override;
    procedure SetLanguage; override;
  end;


function SelectDirectoryDialogExecute(Language: TLanguage; var Path: String): Boolean;


implementation

{$R *.lfm}

uses
  DialogParameters, InputDialogUnit;

type
  TSelectDirectoryDialogParameters = class(TDialogParameters)
    Path: String;
  end;


function SelectDirectoryDialogExecute(Language: TLanguage; var Path: String): Boolean;
var
  Params: TSelectDirectoryDialogParameters;
begin
  Params := TSelectDirectoryDialogParameters.Create;
  Params.Language := Language;
  Params.Path := Path;
  try
    with TSelectDirectoryDialogForm.Create(Params) do
    begin
      Result := False;
      ShowModal;

      if Tag = 1 then
      begin
        Result := True;
        Path := stvDirectoryTree.Path;
      end;

      Free;
    end;

  finally
    Params.Free;
  end;
end;


procedure TSelectDirectoryDialogForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TSelectDirectoryDialogForm.btnAddDirectoryClick(Sender: TObject);
var
  Dir, DirName: String;
begin
  DirName := '';
  if InputDialogExecute(
    FParameters.Language,
    FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'InputName', 'Введите имя каталога'),
    DirName) then
  begin
    Dir := IncludeTrailingBackslash(stvDirectoryTree.Path + DirName);

    if not DirectoryExists(Dir) then
    begin
      ForceDirectories(Dir);

      { #todo : Не обновляется дерево если добавить папку в пустой каталог }
      stvDirectoryTree.Refresh(stvDirectoryTree.Selected);
    end;

    stvDirectoryTree.Path := Dir;
    SetPath(Dir);
  end;
end;


procedure TSelectDirectoryDialogForm.btnSelectClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TSelectDirectoryDialogForm.FormResize(Sender: TObject);
const
  SPACE = 10;
var
  W: Integer;
begin
  W := btnSelect.Width + btnClose.Width + SPACE;
  btnSelect.Left := (pnlButton.Width - W) div 2;
  btnClose.Left := btnSelect.Left + btnSelect.Width + SPACE;
end;


procedure TSelectDirectoryDialogForm.stvDirectoryTreeClick(Sender: TObject);
begin
  SetPath(stvDirectoryTree.Path);
end;


procedure TSelectDirectoryDialogForm.SetPath(APath: String);
var
  Exist: Boolean;
begin
  edCurrentPath.Text := APath;

  Exist := DirectoryExists(stvDirectoryTree.Path);
  btnSelect.Enabled := Exist;
  btnAddDirectory.Enabled := Exist;
end;


procedure TSelectDirectoryDialogForm.PrepareInterface;
var
  Params: TSelectDirectoryDialogParameters;
begin
  stvDirectoryTree.DoubleBuffered := True;

  Params := FParameters as TSelectDirectoryDialogParameters;

  if DirectoryExists(Params.Path) then
  begin
    stvDirectoryTree.Path := Params.Path;
    SetPath(Params.Path);
  end;
end;


procedure TSelectDirectoryDialogForm.SetLanguage;
begin
  Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Caption', 'Выбор каталога');
  btnSelect.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Select', 'Выбрать');
  btnClose.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Close', 'Закрыть');
  btnAddDirectory.Hint := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'AddDirectory', 'Создать каталог');
end;



end.

