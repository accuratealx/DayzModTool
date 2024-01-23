unit SelectDirectoryDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Controls, Graphics, Dialogs, ShellCtrls, Buttons, StdCtrls,
  Language,
  DialogCommonUnit;

type
  TSelectDirectoryDialogForm = class(TDialogCommonForm)
    btnClose: TSpeedButton;
    btnSelect: TSpeedButton;
    edCurrentPath: TEdit;
    stvDirectoryTree: TShellTreeView;
    procedure btnCloseClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure stvDirectoryTreeClick(Sender: TObject);
  private
    const
      PREFIX_DIALOG = 'Dialogs.SelectDirectory.';
  protected
    procedure PrepareInterface; override;
    procedure SetLanguage; override;
  end;


function SelectDirectoryDialogExecute(Language: TLanguage; var Path: String): Boolean;


implementation

{$R *.lfm}

uses
  DialogParameters;

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
  btnSelect.Enabled := DirectoryExists(stvDirectoryTree.Path);
  edCurrentPath.Text := stvDirectoryTree.Path;
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
    edCurrentPath.Text := Params.Path;
    btnSelect.Enabled := True;
  end;
end;


procedure TSelectDirectoryDialogForm.SetLanguage;
begin
  Caption := FParameters.Language.GetLocalizedString(PREFIX_DIALOG + 'Caption', 'Выбор каталога');
  btnSelect.Caption := FParameters.Language.GetLocalizedString(PREFIX_DIALOG + 'Select', 'Выбрать');
  btnClose.Caption := FParameters.Language.GetLocalizedString(PREFIX_DIALOG + 'Close', 'Закрыть');
end;



end.

