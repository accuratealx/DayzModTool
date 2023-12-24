unit SelectDirectoryDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ShellCtrls, Buttons,
  StdCtrls, DialogCommonUnit;

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
    procedure PrepareInterface(ACaption, APath: String);
  end;


function SelectDirectoryDialogExecute(ACaption: String; var APath: String): Boolean;


implementation

{$R *.lfm}


function SelectDirectoryDialogExecute(ACaption: String; var APath: String): Boolean;
begin
  with TSelectDirectoryDialogForm.Create(nil) do
  begin
    Result := False;
    PrepareInterface(ACaption, APath);
    ShowModal;

    if Tag = 1 then
    begin
      Result := True;
      APath := stvDirectoryTree.Path;
    end;

    Free;
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


procedure TSelectDirectoryDialogForm.PrepareInterface(ACaption, APath: String);
begin
  stvDirectoryTree.DoubleBuffered := True;
  Caption := ACaption;

  if DirectoryExists(APath) then
  begin
    stvDirectoryTree.Path := APath;
    edCurrentPath.Text := APath;
  end;
end;



end.

