unit DirectoryEditorDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls,
  DialogCommonUnit, SelectDirectoryDialogUnit, IconSelectorDialogUnit;

type
  TPathEditorMode = (
    pemNew,   //Новый
    pemEdit   //Изменение
  );

  TDirectoryEditorDialogForm = class(TDialogCommonForm)
    btnCancel: TSpeedButton;
    btnOK: TSpeedButton;
    edCaption: TEdit;
    edPath: TEdit;
    imgIcon: TImage;
    lblCaption: TLabel;
    lblPath: TLabel;
    btnSelectDirectory: TSpeedButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSelectDirectoryClick(Sender: TObject);
    procedure edEditChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure imgIconClick(Sender: TObject);
  private
    FMode: TPathEditorMode;
    FIconDirectory: String;
    FIconFileName: String;

    procedure SetMode(AMode: TPathEditorMode);
    procedure SetIconByFileName(AFileName: String);
  public
    constructor Create(AMode: TPathEditorMode; const AIconDirectory, ACaption, APath, AIconFileName: String); reintroduce;
  end;


function DirectoryEditorDialogExecute(AMode: TPathEditorMode; const AIconDirectory: String; var ACaption, APath, AIconFileName: String): Boolean;


implementation

{$R *.lfm}


function DirectoryEditorDialogExecute(AMode: TPathEditorMode; const AIconDirectory: String; var ACaption, APath, AIconFileName: String): Boolean;
begin
  with TDirectoryEditorDialogForm.Create(AMode, AIconDirectory, ACaption, APath, AIconFileName) do
  begin
    Result := False;
    ShowModal;

    if Tag = 1 then
    begin
      Result := True;
      ACaption := Trim(edCaption.Text);
      APath := ExcludeTrailingBackslash(Trim(edPath.Text));
      AIconFileName := FIconFileName;
    end;

    Free;
  end;
end;


procedure TDirectoryEditorDialogForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;


procedure TDirectoryEditorDialogForm.btnOKClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TDirectoryEditorDialogForm.btnSelectDirectoryClick(Sender: TObject);
var
  Path: String;
begin
  Path := edPath.Text;

  if SelectDirectoryDialogExecute('Выберите каталог', Path) then
  begin
    edPath.Text := Path;
  end;
end;


procedure TDirectoryEditorDialogForm.edEditChange(Sender: TObject);
begin
  btnOK.Enabled := (Trim(edCaption.Text) <> '') and (Trim(edPath.Text) <> '');
end;


procedure TDirectoryEditorDialogForm.FormResize(Sender: TObject);
const
  SPACE = 10;
var
  W: Integer;
begin
  W := btnOK.Width + btnCancel.Width + SPACE;
  btnOK.Left := (pnlButton.Width - W) div 2;
  btnCancel.Left := btnOK.Left + btnOK.Width + SPACE;
end;


procedure TDirectoryEditorDialogForm.imgIconClick(Sender: TObject);
var
  IconName: String;
begin
  IconName := FIconFileName;

  if IconSelectorDialogExecute(FIconDirectory, IconName) then
    SetIconByFileName(IconName);
end;


procedure TDirectoryEditorDialogForm.SetMode(AMode: TPathEditorMode);
var
  s: String;
begin
  FMode := AMode;

  case FMode of
    pemNew:
      s := 'Добавить';

    pemEdit:
      s := 'Изменить';

    else
      s := '';
  end;

  //Поправить внешний вид
  Caption := s;
  btnOK.Caption := s;
end;


procedure TDirectoryEditorDialogForm.SetIconByFileName(AFileName: String);
var
  AIcon: TIcon;
begin
  if FileExists(FIconDirectory + AFileName) then
  begin
    FIconFileName := AFileName;

    AIcon := TIcon.Create;
    try
      AIcon.LoadFromFile(FIconDirectory + FIconFileName);
      imgIcon.Picture.Assign(AIcon);

    finally
      AIcon.Free;
    end;
  end;

end;


constructor TDirectoryEditorDialogForm.Create(AMode: TPathEditorMode; const AIconDirectory, ACaption, APath, AIconFileName: String);
begin
  inherited Create(nil);

  FIconDirectory := AIconDirectory;

  SetMode(AMode);
  SetIconByFileName(AIconFileName);
  edCaption.Text := ACaption;
  edPath.Text := APath;
end;



end.

