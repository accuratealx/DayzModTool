unit DirectoryEditorDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, Buttons, ExtCtrls, StdCtrls,
  Language,
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
    const
      LANGUAGE_PREFIX = 'Dialogs.DirectoryEditor.';
  private
    FIconFileName: String;

    function  GetIconDirectory: String;

    procedure SetMode(AMode: TPathEditorMode);
    procedure SetIconByFileName(AFileName: String);
  protected
    procedure PrepareInterface; override;
    procedure SetLanguage; override;
  end;


function DirectoryEditorDialogExecute(Language: TLanguage; Mode: TPathEditorMode; const IconDirectory: String; var ACaption, APath, AIconFileName: String): Boolean;


implementation

{$R *.lfm}

uses
  DialogParameters;

type
  TDirectoryEditorDialogParameters = class(TDialogParameters)
    Mode: TPathEditorMode;
    IconDirectory: String;
    Caption: String;
    Path: String;
    IconFileName: String;
  end;


function DirectoryEditorDialogExecute(Language: TLanguage; Mode: TPathEditorMode; const IconDirectory: String; var ACaption, APath, AIconFileName: String): Boolean;
var
  Params: TDirectoryEditorDialogParameters;
begin
  Params := TDirectoryEditorDialogParameters.Create;
  Params.Language := Language;
  Params.Mode := Mode;
  Params.IconDirectory := IconDirectory;
  Params.Caption := ACaption;
  Params.Path := APath;
  Params.IconFileName := AIconFileName;
  try
    with TDirectoryEditorDialogForm.Create(Params) do
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

  finally
    Params.Free;
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

  if SelectDirectoryDialogExecute(FParameters.Language, Path) then
    edPath.Text := Path;
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

  if IconSelectorDialogExecute(FParameters.Language, GetIconDirectory, IconName) then
    SetIconByFileName(IconName);
end;


function TDirectoryEditorDialogForm.GetIconDirectory: String;
begin
  Result := (FParameters as TDirectoryEditorDialogParameters).IconDirectory;
end;


procedure TDirectoryEditorDialogForm.SetMode(AMode: TPathEditorMode);
var
  s: String;
begin
  case AMode of
    pemNew:
      s := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Add', 'Добавить');

    pemEdit:
      s := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Edit', 'Изменить');

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
  if FileExists(GetIconDirectory + AFileName) then
  begin
    FIconFileName := AFileName;

    AIcon := TIcon.Create;
    try
      AIcon.LoadFromFile(GetIconDirectory + FIconFileName);
      imgIcon.Picture.Assign(AIcon);

    finally
      AIcon.Free;
    end;
  end;
end;


procedure TDirectoryEditorDialogForm.PrepareInterface;
var
  Params: TDirectoryEditorDialogParameters;
begin
  Params := FParameters as TDirectoryEditorDialogParameters;

  SetMode(Params.Mode);
  SetIconByFileName(Params.IconFileName);
  edCaption.Text := Params.Caption;
  edPath.Text := Params.Path;
end;


procedure TDirectoryEditorDialogForm.SetLanguage;
begin
  btnCancel.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Cancel', 'Отмена');
  lblCaption.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Caption', 'Название');
  lblPath.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Directory', 'Каталог');
  btnSelectDirectory.Hint := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'SelectDirectory', 'Выбрать каталог');
  imgIcon.Hint := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ChangeIcon', 'Изменить иконку');
end;



end.

