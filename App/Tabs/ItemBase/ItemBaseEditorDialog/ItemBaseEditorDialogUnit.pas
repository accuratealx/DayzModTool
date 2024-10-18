unit ItemBaseEditorDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, Buttons, ExtCtrls, StdCtrls,
  Language,
  DialogCommonUnit;

type
  //Обработчик проверки на существование Класса
  TItemBaseObjExistChecker = function(ObjName, OriginalName: String): Boolean of object;

  TItemBaseEditorMode = (
    ibemNew,  //Новый
    ibemEdit  //Изменение
  );


  TItemBaseEditorDialogForm = class(TDialogCommonForm)
    btnCancel: TSpeedButton;
    btnOK: TSpeedButton;
    edObjName: TEdit;
    edObjTitle: TEdit;
    edObjDescription: TEdit;
    lblObjName: TLabel;
    lblObjTitle: TLabel;
    lblObjDescription: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure edEditChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'Dialogs.ItemBaseEditor.';
  private
    function  ObjExist(AObjName: String): Boolean;

    procedure SetMode(AMode: TItemBaseEditorMode);
  protected
    procedure PrepareInterface; override;
    procedure SetLanguage; override;
  end;


function ItemBaseEditorDialogExecute(Language: TLanguage; AObjChecker: TItemBaseObjExistChecker; Mode: TItemBaseEditorMode; var AObjName, AObjTitle, AObjDescription: String): Boolean;


implementation

{$R *.lfm}

uses
  DialogParameters;

type
  TItemBaseEditorDialogParameters = class(TDialogParameters)
    Mode: TItemBaseEditorMode;
    ObjChecker: TItemBaseObjExistChecker;
    ObjName: String;
    ObjTitle: String;
    ObjDescription: String;
  end;


function ItemBaseEditorDialogExecute(Language: TLanguage; AObjChecker: TItemBaseObjExistChecker; Mode: TItemBaseEditorMode; var AObjName, AObjTitle, AObjDescription: String): Boolean;
var
  Params: TItemBaseEditorDialogParameters;
begin
  Params := TItemBaseEditorDialogParameters.Create;
  Params.Language := Language;
  Params.Mode := Mode;
  Params.ObjChecker := AObjChecker;
  Params.ObjName := AObjName;
  Params.ObjTitle := AObjTitle;
  Params.ObjDescription := AObjDescription;
  try
    with TItemBaseEditorDialogForm.Create(Params) do
    begin
      Result := False;
      ShowModal;

      if Tag = 1 then
      begin
        Result := True;

        AObjName := Trim(edObjName.Text);
        AObjTitle := Trim(edObjTitle.Text);
        AObjDescription := Trim(edObjDescription.Text);
      end;

      Free;
    end;

  finally
    Params.Free;
  end;
end;


procedure TItemBaseEditorDialogForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;


procedure TItemBaseEditorDialogForm.btnOKClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TItemBaseEditorDialogForm.edEditChange(Sender: TObject);
var
  ObjName: String;
begin
  ObjName := Trim(edObjName.Text);
  btnOK.Enabled := (ObjName <> '') and (Trim(edObjTitle.Text) <> '') and not ObjExist(ObjName);
end;


procedure TItemBaseEditorDialogForm.FormResize(Sender: TObject);
const
  SPACE = 10;
var
  W: Integer;
begin
  W := btnOK.Width + btnCancel.Width + SPACE;
  btnOK.Left := (pnlButton.Width - W) div 2;
  btnCancel.Left := btnOK.Left + btnOK.Width + SPACE;
end;


function TItemBaseEditorDialogForm.ObjExist(AObjName: String): Boolean;
var
  Params: TItemBaseEditorDialogParameters;
begin
  Params := FParameters as TItemBaseEditorDialogParameters;
  if Params = nil then
    Exit(False);

  Result := Params.ObjChecker(AObjName, (FParameters as TItemBaseEditorDialogParameters).ObjName);
end;


procedure TItemBaseEditorDialogForm.SetMode(AMode: TItemBaseEditorMode);
var
  s: String;
begin
  case AMode of
    ibemNew:
      s := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Add', 'Добавить');

    ibemEdit:
      s := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Edit', 'Изменить');

    else
      s := '';
  end;

  //Поправить внешний вид
  Caption := s;
  btnOK.Caption := s;
end;


procedure TItemBaseEditorDialogForm.PrepareInterface;
var
  Params: TItemBaseEditorDialogParameters;
begin
  Params := FParameters as TItemBaseEditorDialogParameters;

  SetMode(Params.Mode);
  edObjName.Text := Params.ObjName;
  edObjTitle.Text := Params.ObjTitle;
  edObjDescription.Text := Params.ObjDescription;
end;


procedure TItemBaseEditorDialogForm.SetLanguage;
begin
  btnCancel.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Cancel', 'Отмена');

  lblObjName.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ObjName', 'Имя объекта');
  lblObjTitle.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ObjTitle', 'Заголовок');
  lblObjDescription.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ObjDescription', 'Описание');
end;



end.

