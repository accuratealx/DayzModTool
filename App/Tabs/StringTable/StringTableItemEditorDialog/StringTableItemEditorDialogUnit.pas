unit StringTableItemEditorDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  Language,
  StringTableItem, DialogCommonUnit;

type
  TStringTableItemEditorMode = (
    stiemNew, //Новый
    stiemEdit //Изменение
  );


  TStringTableItemEditorDialogForm = class(TDialogCommonForm)
    btnCancel: TSpeedButton;
    btnOK: TSpeedButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'Dialogs.StringTableItemEditor.';
  protected
    procedure SetMode(AMode: TStringTableItemEditorMode);

    procedure PrepareInterface; override;
    procedure SetLanguage; override;
  end;


function StringTableItemEditorDialogExecute(
  Language: TLanguage;
  Mode: TStringTableItemEditorMode;
  EditItem: TStringTableItem
  ): Boolean;


implementation

{$R *.lfm}

uses
  DialogParameters;

type
  TStringTableItemEditorDialogParameters = class(TDialogParameters)
    Mode: TStringTableItemEditorMode;
    EditItem: TStringTableItem;
  end;


function StringTableItemEditorDialogExecute(Language: TLanguage; Mode: TStringTableItemEditorMode; EditItem: TStringTableItem): Boolean;
var
  Params: TStringTableItemEditorDialogParameters;
begin
  Params := TStringTableItemEditorDialogParameters.Create;
  Params.Language := Language;
  Params.Mode := Mode;
  Params.EditItem := EditItem;
  try
    with TStringTableItemEditorDialogForm.Create(Params) do
    begin
      ShowModal;
      Result := Tag = 1;
      Free;
    end;

  finally
    Params.Free;
  end;
end;


procedure TStringTableItemEditorDialogForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;


procedure TStringTableItemEditorDialogForm.btnOKClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TStringTableItemEditorDialogForm.FormResize(Sender: TObject);
const
  SPACE = 10;
var
  W: Integer;
begin
  W := btnOK.Width + btnCancel.Width + SPACE;
  btnOK.Left := (pnlButton.Width - W) div 2;
  btnCancel.Left := btnOK.Left + btnOK.Width + SPACE;
end;


procedure TStringTableItemEditorDialogForm.SetMode(AMode: TStringTableItemEditorMode);
var
  s: String;
begin
  case AMode of
    stiemNew:
      s := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Add', 'Добавить');

    stiemEdit:
      s := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Edit', 'Изменить');

    else
      s := '';
  end;

  //Поправить внешний вид
  Caption := s;
  btnOK.Caption := s;
end;


procedure TStringTableItemEditorDialogForm.PrepareInterface;
var
  Params: TStringTableItemEditorDialogParameters;
begin
  Params := FParameters as TStringTableItemEditorDialogParameters;

  SetMode(Params.Mode);

  //Заполнить данные
  Params.EditItem.ID := 'Test';
  Params.EditItem.LocalizedText[ltRussian] := 'Русский текст';
end;


procedure TStringTableItemEditorDialogForm.SetLanguage;
begin
  btnCancel.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Cancel', 'Отмена');
end;

end.

