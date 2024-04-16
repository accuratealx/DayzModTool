unit YesNoQuestionDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, Math,
  Language,
  DialogCommonUnit;

type
  TYesNoQuestionDialogForm = class(TDialogCommonForm)
    btnNo: TSpeedButton;
    btnYes: TSpeedButton;
    lblText: TLabel;
    procedure btnNoClick(Sender: TObject);
    procedure btnYesClick(Sender: TObject);
    procedure pnlButtonResize(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'Dialogs.YesNo.';
  protected
    procedure PrepareInterface; override;
    procedure SetLanguage; override;
  end;


function YesNoQuestionDialogExecute(Language: TLanguage; const Text: String): Boolean;


implementation

{$R *.lfm}

uses
  DialogUtils, DialogParameters;

type
  TYesNoQuestionDialogParameters = class(TDialogParameters)
    Text: String;
  end;


function YesNoQuestionDialogExecute(Language: TLanguage; const Text: String): Boolean;
var
  Params: TYesNoQuestionDialogParameters;
begin
  Params := TYesNoQuestionDialogParameters.Create;
  Params.Language := Language;
  Params.Text := Text;
  try
    with TYesNoQuestionDialogForm.Create(Params) do
    begin
      ShowModal;
      Result := (Tag = 1);
      Free;
    end;

  finally
    Params.Free;
  end;
end;


procedure TYesNoQuestionDialogForm.pnlButtonResize(Sender: TObject);
const
  SPACE = 10;
var
  W: Integer;
begin
  W := btnYes.Width + btnNo.Width + SPACE;
  btnYes.Left := (pnlButton.Width - W) div 2;
  btnNo.Left := btnYes.Left + btnYes.Width + SPACE;
end;


procedure TYesNoQuestionDialogForm.btnYesClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TYesNoQuestionDialogForm.btnNoClick(Sender: TObject);
begin
  Close;
end;


procedure TYesNoQuestionDialogForm.PrepareInterface;
const
  SPACE = 10;
var
  W, H: Integer;
  Params: TYesNoQuestionDialogParameters;
begin
  Params := FParameters as TYesNoQuestionDialogParameters;

  //Размеры текста
  GetTextSize(lblText, Params.Text, W, H);

  //Ширина диалога
  Width := Max(btnYes.Width + btnNo.Width + SPACE * 3, W + SPACE * 4);

  //Высота диалога
  Height := pnlButton.Height + H + SPACE * 2;

  //Настроить текст
  lblText.Caption := Params.Text;
  lblText.Left := (ClientWidth - W) div 2;
  lblText.Top := 10;
  lblText.Width := W;
  lblText.Height := H;
end;


procedure TYesNoQuestionDialogForm.SetLanguage;
begin
  Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Caption', 'Вопрос');
  btnYes.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Yes', 'Да');
  btnNo.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'No', 'Нет');
end;



end.

