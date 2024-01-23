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
      PREFIX_DIALOG = 'Dialogs.YesNo.';
  private
    procedure GetTextSize(const Str: String; out AWidth, AHeight: Integer);
  protected
    procedure PrepareInterface; override;
    procedure SetLanguage; override;
  end;


function YesNoQuestionDialogExecute(Language: TLanguage; const Text: String): Boolean;


implementation

{$R *.lfm}

uses
  DialogParameters;

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


procedure TYesNoQuestionDialogForm.GetTextSize(const Str: String; out AWidth, AHeight: Integer);
var
  List: TStringList;
  tw, th, i: Integer;
begin
  AWidth := 0;
  AHeight := 0;

  th := lblText.Canvas.GetTextHeight('W');

  List := TStringList.Create;
  List.LineBreak := sLineBreak;
  List.Text := Str;

  for i := 0 to List.Count - 1 do
  begin
    tw := lblText.Canvas.GetTextWidth(List.Strings[i]);
    if tw > AWidth then
      AWidth := tw;
  end;

  AHeight := th * List.Count;

  List.Free;
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
  GetTextSize(Params.Text, W, H);

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
  Caption := FParameters.Language.GetLocalizedString(PREFIX_DIALOG + 'Caption', 'Вопрос');
  btnYes.Caption := FParameters.Language.GetLocalizedString(PREFIX_DIALOG + 'Yes', 'Да');
  btnNo.Caption := FParameters.Language.GetLocalizedString(PREFIX_DIALOG + 'No', 'Нет');
end;



end.

