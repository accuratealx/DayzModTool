unit MessageDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  Language, DialogCommonUnit;

type
  TMessageDialogForm = class(TDialogCommonForm)
    btnClose: TSpeedButton;
    lblText: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'Dialogs.Message.';
  protected
    procedure PrepareInterface; override;
    procedure SetLanguage; override;
  end;


procedure MessageDialogExecute(Language: TLanguage; const Text: String);


implementation

{$R *.lfm}

uses
  Math,
  DialogUtils, DialogParameters;

type
  TMessageDialogParameters = class(TDialogParameters)
    Text: String;
  end;

procedure MessageDialogExecute(Language: TLanguage; const Text: String);
var
  Params: TMessageDialogParameters;
begin
  Params := TMessageDialogParameters.Create;
  Params.Language := Language;
  Params.Text := Text;
  try
    with TMessageDialogForm.Create(Params) do
    begin
      ShowModal;
      Free;
    end;

  finally
    Params.Free;
  end;
end;


procedure TMessageDialogForm.FormResize(Sender: TObject);
begin
  btnClose.Left := (pnlButton.Width - btnClose.Width) div 2;
end;


procedure TMessageDialogForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TMessageDialogForm.PrepareInterface;
const
  SPACE = 10;
var
  W, H: Integer;
  Params: TMessageDialogParameters;
begin
  Params := FParameters as TMessageDialogParameters;

  //Размеры текста
  GetTextSize(lblText, Params.Text, W, H);

  //Ширина диалога
  Width := Max(btnClose.Width + SPACE * 2, W + SPACE * 4);

  //Высота диалога
  Height := pnlButton.Height + H + SPACE * 2;

  //Настроить текст
  lblText.Caption := Params.Text;
  lblText.Left := (ClientWidth - W) div 2;
  lblText.Top := 10;
  lblText.Width := W;
  lblText.Height := H;
end;


procedure TMessageDialogForm.SetLanguage;
begin
  Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Caption', 'Сообщение');
  btnClose.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Close', 'Закрыть');
end;



end.

