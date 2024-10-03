unit InputDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  Language, DialogCommonUnit;

type
  TInputDialogForm = class(TDialogCommonForm)
    btnClose: TSpeedButton;
    btnApply: TSpeedButton;
    edValue: TEdit;
    lblText: TLabel;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure edValueChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'Dialogs.Input.';
  protected
    procedure PrepareInterface; override;
    procedure SetLanguage; override;
  end;


function InputDialogExecute(Language: TLanguage; const Text: String; var Value: String): Boolean;


implementation

{$R *.lfm}

uses
  Math,
  DialogUtils, DialogParameters;

type
  TInputDialogParameters = class(TDialogParameters)
    Text: String;
    Value: String;
  end;


function InputDialogExecute(Language: TLanguage; const Text: String; var Value: String): Boolean;
var
  Params: TInputDialogParameters;
begin
  Result := False;

  Params := TInputDialogParameters.Create;
  Params.Language := Language;
  Params.Text := Text;
  Params.Value := Value;
  try
    with TInputDialogForm.Create(Params) do
    begin
      ShowModal;
      if Tag = 1 then
      begin
        Value := Trim(edValue.Caption);
        Result := True;
      end;
      Free;
    end;

  finally
    Params.Free;
  end;
end;


procedure TInputDialogForm.FormResize(Sender: TObject);
const
  SPACE = 10;
var
  W: Integer;
begin
  W := btnApply.Width + btnClose.Width + SPACE;
  btnApply.Left := (pnlButton.Width - W) div 2;
  btnClose.Left := btnApply.Left + btnApply.Width + SPACE;
end;


procedure TInputDialogForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TInputDialogForm.edValueChange(Sender: TObject);
begin
  btnApply.Enabled := (edValue.Text <> '') and (Trim(edValue.Text) <> TInputDialogParameters(FParameters).Value);
end;


procedure TInputDialogForm.btnApplyClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TInputDialogForm.PrepareInterface;
var
  Params: TInputDialogParameters;
begin
  Params := FParameters as TInputDialogParameters;

  //Настроить параметры
  lblText.Caption := Params.Text;
  edValue.Text := Params.Value;
end;


procedure TInputDialogForm.SetLanguage;
begin
  Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Caption', 'Ввод');
  btnApply.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Apply', 'Применить');
  btnClose.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Close', 'Закрыть');
end;



end.

