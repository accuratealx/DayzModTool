unit MemoDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  Language,
  DialogCommonUnit;

type
  TMemoDialogForm = class(TDialogCommonForm)
    mContent: TMemo;
    btnClose: TSpeedButton;
    btnCopyContent: TSpeedButton;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCopyContentClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'Dialogs.Memo.';
  protected
    procedure PrepareInterface; override;
    procedure SetLanguage; override;
  end;


procedure MemoDialogExecute(Language: TLanguage; const Caption: String; const Content: String);


implementation

{$R *.lfm}

uses
  Clipbrd,
  DialogParameters;

type
  TMemoDialogParameters = class(TDialogParameters)
    Caption: String;
    Content: String;
  end;


procedure MemoDialogExecute(Language: TLanguage; const Caption: String; const Content: String);
var
  Params: TMemoDialogParameters;
begin
  Params := TMemoDialogParameters.Create;
  Params.Language := Language;
  Params.Caption := Caption;
  Params.Content := Content;
  try
    with TMemoDialogForm.Create(Params) do
    begin
      ShowModal;
      Free;
    end;

  finally
    Params.Free;
  end;
end;


procedure TMemoDialogForm.FormResize(Sender: TObject);
begin
  btnClose.Left := (pnlButton.Width - btnClose.Width) div 2;
end;


procedure TMemoDialogForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TMemoDialogForm.btnCopyContentClick(Sender: TObject);
begin
  Clipboard.AsText := mContent.Text;
end;


procedure TMemoDialogForm.PrepareInterface;
var
  Param: TMemoDialogParameters;
begin
  Param := FParameters as TMemoDialogParameters;

  Caption := Param.Caption;
  mContent.Text := Param.Content;
end;


procedure TMemoDialogForm.SetLanguage;
begin
  btnClose.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Close', 'Закрыть');
  btnCopyContent.Hint := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'CopyContent', 'Скопировать текст в буфер обмена');
end;



end.

