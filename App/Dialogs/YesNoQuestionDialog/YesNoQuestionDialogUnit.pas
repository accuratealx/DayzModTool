unit YesNoQuestionDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, Math,
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
    procedure GetTextSize(const Str: String; out AWidth, AHeight: Integer);
    procedure PrepareInterface(const ACaption, AText: String);
  public

  end;


function YesNoQuestionDialogExecute(const ACaption, AText: String): Boolean;


implementation

{$R *.lfm}


function YesNoQuestionDialogExecute(const ACaption, AText: String): Boolean;
begin
  with TYesNoQuestionDialogForm.Create(nil) do
  begin
    PrepareInterface(ACaption, AText);
    ShowModal;
    Result := (Tag = 1);
    Free;
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


procedure TYesNoQuestionDialogForm.PrepareInterface(const ACaption, AText: String);
const
  SPACE = 10;
var
  W, H: Integer;
begin
  Caption := ACaption;

  //Размеры текста
  GetTextSize(AText, W, H);

  //Ширина диалога
  Width := Max(btnYes.Width + btnNo.Width + SPACE * 3, W + SPACE * 4);

  //Высота диалога
  Height := pnlButton.Height + H + SPACE * 2;

  //Настроить текст
  lblText.Caption := AText;
  lblText.Left := (ClientWidth - W) div 2;
  lblText.Top := 10;
  lblText.Width := W;
  lblText.Height := H;
end;



end.

