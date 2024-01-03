unit YesNoQuestionDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
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
begin
  Caption := ACaption;
  lblText.Caption := AText;
end;



end.

