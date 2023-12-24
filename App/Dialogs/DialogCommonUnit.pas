unit DialogCommonUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  sgeKeys;

type
  TDialogCommonForm = class(TForm)
    pnlContent: TPanel;
    pnlButton: TPanel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

  public

  end;

var
  DialogCommonForm: TDialogCommonForm;

implementation

{$R *.lfm}


procedure TDialogCommonForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    keyEscape:
      Close;
  end;
end;



end.

