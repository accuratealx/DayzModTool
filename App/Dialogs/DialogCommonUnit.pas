unit DialogCommonUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons;

type
  TDialogCommonForm = class(TForm)
    pnlContent: TPanel;
    pnlButton: TPanel;
  private

  public

  end;

var
  DialogCommonForm: TDialogCommonForm;

implementation

{$R *.lfm}

end.

