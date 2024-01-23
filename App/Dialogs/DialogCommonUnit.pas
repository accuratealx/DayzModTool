unit DialogCommonUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  sgeKeys,
  DialogParameters;

type
  TDialogCommonForm = class(TForm)
    pnlContent: TPanel;
    pnlButton: TPanel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    FParameters: TDialogParameters;
    procedure PrepareInterface; virtual;
    procedure SetLanguage; virtual;
  public
    constructor Create(Parameters: TDialogParameters); reintroduce;
  end;


implementation

{$R *.lfm}


procedure TDialogCommonForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    keyEscape:
      Close;
  end;
end;


procedure TDialogCommonForm.PrepareInterface;
begin
  //Заглушка
end;


procedure TDialogCommonForm.SetLanguage;
begin
  //Заглушка
end;


constructor TDialogCommonForm.Create(Parameters: TDialogParameters);
begin
  inherited Create(nil);

  //Сохранить параметры
  FParameters := Parameters;

  //Подготовить интерфейс
  PrepareInterface;

  //Изменить язык
  SetLanguage;
end;



end.

