unit ParamFrameSimpleUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  StartParamSimple,
  ParamFrameCommonUnit;

type
  TParamFrameSimpleFrame = class(TParamFrameCommonFrame)
    cbParamName: TCheckBox;
    btnDefault: TSpeedButton;
    procedure btnDefaultClick(Sender: TObject);
    procedure cbParamNameClick(Sender: TObject);
  protected
    FItem: TStartParamSimple;

    procedure PrepareInterface(AItem: TStartParamSimple); virtual;
  public
    constructor Create(AItem: TStartParamSimple); reintroduce;

    procedure UpdateInterface; virtual;

    property Item: TStartParamSimple read FItem;
  end;


implementation

{$R *.lfm}


procedure TParamFrameSimpleFrame.btnDefaultClick(Sender: TObject);
begin
  //Установить значение по умолчанию
  FItem.SetDefaultValue;

  //Обновить редактор
  PrepareInterface(FItem);
end;


procedure TParamFrameSimpleFrame.cbParamNameClick(Sender: TObject);
begin
  FItem.Enable := cbParamName.Checked;
end;


procedure TParamFrameSimpleFrame.PrepareInterface(AItem: TStartParamSimple);
begin
  cbParamName.Caption := AItem.Name;
  cbParamName.Checked := AItem.Enable;
  cbParamName.Hint := AItem.Description;
end;


constructor TParamFrameSimpleFrame.Create(AItem: TStartParamSimple);
begin
  inherited Create(nil);
  FItem := AItem;
  PrepareInterface(FItem);
end;


procedure TParamFrameSimpleFrame.UpdateInterface;
begin
  PrepareInterface(FItem);
end;



end.

