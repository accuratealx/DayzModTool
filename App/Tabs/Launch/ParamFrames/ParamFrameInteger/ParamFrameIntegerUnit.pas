unit ParamFrameIntegerUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Spin,
  StartParamSimple, StartParamInteger,
  ParamFrameSimpleUnit, Types;

type
  TParamFrameIntegerFrame = class(TParamFrameSimpleFrame)
    seValue: TSpinEdit;
    procedure seValueChange(Sender: TObject);
    procedure seValueMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  protected
    procedure PrepareInterface(AItem: TStartParamSimple); override;
  end;


implementation

{$R *.lfm}


procedure TParamFrameIntegerFrame.seValueMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
end;


procedure TParamFrameIntegerFrame.seValueChange(Sender: TObject);
begin
  (FItem as TStartParamInteger).Value := seValue.Value;
end;


procedure TParamFrameIntegerFrame.PrepareInterface(AItem: TStartParamSimple);
begin
  inherited PrepareInterface(AItem);

  seValue.Value := (AItem as TStartParamInteger).Value;
end;



end.

