unit ParamFrameIntegerUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Spin, Buttons,
  StartParamSimple, StartParamInteger,
  ParamFrameSimpleUnit;

type
  TParamFrameIntegerFrame = class(TParamFrameSimpleFrame)
    seValue: TSpinEdit;
  protected
    procedure PrepareInterface(AItem: TStartParamSimple); override;
  public

  end;


implementation

{$R *.lfm}


procedure TParamFrameIntegerFrame.PrepareInterface(AItem: TStartParamSimple);
begin
  inherited PrepareInterface(AItem);

  seValue.Value := (AItem as TStartParamInteger).Value;
end;



end.

