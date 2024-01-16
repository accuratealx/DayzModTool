unit ParamFrameStringUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  StartParamSimple, StartParamString,
  ParamFrameSimpleUnit;

type
  TParamFrameStringFrame = class(TParamFrameSimpleFrame)
    edValue: TEdit;
  protected
    procedure PrepareInterface(AItem: TStartParamSimple); override;
  public

  end;


implementation

{$R *.lfm}


procedure TParamFrameStringFrame.PrepareInterface(AItem: TStartParamSimple);
begin
  inherited PrepareInterface(AItem);

  edValue.Text := (AItem as TStartParamString).Value;
end;



end.

