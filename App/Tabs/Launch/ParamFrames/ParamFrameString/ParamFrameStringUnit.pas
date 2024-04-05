unit ParamFrameStringUnit;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Dialogs, StdCtrls,
  StartParamSimple, StartParamString,
  ParamFrameSimpleUnit, Classes;

type
  TParamFrameStringFrame = class(TParamFrameSimpleFrame)
    edValue: TEdit;
    procedure edValueChange(Sender: TObject);
  protected
    procedure PrepareInterface(AItem: TStartParamSimple); override;
  public

  end;


implementation

{$R *.lfm}


procedure TParamFrameStringFrame.edValueChange(Sender: TObject);
begin
  (FItem as TStartParamString).Value := Trim(edValue.Text);
end;


procedure TParamFrameStringFrame.PrepareInterface(AItem: TStartParamSimple);
begin
  inherited PrepareInterface(AItem);

  edValue.Text := (AItem as TStartParamString).Value;
end;



end.

