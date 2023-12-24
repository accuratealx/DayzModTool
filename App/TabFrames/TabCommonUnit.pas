unit TabCommonUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, Controls;

type
  TTabCommonFrame = class(TFrame)
  private
    FCaption: String;
    FIcon: TIcon;

    procedure SetIcon(AIcon: TIcon);
  public
    constructor Create(const ACaption: string; AIcon: TIcon); reintroduce;
    destructor  Destroy; override;

    property Icon: TIcon read FIcon write SetIcon;
    property Caption: String read FCaption write FCaption;
  end;


implementation

{$R *.lfm}


procedure TTabCommonFrame.SetIcon(AIcon: TIcon);
begin
  FIcon.Assign(AIcon);
end;


constructor TTabCommonFrame.Create(const ACaption: string; AIcon: TIcon);
begin
  inherited Create(nil);
  FCaption := ACaption;
  FIcon := TIcon.Create;
  FIcon.Assign(AIcon);
end;


destructor TTabCommonFrame.Destroy;
begin
  FIcon.Free;
  inherited Destroy;
end;



end.

