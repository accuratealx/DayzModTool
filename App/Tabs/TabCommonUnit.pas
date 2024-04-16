unit TabCommonUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  TabParameters;

type
  TTabCommonFrame = class(TFrame)
  protected
    FParams: TTabParameters;

    procedure SetParams(AParameters: TTabParameters);
  public
    constructor Create(Parameters: TTabParameters); reintroduce;
    destructor  Destroy; override;

    procedure ApplyLanguage; virtual;

    property Params: TTabParameters read FParams write SetParams;
  end;


implementation

{$R *.lfm}


procedure TTabCommonFrame.SetParams(AParameters: TTabParameters);
begin
  //Скопировать новые параметры
  FParams.CopyFrom(AParameters);
end;


constructor TTabCommonFrame.Create(Parameters: TTabParameters);
begin
  inherited Create(nil);

  //Подготовить параметры
  FParams := TTabParameters.Create;
  SetParams(Parameters);
end;


destructor TTabCommonFrame.Destroy;
begin
  //Почистить параметры
  FParams.Free;

  inherited Destroy;
end;


procedure TTabCommonFrame.ApplyLanguage;
begin
  //Переопределяется в потомках
end;



end.

