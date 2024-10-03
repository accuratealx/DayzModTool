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
    constructor Create(Parameters: TTabParameters; AParent: TWinControl); reintroduce;
    destructor  Destroy; override;

    procedure ApplyLanguage; virtual;
    procedure SaveSettings; virtual;
    procedure LoadSettings; virtual;


    property Params: TTabParameters read FParams write SetParams;
  end;


implementation

{$R *.lfm}


procedure TTabCommonFrame.SetParams(AParameters: TTabParameters);
begin
  //Скопировать новые параметры
  FParams.CopyFrom(AParameters);
end;


constructor TTabCommonFrame.Create(Parameters: TTabParameters; AParent: TWinControl);
begin
  inherited Create(nil);
  Parent := AParent;

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


procedure TTabCommonFrame.SaveSettings;
begin
  //Переопределяется в потомках
end;


procedure TTabCommonFrame.LoadSettings;
begin
  //Переопределяется в потомках
end;



end.

