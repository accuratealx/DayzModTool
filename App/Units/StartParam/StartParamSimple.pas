unit StartParamSimple;

{$mode ObjFPC}{$H+}

interface

type
  TStartParamSimple = class
  protected
    FEnable: Boolean;     //Флаг активности
    FPrefix: String;      //Префикс перед именем
    FName: String;        //Имя параметра
    FDescription: String; //Описание

    function GetType: String; virtual;
  public
    constructor Create(const AName, APrefix, ADescription: String);

    function  ValueToString: String; virtual;
    procedure ValueFromString(const Str: String); virtual;

    procedure SetDefaultValue; virtual;

    function GetCommandLine: String; virtual;

    property &Type: String read GetType;
    property Enable: Boolean read FEnable write FEnable;
    property Prefix: String read FPrefix write FPrefix;
    property Name: String read FName write FName;
    property Description: String read FDescription write FDescription;
  end;


implementation

uses
  SysUtils;


function TStartParamSimple.GetType: String;
begin
  Result := 'Simple';
end;


constructor TStartParamSimple.Create(const AName, APrefix, ADescription: String);
begin
  FName := AName;
  FPrefix := APrefix;
  FDescription := ADescription;
end;


function TStartParamSimple.ValueToString: String;
begin
  Result := BoolToStr(FEnable);
end;


procedure TStartParamSimple.ValueFromString(const Str: String);
begin
  if not TryStrToBool(Str, FEnable) then
    FEnable := False;
end;


procedure TStartParamSimple.SetDefaultValue;
begin
  //Заглушка
end;


function TStartParamSimple.GetCommandLine: String;
begin
  Result := FPrefix + FName;
end;



end.

