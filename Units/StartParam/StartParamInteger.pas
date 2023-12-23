unit StartParamInteger;

{$mode ObjFPC}{$H+}

interface

uses
  StartParamSimple;

type
  TStartParamInteger = class(TStartParamSimple)
  protected
    FValue: Integer;
    FDefault: Integer;

    function GetType: String; override;
  public
    constructor Create(const AName, APrefix, ADescription: String; AValue: Integer = 0; ADefault: Integer = 0);

    function  ValueToString: String; override;
    procedure ValueFromString(const Str: String); override;

    procedure SetDefaultValue; override;

    function GetCommandLine: String; override;

    property Value: Integer read FValue write FValue;
    property Default: Integer read FDefault write FDefault;
  end;


implementation

uses
  SysUtils, Classes;


function TStartParamInteger.GetType: String;
begin
  Result := 'Integer';
end;


constructor TStartParamInteger.Create(const AName, APrefix, ADescription: String; AValue: Integer; ADefault: Integer);
begin
  inherited Create(AName, APrefix, ADescription);
  FValue := AValue;
  FDefault := ADefault;
end;


function TStartParamInteger.ValueToString: String;
begin
  Result := inherited ValueToString;
  Result := Result + ';' + IntToStr(FValue);
end;


procedure TStartParamInteger.ValueFromString(const Str: String);
var
  List: TStringList;
begin
  List := TStringList.Create;
  List.LineBreak := ';';
  List.Text := Str;

  if List.Count = 2 then
  begin
    inherited ValueFromString(List.Strings[0]);
    if not TryStrToInt(List.Strings[1], FValue) then
      FValue := FDefault;
  end;

  List.Free;
end;


procedure TStartParamInteger.SetDefaultValue;
begin
  FValue := FDefault;
end;


function TStartParamInteger.GetCommandLine: String;
begin
  Result := inherited GetCommandLine + '=' + IntToStr(FValue);
end;



end.

