unit StartParamDirectory;

{$mode ObjFPC}{$H+}

interface

uses
  StartParamSimple;

type
  TStartParamDirectory = class(TStartParamSimple)
  protected
    FValue: String;
    FDefault: String;

    function GetType: String; override;
  public
    constructor Create(const AName, APrefix, ADescription: String; AValue: String = ''; ADefault: String = '');

    function  ValueToString: String; override;
    procedure ValueFromString(const Str: String); override;

    procedure SetDefaultValue; override;

    function GetCommandLine: String; override;

    property Value: String read FValue write FValue;
    property Default: String read FDefault write FDefault;
  end;


implementation

uses
  Classes;


function TStartParamDirectory.GetType: String;
begin
  Result := 'Directory';
end;


constructor TStartParamDirectory.Create(const AName, APrefix, ADescription: String; AValue: String; ADefault: String);
begin
  inherited Create(AName, APrefix, ADescription);
  FValue := AValue;
  FDefault := ADefault;
end;


function TStartParamDirectory.ValueToString: String;
begin
  Result := inherited ValueToString;
  Result := Result + ';' + FValue;
end;


procedure TStartParamDirectory.ValueFromString(const Str: String);
var
  List: TStringList;
begin
  List := TStringList.Create;
  List.LineBreak := ';';
  List.Text := Str;

  //Активность
  if List.Count > 0 then
    inherited ValueFromString(List.Strings[0]);

  //Значение
  if List.Count > 1 then
    FValue := List.Strings[1];

  List.Free;
end;


procedure TStartParamDirectory.SetDefaultValue;
begin
  FValue := FDefault;
end;


function TStartParamDirectory.GetCommandLine: String;
begin
  Result := inherited GetCommandLine + '="' + FValue + '"';
end;



end.

