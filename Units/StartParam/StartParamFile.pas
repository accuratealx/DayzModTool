unit StartParamFile;

{$mode ObjFPC}{$H+}

interface

uses
  StartParamSimple;

type
  TStartParamFile = class(TStartParamSimple)
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


function TStartParamFile.GetType: String;
begin
  Result := 'File';
end;


constructor TStartParamFile.Create(const AName, APrefix, ADescription: String; AValue: String; ADefault: String);
begin
  inherited Create(AName, APrefix, ADescription);
  FValue := AValue;
  FDefault := ADefault;
end;


function TStartParamFile.ValueToString: String;
begin
  Result := inherited ValueToString;
  Result := Result + ';' + FValue;
end;


procedure TStartParamFile.ValueFromString(const Str: String);
var
  List: TStringList;
begin
  List := TStringList.Create;
  List.LineBreak := ';';
  List.Text := Str;

  if List.Count = 2 then
  begin
    inherited ValueFromString(List.Strings[0]);
    FValue := List.Strings[1];
  end;

  List.Free;
end;


procedure TStartParamFile.SetDefaultValue;
begin
  FValue := FDefault;
end;


function TStartParamFile.GetCommandLine: String;
begin
  Result := inherited GetCommandLine + '="' + FValue + '"';
end;



end.

