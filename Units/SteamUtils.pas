unit SteamUtils;

{$mode ObjFPC}{$H+}

interface


function GetSteamInstallPathFromRegistry: String;
function GetDayZToolsInstallPathFromRegistry: String;


implementation

uses
  SysUtils, Registry;


function GetSteamInstallPathFromRegistry: String;
var
  R: TRegistry;
  i: Integer;
begin
  Result := '';

  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    if R.OpenKeyReadOnly('\SOFTWARE\Valve\Steam') then
      Result := R.ReadString('SteamPath');
    R.CloseKey;

    if Result <> '' then
    begin
      Result := IncludeTrailingBackslash(Result) + 'steamapps\common\';
      for i := 1 to Length(Result) do
        if Result[i] = '/' then
          Result[i] := '\';
    end;

  finally
    R.Free;
  end;
end;


function GetDayZToolsInstallPathFromRegistry: String;
var
  R: TRegistry;
begin
  Result := '';

  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    if R.OpenKeyReadOnly('\SOFTWARE\Bohemia Interactive\Dayz Tools') then
      Result := IncludeTrailingBackslash(R.ReadString('path'));
    R.CloseKey;

  finally
    R.Free;
  end;
end;



end.

