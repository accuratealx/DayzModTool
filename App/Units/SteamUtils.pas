unit SteamUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes;


function  GetSteamInstallPathFromRegistry: String;
function  GetDayZToolsInstallPathFromRegistry: String;
procedure GetSteamDayZDirectoryList(List: TStringList);

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


//Формат списка [Заголовок];[Путь]
procedure GetSteamDayZDirectoryList(List: TStringList);

  procedure AddToResult(const Caption, Path: String; const Icon: String = ''; Force: Boolean = False);
  const
    SEPARATOR = ';;;';
  begin
    if Force or DirectoryExists(Path) then
      List.Add(Caption + SEPARATOR + Path + SEPARATOR + Icon);
  end;

var
  SteamPath, DayZToolsPath: String;
begin
  List.Clear;

  SteamPath := GetSteamInstallPathFromRegistry;
  DayZToolsPath := GetDayZToolsInstallPathFromRegistry;

  //Каталоги стим
  if DirectoryExists(SteamPath) then
  begin
    AddToResult('DayZ клиент', SteamPath + 'DayZ', 'Launch.Client.ico');
    AddToResult('DayZ сервер', SteamPath + 'DayZServer', 'Launch.DedicatedServer.ico');
  end;

  //Кататлоги Dayz Tools
  if DirectoryExists(DayZToolsPath) then
  begin
    AddToResult('DayZ утилиты', DayZToolsPath + 'Bin', 'Launch.DayZTools.ico');
    AddToResult('DayZ журналы утилит', DayZToolsPath + 'Bin\Logs', 'Folder.Page.ico');
  end;

  //Каталог настроек клиента
  AddToResult('DayZ настройки пользователя', GetEnvironmentVariable('USERPROFILE') + '\Documents\DayZ', 'Folder.Page.ico', True);

  //Папка логов DayZ
  AddToResult('DayZ журналы', GetEnvironmentVariable('USERPROFILE') + '\AppData\Local\DayZ', 'Folder.Page.ico', True);
end;



end.

