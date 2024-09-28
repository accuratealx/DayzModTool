unit DayZUtils;

{$mode ObjFPC}{$H+}

interface

uses
  sgeStringList;

function  ExecuteFileAndWait(const FileName, Params: String; StartDir: String = ''): Integer;
function  DeleteFileToRecycle(FileName: String): Boolean;
procedure OpenFolderInExplorer(const Folder: String);
procedure DeleteFolderToRecycle(const Folder: String);
procedure DeleteFilesToRecycle(const Folder: String);
procedure ExecuteFile(const ExeFile: String; const Params: String);
procedure KillProcess(const ProcName: String);
procedure GetDayZTrashDirectoryList(List: TsgeStringList);
procedure GetDayzTrashFileList(list: TsgeStringList);
function  IsDirectoryEmpty(Dir: String): Boolean;

implementation

uses
  sgeFileUtils,
  LazUTF8, SysUtils, JwaTlHelp32, windows, ShellApi, SteamUtils;


function ExecuteFileAndWait(const FileName, Params: String; StartDir: String): Integer;
var
  Info: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  FillChar(Info, SizeOf(Info), 0);
  Info.cbSize := SizeOf(TShellExecuteInfo);

  if Trim(StartDir) = '' then
    StartDir := ExtractFilePath(FileName);

  with Info do
  begin
    fMask := SEE_MASK_NOCLOSEPROCESS;
    lpFile := PChar(FileName);
    lpParameters := PChar(Params);
    lpDirectory := PChar(StartDir);
    nShow := SW_HIDE;
  end;

  if ShellExecuteExA(@Info) then
  begin
    repeat
      GetExitCodeProcess(Info.hProcess, ExitCode);
    until (ExitCode <> STILL_ACTIVE);
    Result := ExitCode;
  end
  else
    Result := -1;
end;


procedure OpenFolderInExplorer(const Folder: String);
var
  Dir: String;
begin
  Dir := UTF8ToWinCP(Folder);
  ShellExecute(0, 'explore', pChar(Dir), nil, nil, SW_SHOWNORMAL);
end;


function DeleteFileToRecycle(FileName: String): Boolean;
var
  fos: TSHFILEOPSTRUCT;
begin
  FileName := UTF8ToWinCP(FileName);
  ZeroMemory(@fos, SizeOf(fos));
  fos.wFunc  := FO_DELETE;
  fos.fFlags := FOF_SILENT or FOF_NOCONFIRMATION or FOF_ALLOWUNDO;
  fos.pFrom  := PChar(FileName + #0);
  Result := ShFileOperation(fos) = 0;
end;


procedure DeleteFolderToRecycle(const Folder: String);
var
  DirectoryList: TsgeStringList;
  i: Integer;
  Dir: String;
begin
  Dir := sgeCheckPathDelimiter(Folder);

  if not DirectoryExists(Dir) then
    Exit;

  DirectoryList := TsgeStringList.Create;
  try
    //Получить список каталогов в папке
    sgeGetDirectoryListInFolder(Dir, DirectoryList);

    //Удалить каталоги
    for i := 0 to DirectoryList.Count - 1 do
      DeleteFileToRecycle(Dir + DirectoryList.Part[i]);

    //Удалить файлы
    DeleteFilesToRecycle(Dir);

  finally
    DirectoryList.Free;
  end;
end;


procedure DeleteFilesToRecycle(const Folder: String);
var
  FileList: TsgeStringList;
  i: Integer;
  Dir: String;
begin
  Dir := sgeCheckPathDelimiter(Folder);

  if not DirectoryExists(Dir) then
    Exit;

  FileList := TsgeStringList.Create;
  try
    //Получить список файлов в папке
    sgeGetFileListInFolder(Dir, FileList);

    //Удалить файлы
    for i := 0 to FileList.Count - 1 do
      DeleteFileToRecycle(Dir + FileList.Part[i]);

  finally
    FileList.Free;
  end;
end;


procedure ExecuteFile(const ExeFile: String; const Params: String);
var
  Fn, Prm, Dir: String;
begin
  Fn := UTF8ToWinCP(ExeFile);
  Prm := UTF8ToWinCP(Params);
  Dir := UTF8ToWinCP(sgeExtractFilePath(ExeFile));
  ShellExecute(0, nil, pChar(Fn), PChar(Prm), PChar(Dir), SW_SHOWNORMAL);
end;


procedure KillProcess(const ProcName: String);
const
  PROCESS_TERMINATE = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
  Process: HANDLE;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);

  while Integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) = UpperCase(ProcName)) or
        (UpperCase(FProcessEntry32.szExeFile) = UpperCase(ProcName))) then
    begin
      Process := OpenProcess(PROCESS_TERMINATE, BOOL(0), FProcessEntry32.th32ProcessID);
      TerminateProcess(Process, 0);
     end;
     ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;

  CloseHandle(FSnapshotHandle);
end;


procedure GetDayZTrashDirectoryList(List: TsgeStringList);
var
  upd: String;
begin
  //Каталог профиля
  upd := SysUtils.GetEnvironmentVariable('USERPROFILE') + '\';

  //Каталог инструментов DayZ
  List.Add(GetDayZToolsInstallPathFromRegistry + 'Bin\Logs');

  //Документы
  List.Add(upd + 'Documents\DayZ');
  List.Add(upd + 'Documents\DayZ Other Profiles');
  List.Add(upd + 'Documents\DayZ Projects');

  //AppData
  List.Add(upd + 'AppData\Local\D3DSCache');
  List.Add(upd + 'AppData\Local\Bohemia_Interactive');
  List.Add(upd + 'AppData\Local\Bohemia_Interactive_a.s');
  List.Add(upd + 'AppData\Local\DayZ');
  List.Add(upd + 'AppData\Local\DayZ Exp');
  List.Add(upd + 'AppData\Local\DayZ Launcher');
  List.Add(upd + 'AppData\Local\DayZ Publisher');
end;


procedure GetDayzTrashFileList(List: TsgeStringList);
var
  DirList: TsgeStringList;
  i: Integer;
begin
  DirList := TsgeStringList.Create;
  try
    //Получить список каталогов с мусором
    GetDayZTrashDirectoryList(DirList);

    //Собрать до кучи
    for i := 0 to DirList.Count - 1 do
      sgeFindFilesInFolder(DirList.Part[i], List);

  finally
    DirList.Free;
  end;
end;


function IsDirectoryEmpty(Dir: String): Boolean;
const
  FileAttribAnyFile = $000001FF;
var
  o: TSearchRec;
  Idx: Integer;
begin
  Result := True;
  Dir := sgeCheckPathDelimiter(Dir);

  Idx := FindFirst(Dir + '*', FileAttribAnyFile, o);
  while Idx = 0 do
  begin
    if (o.Name <> '.') and (o.Name <> '..') then
      begin
        Result := False;
        Break;
      end;
    Idx := FindNext(o);
  end;

  SysUtils.FindClose(o);
end;



end.

