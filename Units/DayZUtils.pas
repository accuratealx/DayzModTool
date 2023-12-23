unit DayZUtils;

{$mode ObjFPC}{$H+}

interface


procedure OpenFolderInExplorer(const Folder: String);
procedure DeleteFolderToRecycle(const Folder: String);
procedure ExecuteFile(const ExeFile: String; const Params: String);
procedure KillProcess(const ProcName: String);


implementation

uses
  sgeStringList, sgeFileUtils,
  LazUTF8, SysUtils, JwaTlHelp32, windows, shlobj;


procedure OpenFolderInExplorer(const Folder: String);
var
  Dir: String;
begin
  Dir := UTF8ToWinCP(Folder);
  ShellExecute(0, 'explore', pChar(Dir), nil, nil, SW_SHOWNORMAL);
end;


procedure DeleteFolderToRecycle(const Folder: String);

  procedure DeleteFileToRecycle(FileName: String);
  var
    fos: TSHFILEOPSTRUCT;
  begin
    FileName := UTF8ToWinCP(FileName);
    ZeroMemory(@fos, SizeOf(fos));
    fos.wFunc  := FO_DELETE;
    fos.fFlags := FOF_SILENT or FOF_NOCONFIRMATION or FOF_ALLOWUNDO;
    fos.pFrom  := PChar(FileName + #0);
    ShFileOperation(fos);
  end;

var
  FileList, DirectoryList: TsgeStringList;
  i: Integer;
  Dir: String;
begin
  Dir := sgeCheckPathDelimiter(Folder);

  if not DirectoryExists(Dir) then
    Exit;

  FileList := TsgeStringList.Create;
  DirectoryList := TsgeStringList.Create;
  try

    //Получить список каталогов в папке
    sgeGetDirectoryListInFolder(Dir, DirectoryList);

    //Получить список файлов в папке
    sgeGetFileListInFolder(Dir, FileList);

    //Удалить каталоги
    for i := 0 to DirectoryList.Count - 1 do
      DeleteFileToRecycle(Dir + DirectoryList.Part[i]);

    //Удалить файлы
    for i := 0 to FileList.Count - 1 do
      DeleteFileToRecycle(Dir + FileList.Part[i]);

  finally
    DirectoryList.Free;
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



end.

