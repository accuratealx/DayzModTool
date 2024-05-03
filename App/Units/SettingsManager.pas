unit SettingsManager;

{$mode ObjFPC}{$H+}

interface

uses
  Zipper, SysUtils, FileUtil;


procedure SettingsManager_Export(SettingsDirectory: String; BackupFile: String);
procedure SettingsManager_Import(SettingsDirectory: String; BackupFile: String);

implementation

uses
  sgeStringList, sgeFileUtils;


procedure SettingsManager_Export(SettingsDirectory: String; BackupFile: String);
var
  Zip: TZipper;
  List: TsgeStringList;
  i, Len: Integer;
  s: String;
begin
  //Удалить старый файл, что бы не обновлять их в архиве
  if FileExists(BackupFile) then
    DeleteFile(BackupFile);

  SettingsDirectory := IncludeTrailingBackslash(SettingsDirectory);
  Len := Length(SettingsDirectory);

  SetCurrentDir(SettingsDirectory);

  Zip := TZipper.Create;
  List := TsgeStringList.Create;
  try
    Zip.FileName := BackupFile;

    //Найдем все файлы в каталоге
    sgeFindFilesInFolder(SettingsDirectory, List);

    //Добавим файлы в запаковщик
    for i := 0 to List.Count - 1 do
    begin
      s := List.Part[i];
      Delete(s, 1, Len);
      Zip.Entries.AddFileEntry(s);
    end;

    //Запаковать
    Zip.ZipAllFiles;

  finally
    List.Free;
    Zip.Free;
  end;
end;


procedure SettingsManager_Import(SettingsDirectory: String; BackupFile: String);
var
  Zip: TUnZipper;
  TmpDir: String;
  List: TsgeStringList;
  i, Len: Integer;
  fn, dst: String;
begin
  SettingsDirectory := IncludeTrailingBackslash(SettingsDirectory);

  //Подготовить временный каталог
  TmpDir := IncludeTrailingBackslash(GetEnvironmentVariable('TEMP')) + 'DayZModTool.tmp\';
  if DirectoryExists(TmpDir) then
    DeleteDirectory(TmpDir, True)
  else
    ForceDirectories(TmpDir);


  Zip := TUnZipper.Create;
  List := TsgeStringList.Create;
  try
    //Настроить распаковщик
    Zip.FileName := BackupFile;
    Zip.OutputPath := TmpDir;

    //Распаковать все во временную папку
    Zip.UnZipAllFiles;

    //Удалить все файлы в каталоге настроек
    DeleteDirectory(SettingsDirectory, True);

    //Найти все файлы во временной папке
    sgeFindFilesInFolder(TmpDir, List);

    //Скопировать файлы из временной папки в каталог настроек
    Len := Length(TmpDir);
    for i := 0 to List.Count - 1 do
    begin
      fn := List.Part[i];
      Delete(fn, 1, Len);
      dst := SettingsDirectory + fn;
      CopyFile(List.Part[i], dst, [cffOverwriteFile, cffCreateDestDirectory], True);
    end;

  finally
    //Удалить временный каталог
    DeleteDirectory(TmpDir, False);

    List.Free;
    Zip.Free;
  end;
end;



end.

