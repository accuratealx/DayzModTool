unit BuilderTask;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, process, Windows,
  BuilderUtils, DayZUtils, SteamUtils;

const
  WM_BUILDER_EVENT = WM_USER + 1;

type
  TBuilderTaskResult = (
    btrOK,
    btrCantFindBuildTool,
    btrCantCreateIncFile,
    btrSourceDirectoryNotExist,
    btrDestinationDirectoryNotExist,
    btrBuildFail
  );

  TBuilderTask = class(TThread)
  private
    FOutput: TStringList;
    FParams: TBuilderItemBuildData;
    FResult: TBuilderTaskResult;
    FMainFormHandle: THANDLE;

  public
    constructor Create(MainFormHandle: THANDLE; Params: TBuilderItemBuildData);
    destructor  Destroy; override;

    procedure Execute; override;

    property Output: TStringList read FOutput;
    property Result: TBuilderTaskResult read FResult;
  end;


implementation


constructor TBuilderTask.Create(MainFormHandle: THANDLE; Params: TBuilderItemBuildData);
begin
  inherited Create(True);
  FreeOnTerminate := False;

  FOutput := TStringList.Create;
  FParams := Params;
  FMainFormHandle := MainFormHandle;
end;


destructor TBuilderTask.Destroy;
begin
  FOutput.Free;

  inherited Destroy;
end;


procedure TBuilderTask.Execute;
const
  BUF_SIZE = 1024 * 64;
var
  Buffer: string = '';
  BytesRead: LongInt = 0;
  Proc: TProcess;
  Prm, Exe, IncFile: String;
begin
  Proc := TProcess.Create(nil);
  try
    //Каталог исходника
    if not DirectoryExists(FParams.SourceDirectory) then
    begin
      FResult := btrSourceDirectoryNotExist;
      Exit;
    end;

    //Каталог назначения
    if not DirectoryExists(FParams.DestinationDirectory) then
    begin
      FResult := btrDestinationDirectoryNotExist;
      Exit;
    end;

    //Билдилка
    Exe := GetPackToolFile;
    if not FileExists(Exe) then
    begin
      FResult := btrCantFindBuildTool;
      Exit;
    end;

    //Файл с расширениями
    IncFile := GetIncludeFilePath;
    if not CreateIncludeFile(IncFile, FParams.Extensions) then
    begin
      FResult := btrCantCreateIncFile;
      Exit;
    end;

    //Подготовим параметры для билдилки
    Prm := GetBuildParamString(
      FParams.SourceDirectory,
      FParams.DestinationDirectory,
      FParams.Prefix,
      FParams.Sign,
      IncFile,
      FParams.ProjectDrive,
      FParams.Version
    );

    //Настроим параметры
    Proc.Options := [poUsePipes, poStderrToOutPut];
    Proc.ShowWindow := swoHIDE;
    Proc.Executable := Exe;
    Proc.Parameters.Add(Prm);

    //Запустим процесс
    Proc.Execute;

    //Выбираем данные из потока
    repeat
      SetLength(Buffer, BUF_SIZE);
      BytesRead := Proc.Output.Read(Buffer[1], Length(Buffer));

      if BytesRead > 0 then
      begin
        SetLength(Buffer, BytesRead);
        FOutput.Append(Buffer);
      end;
    until BytesRead = 0;

    //Проверить статус билда
    if Pos('Build Successful', FOutput.Text) > 0 then
      FResult := btrOK
    else
      FResult := btrBuildFail;

  finally
    Proc.Free;

    //Скажем основному потоку что задание выполнено
    PostMessage(FMainFormHandle, WM_BUILDER_EVENT, 0, 0);
  end;
end;



end.

