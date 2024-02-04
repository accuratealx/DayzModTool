unit ExtractWorker;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Windows;

const
  WM_EXTRACT_WORKER = WM_USER + 1;
  WORKER_PBO_ERROR = 1;
  WORKER_PBO_COUNT = 2;
  WORKER_PBO_STEP = 3;
  WORKER_BIN_ERROR = 4;
  WORKER_BIN_COUNT = 5;
  WORKER_BIN_STEP = 6;
  WORKER_FINISH = 7;
  WORKER_CANCEL = 8;

type
  TExtractWorker = class(TThread)
  private
    type
      TMessageType = (
        mtPBOError,
        mtPBOCount,
        mtPBOStep,
        mtBINError,
        mtBINCount,
        mtBINStep,
        mtFinish,
        mtCancel
      );
  private
    FExtractTool: String;
    FCfgConverterTool: String;
    FProjectDirectory: String;
    FGameDirectory: String;
    FMainFormHandle: THandle;

    FCancel: Boolean;
    FError: Boolean;

    procedure SendMessage(AType: TMessageType; Count: Integer = 0);

    procedure GetPBOListForExtract(const Dir: String; OutList: TStringList);
    procedure GetBinListForConvert(const Dir: String; OutList: TStringList);

    procedure ExtractPBO;
    procedure ConvertBin;
  public
    constructor Create(const ExtractTool, CfgConverterTool, ProjectDirectory, GameDirectory: String; MainFormHandle: THandle);

    procedure Execute; override;

    property Cancel: Boolean read FCancel write FCancel;
  end;


implementation

uses
  sgeStringList, sgeFileUtils,
  DayZUtils;


procedure TExtractWorker.SendMessage(AType: TMessageType; Count: Integer);
begin
  case AType of
    mtPBOError:
      PostMessage(FMainFormHandle, WM_EXTRACT_WORKER, WORKER_PBO_ERROR, 0);

    mtPBOCount:
      PostMessage(FMainFormHandle, WM_EXTRACT_WORKER, WORKER_PBO_COUNT, Count);

    mtPBOStep:
      PostMessage(FMainFormHandle, WM_EXTRACT_WORKER, WORKER_PBO_STEP, 0);

    mtBINError:
      PostMessage(FMainFormHandle, WM_EXTRACT_WORKER, WORKER_BIN_ERROR, 0);

    mtBINCount:
      PostMessage(FMainFormHandle, WM_EXTRACT_WORKER, WORKER_BIN_COUNT, Count);

    mtBINStep:
      PostMessage(FMainFormHandle, WM_EXTRACT_WORKER, WORKER_BIN_STEP, 0);

    mtFinish:
      PostMessage(FMainFormHandle, WM_EXTRACT_WORKER, WORKER_FINISH, 0);

    mtCancel:
      PostMessage(FMainFormHandle, WM_EXTRACT_WORKER, WORKER_CANCEL, 0);
  end;
end;


procedure TExtractWorker.GetPBOListForExtract(const Dir: String; OutList: TStringList);
var
  PBOList: TsgeStringList;
  i: Integer;
begin
  OutList.Clear;

  PBOList := TsgeStringList.Create;
  try
    //Получим весь список
    sgeFindFilesInFolderByExt(Dir, PBOList, 'pbo');

    for i := 0 to PBOList.Count - 1 do
    begin
      //Пропуск мусора
      if Pos('!Workshop', PBOList.Part[i]) <> 0 then
        Continue;

      //Добавить нужное
      OutList.Add(ExcludeTrailingBackslash(Dir) + PBOList.Part[i]);
    end;

  finally
    PBOList.Free;
  end;
end;


procedure TExtractWorker.GetBinListForConvert(const Dir: String; OutList: TStringList);
var
  BinList: TsgeStringList;
  i: Integer;
begin
  OutList.Clear;

  BinList := TsgeStringList.Create;
  try
    //Получим весь список
    sgeFindFilesInFolderByExt(Dir, BinList, 'bin');

    for i := 0 to BinList.Count - 1 do
      OutList.Add(ExcludeTrailingBackslash(Dir) + BinList.Part[i]);

  finally
    BinList.Free;
  end;

end;


procedure TExtractWorker.ExtractPBO;
var
  i: Integer;
  Params: String;
  PBOList: TStringList;
begin
  PBOList := TStringList.Create;
  try
    GetPBOListForExtract(FGameDirectory, PBOList);

    //Отослать сколько всего элементов
    SendMessage(mtPBOCount, PBOList.Count);

    //Прогнать все архивы
    for i := 0 to PBOList.Count - 1 do
    begin
      try
        //Параметры запуска
        Params := Format('-prefix -f "%s" "%s"', [FProjectDirectory, PBOList.Strings[i]]);

        //Выполнить распаковку
        //Bankrev.exe -f o:\Addons\ "e:\Steam\steamapps\common\Arma 3\Addons\A3.pbo -prefix"
        ExecuteFileAndWait(FExtractTool, Params);

        //Сказать что следующий файл
        SendMessage(mtPBOStep);

        //Проверить отмену
        if FCancel then
          Break;

      except
        FError := True;
        Break;
      end;
    end;

  finally
    PBOList.Free;
  end;
end;


procedure TExtractWorker.ConvertBin;
var
  i: Integer;
  Params: String;
  BinList: TStringList;
begin
  BinList := TStringList.Create;
  try
    GetBinListForConvert(FProjectDirectory, BinList);

    //Отослать сколько всего элементов
    SendMessage(mtBINCount, BinList.Count);

    //Прогнать все архивы
    for i := 0 to BinList.Count - 1 do
    begin
      try
        //Параметры запуска
        Params := Format('-txt -dst "%s" "%s"', [ChangeFileExt(BinList.Strings[i], '.cpp'), BinList.Strings[i]]);

        //Выполнить распаковку
        //CfgConvert.exe -txt -dst C:\Users\Администратор\Desktop\1\config.cpp .\config.bin
        ExecuteFileAndWait(FCfgConverterTool, Params);

        //Удалить исходник
        SysUtils.DeleteFile(BinList.Strings[i]);

        //Сказать что следующий файл
        SendMessage(mtBINStep);

        //Проверить отмену
        if FCancel then
          Break;

      except
        FError := True;
        Break;
      end;
    end;

  finally
    BinList.Free;
  end;
end;


constructor TExtractWorker.Create(const ExtractTool, CfgConverterTool, ProjectDirectory, GameDirectory: String; MainFormHandle: THandle);
begin

  FExtractTool := ExtractTool;
  FCfgConverterTool := CfgConverterTool;
  FProjectDirectory := ExcludeTrailingBackslash(ProjectDirectory);
  FGameDirectory := GameDirectory;
  FMainFormHandle := MainFormHandle;
  FCancel := False;
  FError := False;

  FreeOnTerminate := False;

  inherited Create(False);
end;


procedure TExtractWorker.Execute;
begin
  //Распаковать PBO
  ExtractPBO;

  if FError then
  begin
    SendMessage(mtPBOError);
    Exit;
  end;

  //Сообщить что работа прервана
  if FCancel then
  begin
    SendMessage(mtCancel);
    Exit;
  end;


  //Конвертировать Bin -> Cpp
  ConvertBin;

  if FError then
  begin
    SendMessage(mtBINError);
    Exit;
  end;

  //Сообщить что работа прервана
  if FCancel then
  begin
    SendMessage(mtCancel);
    Exit;
  end;

  //Сказать что все прошло успешно
  SendMessage(mtFinish);
end;



end.

