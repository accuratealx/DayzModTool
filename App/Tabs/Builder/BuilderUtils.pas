unit BuilderUtils;

{$mode ObjFPC}{$H+}

interface

uses
  ExtCtrls, SysUtils, Classes;

type
  //Параметры для сборки pbo
  TBuilderItemBuildData = record
    Icon: TImage;
    Title: String;

    Extensions: String;
    SourceDirectory: String;
    DestinationDirectory: String;
    ProjectDrive: String;
    Prefix: String;
    Sign: String;
    Version: String;
  end;

  //Список параметров для сборки pbo
  TBuilderItemBuildDataList = array of TBuilderItemBuildData;


function CreateIncludeFile(Path: String; Line: String): Boolean;
function GetBuildParamString(Src, Dst, Prefix, SignFile, ExtensionFile, ProjectDrive, Version: String): String;

implementation


function CreateIncludeFile(Path: String; Line: String): Boolean;
var
  F: TFileStream;
begin
  Result := False;

  F := nil;
  try
    F := TFileStream.Create(Path, fmCreate);

    //Если есть что записывать
    if Length(Line) > 0 then
      F.Write(Line[1], Length(Line));

    F.Free;

    Result := True;
  except;
  end;
end;


function GetBuildParamString(Src, Dst, Prefix, SignFile, ExtensionFile, ProjectDrive, Version: String): String;
var
  List: TStringList;
begin
  List := TStringList.Create;
  List.LineBreak := ' ';
  try
    //Исходный каталог
    List.Add(Format('%s', [Trim(Src)]));

    //Каталог назначения
    List.Add(Format('%s', [Trim(Dst)]));

    //Очищать временные файлы
    List.Add('-clear');

    //Путь к каталогу проекта
    List.Add(Format('"-project=%s:"', [ProjectDrive]));

    //Полный лог упаковки
    List.Add('-binarizeFullLogs');

    //Если есть префикс pbo
    if Prefix <> '' then
      List.Add(Format('-prefix=%s', [Trim(Prefix)]));

    //Если есть файл подписи
    if SignFile <> '' then
      List.Add(Format('"-sign=%s"', [SignFile]));

    //Список расширений
    List.Add(Format('"-include=%s"', [ExtensionFile]));

    //Версия
    if Version <> '' then
      List.Add(Format('"-pboversion=%s"', [Version]));

    //Результат
    Result := Trim(List.Text);
  finally
    List.Free;
  end;
end;



end.

