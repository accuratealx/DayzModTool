unit DirectoryUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, IniFiles,
  DirectoryItemUnit;

type
  TDirectoryFrame = class(TFrame)
    btnExplore: TSpeedButton;
    btnEdit: TSpeedButton;
    btnDelete: TSpeedButton;
    btnUp: TSpeedButton;
    btnDown: TSpeedButton;
    pnlTools: TPanel;
    btnAdd: TSpeedButton;
    sbContent: TScrollBox;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnExploreClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure FrameClick(Sender: TObject);
  private
    FIconDirectory: String;
    FSettingsFile: String;
    FFrames: TDirectoryItemFrameArray;

    procedure AddDirectoryFrame(AFrame: TDirectoryItemFrame);
    procedure ClearDirectoryFrames;
    procedure ArrangeDirectoryFrames;
    procedure SwapDirectoryFrame(Index1, Index2: Integer);
    procedure DeleteDirectoryFrameByIndex(Index: Integer);

    function  GetSelectedDirectoryFrame: TDirectoryItemFrame;
    function  GetDirectoryFrameIndex(Frame: TDirectoryItemFrame): Integer;
    procedure CorrectToolButtons;

    procedure SaveSettings(const FileName: String);
    procedure LoadSettings(const FileName: String);

    //Обработчик выделения элемента
    procedure OnItemSelect(Sender: TObject);
  public
    constructor Create(const SettingsFile: String; const IconDirectory: String); reintroduce;
    destructor  Destroy; override;

    procedure DeleteIncorrectDirectory;
    //procedure AddStandartDirectory;

    property Frames: TDirectoryItemFrameArray read FFrames;
  end;


implementation

{$R *.lfm}

uses
  YesNoQuestionDialogUnit, DirectoryEditorDialogUnit;

const
  SECTION_DIRECTORY = 'Directory';


procedure TDirectoryFrame.FrameClick(Sender: TObject);
begin
  OnItemSelect(nil);
end;


procedure TDirectoryFrame.btnUpClick(Sender: TObject);
var
  Index: Integer;
begin
  //индекс выделенного фрейма
  Index := GetDirectoryFrameIndex(GetSelectedDirectoryFrame);

  //Защита от падения
  if Index = 0 then
    Exit;

  //Поменять местами
  SwapDirectoryFrame(Index, Index - 1);

  //Перестроить элементы
  ArrangeDirectoryFrames;

  //Поправить панель инструментов
  CorrectToolButtons;

  //Сохранить настройки
  SaveSettings(FSettingsFile);
end;


procedure TDirectoryFrame.btnDownClick(Sender: TObject);
var
  Index: Integer;
begin
  //индекс выделенного фрейма
  Index := GetDirectoryFrameIndex(GetSelectedDirectoryFrame);

  //Защита от падения
  if Index = Length(FFrames) - 1 then
    Exit;

  //Поменять местами
  SwapDirectoryFrame(Index, Index + 1);

  //Перестроить элементы
  ArrangeDirectoryFrames;

  //Поправить панель инструментов
  CorrectToolButtons;

  //Сохранить настройки
  SaveSettings(FSettingsFile);
end;


procedure TDirectoryFrame.btnEditClick(Sender: TObject);
var
  ACaption, APath, AIconFileName: String;
  Item: TDirectoryItemFrame;
begin
  //Ссылка на элемент
  Item := GetSelectedDirectoryFrame;

  if Item = nil then
    Exit;

  //Подготовить параметры
  ACaption := Item.Caption;
  APAth := Item.Path;
  AIconFileName := Item.IconName;

  if DirectoryEditorDialogExecute(pemEdit, FIconDirectory, ACaption, APath, AIconFileName) then
  begin
    //Поправить
    Item.Caption := ACaption;
    Item.Path := APath;
    Item.IconName := AIconFileName;

    //Поправить панель инструментов
    CorrectToolButtons;

    //Сохранить настройки
    SaveSettings(FSettingsFile);
  end;
end;


procedure TDirectoryFrame.btnExploreClick(Sender: TObject);
var
  Item: TDirectoryItemFrame;
begin
  //Ссылка на элемент
  Item := GetSelectedDirectoryFrame;

  //Защита от дурака
  if Item = nil then
    Exit;

  //Открыть каталог
  Item.OpenDirectory;
end;


procedure TDirectoryFrame.btnDeleteClick(Sender: TObject);
var
  Frame: TDirectoryItemFrame;
  Index: Integer;
begin
  //Выделенный фрейм
  Frame := GetSelectedDirectoryFrame;

  if YesNoQuestionDialogExecute('Вопрос', Format('Удалить из списка каталог %s"%s"?', [sLineBreak, Frame.Caption])) then
  begin
    //индекс выделенного фрейма
    Index := GetDirectoryFrameIndex(Frame);

    //Удалить фрейм
    DeleteDirectoryFrameByIndex(Index);

    //Перестроить элементы
    ArrangeDirectoryFrames;

    //Поправить панель инструментов
    CorrectToolButtons;

    //Сохранить настройки
    SaveSettings(FSettingsFile);
  end;
end;


procedure TDirectoryFrame.btnAddClick(Sender: TObject);
var
  ACaption, APath, AIconFileName: String;
  Frame: TDirectoryItemFrame;
begin
  //Подготовить параметры
  ACaption := 'Новый каталог';
  APAth := '';
  AIconFileName := '';

  if DirectoryEditorDialogExecute(pemNew, FIconDirectory, ACaption, APath, AIconFileName) then
  begin
    //Создать элемент каталога
    Frame := TDirectoryItemFrame.Create(FIconDirectory, ACaption, APath, AIconFileName);
    Frame.OnSelect := @OnItemSelect;

    //Добавить в массив
    AddDirectoryFrame(Frame);

    //Упорядочить элементы
    ArrangeDirectoryFrames;

    //Поправить панель инструментов
    CorrectToolButtons;

    //Сохранить настройки
    SaveSettings(FSettingsFile);
  end;
end;


procedure TDirectoryFrame.AddDirectoryFrame(AFrame: TDirectoryItemFrame);
var
  c: Integer;
begin
  c := Length(FFrames);
  SetLength(FFrames, c + 1);
  FFrames[c] := AFrame;
end;


procedure TDirectoryFrame.ClearDirectoryFrames;
var
  i, c: Integer;
begin
  c := Length(FFrames) - 1;
  for i := c downto 0 do
    FFrames[i].Free;

  SetLength(FFrames, 0);
end;


procedure TDirectoryFrame.ArrangeDirectoryFrames;
var
  i, c, Y: Integer;
  Frame: TDirectoryItemFrame;
begin
  c := Length(FFrames) - 1;

  //Y := pnlTools.Height;
  Y := 0;

  for i := 0 to c do
  begin
    Frame := FFrames[i];

    Frame.Anchors := [akLeft, akTop, akRight];
    Frame.Parent := sbContent;
    Frame.Left := 0;
    Frame.Top := Y;
    Frame.Width := ClientWidth;

    Inc(Y, Frame.Height);
  end;
end;


procedure TDirectoryFrame.SwapDirectoryFrame(Index1, Index2: Integer);
var
  Frame: TDirectoryItemFrame;
begin
  Frame := FFrames[Index1];
  FFrames[Index1] := FFrames[Index2];
  FFrames[Index2] := Frame;
end;


procedure TDirectoryFrame.DeleteDirectoryFrameByIndex(Index: Integer);
var
  c, i: Integer;
begin
  c := Length(FFrames) - 1;
  if (Index < 0) or (Index > c) then
    Exit;

  //Удалить память объекта
  FFrames[Index].Free;

  //Сдвинуть хвост
  for i := Index to c - 1 do
    FFrames[i] := FFrames[i + 1];

  //Удалить последний элемент
  SetLength(FFrames, c);
end;


function TDirectoryFrame.GetSelectedDirectoryFrame: TDirectoryItemFrame;
var
  i, c: Integer;
begin
  Result := nil;

  c := Length(FFrames) - 1;
  for i := 0 to c do
  begin
    if FFrames[i].Selected then
      Exit(FFrames[i]);
  end;
end;


function TDirectoryFrame.GetDirectoryFrameIndex(Frame: TDirectoryItemFrame): Integer;
var
  i, c: Integer;
begin
  Result := -1;

  c := Length(FFrames) - 1;
  for i := 0 to c do
  begin
    if FFrames[i] = Frame then
      Exit(i);
  end;
end;


procedure TDirectoryFrame.CorrectToolButtons;
var
  Item: TDirectoryItemFrame;
begin
  Item := GetSelectedDirectoryFrame;

  btnDelete.Enabled := Item <> nil;
  btnEdit.Enabled := Item <> nil;
  btnUp.Enabled := (Item <> nil) and (GetDirectoryFrameIndex(Item) > 0);
  btnDown.Enabled := (Item <> nil) and (GetDirectoryFrameIndex(Item) < Length(FFrames) - 1);
  btnExplore.Enabled := (Item <> nil) and (Item.IsPathCorrect);
end;


procedure TDirectoryFrame.SaveSettings(const FileName: String);
var
  F: TIniFile;
  i: Integer;
begin
  F := TIniFile.Create(FSettingsFile);
  try
    //Стереть секцию
    F.EraseSection(SECTION_DIRECTORY);

    for i := 0 to Length(FFrames) - 1 do
      F.WriteString(SECTION_DIRECTORY, IntToStr(i), FFrames[i].ValueToString);

  finally
    F.Free;
  end;
end;


procedure TDirectoryFrame.LoadSettings(const FileName: String);
var
  F: TIniFile;
  Values: TStringList;
  i: Integer;
  Line: String;
  Frame: TDirectoryItemFrame;
begin
  //Удалить фреймы
  ClearDirectoryFrames;


  F := TIniFile.Create(FileName);
  Values := TStringList.Create;
  try
    F.ReadSection(SECTION_DIRECTORY, Values);

    for i := 0 to Values.Count - 1 do
    begin
      Line := Trim(F.ReadString(SECTION_DIRECTORY, Values.Strings[i], ''));

      if Line = '' then
        Continue;

      //Создать элемент каталога
      Frame := TDirectoryItemFrame.Create(FIconDirectory, Line);
      Frame.OnSelect := @OnItemSelect;

      //Добавить в массив
      AddDirectoryFrame(Frame);
    end;

    //Упорядочить фреймы
    ArrangeDirectoryFrames;

  finally
    Values.Free;
    F.Free;
  end;
end;


procedure TDirectoryFrame.OnItemSelect(Sender: TObject);
var
  i, c: Integer;
begin
  //Сбросить выделение у остальных элементов
  c := Length(FFrames) - 1;
  for i := 0 to c do
  begin
    if FFrames[i] = Sender then
      Continue;

    FFrames[i].Selected := False;
  end;

  //Поправить панель кнопок
  CorrectToolButtons;
end;


constructor TDirectoryFrame.Create(const SettingsFile: String; const IconDirectory: String);
begin
  inherited Create(nil);

  FSettingsFile := SettingsFile;
  FIconDirectory := IncludeTrailingBackslash(IconDirectory);
  ForceDirectories(FIconDirectory);

  LoadSettings(FSettingsFile);
end;


destructor TDirectoryFrame.Destroy;
begin
  SaveSettings(FSettingsFile);

  ClearDirectoryFrames;

  inherited Destroy;
end;


procedure TDirectoryFrame.DeleteIncorrectDirectory;
var
  i: Integer;
begin
  for i := Length(FFrames) - 1 downto 0 do
  begin
    if not FFrames[i].IsPathCorrect then
      DeleteDirectoryFrameByIndex(i);
  end;

  //Упорядочить фреймы
  ArrangeDirectoryFrames;

  //Поправить панель кнопок
  CorrectToolButtons;

  //Сохранить настройки
  SaveSettings(FSettingsFile);
end;



end.

