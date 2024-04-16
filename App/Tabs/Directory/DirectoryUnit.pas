unit DirectoryUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, IniFiles,
  Language, SteamUtils,
  TabParameters, TabCommonUnit, DirectoryItemUnit;

type
  TDirectoryFrame = class(TTabCommonFrame)
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
    const
      SECTION_DIRECTORY = 'Directory';

      PREFIX_TAB_DIRECTORY = 'TabDirectory.';
  private
    FIconDirectory: String;
    FSettingsFile: String;
    FFrames: TDirectoryItemFrameArray;
    FLanguage: TLanguage;

    procedure AddDirectoryFrame(AFrame: TDirectoryItemFrame);
    procedure ClearDirectoryFrames;
    procedure ArrangeDirectoryFrames;
    procedure SwapDirectoryFrame(Index1, Index2: Integer);
    procedure DeleteDirectoryFrameByIndex(Index: Integer);

    function  GetSelectedDirectoryFrame: TDirectoryItemFrame;
    function  GetDirectoryFrameIndex(Frame: TDirectoryItemFrame): Integer;
    function  IsDirectoryFrameExist(APath: String): Boolean;
    procedure CorrectToolButtons;

    procedure SaveSettings(const FileName: String);
    procedure LoadSettings(const FileName: String);

    //Обработчик выделения элемента
    procedure OnItemSelect(Sender: TObject);
  public
    constructor Create(Parameters: TTabParameters); reintroduce;
    //constructor Create(const SettingsFile: String; const IconDirectory: String); reintroduce;
    destructor  Destroy; override;

    procedure DeleteIncorrectDirectory;
    procedure AddStandartDirectory;
    procedure DeleteAll;

    procedure ChangeLanguage(Language: TLanguage);

    property Frames: TDirectoryItemFrameArray read FFrames;
  end;


implementation

{$R *.lfm}

uses
  YesNoQuestionDialogUnit, DirectoryEditorDialogUnit;


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

  if DirectoryEditorDialogExecute(FLanguage, pemEdit, FIconDirectory, ACaption, APath, AIconFileName) then
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

  if YesNoQuestionDialogExecute(
    FLanguage,
    Format(FLanguage.GetLocalizedString(PREFIX_TAB_DIRECTORY + 'DeleteCurrent', 'Удалить каталог "%s" из списка?'), [Frame.Caption])
  ) then
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
  ACaption := FLanguage.GetLocalizedString(PREFIX_TAB_DIRECTORY + 'NewItemName', 'Новый каталог');
  APAth := '';
  AIconFileName := '';

  if DirectoryEditorDialogExecute(FLanguage, pemNew, FIconDirectory, ACaption, APath, AIconFileName) then
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
  i, Y: Integer;
  Frame: TDirectoryItemFrame;
begin
  Y := 0;

  for i := 0 to Length(FFrames) - 1 do
  begin
    Frame := FFrames[i];

    Frame.Anchors := [akLeft, akTop, akRight];
    Frame.Parent := sbContent;
    Frame.Left := 0;
    Frame.Top := Y;
    Frame.Width := sbContent.ClientWidth;

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


function TDirectoryFrame.IsDirectoryFrameExist(APath: String): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to Length(FFrames) - 1 do
  begin
    if LowerCase(ExcludeTrailingBackslash(FFrames[i].Path)) = LowerCase(ExcludeTrailingBackslash(APath)) then
      Exit(True);
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


constructor TDirectoryFrame.Create(Parameters: TTabParameters);
begin
  inherited Create(Parameters);

  //Подготовить каталог данных
  FIconDirectory := FParams.DataDirectory + 'Directory\';
  ForceDirectories(FIconDirectory);

  //Определить файл настроек
  FSettingsFile := FParams.SettingsDirectory + '\Directory.ini';

  //Загрузить настройки
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
  if YesNoQuestionDialogExecute(
    FLanguage,
    FLanguage.GetLocalizedString(PREFIX_TAB_DIRECTORY + 'DeleteIncorrect', 'Удалить несуществующие каталоги? Действие необратимо.')
  ) then
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
end;


procedure TDirectoryFrame.AddStandartDirectory;

  function GetPathFromString(const Path: String): String;
  var
    List: TStringList;
  begin
    Result := '';
    List := TStringList.Create;
    List.LineBreak := ';;;';
    List.Text := Path;
    if List.Count > 1 then
      Result := List[1];
    List.Free;
  end;

var
  List: TStringList;
  i: Integer;
  Frame: TDirectoryItemFrame;
begin
  List := TStringList.Create;

  //Получить список каталогов
  GetSteamDayZDirectoryList(List);

  for i := 0 to List.Count - 1 do
  begin
    //Пропуск существующих
    if IsDirectoryFrameExist(GetPathFromString(List.Strings[i])) then
      Continue;

    //Создать элемент каталога
    Frame := TDirectoryItemFrame.Create(FIconDirectory, List.Strings[i]);
    Frame.OnSelect := @OnItemSelect;

    //Проверить перевод
    Frame.Caption := FLanguage.GetLocalizedString(PREFIX_TAB_DIRECTORY + 'DirectoryName.' + Frame.Caption, Frame.Caption);

    //Добавить в массив
    AddDirectoryFrame(Frame);
  end;

  //Сохранить настройки
  SaveSettings(FSettingsFile);

  //Упорядочить фреймы
  ArrangeDirectoryFrames;

  List.Free;
end;


procedure TDirectoryFrame.DeleteAll;
begin
  if YesNoQuestionDialogExecute(
    FLanguage,
    FLanguage.GetLocalizedString(PREFIX_TAB_DIRECTORY + 'DeleteAll', 'Удалить все каталоги? Действие необратимо.')
  ) then
  begin
    //Удалить все каталоги
    ClearDirectoryFrames;

    //Сохранить настройки
    SaveSettings(FSettingsFile);

    //Поправить панель кнопок
    CorrectToolButtons;
  end;
end;


procedure TDirectoryFrame.ChangeLanguage(Language: TLanguage);
begin
  FLanguage := Language;

  //Пеервод
  btnAdd.Caption := FLanguage.GetLocalizedString(PREFIX_TAB_DIRECTORY + 'Add', 'Добавить');
  btnEdit.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_DIRECTORY + 'Edit', 'Изменить');
  btnUp.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_DIRECTORY + 'Up', 'Вверх');
  btnDown.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_DIRECTORY + 'Down', 'Вниз');
  btnDelete.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_DIRECTORY + 'Delete', 'Удалить');
  btnExplore.Hint := FLanguage.GetLocalizedString(PREFIX_TAB_DIRECTORY + 'OpenDirectory', 'Открыть в проводнике');
end;



end.

