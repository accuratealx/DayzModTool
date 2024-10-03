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

      LANGUAGE_PREFIX = 'TabDirectory.';
  private
    FSettingsFile: String;
    FFrames: TDirectoryItemFrameArray;

    procedure AddItemFrame(AFrame: TDirectoryItemFrame);
    procedure ClearItemFrames;
    procedure ArrangeItemFrames;
    procedure SwapItemFrames(Index1, Index2: Integer);
    procedure DeleteItemFrameByIndex(Index: Integer);
    function  GetSelectedItemFrame: TDirectoryItemFrame;
    function  GetItemFrameIndex(Frame: TDirectoryItemFrame): Integer;
    function  IsItemFrameExist(APath: String): Boolean;

    procedure CorrectToolButtons;

    //Обработчик выделения элемента
    procedure OnItemSelect(Sender: TObject);
  public
    constructor Create(Parameters: TTabParameters; AParent: TWinControl); reintroduce;
    destructor  Destroy; override;

    procedure DeleteIncorrectDirectory;
    procedure AddStandartDirectory;
    procedure DeleteAll;

    procedure ApplyLanguage; override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;

    property Frames: TDirectoryItemFrameArray read FFrames;
  end;


implementation

{$R *.lfm}

uses
  YesNoQuestionDialogUnit, DirectoryItemEditorDialogUnit;


procedure TDirectoryFrame.FrameClick(Sender: TObject);
begin
  OnItemSelect(nil);
end;


procedure TDirectoryFrame.btnUpClick(Sender: TObject);
var
  Index: Integer;
begin
  //индекс выделенного фрейма
  Index := GetItemFrameIndex(GetSelectedItemFrame);

  //Защита от падения
  if Index = 0 then
    Exit;

  //Поменять местами
  SwapItemFrames(Index, Index - 1);

  //Перестроить элементы
  ArrangeItemFrames;

  //Поправить панель инструментов
  CorrectToolButtons;

  //Сохранить настройки
  SaveSettings;
end;


procedure TDirectoryFrame.btnDownClick(Sender: TObject);
var
  Index: Integer;
begin
  //индекс выделенного фрейма
  Index := GetItemFrameIndex(GetSelectedItemFrame);

  //Защита от падения
  if Index = Length(FFrames) - 1 then
    Exit;

  //Поменять местами
  SwapItemFrames(Index, Index + 1);

  //Перестроить элементы
  ArrangeItemFrames;

  //Поправить панель инструментов
  CorrectToolButtons;

  //Сохранить настройки
  SaveSettings;
end;


procedure TDirectoryFrame.btnEditClick(Sender: TObject);
var
  ACaption, APath, AIconFileName: String;
  Item: TDirectoryItemFrame;
begin
  //Ссылка на элемент
  Item := GetSelectedItemFrame;

  if Item = nil then
    Exit;

  //Подготовить параметры
  ACaption := Item.Caption;
  APAth := Item.Path;
  AIconFileName := Item.IconName;

  if DirectoryItemEditorDialogExecute(FParams.Language, diemEdit, FParams.IconDirectory, ACaption, APath, AIconFileName) then
  begin
    //Поправить
    Item.Caption := ACaption;
    Item.Path := APath;
    Item.IconName := AIconFileName;

    //Поправить панель инструментов
    CorrectToolButtons;

    //Сохранить настройки
    SaveSettings;
  end;
end;


procedure TDirectoryFrame.btnExploreClick(Sender: TObject);
var
  Item: TDirectoryItemFrame;
begin
  //Ссылка на элемент
  Item := GetSelectedItemFrame;

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
  Frame := GetSelectedItemFrame;

  if YesNoQuestionDialogExecute(
    FParams.Language,
    Format(FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'DeleteCurrent', 'Удалить каталог "%s" из списка?'), [Frame.Caption])
  ) then
  begin
    //индекс выделенного фрейма
    Index := GetItemFrameIndex(Frame);

    //Удалить фрейм
    DeleteItemFrameByIndex(Index);

    //Перестроить элементы
    ArrangeItemFrames;

    //Поправить панель инструментов
    CorrectToolButtons;

    //Сохранить настройки
    SaveSettings;
  end;
end;


procedure TDirectoryFrame.btnAddClick(Sender: TObject);
var
  ACaption, APath, AIconFileName: String;
  Frame: TDirectoryItemFrame;
begin
  //Подготовить параметры
  ACaption := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'NewItemName', 'Новый каталог');
  APAth := '';
  AIconFileName := '';

  if DirectoryItemEditorDialogExecute(FParams.Language, diemNew, FParams.IconDirectory, ACaption, APath, AIconFileName) then
  begin
    //Создать элемент каталога
    Frame := TDirectoryItemFrame.Create(FParams.IconDirectory, ACaption, APath, AIconFileName);
    Frame.OnSelect := @OnItemSelect;

    //Добавить в массив
    AddItemFrame(Frame);

    //Упорядочить элементы
    ArrangeItemFrames;

    //Поправить панель инструментов
    CorrectToolButtons;

    //Сохранить настройки
    SaveSettings;
  end;
end;


procedure TDirectoryFrame.AddItemFrame(AFrame: TDirectoryItemFrame);
var
  c: Integer;
begin
  c := Length(FFrames);
  SetLength(FFrames, c + 1);
  FFrames[c] := AFrame;
end;


procedure TDirectoryFrame.ClearItemFrames;
var
  i, c: Integer;
begin
  c := Length(FFrames) - 1;
  for i := c downto 0 do
    FFrames[i].Free;

  SetLength(FFrames, 0);
end;


procedure TDirectoryFrame.ArrangeItemFrames;
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
    Frame.Highlight := not Odd(i);

    Inc(Y, Frame.Height);
  end;
end;


procedure TDirectoryFrame.SwapItemFrames(Index1, Index2: Integer);
var
  Frame: TDirectoryItemFrame;
begin
  Frame := FFrames[Index1];
  FFrames[Index1] := FFrames[Index2];
  FFrames[Index2] := Frame;
end;


procedure TDirectoryFrame.DeleteItemFrameByIndex(Index: Integer);
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


function TDirectoryFrame.GetSelectedItemFrame: TDirectoryItemFrame;
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


function TDirectoryFrame.GetItemFrameIndex(Frame: TDirectoryItemFrame): Integer;
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


function TDirectoryFrame.IsItemFrameExist(APath: String): Boolean;
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
  Item := GetSelectedItemFrame;

  btnDelete.Enabled := Item <> nil;
  btnEdit.Enabled := Item <> nil;
  btnUp.Enabled := (Item <> nil) and (GetItemFrameIndex(Item) > 0);
  btnDown.Enabled := (Item <> nil) and (GetItemFrameIndex(Item) < Length(FFrames) - 1);
  btnExplore.Enabled := (Item <> nil) and (Item.IsPathCorrect);
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


constructor TDirectoryFrame.Create(Parameters: TTabParameters; AParent: TWinControl);
begin
  inherited Create(Parameters, AParent);

  //Определить файл настроек
  FSettingsFile := FParams.SettingsDirectory + 'Directory.ini';

  //Загрузить настройки
  LoadSettings;
end;


destructor TDirectoryFrame.Destroy;
begin
  SaveSettings;

  ClearItemFrames;

  inherited Destroy;
end;


procedure TDirectoryFrame.DeleteIncorrectDirectory;
var
  i: Integer;
begin
  if YesNoQuestionDialogExecute(
    FParams.Language,
    FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'DeleteIncorrect', 'Удалить несуществующие каталоги? Действие необратимо.')
  ) then
  begin
    for i := Length(FFrames) - 1 downto 0 do
    begin
      if not FFrames[i].IsPathCorrect then
        DeleteItemFrameByIndex(i);
    end;

    //Упорядочить фреймы
    ArrangeItemFrames;

    //Поправить панель кнопок
    CorrectToolButtons;

    //Сохранить настройки
    SaveSettings;
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
    if IsItemFrameExist(GetPathFromString(List.Strings[i])) then
      Continue;

    //Создать элемент каталога
    Frame := TDirectoryItemFrame.Create(FParams.IconDirectory, List.Strings[i]);
    Frame.OnSelect := @OnItemSelect;

    //Проверить перевод
    Frame.Caption := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'DirectoryName.' + Frame.Caption, Frame.Caption);

    //Добавить в массив
    AddItemFrame(Frame);
  end;

  //Сохранить настройки
  SaveSettings;

  //Упорядочить фреймы
  ArrangeItemFrames;

  List.Free;
end;


procedure TDirectoryFrame.DeleteAll;
begin
  if YesNoQuestionDialogExecute(
    FParams.Language,
    FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'DeleteAll', 'Удалить все каталоги? Действие необратимо.')
  ) then
  begin
    //Удалить все каталоги
    ClearItemFrames;

    //Сохранить настройки
    SaveSettings;

    //Поправить панель кнопок
    CorrectToolButtons;
  end;
end;


procedure TDirectoryFrame.ApplyLanguage;
begin
  //Пеервод
  btnAdd.Caption := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Add', 'Добавить');
  btnEdit.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Edit', 'Изменить');
  btnUp.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Up', 'Вверх');
  btnDown.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Down', 'Вниз');
  btnDelete.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Delete', 'Удалить');
  btnExplore.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'OpenDirectory', 'Открыть в проводнике');
end;


procedure TDirectoryFrame.SaveSettings;
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


procedure TDirectoryFrame.LoadSettings;
var
  F: TIniFile;
  Values: TStringList;
  i: Integer;
  Line: String;
  Frame: TDirectoryItemFrame;
begin
  //Удалить фреймы
  ClearItemFrames;


  F := TIniFile.Create(FSettingsFile);
  Values := TStringList.Create;
  try
    F.ReadSection(SECTION_DIRECTORY, Values);

    for i := 0 to Values.Count - 1 do
    begin
      Line := Trim(F.ReadString(SECTION_DIRECTORY, Values.Strings[i], ''));

      if Line = '' then
        Continue;

      //Создать элемент каталога
      Frame := TDirectoryItemFrame.Create(FParams.IconDirectory, Line);
      Frame.OnSelect := @OnItemSelect;

      //Добавить в массив
      AddItemFrame(Frame);
    end;

    //Упорядочить фреймы
    ArrangeItemFrames;

  finally
    Values.Free;
    F.Free;
  end;
end;



end.

