unit BuilderUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  TabParameters, TabCommonUnit, BuilderItemUnit;

type
  TBuilderFrame = class(TTabCommonFrame)
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    btnChangeIcon: TSpeedButton;
    btnDown: TSpeedButton;
    btnRename: TSpeedButton;
    btnExplore: TSpeedButton;
    btnUp: TSpeedButton;
    pnlTools: TPanel;
    sbContent: TScrollBox;
    procedure btnAddClick(Sender: TObject);
    procedure btnChangeIconClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure sbContentClick(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'TabBuilder.';

      SECTION_BUILDER = 'Builder';
  private
    FFrames: TBuilderItemFrameList;

    FSettingsFile: String;

    procedure AddItemFrame(AFrame: TBuilderItemFrame);
    procedure ClearItemFrames;
    procedure ArrangeItemFrames;
    procedure DeleteItemFrameByIndex(Index: Integer);
    procedure SwapItemFrames(Index1, Index2: Integer);
    function  GetSelectedItemFrame: TBuilderItemFrame;
    function  GetItemFrameIndex(Frame: TBuilderItemFrame): Integer;

    procedure CorrectToolButtons;

    procedure OnChangeBuilderContentHeight(Sender: TObject);
    procedure OnItemSelect(Sender: TObject);
  public
    constructor Create(Parameters: TTabParameters; AParent: TWinControl); reintroduce;
    destructor  Destroy; override;

    procedure SetCollapset(ACollapsed: Boolean);

    procedure ApplyLanguage; override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;

    property Frames: TBuilderItemFrameList read FFrames;
  end;


implementation

{$R *.lfm}

uses
  IniFiles,
  InputDialogUnit, YesNoQuestionDialogUnit, IconSelectorDialogUnit;


procedure TBuilderFrame.btnAddClick(Sender: TObject);
var
  Frame: TBuilderItemFrame;
  AName: String;
begin
  AName := '';
  if InputDialogExecute(
    FParams.Language,
    FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'NewItem', 'Введите имя'),
    AName) then
  begin
    //Создать фрейм
    Frame := TBuilderItemFrame.Create(FParams.IconDirectory);
    Frame.Title := AName;

    //Добавить в список
    AddItemFrame(Frame);

    //Упорядочить фреймы
    ArrangeItemFrames;

    //Сохранить настройки
    SaveSettings;
  end;
end;


procedure TBuilderFrame.btnChangeIconClick(Sender: TObject);
var
  Frame: TBuilderItemFrame;
  s: String;
begin
  //Выделенный фрейм
  Frame := GetSelectedItemFrame;

  s := Frame.IconName;
  if IconSelectorDialogExecute(FParams.Language, FParams.IconDirectory, s) then
  begin
    //Изменить иконку
    Frame.IconName := s;

    //Сохранить параметры
    SaveSettings;
  end;
end;


procedure TBuilderFrame.btnDeleteClick(Sender: TObject);
var
  Frame: TBuilderItemFrame;
  Index: Integer;
begin
  //Выделенный фрейм
  Frame := GetSelectedItemFrame;

  if YesNoQuestionDialogExecute(
    FParams.Language,
    Format(FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'DeleteCurrent', 'Удалить настройку "%s" из списка?'), [Frame.Title])
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


procedure TBuilderFrame.btnDownClick(Sender: TObject);
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


procedure TBuilderFrame.btnRenameClick(Sender: TObject);
var
  Frame: TBuilderItemFrame;
  AName: String;
begin
  Frame := GetSelectedItemFrame;

  AName := Frame.Title;
  if InputDialogExecute(
    FParams.Language,
    FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'RenameItem', 'Введите имя'),
    AName) then
  begin
    //Изменить имя
    Frame.Title := AName;

    //Сохранить настройки
    SaveSettings;
  end;
end;


procedure TBuilderFrame.btnUpClick(Sender: TObject);
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


procedure TBuilderFrame.FrameResize(Sender: TObject);
begin
  ArrangeItemFrames;
end;


procedure TBuilderFrame.sbContentClick(Sender: TObject);
begin
  OnItemSelect(nil);
end;


procedure TBuilderFrame.AddItemFrame(AFrame: TBuilderItemFrame);
var
  c: Integer;
begin
  //Установим обработчики
  AFrame.OnChangeBounds := @OnChangeBuilderContentHeight;
  AFrame.OnSelect := @OnItemSelect;

  //Добавим в массив
  c := Length(FFrames);
  SetLength(FFrames, c + 1);
  FFrames[c] := AFrame;
end;


procedure TBuilderFrame.ClearItemFrames;
var
  i, c: Integer;
begin
  c := Length(FFrames) - 1;
  for i := c downto 0 do
    FFrames[i].Free;

  SetLength(FFrames, 0);
end;


procedure TBuilderFrame.ArrangeItemFrames;
var
  i, Y: Integer;
  Frame: TBuilderItemFrame;
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


procedure TBuilderFrame.DeleteItemFrameByIndex(Index: Integer);
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


procedure TBuilderFrame.SwapItemFrames(Index1, Index2: Integer);
var
  Frame: TBuilderItemFrame;
begin
  Frame := FFrames[Index1];
  FFrames[Index1] := FFrames[Index2];
  FFrames[Index2] := Frame;
end;


function TBuilderFrame.GetSelectedItemFrame: TBuilderItemFrame;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Length(FFrames) - 1 do
  begin
    if FFrames[i].Selected then
      Exit(FFrames[i]);
  end;
end;


function TBuilderFrame.GetItemFrameIndex(Frame: TBuilderItemFrame): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 0 to Length(FFrames) - 1 do
  begin
    if FFrames[i] = Frame then
      Exit(i);
  end;
end;


procedure TBuilderFrame.CorrectToolButtons;
var
  Item: TBuilderItemFrame;
begin
  Item := GetSelectedItemFrame;

  btnDelete.Enabled := Item <> nil;
  btnChangeIcon.Enabled := Item <> nil;
  btnRename.Enabled := Item <> nil;
  btnUp.Enabled := (Item <> nil) and (GetItemFrameIndex(Item) > 0);
  btnDown.Enabled := (Item <> nil) and (GetItemFrameIndex(Item) < Length(FFrames) - 1);
end;


procedure TBuilderFrame.OnChangeBuilderContentHeight(Sender: TObject);
begin
  ArrangeItemFrames;
end;


procedure TBuilderFrame.OnItemSelect(Sender: TObject);
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


constructor TBuilderFrame.Create(Parameters: TTabParameters; AParent: TWinControl);
begin
  inherited Create(Parameters, AParent);

  FSettingsFile := FParams.SettingsDirectory + 'Build.ini';

  //Загрузить настройки
  LoadSettings;
end;


destructor TBuilderFrame.Destroy;
begin
  //Сохранить настройки
  SaveSettings;

  //Удалить элементы
  ClearItemFrames;

  inherited Destroy;
end;


procedure TBuilderFrame.SetCollapset(ACollapsed: Boolean);
var
  i: Integer;
begin
  for i := Length(FFrames) - 1 downto 0 do
    FFrames[i].Collapsed := ACollapsed;
end;


procedure TBuilderFrame.ApplyLanguage;
var
  i: Integer;
begin
  //Перевод
  btnAdd.Caption := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Add', 'Добавить');
  btnChangeIcon.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ChangeIcon', 'Изменить иконку');
  btnRename.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Rename', 'Переименовать');
  btnUp.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Up', 'Вверх');
  btnDown.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Down', 'Вниз');
  btnDelete.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Delete', 'Удалить');

  //Элементы
  for i := 0 to Length(FFrames) - 1 do
    FFrames[i].ChangeLanguage(FParams.Language);
end;


procedure TBuilderFrame.SaveSettings;
var
  F: TIniFile;
  i: Integer;
begin
  F := TIniFile.Create(FSettingsFile);
  try
    //Стереть секцию
    F.EraseSection(SECTION_BUILDER);

    //Фреймы
    for i := 0 to Length(FFrames) - 1 do
      F.WriteString(SECTION_BUILDER, IntToStr(i), FFrames[i].ValueToString);

  finally
    F.Free;
  end;
end;


procedure TBuilderFrame.LoadSettings;
var
  F: TIniFile;
  Values: TStringList;
  i: Integer;
  Line: String;
  Frame: TBuilderItemFrame;
begin
  //Удалить фреймы
  ClearItemFrames;

  F := TIniFile.Create(FSettingsFile);
  Values := TStringList.Create;
  try
    F.ReadSection(SECTION_BUILDER, Values);

    for i := 0 to Values.Count - 1 do
    begin
      Line := Trim(F.ReadString(SECTION_BUILDER, Values.Strings[i], ''));

      if Line = '' then
        Continue;

      //Создать элемент каталога
      Frame := TBuilderItemFrame.Create(FParams.IconDirectory, Line);

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


