unit BuilderUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, ComCtrls, StdCtrls, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, TabParameters, TabCommonUnit, BuilderItemUnit;

type
  TBuilderFrame = class(TTabCommonFrame)
    btnAdd: TSpeedButton;
    btnBuildAll: TSpeedButton;
    btnDelete: TSpeedButton;
    btnChangeIcon: TSpeedButton;
    btnClearFilter: TSpeedButton;
    btnDown: TSpeedButton;
    btnRename: TSpeedButton;
    btnUp: TSpeedButton;
    edFilter: TEdit;
    pnlTools: TPanel;
    sbContent: TScrollBox;
    StatusBar: TStatusBar;
    procedure btnAddClick(Sender: TObject);
    procedure btnBuildAllClick(Sender: TObject);
    procedure btnChangeIconClick(Sender: TObject);
    procedure btnClearFilterClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure edFilterChange(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure sbContentClick(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'TabBuilder.';

      SECTION_BUILDER = 'Builder';
      SECTION_SETTINGS = 'Settings';
      PARAM_FILTER = 'Filter';
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
    procedure CorrectBuildAllButton;
    procedure UpdateStatusBar;

    procedure OnChangeBuilderContentHeight(Sender: TObject);
    procedure OnItemSelect(Sender: TObject);

    function GetDefaultFileExtensions: String;
    function GetFilter: String;

    procedure EventHandler;
  public
    constructor Create(Parameters: TTabParameters; AParent: TWinControl); reintroduce;
    destructor  Destroy; override;

    procedure SetCollapset(ACollapsed: Boolean);

    procedure ApplyLanguage; override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;

    procedure Sort;
    procedure Clear;

    property Frames: TBuilderItemFrameList read FFrames;
  end;


implementation

{$R *.lfm}

uses
  IniFiles,
  InputDialogUnit, YesNoQuestionDialogUnit, IconSelectorDialogUnit,
  EventSystem, BuilderUtils, BuildAllDialogUnit;


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

    Frame.edFileExtensions.Text := GetDefaultFileExtensions;

    //Добавить в список
    AddItemFrame(Frame);

    //Упорядочить фреймы
    ArrangeItemFrames;

    //Поправить кнопку собрать все
    CorrectBuildAllButton;

    //Сохранить настройки
    SaveSettings;
  end;
end;


procedure TBuilderFrame.btnBuildAllClick(Sender: TObject);
var
  BuildData: TBuilderItemBuildDataList;
  i, c, cnt: Integer;
begin
  c := Length(FFrames);
  if c = 0 then
    Exit;

  //Собрать всё настройки в список
  for i := 0 to c - 1 do
  begin
    if FFrames[i].EnableBuild then
    begin
      cnt := Length(BuildData);
      SetLength(BuildData, cnt + 1);
      BuildData[cnt] := FFrames[i].GetBuildData;
    end;
  end;

  //Показать диалог сборки
  BuildAllDialogExecute(FParams.Language, BuildData);
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


procedure TBuilderFrame.btnClearFilterClick(Sender: TObject);
begin
  edFilter.Text := '';
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


procedure TBuilderFrame.edFilterChange(Sender: TObject);
begin
  edFilter.OnChange := nil;
  sbContent.BeginUpdateBounds;

  try
    ArrangeItemFrames;
    UpdateStatusBar;

  finally
    sbContent.EndUpdateBounds;
    edFilter.OnChange := @edFilterChange;
  end;
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
  AFrame.OnHeightChange := @OnChangeBuilderContentHeight;
  AFrame.OnSelect := @OnItemSelect;
  AFrame.ChangeLanguage(FParams.Language);

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

  function IsFrameVisible(Frame: TBuilderItemFrame): Boolean;
  var
    Fltr, Cpt: String;
  begin
    Fltr := GetFilter;
    if Fltr = '' then
      Exit(True);
    Cpt := UnicodeLowerCase(Frame.Title);
    Result := Pos(Fltr, Cpt) > 0;
  end;

var
  i, Y: Integer;
  Highlight: Boolean;
  Frame: TBuilderItemFrame;
begin
  Y := 0;
  Highlight := False;

  for i := 0 to Length(FFrames) - 1 do
  begin
    Frame := FFrames[i];

    if IsFrameVisible(Frame) then
    begin
      Frame.Anchors := [akLeft, akTop, akRight];
      Frame.Parent := sbContent;
      Frame.Left := 0;
      Frame.Top := Y;
      Frame.Width := sbContent.ClientWidth;
      Frame.Highlight := Highlight;
      Frame.Visible := True;

      Inc(Y, Frame.Height);
      Highlight := not Highlight;
    end
    else
    begin
      Frame.Visible := False;
      Frame.Parent := nil;
    end;
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

  //Поправить кнопку собрать все
  CorrectBuildAllButton;

  //Поправить строку статуса
  UpdateStatusBar;
end;


procedure TBuilderFrame.CorrectBuildAllButton;
begin
  btnBuildAll.Enabled := Length(FFrames) > 0;
end;


procedure TBuilderFrame.UpdateStatusBar;
var
  i, vis: Integer;
  Frame: TBuilderItemFrame;
begin
  //Определим сколько видимых фреймов
  vis := 0;
  for i := 0 to Length(FFrames) - 1 do
  begin
    Frame := FFrames[i];
    if Frame.Visible then
      Inc(vis);
  end;

  StatusBar.Panels[1].Text := IntToStr(Length(FFrames));
  StatusBar.Panels[3].Text := IntToStr(vis);
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


function TBuilderFrame.GetDefaultFileExtensions: String;
begin
  Result := '*.emat,*.edds,*.ptc,*.c,*.imageset,*.layout,*.ogg,*.xml,*.paa,*.csv,*.rvmat,*.p3d';
end;


function TBuilderFrame.GetFilter: String;
begin
  Result := UnicodeLowerCase(Trim(edFilter.Text));
end;


procedure TBuilderFrame.EventHandler;
var
  i: Integer;
  Frm: TBuilderItemFrame;
begin
  for i := 0 to Length(FFrames) - 1 do
  begin
    Frm := FFrames[i];
    Frm.CorrectButtonVisible;
  end;
end;


constructor TBuilderFrame.Create(Parameters: TTabParameters; AParent: TWinControl);
begin
  inherited Create(Parameters, AParent);

  FSettingsFile := FParams.SettingsDirectory + 'Build.ini';

  //Загрузить настройки
  LoadSettings;

  //Подписаться на событие
  FParams.EventSystem.Subscribe(esMountUnmountWorkDrive, @EventHandler);

  //Двойная буферизация
  sbContent.DoubleBuffered := True;
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
  btnAdd.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Add', 'Добавить');
  btnChangeIcon.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ChangeIcon', 'Изменить иконку');
  btnRename.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Rename', 'Переименовать');
  btnUp.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Up', 'Вверх');
  btnDown.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Down', 'Вниз');
  btnDelete.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Delete', 'Удалить');
  btnClearFilter.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ClearFilter', 'Очистить фильтр');
  btnBuildAll.Caption := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'BuildAll', 'Собрать все');

  //Строка статуса
  StatusBar.Panels[0].Text := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'TotalItems', 'Всего элементов') + ':';
  StatusBar.Panels[2].Text := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'VisibleItems', 'Видимых элементов') + ':';

  //Элементы
  for i := 0 to Length(FFrames) - 1 do
    FFrames[i].ChangeLanguage(FParams.Language);

  //Строка статуса
  UpdateStatusBar;
end;


procedure TBuilderFrame.SaveSettings;
var
  F: TIniFile;
  i: Integer;
begin
  F := TIniFile.Create(FSettingsFile);

  //Настройки
  F.WriteString(SECTION_SETTINGS, PARAM_FILTER, GetFilter);

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

    //Прочитать настройки фильтра
    edFilter.Text := F.ReadString(SECTION_SETTINGS, PARAM_FILTER, '');

    //Поправить строку статуса
    UpdateStatusBar;

    //Поправить кнопку собрать все
    CorrectBuildAllButton;

  finally
    Values.Free;
    F.Free;
  end;
end;


procedure TBuilderFrame.Sort;
var
  i, j: Integer;
  frm: TBuilderItemFrame;
begin
  for i := 0 to Length(FFrames) - 1 do
    for j := i to Length(FFrames) - 1 do
      if FFrames[i].Title > FFrames[j].Title then
      begin
        frm := FFrames[i];
        FFrames[i] := FFrames[j];
        FFrames[j] := frm;
      end;

  //Упорядочить фреймы
  ArrangeItemFrames;
end;


procedure TBuilderFrame.Clear;
begin
  //Удалить
  ClearItemFrames;

  //Сохранить настройки
  SaveSettings;

  //Поправить строку статуса
  UpdateStatusBar;

  //Поправить панель инструментов
  CorrectToolButtons;
end;



end.


