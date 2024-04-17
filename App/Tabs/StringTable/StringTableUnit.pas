unit StringTableUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, IniFiles, Buttons, Grids,
  Dialogs, ComCtrls, windows,
  StringTableItemList, StringTableItem, TabParameters, TabCommonUnit;

type
  TStringTableFrame = class(TTabCommonFrame)
    btnColumnAdjust: TSpeedButton;
    btnOpen: TSpeedButton;
    btnDelete: TSpeedButton;
    btnNew: TSpeedButton;
    btnEdit: TSpeedButton;
    btnSave: TSpeedButton;
    btnAdd: TSpeedButton;
    OpenDialog: TOpenDialog;
    pnlTools: TPanel;
    SaveDialog: TSaveDialog;
    sbContent: TScrollBox;
    StatusBar: TStatusBar;
    TableGrid: TStringGrid;
    procedure btnAddClick(Sender: TObject);
    procedure btnColumnAdjustClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure TableGridClick(Sender: TObject);
    procedure TableGridDblClick(Sender: TObject);
  private
    const
      SECTION_COLUMN_WIDTH = 'ColumnWidth';
      SECTION_COLUMN_VISIBLE = 'ColumnVisible';

      LANGUAGE_PREFIX = 'StringTable.';
  private
    FLangTable: TStringTableItemList;
    FSettingsDir: String;
    FSettingsFile: String;
    FLangTableFile: String;

    procedure SaveSettings(const FileName: String);
    procedure LoadSettings(const FileName: String);

    function  GetColumnCaptionByIndex(index: Integer): String;
    procedure PrepareTableGrid;
    procedure ClearTableGrid;
    procedure UpdateTableListRowByIndex(Index: Integer);
    procedure LoadTableListToGrid(LangTable: TStringTableItemList);
    procedure CorrectToolbar;
    function  GetTableSelectedRowIndex: Integer;
    procedure UpdateStatusBar;
    procedure SaveTable(FileName: String);
    procedure LoadTable(FileName: String);

  public
    constructor Create(Parameters: TTabParameters); reintroduce;

    destructor  Destroy; override;

    procedure ApplyLanguage; override;
    procedure FitColumns;
  end;


implementation

{$R *.lfm}

uses
  YesNoQuestionDialogUnit, MessageDialogUnit, ColumnAdjustUnit, StringTableItemEditorDialogUnit;


procedure TStringTableFrame.btnNewClick(Sender: TObject);
begin
  //Если таблица пустая, то нечего спрашивать
  if FLangTable.Count = 0 then
    Exit;

  if YesNoQuestionDialogExecute(
    FParams.Language,
    FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ProceedChange', 'Текущая таблица будет стерта, продолжить?')
  ) then
  begin
    //Очистить список
    FLangTable.Clear;

    //Очистить таблицу
    ClearTableGrid;

    //Поправить панель
    CorrectToolbar;
  end;
end;


procedure TStringTableFrame.btnDeleteClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetTableSelectedRowIndex;

  //Защита от дурака
  if Index = -1 then
    Exit;

  if YesNoQuestionDialogExecute(
    FParams.Language,
    Format(FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'DeleteQuestion', 'Удалить идентификатор "%s"?'), [FLangTable.Item[Index].ID])
  ) then
  begin
    //Удалить идентификатор
    FLangTable.Delete(Index);

    //Обновить таблицу
    LoadTableListToGrid(FLangTable);

    //Поправить панель
    CorrectToolbar;
  end;
end;


procedure TStringTableFrame.btnEditClick(Sender: TObject);
var
  Item: TStringTableItem;
  Index: Integer;
begin
  Item := TStringTableItem.Create;
  try

    //Определить номер строки для изменения
    Index := GetTableSelectedRowIndex;
    if Index = -1 then
      Exit;

    //Подготовить элемент для редактирования
    Item.CopyFrom(FLangTable.Item[Index]);


    if StringTableItemEditorDialogExecute(FParams.Language, stiemEdit, Item) then
    begin
      //Обновить элемент
      FLangTable.Item[Index].CopyFrom(Item);

      //Поправить интерфейс
      UpdateTableListRowByIndex(Index);
    end;

  finally
    Item.Free;
  end;
end;


procedure TStringTableFrame.btnColumnAdjustClick(Sender: TObject);
var
  Pos: TPoint;
begin
  Pos.X := 0;
  Pos.Y := 0;
  Pos := btnColumnAdjust.ClientToScreen(Pos);

  ColumnAdjustExecute(Pos, TableGrid);
end;


procedure TStringTableFrame.btnAddClick(Sender: TObject);
var
  Item, NewItem: TStringTableItem;
begin
  Item := TStringTableItem.Create;
  try

    if StringTableItemEditorDialogExecute(FParams.Language, stiemNew, Item) then
    begin
      NewItem := TStringTableItem.Create;
      NewItem.CopyFrom(Item);

      //Добавить элемент
      FLangTable.Add(NewItem);

      //Поправить интерфейс
      LoadTableListToGrid(FLangTable);
      CorrectToolbar;
    end;

  finally
    Item.Free;
  end;
end;


procedure TStringTableFrame.btnOpenClick(Sender: TObject);
begin
  OpenDialog.FileName := '';
  if OpenDialog.Execute then
    LoadTable(OpenDialog.FileName);
end;


procedure TStringTableFrame.btnSaveClick(Sender: TObject);
begin
  SaveDialog.FileName := 'stringtable.csv';
  if SaveDialog.Execute then
    SaveTable(SaveDialog.FileName);
end;


procedure TStringTableFrame.TableGridClick(Sender: TObject);
begin
  CorrectToolbar;
end;


procedure TStringTableFrame.TableGridDblClick(Sender: TObject);
begin
  btnEdit.Click;
end;


procedure TStringTableFrame.SaveSettings(const FileName: String);
var
  F: TIniFile;
  i: Integer;
begin
  F := TIniFile.Create(FSettingsFile);
  try

    //Столбцы таблицы
    for i := 0 to TableGrid.Columns.Count - 1 do
    begin
      F.WriteBool(SECTION_COLUMN_VISIBLE, IntToStr(i), TableGrid.Columns.Items[i].Visible);
      F.WriteInteger(SECTION_COLUMN_WIDTH, IntToStr(i), TableGrid.Columns.Items[i].StoredWidth);
    end;

  finally
    F.Free;
  end;

  //Сохранить тоследнюю таблицу
  SaveTable(FLangTableFile);
end;


procedure TStringTableFrame.LoadSettings(const FileName: String);
var
  F: TIniFile;
  i: Integer;
begin
  F := TIniFile.Create(FSettingsFile);
  try

    //Столбцы таблицы
    for i := 0 to TableGrid.Columns.Count - 1 do
    begin
      TableGrid.Columns.Items[i].Width := F.ReadInteger(SECTION_COLUMN_WIDTH, IntToStr(i), TableGrid.DefaultColWidth);
      TableGrid.Columns.Items[i].Visible := F.ReadBool(SECTION_COLUMN_VISIBLE, IntToStr(i), True);
    end;

  finally
    F.Free;
  end;

  //Загрузить тоследнюю таблицу
  if FileExists(FLangTableFile) then
    LoadTable(FLangTableFile);
end;


function TStringTableFrame.GetColumnCaptionByIndex(index: Integer): String;
begin
  Result := '';
  if index > TableGrid.Columns.Count - 1 then
    Exit;

  if index = 0 then
    Result := 'Идентификатор'
  else
    Result := StringTableLanguageNames[TStringTableLanguageTypes(index - 1)];
end;


procedure TStringTableFrame.PrepareTableGrid;
var
  Col: TGridColumn;
  i: TStringTableLanguageTypes;
begin
  //Почистить столбцы
  TableGrid.Columns.Clear;

  //Добавить обязательный столбец ID
  Col := TGridColumn.Create(TableGrid.Columns);
  Col.Title.Caption := GetColumnCaptionByIndex(0);
  Col.Title.Alignment := taCenter;

  //Добавить остальные столбцы
  for i := Low(TStringTableLanguageTypes) to High(TStringTableLanguageTypes) do
  begin
    Col := TGridColumn.Create(TableGrid.Columns);
    Col.Title.Caption := GetColumnCaptionByIndex(Ord(i) + 1);
    Col.Title.Alignment := taCenter;
  end;

  //Выровнять ширину первого столбика
  TableGrid.AutoSizeColumns;
end;


procedure TStringTableFrame.ClearTableGrid;
begin
  TableGrid.RowCount := 1;
end;


procedure TStringTableFrame.UpdateTableListRowByIndex(Index: Integer);
var
  l: TStringTableLanguageTypes;
begin
  //Заполнить идентификатор
  TableGrid.Cells[0, Index + 1] := FLangTable.Item[Index].ID;

  //Языки
  for l := Low(TStringTableLanguageTypes) to High(TStringTableLanguageTypes) do
    TableGrid.Cells[Ord(l) + 1, Index + 1] := FLangTable.Item[Index].Table[l];
end;


procedure TStringTableFrame.LoadTableListToGrid(LangTable: TStringTableItemList);
var
  i: Integer;
begin
  TableGrid.RowCount := LangTable.Count + 1;

  for i := 0 to LangTable.Count - 1 do
    UpdateTableListRowByIndex(i);
end;


procedure TStringTableFrame.CorrectToolbar;
var
  RowIndex: Integer;
begin
  RowIndex := GetTableSelectedRowIndex;

  //Кнопки
  btnEdit.Enabled := RowIndex <> -1;
  btnDelete.Enabled := RowIndex <> -1;

  //Поправить строку статуса
  UpdateStatusBar;
end;


function TStringTableFrame.GetTableSelectedRowIndex: Integer;
begin
  Result := TableGrid.Row - 1;
end;


procedure TStringTableFrame.UpdateStatusBar;
begin
  StatusBar.SimpleText := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'TotalItems', 'Всего элементов') + ': ' + IntToStr(FLangTable.Count);
end;


procedure TStringTableFrame.SaveTable(FileName: String);
begin
  try
    //Сохранить на диск
    FLangTable.SaveToFile(FileName);

  except
    MessageDialogExecute(
      FParams.Language,
      Format(FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'FileAccessError', 'При доступе к файлу "%s"%sпроизошла ошибка, продолжение невозможно'), [OpenDialog.FileName, sLineBreak])
      );
  end;
end;


procedure TStringTableFrame.LoadTable(FileName: String);
begin
  try
    //Грузим из файла
    FLangTable.LoadFromFile(FileName);

    //Обновить таблицу
    LoadTableListToGrid(FLangTable);

    //Поправить панель
    CorrectToolbar;

  except
    MessageDialogExecute(
      FParams.Language,
      Format(FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'FileAccessError', 'При доступе к файлу "%s"%sпроизошла ошибка, продолжение невозможно'), [OpenDialog.FileName, sLineBreak])
      );
  end;
end;


constructor TStringTableFrame.Create(Parameters: TTabParameters);
begin
  inherited Create(Parameters);

  //Создать классы
  FLangTable := TStringTableItemList.Create;

  //Подготовить таблицу
  PrepareTableGrid;

  //Определить настройки
  FSettingsDir := FParams.SettingsDirectory + 'StringTable\';
  ForceDirectories(FSettingsDir);
  FSettingsFile := FSettingsDir + 'StringTable.ini';
  FLangTableFile := FSettingsDir + 'Work.csv';

  //Загрузить настройки
  LoadSettings(FSettingsFile);
end;


destructor TStringTableFrame.Destroy;
begin
  //Сохранить настройки
  SaveSettings(FSettingsFile);

  FLangTable.Free;

  inherited Destroy;
end;


procedure TStringTableFrame.ApplyLanguage;
var
  i: Integer;
begin
  //Кнопки
  btnNew.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'New', 'Создать новую');
  btnOpen.Caption := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Open', 'Открыть');
  btnSave.Caption := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Save', 'Сохранить');
  btnAdd.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Add', 'Добавить');
  btnEdit.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Edit', 'Изменить');
  btnDelete.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Delete', 'Удалить');
  btnColumnAdjust.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ColumnAdjust', 'Настройка колонок');
  OpenDialog.Filter := Format('%s (*.csv)|*.csv', [FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Executable', 'Таблицы строк')]);
  SaveDialog.Filter := OpenDialog.Filter;

  //Заголовок таблицы
  for i := 0 to TableGrid.Columns.Count - 1 do
    TableGrid.Columns.Items[i].Title.Caption := FParams.Language.GetLocalizedString(
      LANGUAGE_PREFIX + 'Column.' + GetColumnCaptionByIndex(i), GetColumnCaptionByIndex(i));

  //Строка статуса
  UpdateStatusBar;
end;


procedure TStringTableFrame.FitColumns;
var
  i: Integer;
begin
  for i := 0 to TableGrid.Columns.Count - 1 do
    if TableGrid.Columns.Items[i].Visible then
      TableGrid.AutoSizeColumn(i);
end;



end.

