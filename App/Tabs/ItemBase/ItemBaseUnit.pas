unit ItemBaseUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Buttons,
  ComCtrls, Grids,
  TabCommonUnit, TabParameters, ObjectItemList;

type
  TItemBaseFrame = class(TTabCommonFrame)
    btnAdd: TSpeedButton;
    btnClearFilter: TSpeedButton;
    btnClearTable: TSpeedButton;
    btnDelete: TSpeedButton;
    btnEdit: TSpeedButton;
    btnOpen: TSpeedButton;
    btnImport: TSpeedButton;
    btnSave: TSpeedButton;
    btnCopyObjName: TSpeedButton;
    edFilter: TEdit;
    OpenDialog: TOpenDialog;
    pnlTools: TPanel;
    SaveDialog: TSaveDialog;
    sbContent: TScrollBox;
    StatusBar: TStatusBar;
    TableGrid: TStringGrid;
    procedure btnAddClick(Sender: TObject);
    procedure btnClearFilterClick(Sender: TObject);
    procedure btnClearTableClick(Sender: TObject);
    procedure btnCopyObjNameClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure edFilterChange(Sender: TObject);
    procedure TableGridDblClick(Sender: TObject);
    procedure TableGridSelection(Sender: TObject; aCol, aRow: Integer);
  private
    const
      LANGUAGE_PREFIX = 'TabItemBase.';

      SECTION_COLUMN_WIDTH = 'ColumnWidth';
      SECTION_SETTINGS = 'Settings';

      PARAM_FILTER = 'Filter';

      ITEM_BASE_EXT = 'dzmtib';
  private
    FSettingsFile: String;
    FItemBaseFile: String;

    //База предметов
    FItemBase: TObjectItemList;

    procedure LoadItemsToTableGrid(Filter: String);
    procedure ClearTableGrid;
    function  GetSelectedRow: Integer;

    procedure EditItem;
    procedure UpdateStatusBar;
    procedure CorrectToolButtons;
    function  GetFilter: String;

    procedure LoadBase(FileName: String);
    procedure SaveBase(FileName: String);

    //Функция проверки на существование имени класса
    function ItemClassNameChecker(AObjName, OriginalName: String): Boolean;
  public
    constructor Create(Parameters: TTabParameters; AParent: TWinControl); reintroduce;
    destructor  Destroy; override;

    procedure ApplyLanguage; override;
    procedure SaveSettings; override;
    procedure LoadSettings; override;

    procedure FitColumns;
    procedure Sort;
  end;


implementation

{$R *.lfm}

uses
  IniFiles, Clipbrd,
  MessageDialogUnit, ItemBaseEditorDialogUnit, YesNoQuestionDialogUnit;


procedure TItemBaseFrame.btnClearFilterClick(Sender: TObject);
begin
  edFilter.Text := '';
end;


procedure TItemBaseFrame.btnClearTableClick(Sender: TObject);
begin
  //Если таблица пустая, то нечего спрашивать
  if FItemBase.Count = 0 then
    Exit;

  if YesNoQuestionDialogExecute(
    FParams.Language,
    FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ProceedChange', 'Текущая таблица будет стерта, продолжить?')
  ) then
  begin
    //Очистить список
    FItemBase.Clear;

    //Перезагрузить таблицу
    LoadItemsToTableGrid(GetFilter);

    //Сохранить таблицу
    SaveBase(FItemBaseFile);

    //Поправить кнопки
    CorrectToolButtons;
  end;
end;


procedure TItemBaseFrame.btnCopyObjNameClick(Sender: TObject);
var
  i: Integer;
begin
  i := GetSelectedRow + 1;
  if i = -1 then
    Exit;

  Clipboard.AsText := TableGrid.Cells[1, i];
end;


procedure TItemBaseFrame.btnDeleteClick(Sender: TObject);
var
  i, idx: Integer;
  Item: TObjectItem;
begin
  i := GetSelectedRow + 1;
  if i = -1 then
    Exit;

  //Номер записи
  idx := StrToInt(TableGrid.Cells[0, i]);

  //Выделенный итем
  Item := FItemBase.Item[idx];

  if YesNoQuestionDialogExecute(
    FParams.Language,
    Format(FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'DeleteCurrent', 'Удалить запись "%s" из списка?'), [Item.ObjName])
  ) then
  begin
    //Удалить
    FItemBase.Delete(idx);

    //Перезагрузить таблицу
    LoadItemsToTableGrid(GetFilter);

    //Сохранить таблицу
    SaveBase(FItemBaseFile);

    //Поправить кнопки
    CorrectToolButtons;
  end;
end;


procedure TItemBaseFrame.btnEditClick(Sender: TObject);
begin
  EditItem;
end;


procedure TItemBaseFrame.btnImportClick(Sender: TObject);
begin
  OpenDialog.FileName := '';
  if OpenDialog.Execute then
  begin
    //Открыть таблицу
    FItemBase.AddFromFile(OpenDialog.FileName);

    //Сохранить таблицу
    SaveBase(FItemBaseFile);

    //Перезагрузить таблицу
    LoadItemsToTableGrid(GetFilter);

    //Поправить кнопки
    CorrectToolButtons;
  end;
end;


procedure TItemBaseFrame.btnOpenClick(Sender: TObject);
begin
  OpenDialog.FileName := '';
  if OpenDialog.Execute then
  begin
    //Открыть таблицу
    FItemBase.LoadFromFile(OpenDialog.FileName);

    //Сохранить таблицу
    SaveBase(FItemBaseFile);

    //Перезагрузить таблицу
    LoadItemsToTableGrid(GetFilter);

    //Поправить кнопки
    CorrectToolButtons;
  end;
end;


procedure TItemBaseFrame.btnSaveClick(Sender: TObject);
begin
  SaveDialog.FileName := 'ItemBase.' + ITEM_BASE_EXT;
  if SaveDialog.Execute then
    FItemBase.SaveToFile(SaveDialog.FileName);
end;


procedure TItemBaseFrame.btnAddClick(Sender: TObject);
var
  AName, ATitle, ADescription: String;
begin
  AName := '';
  ATitle := '';
  ADescription := '';
  if ItemBaseEditorDialogExecute(
    FParams.Language,
    @ItemClassNameChecker,
    ibemNew,
    AName,
    ATitle,
    ADescription) then
  begin
    //Добавить в базу
    FItemBase.Add(AName, ATitle, ADescription);

    //Перезагрузить таблицу
    LoadItemsToTableGrid(GetFilter);

    //Сохранить таблицу
    SaveBase(FItemBaseFile);

    //Поправить кнопки
    CorrectToolButtons;
  end;
end;


procedure TItemBaseFrame.edFilterChange(Sender: TObject);
begin
  LoadItemsToTableGrid(GetFilter);
  CorrectToolButtons;
end;


procedure TItemBaseFrame.TableGridDblClick(Sender: TObject);
begin
  EditItem;
end;


procedure TItemBaseFrame.TableGridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  CorrectToolButtons;
end;


procedure TItemBaseFrame.ClearTableGrid;
begin
  TableGrid.RowCount := 1;
end;


function TItemBaseFrame.GetSelectedRow: Integer;
begin
  Result := TableGrid.Row - 1;
end;


procedure TItemBaseFrame.EditItem;
var
  AName, ATitle, ADescription: String;
  i, idx: Integer;
  Item: TObjectItem;
begin
  i := GetSelectedRow + 1;
  if i = -1 then
    Exit;

  //Номер записи
  idx := StrToInt(TableGrid.Cells[0, i]);

  Item := FItemBase.Item[idx];

  AName := Item.ObjName;
  ATitle := Item.ObjTitle;
  ADescription := Item.ObjDescription;
  if ItemBaseEditorDialogExecute(
    FParams.Language,
    @ItemClassNameChecker,
    ibemEdit,
    AName,
    ATitle,
    ADescription) then
  begin
    //Обновить данные в базе
    Item.ObjName := AName;
    Item.ObjTitle := ATitle;
    Item.ObjDescription := ADescription;

    //Поправить таблицу
    TableGrid.Cells[1, i] := AName;
    TableGrid.Cells[2, i] := ATitle;
    TableGrid.Cells[3, i] := ADescription;

    //Сохранить таблицу
    SaveBase(FItemBaseFile);

    //Поправить кнопки
    CorrectToolButtons;
  end;

end;


procedure TItemBaseFrame.LoadItemsToTableGrid(Filter: String);

  function IsMatch(AItem: TObjectItem; AFilter: String): Boolean;
  begin
    Result := False;

    //Пустой фильтр, все подходит
    if AFilter = '' then
      Exit(True);

    //Проверить данные
    if Pos(AFilter, UnicodeLowerCase(AItem.ObjName)) > 0 then
      Exit(True);

    if Pos(AFilter, UnicodeLowerCase(AItem.ObjTitle)) > 0 then
      Exit(True);

    if Pos(AFilter, UnicodeLowerCase(AItem.ObjDescription)) > 0 then
      Exit(True);
  end;

var
  i, c, SelRow: Integer;
  Item: TObjectItem;
begin
  SelRow := TableGrid.Row;

  //Очистить таблицу
  ClearTableGrid;

  TableGrid.BeginUpdate;
  try
    Filter := UnicodeLowerCase(Trim(Filter));

    for i := 0 to FParams.ItemBase.Count - 1 do
    begin
      Item := FParams.ItemBase.Item[i];

      //Добавить строку
      if IsMatch(Item, Filter) then
      begin
        c := TableGrid.RowCount;
        TableGrid.RowCount := c + 1;

        TableGrid.Cells[0, c] := IntToStr(i);
        TableGrid.Cells[1, c] := Item.ObjName;
        TableGrid.Cells[2, c] := Item.ObjTitle;
        TableGrid.Cells[3, c] := Item.ObjDescription;
      end;
    end;

    UpdateStatusBar;

    TableGrid.Row := SelRow;

  finally
    TableGrid.EndUpdate;
  end;
end;


procedure TItemBaseFrame.UpdateStatusBar;
begin
  StatusBar.Panels[1].Text := IntToStr(FParams.ItemBase.Count);
  StatusBar.Panels[3].Text := IntToStr(TableGrid.RowCount - 1);
end;


procedure TItemBaseFrame.CorrectToolButtons;
begin
  btnEdit.Enabled := (GetSelectedRow <> -1);
  btnDelete.Enabled := btnEdit.Enabled;
  btnCopyObjName.Enabled := btnEdit.Enabled;
end;


function TItemBaseFrame.GetFilter: String;
begin
  Result := Trim(edFilter.Text);
end;


procedure TItemBaseFrame.LoadBase(FileName: String);
begin
  try
    FItemBase.LoadFromFile(FileName);
  except
    MessageDialogExecute(
      FParams.Language,
      Format(FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'FileAccessError', 'Произошла ошибка при доступе к файлу "%s" '), [FileName])
      );
  end;
end;


procedure TItemBaseFrame.SaveBase(FileName: String);
begin
  try
    FItemBase.SaveToFile(FileName);
  except
    MessageDialogExecute(
      FParams.Language,
      Format(FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'FileAccessError', 'Произошла ошибка при доступе к файлу "%s" '), [FileName])
      );
  end;
end;


function TItemBaseFrame.ItemClassNameChecker(AObjName, OriginalName: String): Boolean;
var
  i: Integer;
  s: String;
begin
  Result := False;

  AObjName := LowerCase(Trim(AObjName));
  OriginalName := LowerCase(Trim(OriginalName));

  for i := 0 to FItemBase.Count - 1 do
  begin
    s := LowerCase(FItemBase.Item[i].ObjName);

    if s = OriginalName then
      Continue;

    if s = AObjName then
      Exit(True);
  end;
end;


constructor TItemBaseFrame.Create(Parameters: TTabParameters; AParent: TWinControl);
var
  Dir: String;
begin
  inherited Create(Parameters, AParent);

  //Каталог настроек
  Dir := FParams.SettingsDirectory + 'ItemBase\';
  ForceDirectories(Dir);

  //Имена файлов
  FSettingsFile := Dir + 'ItemBase.ini';
  FItemBaseFile := Dir + 'ItemBase.' + ITEM_BASE_EXT;

  //База предметов
  FItemBase := TObjectItemList.Create;

  //Записать ссылку на таблицу для других закладок
  FParams.ItemBase := FItemBase;

  //Загрузка настроек
  LoadSettings;

  //Построить таблицу
  LoadItemsToTableGrid(GetFilter);
  CorrectToolButtons;
end;


destructor TItemBaseFrame.Destroy;
begin
  SaveSettings;

  FItemBase.Free;

  inherited Destroy;
end;


procedure TItemBaseFrame.ApplyLanguage;
begin
  //Кнопки
  btnClearTable.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ClearTable', 'Очистить таблицу');
  btnOpen.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Open', 'Открыть');
  btnImport.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Import', 'Импортировать');
  btnSave.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Save', 'Сохранить');
  btnCopyObjName.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'CopyObjectName', 'Скопировать имя объекта');
  btnAdd.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Add', 'Добавить');
  btnEdit.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Edit', 'Изменить');
  btnDelete.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Delete', 'Удалить');
  btnClearFilter.Hint := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'ClearFilter', 'Очистить фильтр');

  //Диалоги
  OpenDialog.Filter := Format('%s (*.%s)|*.%s', [FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Extension', 'Базы предметов'), ITEM_BASE_EXT, ITEM_BASE_EXT]);
  SaveDialog.Filter := OpenDialog.Filter;

  //Строка статуса
  StatusBar.Panels[0].Text := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'TotalItems', 'Всего элементов') + ':';
  StatusBar.Panels[2].Text := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'VisibleItems', 'Видимых элементов') + ':';

  //Таблица
  TableGrid.Columns.Items[1].Title.Caption := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Column.Name', 'Имя');
  TableGrid.Columns.Items[2].Title.Caption := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Column.Title', 'Заголовок');
  TableGrid.Columns.Items[3].Title.Caption := FParams.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Column.Description', 'Описание');
end;


procedure TItemBaseFrame.SaveSettings;
var
  F: TIniFile;
  i: Integer;
begin
  F := TIniFile.Create(FSettingsFile);
  try
    //Фильтр
    F.WriteString(SECTION_SETTINGS, PARAM_FILTER, GetFilter);

    //Столбцы таблицы
    for i := 1 to TableGrid.Columns.Count - 1 do
      F.WriteInteger(SECTION_COLUMN_WIDTH, IntToStr(i), TableGrid.Columns.Items[i].StoredWidth);

  finally
    F.Free;
  end;
end;


procedure TItemBaseFrame.LoadSettings;
var
  F: TIniFile;
  i: Integer;
begin
  F := TIniFile.Create(FSettingsFile);
  try
    //Фильтр
    edFilter.Text := F.ReadString(SECTION_SETTINGS, PARAM_FILTER, '');

    //Столбцы таблицы
    for i := 1 to TableGrid.Columns.Count - 1 do
      TableGrid.Columns.Items[i].Width := F.ReadInteger(SECTION_COLUMN_WIDTH, IntToStr(i), TableGrid.DefaultColWidth);

  finally
    F.Free;
  end;

  //База предметов
  if FileExists(FItemBaseFile) then
  begin
    LoadBase(FItemBaseFile);

    LoadItemsToTableGrid(GetFilter);

    CorrectToolButtons;
  end;
end;


procedure TItemBaseFrame.FitColumns;
var
  i: Integer;
begin
  for i := 1 to TableGrid.Columns.Count - 1 do
    TableGrid.AutoSizeColumn(i);
end;


procedure TItemBaseFrame.Sort;
begin
  //Упорядочить базу
  FItemBase.Sort(stName, sdForward);

  //Сохранить на диске
  SaveBase(FItemBaseFile);

  //Перечитать таблицу
  LoadItemsToTableGrid(GetFilter);
end;



end.

