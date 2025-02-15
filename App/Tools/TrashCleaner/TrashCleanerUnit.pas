unit TrashCleanerUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, Buttons, CheckLst,
  DialogCommonUnit, DialogParameters, Language,
  sgeStringList;

type
  TTrashCleanerForm = class(TDialogCommonForm)
    btnClose: TSpeedButton;
    btnClean: TSpeedButton;
    FileListBox: TCheckListBox;
    procedure btnCleanClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FileListBoxDblClick(Sender: TObject);
    procedure FileListBoxItemClick(Sender: TObject; Index: integer);
    procedure FormResize(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'Tools.TrashCleaner.';
  private
    FTrashList: TsgeStringList;
    FCleanBtnCaption: String;

    procedure LoadTrashList;
    procedure LoadListBox;
    procedure CorrectCleanButton;
    function  GetCheckedCount: Integer;
    function  DeleteCheckedFiles: Boolean;
    procedure DeleteEmptyDirectories;
  public
    constructor Create(Parameters: TDialogParameters); reintroduce;
    destructor  Destroy; override;

    procedure PrepareInterface; override;
    procedure SetLanguage; override;
  end;


procedure TrashCleanerExecute(Language: TLanguage);


implementation

{$R *.lfm}

uses
  sgeFileUtils,
  DayZUtils,
  MessageDialogUnit;


procedure TrashCleanerExecute(Language: TLanguage);
var
  Params: TDialogParameters;
begin
  Params := TDialogParameters.Create;
  Params.Language := Language;

  try
    with TTrashCleanerForm.Create(Params) do
    begin
      CorrectCleanButton; //Костыль, для правильного отображения текста на кнопке Очистить

      ShowModal;
      Free;
    end;

  finally
    Params.Free;
  end;
end;


procedure TTrashCleanerForm.FormResize(Sender: TObject);
const
  SPACE = 10;
var
  W: Integer;
begin
  W := btnClean.Width + btnClose.Width + SPACE;
  btnClean.Left := (pnlButton.Width - W) div 2;
  btnClose.Left := btnClean.Left + btnClean.Width + SPACE;
end;


procedure TTrashCleanerForm.LoadTrashList;
begin
  //Очистить список файлов с мусором
  FTrashList.Clear;

  //Получить список файлов с мусором
  GetDayzTrashFileList(FTrashList);
  FTrashList.Sort;
end;


procedure TTrashCleanerForm.LoadListBox;
var
  i: Integer;
begin
  //Почистить список
  FileListBox.Clear;

  //Заполинть данными
  for i := 0 to FTrashList.Count - 1 do
  begin
    FileListBox.Items.Add(FTrashList.Part[i]);
    FileListBox.Checked[i] := True;
  end;
end;


procedure TTrashCleanerForm.CorrectCleanButton;
var
  cnt: Integer;
  cpt: String;
begin
  cnt := GetCheckedCount;
  btnClean.Enabled := cnt > 0;

  if cnt = 0 then
    cpt := FCleanBtnCaption
  else
    cpt := FCleanBtnCaption + ' (' + IntToStr(cnt) + ')';

  btnClean.Caption := cpt;
end;


function TTrashCleanerForm.GetCheckedCount: Integer;
var
  i: Integer;
begin
  Result := 0;

  for i := 0 to FileListBox.Count - 1 do
    if FileListBox.Checked[i] then
      Inc(Result);
end;


function TTrashCleanerForm.DeleteCheckedFiles: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FileListBox.Count - 1 do
    if FileListBox.Checked[i] then
      if not DeleteFileToRecycle(FileListBox.Items.Strings[i]) then
        Result := False;
end;


procedure TTrashCleanerForm.DeleteEmptyDirectories;
var
  BaseDirs, AllDirs: TsgeStringList;
  i: Integer;
  Dir: String;
begin
  BaseDirs := TsgeStringList.Create;
  AllDirs := TsgeStringList.Create;
  try
    //Получим список базовых каталогов
    GetDayZTrashDirectoryList(BaseDirs);

    //Получим полный список дочерних каталогов
    for i := 0 to BaseDirs.Count - 1 do
    begin
      Dir := BaseDirs.Part[i];

      if DirectoryExists(Dir) then
      begin
        //Добавим родителя
        AllDirs.Add(Dir);

        //Найдем подкаталоги
        sgeFindFoldersInFolder(Dir, AllDirs);
      end;
    end;

    //Удалим пустые каталоги
    AllDirs.Sort;
    for i := AllDirs.Count - 1 downto 0 do
    begin
      if IsDirectoryEmpty(AllDirs.Part[i]) then
        RmDir(AllDirs.Part[i]); //Капризная функция, может упасть если не найдет каталог
    end;

  finally
    AllDirs.Free;
    BaseDirs.Free;
  end;
end;


constructor TTrashCleanerForm.Create(Parameters: TDialogParameters);
begin
  FTrashList := TsgeStringList.Create;

  inherited Create(Parameters);
end;


destructor TTrashCleanerForm.Destroy;
begin
  FTrashList.Free;

  inherited Destroy;
end;


procedure TTrashCleanerForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TTrashCleanerForm.FileListBoxDblClick(Sender: TObject);
var
  dir: String;
begin
  if FileListBox.ItemIndex <> -1 then
  begin
    dir := ExtractFilePath(FileListBox.Items.Strings[FileListBox.ItemINdex]);
    OpenFolderInExplorer(dir);
  end;
end;


procedure TTrashCleanerForm.btnCleanClick(Sender: TObject);
var
  IsShowAlert: Boolean;
begin
  btnClean.Enabled := False;
  btnClose.Enabled := False;
  FileListBox.Enabled := False;

  try
    //Удалить выбранные файлы
    IsShowAlert := not DeleteCheckedFiles;

    //Удалить пустые каталоги
    DeleteEmptyDirectories;

  finally
    //Перезагрузить список
    PrepareInterface;

    FileListBox.Enabled := True;
    btnClose.Enabled := True;

    if IsShowAlert then
    begin
      MessageDialogExecute(
        FParameters.Language,
        FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'CantDeleteAll', 'Не все файлы были удалены')
      );
    end;
  end;
end;


procedure TTrashCleanerForm.FileListBoxItemClick(Sender: TObject; Index: integer);
begin
  CorrectCleanButton;
end;


procedure TTrashCleanerForm.PrepareInterface;
begin
  LoadTrashList;
  LoadListBox;
  CorrectCleanButton;
end;


procedure TTrashCleanerForm.SetLanguage;
begin
  Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Caption', 'Очистка мусора');
  btnClose.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Close', 'Закрыть');
  FCleanBtnCaption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Clean', 'Очистить');
end;



end.

