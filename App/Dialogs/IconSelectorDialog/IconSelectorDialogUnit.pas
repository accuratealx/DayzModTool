unit IconSelectorDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Controls, Graphics, Dialogs, Buttons, ComCtrls,
  Language,
  DialogCommonUnit;

type
  TIconSelectorDialogForm = class(TDialogCommonForm)
    btnClose: TSpeedButton;
    btnSelect: TSpeedButton;
    ilIcons: TImageList;
    lvIcons: TListView;
    procedure btnCloseClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lvIconsClick(Sender: TObject);
    procedure lvIconsDblClick(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'Dialogs.IconSelector.';
  private
    FSelectedIconName: String;

    procedure LoadIconToListView;
    procedure CorrectSelectButton;
    procedure SelectIconItem(AName: String);
  protected
    procedure PrepareInterface; override;
    procedure SetLanguage; override;
  end;


function IconSelectorDialogExecute(Language: TLanguage; const IconDirectory: String; var SelectedIcon: String): Boolean;


implementation

{$R *.lfm}

uses
  sgeStringList, sgeFileUtils,
  DialogParameters;

type
  TIconSelectorDialogParameters = class(TDialogParameters)
    IconDirectory: String;
    SelectedIcon: String;
  end;


function IconSelectorDialogExecute(Language: TLanguage; const IconDirectory: String; var SelectedIcon: String): Boolean;
var
  Params: TIconSelectorDialogParameters;
begin
  Params := TIconSelectorDialogParameters.Create;
  Params.Language := Language;
  Params.IconDirectory := IconDirectory;
  Params.SelectedIcon := SelectedIcon;
  try
    with TIconSelectorDialogForm.Create(Params) do
    begin
      ShowModal;
      Result := (Tag = 1);
      if Tag = 1 then
        SelectedIcon := FSelectedIconName;
      Free;
    end;

  finally
    Params.Free;
  end;
end;


procedure TIconSelectorDialogForm.FormResize(Sender: TObject);
const
  SPACE = 10;
var
  W: Integer;
begin
  W := btnSelect.Width + btnClose.Width + SPACE;
  btnSelect.Left := (pnlButton.Width - W) div 2;
  btnClose.Left := btnSelect.Left + btnSelect.Width + SPACE;
end;


procedure TIconSelectorDialogForm.lvIconsClick(Sender: TObject);
begin
  FSelectedIconName := '';

  //Если попали в элемен, то запомним
  if lvIcons.Selected <> nil then
    FSelectedIconName := lvIcons.Selected.Caption + '.ico';

  //Поправить кнопку выбора
  CorrectSelectButton;
end;


procedure TIconSelectorDialogForm.lvIconsDblClick(Sender: TObject);
begin
  if btnSelect.Enabled then
    btnSelect.Click;
end;


procedure TIconSelectorDialogForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TIconSelectorDialogForm.btnSelectClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TIconSelectorDialogForm.LoadIconToListView;
var
  List: TsgeStringList;
  i: Integer;
  Item: TListItem;
  AIcon: TIcon;
  Fn: String;
  Params: TIconSelectorDialogParameters;
begin
  Params := FParameters as TIconSelectorDialogParameters;

  List := TsgeStringList.Create;
  AIcon := TIcon.Create;
  try
    //Почистить лишнее
    ilIcons.Clear;
    lvIcons.Clear;

    //Найти все иконки в каталоге
    sgeFindFilesInFolderByExt(Params.IconDirectory, List, 'ico');

    //Упорядочить
    List.Sort;

    for i := 0 to List.Count - 1 do
    begin
      //Полный путь к иконке
      Fn := Params.IconDirectory + List.Part[i];

      //Загрузить иконку
      AIcon.LoadFromFile(Fn);

      //Добавить в список
      ilIcons.AddIcon(AIcon);

      //Добавить элемент
      Item := lvIcons.Items.Add;
      Item.Caption := ChangeFileExt(List.Part[i], '');
      Item.ImageIndex := i;
    end;

  finally
    AIcon.Free;
    List.Free;
  end;
end;


procedure TIconSelectorDialogForm.CorrectSelectButton;
begin
  btnSelect.Enabled := FSelectedIconName <> '';
end;


procedure TIconSelectorDialogForm.SelectIconItem(AName: String);
var
  i: Integer;
begin
  for i := 0 to lvIcons.Items.Count - 1 do
  begin
    if LowerCase(lvIcons.Items.Item[i].Caption) = LowerCase(ChangeFileExt(AName, '')) then
    begin
      lvIcons.Items.Item[i].Selected := True;
      CorrectSelectButton;
      Break;
    end;
  end;
end;


procedure TIconSelectorDialogForm.PrepareInterface;
var
  Params: TIconSelectorDialogParameters;
begin
  Params := FParameters as TIconSelectorDialogParameters;
  FSelectedIconName := Params.SelectedIcon;

  LoadIconToListView;
  SelectIconItem(FSelectedIconName);
end;


procedure TIconSelectorDialogForm.SetLanguage;
begin
  Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Caption', 'Выбор иконки');
  btnSelect.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Select', 'Выбрать');
  btnClose.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Close', 'Закрыть');
end;



end.

