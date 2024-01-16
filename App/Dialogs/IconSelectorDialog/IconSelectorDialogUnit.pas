unit IconSelectorDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  ComCtrls, DialogCommonUnit;

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
    FIconDirectory: String;
    FSelectedIconName: String;

    procedure LoadIconToListView;
    procedure CorrectSelectButton;
    procedure SelectIconItem(AName: String);
  public
    constructor Create(const IconDirectory: String; const SelectedIcon: String = ''); reintroduce;
  end;


function IconSelectorDialogExecute(const IconDirectory: String; var SelectedIcon: String): Boolean;


implementation

{$R *.lfm}

uses
  sgeStringList, sgeFileUtils;


function IconSelectorDialogExecute(const IconDirectory: String; var SelectedIcon: String): Boolean;
begin
  with TIconSelectorDialogForm.Create(IconDirectory, SelectedIcon) do
  begin
    ShowModal;
    Result := (Tag = 1);
    if Tag = 1 then
      SelectedIcon := FSelectedIconName;
    Free;
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
begin
  List := TsgeStringList.Create;
  AIcon := TIcon.Create;
  try
    //Почистить лишнее
    ilIcons.Clear;
    lvIcons.Clear;

    //Найти все иконки в каталоге
    sgeFindFilesInFolderByExt(FIconDirectory, List, 'ico');

    //Упорядочить
    List.Sort;

    for i := 0 to List.Count - 1 do
    begin
      //Полный путь к иконке
      Fn := FIconDirectory + List.Part[i];

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


constructor TIconSelectorDialogForm.Create(const IconDirectory: String; const SelectedIcon: String);
begin
  inherited Create(nil);

  FIconDirectory := IncludeTrailingBackslash(IconDirectory);
  FSelectedIconName := SelectedIcon;

  LoadIconToListView;
  SelectIconItem(FSelectedIconName);
end;



end.

