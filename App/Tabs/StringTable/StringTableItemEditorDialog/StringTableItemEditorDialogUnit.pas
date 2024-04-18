unit StringTableItemEditorDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  Language,
  StringTableItem, DialogCommonUnit, DirectoryItemEditorDialogItemUnit;

type
  TStringTableItemEditorMode = (
    stiemNew, //Новый
    stiemEdit //Изменение
  );


  TStringTableItemEditorDialogForm = class(TDialogCommonForm)
    btnCancel: TSpeedButton;
    btnOK: TSpeedButton;
    edIDValue: TEdit;
    lblIDTitle: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'Dialogs.StringTableItemEditor.';
      LANGUAGE_PREFIX_LNGNAME = 'StringTable.Column.';
  private
    FItemList: TDirectoryItemEditorDialogItemFrameList;

    procedure CreateItems;
    procedure DestroyItems;
  protected
    procedure SetMode(AMode: TStringTableItemEditorMode);

    procedure InterfaceToItem;
    procedure PrepareInterface; override;
    procedure SetLanguage; override;
  public
    destructor Destroy; override;
  end;


function StringTableItemEditorDialogExecute(
  Language: TLanguage;
  Mode: TStringTableItemEditorMode;
  EditItem: TStringTableItem
  ): Boolean;


implementation

{$R *.lfm}

uses
  DialogParameters;

type
  TStringTableItemEditorDialogParameters = class(TDialogParameters)
    Mode: TStringTableItemEditorMode;
    EditItem: TStringTableItem;
  end;


function StringTableItemEditorDialogExecute(Language: TLanguage; Mode: TStringTableItemEditorMode; EditItem: TStringTableItem): Boolean;
var
  Params: TStringTableItemEditorDialogParameters;
begin
  Params := TStringTableItemEditorDialogParameters.Create;
  Params.Language := Language;
  Params.Mode := Mode;
  Params.EditItem := EditItem;
  try
    with TStringTableItemEditorDialogForm.Create(Params) do
    begin
      ShowModal;
      if Tag = 1 then
      begin
        InterfaceToItem;
        Result := True;
      end;
      Free;
    end;

  finally
    Params.Free;
  end;
end;


procedure TStringTableItemEditorDialogForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;


procedure TStringTableItemEditorDialogForm.btnOKClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;


procedure TStringTableItemEditorDialogForm.FormResize(Sender: TObject);
const
  SPACE = 10;
var
  W: Integer;
begin
  W := btnOK.Width + btnCancel.Width + SPACE;
  btnOK.Left := (pnlButton.Width - W) div 2;
  btnCancel.Left := btnOK.Left + btnOK.Width + SPACE;
end;


procedure TStringTableItemEditorDialogForm.CreateItems;

  procedure AddToList(Item: TDirectoryItemEditorDialogItemFrame);
  var
    c: Integer;
  begin
    c := Length(FItemList);
    SetLength(FItemList, c + 1);
    FItemList[c] := Item;
  end;

var
  i: TStringTableLanguageTypes;
  Y: Integer;
  cpt: String;
  Item: TDirectoryItemEditorDialogItemFrame;
  Params: TStringTableItemEditorDialogParameters;
begin
  Params := FParameters as TStringTableItemEditorDialogParameters;

  Y := 38;

  for i := Low(TStringTableLanguageTypes) to High(TStringTableLanguageTypes) do
  begin
    cpt := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX_LNGNAME + StringTableLanguageNames[i], StringTableLanguageNames[i]);
    Item := TDirectoryItemEditorDialogItemFrame.Create(FParameters.Language, i, cpt, Params.EditItem.Table[i]);
    Item.Left := 0;
    Item.Top := Y;
    Item.Width := pnlContent.Width;
    Item.Parent := pnlContent;
    Item.Anchors := [akTop, akLeft, akRight];

    AddToList(Item);

    Inc(Y, Item.Height);
  end;

  Height := Y + pnlButton.Height + 5;
  Constraints.MinHeight := Height;
end;


procedure TStringTableItemEditorDialogForm.DestroyItems;
var
  i: Integer;
begin
  for i := 0 to Length(FItemList) - 1 do
    FItemList[i].Free;
  SetLength(FItemList, 0);
end;


procedure TStringTableItemEditorDialogForm.SetMode(AMode: TStringTableItemEditorMode);
var
  s: String;
begin
  case AMode of
    stiemNew:
      s := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Add', 'Добавить');

    stiemEdit:
      s := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Edit', 'Изменить');

    else
      s := '';
  end;

  //Поправить внешний вид
  Caption := s;
  btnOK.Caption := s;
end;


procedure TStringTableItemEditorDialogForm.InterfaceToItem;
var
  Params: TStringTableItemEditorDialogParameters;
  i: Integer;
begin
  Params := FParameters as TStringTableItemEditorDialogParameters;

  Params.EditItem.ID := Trim(edIDValue.Text);
  for i := 0 to Length(FItemList) - 1 do
    Params.EditItem.LocalizedText[TStringTableLanguageTypes(i)] := FItemList[i].Value;
end;


procedure TStringTableItemEditorDialogForm.PrepareInterface;
var
  Params: TStringTableItemEditorDialogParameters;
begin
  Params := FParameters as TStringTableItemEditorDialogParameters;

  //Подготовить интерфейс
  SetMode(Params.Mode);

  //Установить идентификатор
  lblIDTitle.Caption := Params.Language.GetLocalizedString(LANGUAGE_PREFIX_LNGNAME + 'Идентификатор', 'Идентификатор');
  edIDValue.Text := Params.EditItem.ID;

  //Создать языки
  CreateItems;
end;


procedure TStringTableItemEditorDialogForm.SetLanguage;
begin
  btnCancel.Caption := FParameters.Language.GetLocalizedString(LANGUAGE_PREFIX + 'Cancel', 'Отмена');
end;


destructor TStringTableItemEditorDialogForm.Destroy;
begin
  DestroyItems;

  inherited Destroy;
end;



end.

