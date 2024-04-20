unit StringTableItemEditorDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, windows,
  DialogParameters, Language, Translator,
  StringTableItem, DialogCommonUnit, DirectoryItemEditorDialogItemUnit,
  StringTableProcessDialogUnit, TranslationWorker;

type
  //Проверка на совпадение ID
  TStringTableExistCheckerHandler = function(IDName, OriginalIDName: String): Boolean of object;


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
    procedure edIDValueChange(Sender: TObject);
    procedure edIDValueKeyPress(Sender: TObject; var Key: char);
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
    procedure CorrectOKButton;

    procedure LanguageClickHandler(Sender: TObject; Lng: TStringTableLanguageTypes; Msg: String);
  private
    FOriginalIDName: String;
    FWorkerItem: TStringTableItem;  //Объект для заполнения переводом

    procedure TranslatorMessageHandler(var Msg: TMessage); message TRANSLATOR_MESSAGE;
  public
    constructor Create(Parameters: TDialogParameters); reintroduce;
    destructor Destroy; override;
  end;


function StringTableItemEditorDialogExecute(
  Language: TLanguage;
  Mode: TStringTableItemEditorMode;
  EditItem: TStringTableItem;
  ExistChecker: TStringTableExistCheckerHandler
  ): Boolean;


implementation

{$R *.lfm}

type
  TStringTableItemEditorDialogParameters = class(TDialogParameters)
    Mode: TStringTableItemEditorMode;
    EditItem: TStringTableItem;
    ExistChecker: TStringTableExistCheckerHandler
  end;


function StringTableItemEditorDialogExecute(Language: TLanguage; Mode: TStringTableItemEditorMode; EditItem: TStringTableItem; ExistChecker: TStringTableExistCheckerHandler): Boolean;
var
  Params: TStringTableItemEditorDialogParameters;
begin
  Result := False;
  Params := TStringTableItemEditorDialogParameters.Create;
  Params.ExistChecker := ExistChecker;
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


procedure TStringTableItemEditorDialogForm.edIDValueChange(Sender: TObject);
begin
  CorrectOKButton;
end;


procedure TStringTableItemEditorDialogForm.edIDValueKeyPress(Sender: TObject; var Key: char);
const
  VALID_CHARS = ['a'..'z', 'A'..'Z', '0'..'9', '_', #8];
begin
  if not (Key in VALID_CHARS) then
    Key := #0;
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
    Item.OnTranslateClick := @LanguageClickHandler;

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

  //Запомнить оригинальный ID
  FOriginalIDName := Params.EditItem.ID;

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


procedure TStringTableItemEditorDialogForm.CorrectOKButton;

  function IsFirstCharNumber: Boolean;
  const
    NUMBERS = ['0'..'9'];
  var
    s: String;
  begin
    Result := False;
    s := Trim(edIDValue.Text);
    if Length(s) > 0 then
    begin
      if s[1] in NUMBERS then
        Exit(True);
    end;
  end;

  function IsIDEmpty: Boolean;
  begin
    Result := Trim(edIDValue.Text) = '';
  end;

var
  IsExist: Boolean;
begin
  //Проверить на совпадение по таблице
  IsExist := (FParameters as TStringTableItemEditorDialogParameters).ExistChecker(edIDValue.Text, FOriginalIDName);

  btnOK.Enabled := (not IsExist) and (not IsFirstCharNumber) and (not IsIDEmpty);
  //{$BoolEval ON}
  //btnOK.Enabled := not (IsExist and IsFirstCharNumber and IsIDEmpty);
  //{$BoolEval OFF}
end;


procedure TStringTableItemEditorDialogForm.LanguageClickHandler(Sender: TObject; Lng: TStringTableLanguageTypes; Msg: String);
begin
  //Почистить буфер для заполнения
  FWorkerItem.ClearTable;

  //Создать поток перевода
  TTranslateWorker.Create(Handle, FWorkerItem, Lng, Msg);
end;


procedure TStringTableItemEditorDialogForm.TranslatorMessageHandler(var Msg: TMessage);
var
  LngType: TStringTableLanguageTypes;
  i: Integer;
begin
  case Msg.wParam of
    TRANSLATOR_WPARAM_START:
    begin
      Enabled := False;
      StringTableProcessDialogShow(Self, FParameters.Language);
    end;

    TRANSLATOR_WPARAM_LANGUAGE_ID:
    begin
      LngType := TStringTableLanguageTypes(Msg.lParam);
      StringTableProcessDialogUpdateMessage(StringTableLanguageNames[LngType]);
    end;

    TRANSLATOR_WPARAM_FINISH, TRANSLATOR_WPARAM_ERROR:
    begin
      StringTableProcessDialogHide;
      Enabled := True;

      //Заполнить элементы
      for i := 0 to Length(FItemList) - 1 do
        FItemList[i].Value := FWorkerItem.LocalizedText[TStringTableLanguageTypes(i)];
    end;
  end;
end;


constructor TStringTableItemEditorDialogForm.Create(Parameters: TDialogParameters);
begin
  inherited Create(Parameters);

  FWorkerItem := TStringTableItem.Create;
end;


destructor TStringTableItemEditorDialogForm.Destroy;
begin
  FWorkerItem.Free;
  DestroyItems;

  inherited Destroy;
end;



end.

