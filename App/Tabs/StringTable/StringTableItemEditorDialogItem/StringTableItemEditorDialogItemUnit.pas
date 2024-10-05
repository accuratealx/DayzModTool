unit StringTableItemEditorDialogItemUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons,
  Language, StringTableItem;

type
  //Обработчик кнопки перевода
  TTranslateClick = procedure(Sender: TObject; Lng: TStringTableLanguageTypes; Msg: String) of object;


  TStringTableItemEditorDialogItemFrame = class(TFrame)
    btnTranslate: TSpeedButton;
    btnClear: TSpeedButton;
    edValue: TEdit;
    lblTitle: TLabel;
    procedure btnClearClick(Sender: TObject);
    procedure btnTranslateClick(Sender: TObject);
    procedure edValueChange(Sender: TObject);
  private
    FLanguage: TLanguage;
    FTitle: String;
    FValue: String;
    FLngType: TStringTableLanguageTypes;

    FOnTranslateClick: TTranslateClick;

    procedure SetTitle(ATitle: String);
    procedure SetValue(AValue: String);
    function  GetValue: String;

    procedure ApplyLanguage;
  public
    constructor Create(Language: TLanguage; LngType: TStringTableLanguageTypes; Title: String; Value: String); reintroduce;

    property Title: String read FTitle;
    property Value: String read GetValue write SetValue;
    property LngName: TStringTableLanguageTypes read FLngType write FLngType;
    property OnTranslateClick: TTranslateClick read FOnTranslateClick write FOnTranslateClick;
  end;


  TStringTableItemEditorDialogItemFrameList = array of TStringTableItemEditorDialogItemFrame;


implementation

{$R *.lfm}


procedure TStringTableItemEditorDialogItemFrame.edValueChange(Sender: TObject);
begin
  FValue := edValue.Text;
end;


procedure TStringTableItemEditorDialogItemFrame.btnClearClick(Sender: TObject);
begin
  SetValue('');
end;


procedure TStringTableItemEditorDialogItemFrame.btnTranslateClick(Sender: TObject);
begin
  if Assigned(FOnTranslateClick) then
    FOnTranslateClick(Sender, FLngType, FValue);
end;


procedure TStringTableItemEditorDialogItemFrame.SetTitle(ATitle: String);
begin
  FTitle := ATitle;
  lblTitle.Caption := FTitle;
end;


procedure TStringTableItemEditorDialogItemFrame.SetValue(AValue: String);
begin
  FValue := AValue;
  edValue.Text := FValue;
end;


function TStringTableItemEditorDialogItemFrame.GetValue: String;
begin
  Result := Trim(FValue);
end;


procedure TStringTableItemEditorDialogItemFrame.ApplyLanguage;
begin
  btnTranslate.Hint := FLanguage.GetLocalizedString('Dialogs.StringTableItemEditor.Translate', 'Перевести');
  btnClear.Hint := FLanguage.GetLocalizedString('Dialogs.StringTableItemEditor.Clear', 'Очистить');
end;


constructor TStringTableItemEditorDialogItemFrame.Create(Language: TLanguage; LngType: TStringTableLanguageTypes; Title: String; Value: String);
begin
  inherited Create(nil);

  FLanguage := Language;
  FLngType := LngType;
  SetTitle(Title);
  SetValue(Value);

  ApplyLanguage;
end;



end.

