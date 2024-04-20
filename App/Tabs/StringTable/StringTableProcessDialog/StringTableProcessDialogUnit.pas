unit StringTableProcessDialogUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Language;

type
  TStringTableProcessDialogForm = class(TForm)
    Image: TImage;
    lblMessage: TLabel;
    ImageTimer: TTimer;
    ImageList: TImageList;
    procedure FormShow(Sender: TObject);
    procedure ImageTimerTimer(Sender: TObject);
  private
    const
      LANGUAGE_PREFIX = 'Dialogs.StringTableItemEditor.';
  private
    FLanguage: TLanguage;
    FCurrentLanguage: String;
  public
    constructor Create(FormOwner: TComponent; Language: TLanguage); reintroduce;

    procedure SetCurrentLanguage(LngName: String);
  end;


procedure StringTableProcessDialogShow(Owner: TComponent; Language: TLanguage);
procedure StringTableProcessDialogHide;
procedure StringTableProcessDialogUpdateMessage(LanguageName: String);

implementation

{$R *.lfm}

var
  ProcessForm: TStringTableProcessDialogForm;


procedure StringTableProcessDialogShow(Owner: TComponent; Language: TLanguage);
begin
  if ProcessForm = nil then
    ProcessForm := TStringTableProcessDialogForm.Create(Owner, Language);

  ProcessForm.Show;
end;


procedure StringTableProcessDialogHide;
begin
  if ProcessForm <> nil then
  begin
    ProcessForm.Free;
    ProcessForm := nil;
  end;
end;


procedure StringTableProcessDialogUpdateMessage(LanguageName: String);
begin
  if ProcessForm <> nil then
    ProcessForm.SetCurrentLanguage(LanguageName);
end;


procedure TStringTableProcessDialogForm.FormShow(Sender: TObject);
begin
  ImageTimer.Enabled := True;
end;


procedure TStringTableProcessDialogForm.ImageTimerTimer(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := Image.ImageIndex;
  Inc(Idx);
  if Idx >= ImageList.Count - 1 then
    Idx := 0;

  Image.ImageIndex := idx;
end;


constructor TStringTableProcessDialogForm.Create(FormOwner: TComponent; Language: TLanguage);
begin
  inherited Create(FormOwner);

  FLanguage := Language;
  Caption := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'Progress.Caption', 'Пожалуйста подождите, перевожу...');
  FCurrentLanguage := FLanguage.GetLocalizedString(LANGUAGE_PREFIX + 'Progress.CurrentLanguage', 'Текущий язык');
end;


procedure TStringTableProcessDialogForm.SetCurrentLanguage(LngName: String);
begin
  lblMessage.Caption := FCurrentLanguage + ': ' + FLanguage.GetLocalizedString('StringTable.Column.' + LngName, LngName);
end;



end.

