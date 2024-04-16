unit ParamFrameDirectoryUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  StartParamSimple, StartParamDirectory,
  Language,
  ParamFrameSimpleUnit;

type
  TParamFrameDirectoryFrame = class(TParamFrameSimpleFrame)
    btnClearValue: TSpeedButton;
    btnSelectDirectory: TSpeedButton;
    btnOpenDirectory: TSpeedButton;
    edValue: TEdit;
    procedure btnClearValueClick(Sender: TObject);
    procedure btnOpenDirectoryClick(Sender: TObject);
    procedure btnSelectDirectoryClick(Sender: TObject);
  private
    procedure SetValue(AValue: String);
  protected
    procedure PrepareInterface(AItem: TStartParamSimple); override;
  public
    procedure ChangeLanguage(Language: TLanguage); override;
  end;


implementation

{$R *.lfm}

uses
  DayZUtils,
  SelectDirectoryDialogUnit;


procedure TParamFrameDirectoryFrame.btnSelectDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := ExtractFilePath(edValue.Text);
  if SelectDirectoryDialogExecute(FLanguage, Dir) then
    SetValue(Dir);
end;


procedure TParamFrameDirectoryFrame.SetValue(AValue: String);
begin
  (FItem as TStartParamDirectory).Value := AValue;
  edValue.Text := AValue;
end;


procedure TParamFrameDirectoryFrame.btnClearValueClick(Sender: TObject);
begin
  SetValue('');
end;


procedure TParamFrameDirectoryFrame.btnOpenDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := Trim((FItem as TStartParamDirectory).Value);
  if DirectoryExists(Dir) then
    OpenFolderInExplorer(Dir);
end;


procedure TParamFrameDirectoryFrame.PrepareInterface(AItem: TStartParamSimple);
begin
  inherited PrepareInterface(AItem);

  edValue.Text := (AItem as TStartParamDirectory).Value;
end;


procedure TParamFrameDirectoryFrame.ChangeLanguage(Language: TLanguage);
begin
  inherited ChangeLanguage(Language);

  btnOpenDirectory.Hint := Language.GetLocalizedString(LANGUAGE_PREFIX + 'OpenDirectory', 'Открыть каталог в проводнике');
  btnSelectDirectory.Hint := Language.GetLocalizedString(LANGUAGE_PREFIX + 'SelectDirectory', 'Выбрать каталог');
  btnClearValue.Hint := Language.GetLocalizedString(LANGUAGE_PREFIX + 'ClearValue', 'Очистить значение');
end;



end.

