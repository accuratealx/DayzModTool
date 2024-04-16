unit ParamFrameFileUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, StdCtrls, Buttons,
  StartParamSimple, StartParamFile,
  Language,
  ParamFrameSimpleUnit;

type
  TParamFrameFileFrame = class(TParamFrameSimpleFrame)
    btnClearValue: TSpeedButton;
    btnOpenDirectory: TSpeedButton;
    btnSelectFile: TSpeedButton;
    edValue: TEdit;
    OpenDialog: TOpenDialog;
    procedure btnClearValueClick(Sender: TObject);
    procedure btnOpenDirectoryClick(Sender: TObject);
    procedure btnSelectFileClick(Sender: TObject);
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
  DayZUtils;


procedure TParamFrameFileFrame.btnSelectFileClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := ExtractFilePath(edValue.Text);

  OpenDialog.InitialDir := Dir;
  if OpenDialog.Execute then
  begin
    SetValue(OpenDialog.FileName);
  end;
end;


procedure TParamFrameFileFrame.SetValue(AValue: String);
begin
  (FItem as TStartParamFile).Value := AValue;
  edValue.Text := AValue;
end;


procedure TParamFrameFileFrame.btnClearValueClick(Sender: TObject);
begin
  SetValue('');
end;


procedure TParamFrameFileFrame.btnOpenDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := ExtractFilePath(Trim((FItem as TStartParamFile).Value));
  if DirectoryExists(Dir) then
    OpenFolderInExplorer(Dir);
end;


procedure TParamFrameFileFrame.PrepareInterface(AItem: TStartParamSimple);
begin
  inherited PrepareInterface(AItem);

  edValue.Text := (AItem as TStartParamFile).Value;
end;


procedure TParamFrameFileFrame.ChangeLanguage(Language: TLanguage);
begin
  inherited ChangeLanguage(Language);

  btnOpenDirectory.Hint := Language.GetLocalizedString(LANGUAGE_PREFIX + 'OpenDirectory', 'Открыть каталог в проводнике');
  btnSelectFile.Hint := Language.GetLocalizedString(LANGUAGE_PREFIX + 'SelectFile', 'Выбрать файл');
  btnClearValue.Hint := Language.GetLocalizedString(LANGUAGE_PREFIX + 'ClearValue', 'Очистить значение');
end;



end.

