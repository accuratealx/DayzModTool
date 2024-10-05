unit ParamFrameDirectoryListUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls,
  StartParamSimple, StartParamDirectoryList,
  Language,
  ParamFrameSimpleUnit, ParamFrameDirectoryListModEditorUnit;

type
  TParamFrameDirectoryListFrame = class(TParamFrameSimpleFrame)
    btnOpenDirectory: TSpeedButton;
    btnReload: TSpeedButton;
    btnSelectDirectory: TSpeedButton;
    btnClearValue: TSpeedButton;
    cbFullPath: TCheckBox;
    edValue: TEdit;
    procedure btnClearValueClick(Sender: TObject);
    procedure btnOpenDirectoryClick(Sender: TObject);
    procedure btnReloadClick(Sender: TObject);
    procedure btnSelectDirectoryClick(Sender: TObject);
    procedure cbFullPathClick(Sender: TObject);
  private
    FOnHeightChange: TNotifyEvent;
    FModList: TParamFrameDirectoryListModEditorFrame;

    procedure SetValue(AValue: String);

    procedure UpdateHeight;
  protected
    procedure PrepareInterface(AItem: TStartParamSimple); override;

  public
    constructor Create(AItem: TStartParamSimple); reintroduce;
    destructor  Destroy; override;

    procedure ChangeLanguage(Language: TLanguage); override;

    property OnHeightChange: TNotifyEvent read FOnHeightChange write FOnHeightChange;
  end;


implementation

{$R *.lfm}

uses
  DayZUtils,
  SelectDirectoryDialogUnit;


procedure TParamFrameDirectoryListFrame.btnSelectDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := edValue.Text;
  if SelectDirectoryDialogExecute(FLanguage, Dir) then
    SetValue(Dir);
end;


procedure TParamFrameDirectoryListFrame.btnClearValueClick(Sender: TObject);
begin
  SetValue('');
end;


procedure TParamFrameDirectoryListFrame.btnOpenDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := Trim((FItem as TStartParamDirectoryList).Value);
  if DirectoryExists(Dir) then
    OpenFolderInExplorer(Dir);
end;


procedure TParamFrameDirectoryListFrame.btnReloadClick(Sender: TObject);
begin
  SetValue(edValue.Text);
end;


procedure TParamFrameDirectoryListFrame.cbFullPathClick(Sender: TObject);
begin
  (FItem as TStartParamDirectoryList).FullPath := cbFullPath.Checked;
end;


procedure TParamFrameDirectoryListFrame.SetValue(AValue: String);
begin
  AValue := Trim(AValue);
  (FItem as TStartParamDirectoryList).Value := AValue;
  edValue.Text := AValue;
  FModList.UpdateInterface;
  UpdateHeight;
end;


procedure TParamFrameDirectoryListFrame.UpdateHeight;
begin
  Self.Height := FModList.Top + FModList.Height + 5;

  if Assigned(FOnHeightChange) then
    FOnHeightChange(Self);
end;


procedure TParamFrameDirectoryListFrame.PrepareInterface(AItem: TStartParamSimple);
begin
  inherited PrepareInterface(AItem);

  edValue.Text := (AItem as TStartParamDirectoryList).Value;
  cbFullPath.Checked := (AItem as TStartParamDirectoryList).FullPath;

  FModList.UpdateInterface;
  UpdateHeight;
end;


constructor TParamFrameDirectoryListFrame.Create(AItem: TStartParamSimple);
begin
  FModList := TParamFrameDirectoryListModEditorFrame.Create((AItem as TStartParamDirectoryList).DirectoryList);

  inherited Create(AItem);

  FModList.Parent := Self;
  FModList.Top := edValue.Top + edValue.Height;
  FModList.Left := 180;
  FModList.Width := Self.Width - FModList.Left - 10;
  FModList.UpdateInterface;
  UpdateHeight;
end;


destructor TParamFrameDirectoryListFrame.Destroy;
begin
  FModList.Free;

  inherited Destroy;
end;


procedure TParamFrameDirectoryListFrame.ChangeLanguage(Language: TLanguage);
begin
  inherited ChangeLanguage(Language);

  //Подсказки кнопок
  btnOpenDirectory.Hint := Language.GetLocalizedString(LANGUAGE_PREFIX + 'OpenDirectory', 'Открыть каталог в проводнике');
  btnSelectDirectory.Hint := Language.GetLocalizedString(LANGUAGE_PREFIX + 'SelectDirectory', 'Выбрать каталог');
  btnClearValue.Hint := Language.GetLocalizedString(LANGUAGE_PREFIX + 'ClearValue', 'Очистить значение');
  btnReload.Hint := Language.GetLocalizedString(LANGUAGE_PREFIX + 'ReloadValue', 'Перезагрузить значение');

  //Полный путь
  cbFullPath.Caption := Language.GetLocalizedString(LANGUAGE_PREFIX + 'FullPath', 'Полный путь');

  //Список модов
  FModList.ChangeLanguage(FLanguage);
end;



end.

