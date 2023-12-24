unit ParamFrameDirectoryListUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  StartParamSimple, StartParamDirectoryList,
  ParamFrameSimpleUnit, ParamFrameDirectoryListModEditorUnit;

type
  TParamFrameDirectoryListFrame = class(TParamFrameSimpleFrame)
    btnOpenDirectory: TSpeedButton;
    btnSelectDirectory: TSpeedButton;
    btnClearValue: TSpeedButton;
    cbFullPath: TCheckBox;
    edValue: TEdit;
    procedure btnClearValueClick(Sender: TObject);
    procedure btnOpenDirectoryClick(Sender: TObject);
    procedure btnSelectDirectoryClick(Sender: TObject);
    procedure cbFullPathClick(Sender: TObject);
  private
    FModList: TParamFrameDirectoryListModEditorFrame;

    procedure SetValue(AValue: String);

    procedure UpdateHeight;
  protected
    procedure PrepareInterface(AItem: TStartParamSimple); override;

  public
    constructor Create(AItem: TStartParamSimple); reintroduce;
    destructor  Destroy; override;
  end;


implementation

{$R *.lfm}

uses
  DayZUtils, SelectDirectoryDialogUnit;


procedure TParamFrameDirectoryListFrame.btnSelectDirectoryClick(Sender: TObject);
var
  Dir: String;
begin
  Dir := edValue.Text;
  if SelectDirectoryDialogExecute('Выберите каталог', Dir) then
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


procedure TParamFrameDirectoryListFrame.cbFullPathClick(Sender: TObject);
begin
  (FItem as TStartParamDirectoryList).FullPath := cbFullPath.Checked;
end;


procedure TParamFrameDirectoryListFrame.SetValue(AValue: String);
begin
  (FItem as TStartParamDirectoryList).Value := AValue;
  edValue.Text := AValue;
  FModList.UpdateInterface;
  UpdateHeight;
end;


procedure TParamFrameDirectoryListFrame.UpdateHeight;
begin
  Self.Height := FModList.Top + FModList.Height + 5;
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

  FModList.Parent := ContentPanel;
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



end.

